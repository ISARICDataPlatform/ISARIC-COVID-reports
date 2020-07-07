



#' Import data from Redcap .csv output for processing
#' @param data.file Path of the data file
#' @param data.dict.file Path of the data dictionary file
#' @param column.table.file Path of the column translation file (default required_columns.csv)
#' @param source.name String identifier for the data source (optional)
#' @param message.out.file Path of a file to store messages about fields that were overwritten in the data cleaning process. If not given, this file is not generated
#' @param embargo.length Length of the data embargo in days; patients enrolled less than this period before \code{ref.date} will be excluded from reports. Default 0.
#' @param ref.date Date to be taken as the date of the report. Default is today's date.
#' @param yn.field.type The "Field Type" in the data dictionary for yes/no questions. Usually "radio" but sometimes "dropdown"
#' @param treatment.field.categories The "Form Name" entry or entries (will accept a vector of multiple values) in the data dictionary for treatments.
#' @param comorbidity.categories The "Form Name" entry or entries (will accept a vector of multiple values) in the data dictionary for comorbidities.
#' @param admission.symptom.field.categories The "Form Name" entry or entries (will accept a vector of multiple values) in the data dictionary for sypmtoms at admission.
#' @param overall.switch In some datasets (e.g. RAPID) treatment fields ending "cmyn", "occur" or "prtrt" refer to treatments given on the day of admission, rather than over the entire hospital stay. Overall stay treatments are in columns with "overall_" appending to the start of the name. This option switches these over.
#' @param verbose Flag for verbose output
#'
#' @return A list with components: \describe{
#' \item{unembargoed.data}{Data frame containing patients' records up to \code{ref.date}.}
#' \item{embargo.limit}{Embargo date; i.e. \code{ref.date} - \code{embargo.length}}
#' \item{detailed.data}{Data frame containing patients' records up to \code{embargo.limit}}
#' \item{cst.reference}{Data frame containing name and labels of the symptoms, combordities, and treatments considered.}
#' }
#' @import readr purrr stringr tidyr forcats
#' @importFrom glue glue
#' @importFrom lubridate ymd parse_date_time is.Date today
#' @importFrom magrittr not
#' @export import.and.process.data

import.and.process.data <- function(data.file,
                                    data.dict.file,
                                    column.table.file = NULL,
                                    source.name = NA,
                                    message.out.file = NULL,
                                    check.early.dates = TRUE,
                                    embargo.length = 0,
                                    ref.date = today(),
                                    yn.field.type = "radio", 
                                    treatment.field.categories = "treatment",
                                    comorbidity.categories = "comorbidities",
                                    admission.symptom.field.categories = "admission_signs_and_symptoms",
                                    overall.switch = FALSE,
                                    verbose = FALSE){
  
  if(verbose) cat("Reading data...\n")
  
  raw.data <- import.patient.data(data.file, data.dict.file, source.name, verbose)
  
  if(overall.switch){
    all.colnames <- colnames(raw.data)
    
    # RAPID xxx_cmyn fields are "on day of admission". overall_xxx_cmyn are "ever". Change these over.
    
    overall.col.exists <- map_lgl(all.colnames, function(cn){
      glue("overall_{cn}") %in% all.colnames & 
        (endsWith(cn, "cmyn") | endsWith(cn, "occur") | endsWith(cn, "prtrt"))
    })
    
    
    columns.for.replacement <- all.colnames[overall.col.exists]
    columns.to.replace <- glue("overall_{columns.for.replacement}")
    
    renamer <- function(cn){
      str_match(cn, "overall_(.*)")[,2]
    }
    
    raw.data <- raw.data %>%
      select(-any_of(columns.for.replacement)) %>%
      rename_with(.fn = renamer, .cols = any_of(columns.to.replace))
  }

  
  if(verbose) cat("Identifying symptoms, comorbidities and treatments...\n")
  
  cst.reference <- process.symptoms.comorbidities.treatments(data.dict.file, yn.field.type, treatment.field.categories, comorbidity.categories, admission.symptom.field.categories)
  
  if(verbose) cat("Renaming columns...\n")
  
  temp <- rename.and.drop.columns(raw.data, column.table.file, cst.reference)
  raw.data <- temp$data
  cst.reference <- temp$cst.reference
  
  raw.data <- raw.data %>% mutate(site.number = substr(redcap_data_access_group, 1, 3))
  
  if(!is.null(message.out.file)){
    write("oldvalue,new.value,column.name,subject.id,reason", file = message.out.file, append = FALSE)
  }
  
  raw.data <- pre.nest.sanity.checks(raw.data, message.out.file, check.early.dates, verbose)
  
  if(verbose) cat("Making patient data frame...\n")
  
  demog.data <- raw.data %>%
    group_by(subjid) %>%
    slice(1) %>%
    ungroup()
  
  # Join in a copy of all rows as the events column for each patient
  
  event.data <- raw.data %>%
    group_by(subjid) %>%
    nest() %>%
    dplyr::rename(events = data) %>%
    ungroup()
  
  if(verbose) cat("Joining events tables...\n")
  
  patient.data <- demog.data %>% left_join(event.data, by="subjid")
  
  patient.data <- patient.data %>%
    # cut out any rows where the IDs suggest test data
    mutate(probable.test = str_detect(subjid, "[tT][eE][sS][tT]"))
  
  for(probable.test.id in patient.data %>% filter(probable.test) %>% pull(subjid)){
    warning(glue("Probable test data for patient ID {probable.test.id}. Ignoring this ID.\n"))
    if(!is.null(message.out.file)){
      write(glue("all.values,NULL,all.columns,{probable.test.id},test.id"), file = message.out.file, append = TRUE)
    }
  }
  
  patient.data <- patient.data %>%
    filter(!probable.test) %>%
    dplyr::select(-probable.test)
  
  
  patient.data <- patient.data %>%
    mutate(multiple.exit.rows  = map2_lgl(subjid, events, function(y,x){
      outcome.rows <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm))
      nrow(outcome.rows) > 1
    }))
  
  for(probable.duplicate.id in patient.data %>% filter(multiple.exit.rows) %>% pull(subjid)){
    warning(glue("Probable duplicate patient ID {probable.duplicate.id}. Ignoring this ID.\n"))
    if(!is.null(message.out.file)){
      write(glue("all.values,NULL,all.columns,{probable.duplicate.id},duplicate.id"), file = message.out.file, append = TRUE)
    }
  }
  
  patient.data <- patient.data %>%
    filter(!multiple.exit.rows) %>%
    dplyr::select(-multiple.exit.rows)
  
  patient.data.output <- process.data(patient.data, cst.reference, ref.date - embargo.length, verbose)
  
  patient.data.output$cst.reference <- cst.reference
  patient.data.output$ref.date <- ref.date
  patient.data.output$embargo.limit <- ref.date - embargo.length
  
  patient.data.output$embargo.length <- embargo.length
  
  patient.data.output
  
}

#' @title
#' Generate a PDF report from the data.
#' 
#' @description
#' Generates a PDF report containing summaries of the data, including comparison of lengths of hospital stay by sex and age group, outcome by sex,
#' comorbidity, symptom and treatment distributions and distribution of vital signs on presentation at hospital. 
#' @param patient.data.output List output from \code{import.and.process.data}
#' @param file.name Path to a PDF file for the report
#' @param site.name Name of the site from which this data is derived
#' 
#' @return PDF report containing summaries of the data.
#' @import rmarkdown ggplot2 fitdistrplus boot survival tibble grid ggupset viridis binom
#' @importFrom filesstrings file.move
#' @importFrom gridExtra arrangeGrob
#' @importFrom psych phi
#' @importFrom lubridate epiweek
#' @export generate.report
generate.report <- function(patient.data.output, file.name, site.name){
  
  patient.data <- patient.data.output$detailed.data
  unembargoed.data <- patient.data.output$unembargoed.data
  
  cst.reference <- patient.data.output$cst.reference
  
  ref.date <- patient.data.output$ref.date
  embargo.limit <- patient.data.output$embargo.limit
  embargo.length <- patient.data.output$embargo.length
  
  admission.symptoms <- cst.reference %>% filter(type == "symptom")
  comorbidities <- cst.reference %>% filter(type == "comorbidity")
  treatments <- cst.reference %>% filter(type == "treatment")
  
  de <- d.e(patient.data, unembargoed.data, embargo.limit, comorbidities, admission.symptoms, treatments, site.name, embargo.length)
  
  report.rmd.file <- system.file("rmd", "COV-report.Rmd", package = "COVIDreportwriter")
  render(report.rmd.file, output_file=file.name)
  file.move(system.file("rmd", file.name, package = "COVIDreportwriter"), getwd(), overwrite = TRUE)
  
}

# There is a general problem with non-numeric entries in numerical columns. This function replaces them with NA with a warning

#' @export
#' @keywords internal
careful.as.numeric <- function(value, subjid, colname, message.out.file = NULL){
  out <- suppressWarnings(as.numeric(value))
  if(!is.na(value) & is.na(out)){
    warning(glue("Non-numerical value '{value}' transformed to NA for column {colname}, subject ID {subjid}"))
    if(!is.null(message.out.file)){
      write(glue("{as.character(value)},NA,{colname},{subjid},expected.numeric"), file = message.out.file, append = TRUE)
    }
  }
  out
}


#' @export
#' @keywords internal
pcareful.as.numeric <- function(value.col, subjid, colname, message.out.file = NULL){
  if(is.numeric(value.col)){
    value.col
  } else {
    map2_dbl(value.col, subjid, function(x, y){
      careful.as.numeric(x, y, colname, message.out.file)
    })
  }
}

# Checks for dates in the future and returns NA if they are

#' @export
#' @keywords internal
careful.date.check <- function(value, subjid, colname, check.early = F, message.out.file = NULL){
  if(is.na(value)) {
    return(NA)
  } else if(value > today()){
    warning(glue("Future date '{as.character(value)}' transformed to NA for column {colname}, subject ID {subjid}"))
    if(!is.null(message.out.file)){
      write(glue("{as.character(value)},NA,{colname},{subjid},in.future"), file = message.out.file, append = TRUE)
    }
    return(NA)
  } else if(value < ymd("2019-01-01") & check.early){
    warning(glue("Implausably early date '{as.character(value)}' transformed to NA for column {colname}, subject ID {subjid}"))
    if(!is.null(message.out.file)){
      write(glue("{as.character(value)},NA,{colname},{subjid},too.early"), file = message.out.file, append = TRUE)
    }
    return(NA)
  } else {
    return(as.Date(value, origin = "1970-01-01"))
  }
}

#' @export
#' @keywords internal
pcareful.date.check <- function(value.col, subjid, colname, check.early = FALSE, message.out.file = NULL){
  map2_dbl(value.col, subjid, function(x, y){
    careful.date.check(x, y, colname, check.early, message.out.file)
  })
}

# Checks for ages (strictly) between 0 and 1 and multiples by 100

#' @export
#' @keywords internal
careful.fractional.age <- function(value, subjid, colname, message.out.file = NULL){
  if(is.na(value)) {
    return(NA)
  } else if(0 < value & 1 > value){
    warning(glue("Fractional age {value} transformed to {value*100} for column {colname}, subject ID {subjid}"))
    if(!is.null(message.out.file)){
      write(glue("{as.character(value)},{value*100},{colname},{subjid},fractional.age"), file = message.out.file, append = TRUE)
    }
    return(100*value)
  }
  return(value)
}


#' @export extract.named.column.from.events
#' @keywords internal
extract.named.column.from.events <- function(events.tibble, column.name, sanity.check = F){
  out <- events.tibble %>% filter(!is.na(!!as.name(column.name))) %>% pull(column.name)
  
  if(length(out) > 1 & sanity.check){
    stop("Too many entries")
  } else if(length(out) == 0){
    NA
  } else {
    out
  }
}

# This is the function to return information about a patient's time on an intervention (e.g. IMV) from both daily and final forms

#' @export
#' @keywords internal
process.event.dates <- function(events.tbl, summary.status.name, daily.status.name){
  subtbl <- events.tbl %>% dplyr::select(dsstdat, daily_dsstdat, !!summary.status.name, !!daily.status.name )
  
  colnames(subtbl)[3:4] <- c("summary.col","daily.col")
  
  check.rows <- subtbl %>% filter(!is.na(summary.col) | !is.na(daily.col))
  if(nrow(check.rows) == 0){
    ever <- NA
  } else {
    ever <- any((check.rows$summary.col == 1) | (check.rows$daily.col == 1), na.rm = T)
  }
  
  
  rows <- subtbl %>% filter(!is.na(daily.col) & !(is.na(dsstdat) & is.na(daily_dsstdat))) %>%
    mutate(consolidated.dssdat = map2_chr(dsstdat, daily_dsstdat, function(x,y) ifelse(is.na(y), as.character(x), as.character(y)))) %>%
    mutate(consolidated.dssdat = ymd(consolidated.dssdat))
  
  if(nrow(rows) == 0){
    # no reference
    start.date <- NA
    end.date <- NA
    first.after.date <- NA
    multiple.periods <- NA
  } else if(!any(rows$daily.col == 1)){
    # no "yes"
    start.date <- NA
    end.date <- NA
    first.after.date <- NA
    multiple.periods <- NA
  } else {
    start.date <- rows %>% filter(daily.col == 1) %>% slice(1) %>% pull(consolidated.dssdat)
    last.date <- rows %>% filter(daily.col == 1) %>% slice(n()) %>% pull(consolidated.dssdat)
    if(is.na(start.date)){
      # sometimes happens. ever = TRUE but dates unknown
      end.date <- NA
      first.after.date <- NA
    } else if(last.date == rows %>% filter(!is.na(consolidated.dssdat)) %>% slice(n()) %>% pull(consolidated.dssdat)){
      # Patient was on at last report
      end.date <- NA
      first.after.date <- NA
    } else {
      # They were off at the next report
      end.date <- last.date
      
      next.report.row <- max(which(rows$consolidated.dssdat == last.date)) + 1
      first.after.date <- rows$consolidated.dssdat[next.report.row]
    }
    if(nrow(rows)<=2 | is.na(start.date)){
      multiple.periods <- F
    } else {
      # we are looking for the number of instances of 2 then 1. If this is more than 1, or more than 0 with the first report on NIMV,
      # then the patient went on multiple times
      temp <- map_dbl(2:nrow(rows), function(x)  rows$daily.col[x] - rows$daily.col[x-1] )
      multiple.periods <- length(which(temp == -1)) < 1 | (length(which(temp == -1)) > 0 &  rows$daily.col[1] == 1)
    }
  }
  list(ever = ever, start.date = start.date, end.date = end.date, first.after.date = first.after.date, multiple.periods = multiple.periods)
}


#' @export
#' @import dplyr purrr
#' @keywords internal
import.patient.data <- function(data.file,
                                data.dict.file,
                                source.name = NA,
                                verbose = F){
  
  if(verbose) {
    if(!is.na(source.name)){
      cat(glue("Importing data from source {source.name}..."),"\n")
    } else {
      cat("Importing data...\n")
    }
  }
  
  data.dict <- read_csv(data.dict.file, col_types = cols(), progress = FALSE)
  
  column.types <- data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)
  
  data <- read_csv(data.file, col_types = cols(), guess_max = 100000, progress = FALSE)
  
  # Columns that should be text
  text.columns.temp <- column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <- text.columns[which(text.columns %>% map_lgl(function(x) !is.Date(data %>% pull(x)) & !is.character(data %>% pull(x))))]
  
  # Columns that should be numerical
  nontext.columns.temp <- column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <- colnames(data)[which(map_lgl(colnames(data), function(x) any(startsWith(x, glue("{nontext.columns.temp}___")))))]
  nontext.columns <-intersect(colnames(data), c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <- nontext.columns[which(nontext.columns %>% map_lgl(function(x) !is.numeric(data %>% pull(x))))]
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  data <- data %>%
    # Columns that should be character are converted to character.
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character)
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA.
  # this may actually need a for loop!
  for(ntc in nontext.columns){
    data <- data %>% mutate_at(vars(tidyselect::all_of(ntc)), .funs = ~pcareful.as.numeric(., subjid = subjid, colname = ntc))
  }
  
  data <- data %>%
    dplyr::mutate(data.source = source.name)
  
  data
}

# This standardises column names from the lookup file and drops unncessary ones

#' @export
#' @keywords internal
rename.and.drop.columns <- function(data, column.table.file, cst.reference){
  
  if(!is.null(column.table.file)){
    column.table     <- read_csv(column.table.file, col_types = cols(), progress = FALSE)
    required.columns <- column.table$report.column.name
  } else {
    required.columns <- c('dsstdat', 'daily_dsstdat', 'daily_lbdat', 'hostdat', 'cestdat', 'dsstdtc', 'daily_fio2_lborres', 'age_estimateyears',
                          'hodur', 'invasive_prdur', 'subjid', 'liver_mhyn', 'chroniccard_mhyn', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn',
                          'modliver', 'mildliver', 'chronicneu_mhyn', 'malignantneo_mhyn', 'chronhaemo_mhyn', 'aidshiv_mhyn', 'obesity_mhyn',
                          'diabetescom_mhyn', 'diabetes_mhyn', 'rheumatologic_mhyn', 'dementia_mhyn', 'malnutrition_mhyn', 'smoking_mhyn',
                          'other_mhyn', 'fever_ceoccur_v2', 'cough_ceoccur_v2', 'cough_ceoccur_v2_2', 'coughsput_ceoccur_v2', 'coughhb_ceoccur_v2',
                          'sorethroat_ceoccur_v2', 'runnynose_ceoccur_v2', 'earpain_ceoccur_v2', 'wheeze_ceoccur_v2', 'chestpain_ceoccur_v2',
                          'myalgia_ceoccur_v2', 'jointpain_ceoccur_v2', 'fatigue_ceoccur_v2', 'shortbreath_ceoccur_v2', 'lowerchest_ceoccur_v2',
                          'headache_ceoccur_v2', 'confusion_ceoccur_v2', 'seizures_cecoccur_v2', 'abdopain_ceoccur_v2', 'vomit_ceoccur_v2',
                          'diarrhoea_ceoccur_v2', 'diabetes_mhyn_2', 'conjunct_ceoccur_v2', 'rash_ceoccur_v2', 'skinulcers_ceoccur_v2', 'lymp_ceoccur_v2',
                          'bleed_ceoccur_v2', 'antiviral_cmyn', 'antibiotic_cmyn', 'corticost_cmyn', 'antifung_cmyn', 'oxygen_cmoccur',
                          'noninvasive_proccur', 'invasive_proccur', 'pronevent_prtrt', 'inhalednit_cmtrt', 'tracheo_prtrt', 'extracorp_prtrt',
                          'rrt_prtrt', 'inotrop_cmtrt', 'other_cmyn', 'agedat', 'pregyn_rptestcd', 'redcap_event_name', 'dsterm', 'dsstdtcyn',
                          'icu_hostdat', 'icu_hoendat', 'hodur', 'invasive_prdur', 'antiviral_cmyn', 'antiviral_cmtrt___1', 'antiviral_cmtrt___2',
                          'antiviral_cmtrt___3', 'antiviral_cmtrt___4', 'antiviral_cmtrt___5', 'antiviral_cmtrt___6', 'antiviral_cmtype',
                          'antibiotic_cmyn', 'antifung_cmyn', 'corticost_cmyn', 'noninvasive_proccur', 'daily_noninvasive_prtrt',
                          'invasive_proccur', 'daily_invasive_prtrt', 'extracorp_prtrt', 'daily_ecmo_prtrt', 'icu_hoterm', 'daily_hoterm',
                          'rrt_prtrt', 'daily_rrt_cmtrt', 'inotrop_cmtrt', 'daily_inotrope_cmyn', 'daily_fio2_lborres', 'daily_nasaloxy_cmtrt',
                          'daily_nasaloxy_cmtrt', 'corna_mbcat', 'corna_mbcaty', 'coronaother_mborres', 'mborres', 'mbtestcd',
                          'redcap_data_access_group', 'rheumatologic_mhyn', 'sex', 'healthwork_erterm', 'rr_vsorres', 'oxy_vsorres',
                          'oxy_vsorresu', 'hr_vsorres', 'sysbp_vsorres', 'temp_vsorres', 'temp_vsorresu', 'daily_crp_lborres',
                          'daily_bun_lborres', 'daily_bun_lborresu', 'daily_wbc_lborres', 'daily_wbc_lborresu', 'daily_pt_lborres',
                          'ddimer_lborres', 'daily_bil_lborresu', 'daily_aptt_lborres', 'daily_lymp_lborres', 'daily_neutro_lborres',
                          'daily_alt_lborres', 'daily_ast_lborres', 'daily_bil_lborres', 'daily_bun_lborresu', 'daily_bil_lborresu')
  }
  
  
  for(i in 1:length(required.columns)){
    if(!is.null(column.table.file)){
      old.name <- column.table$redcap.column.name[i]
    } else {
      old.name <- required.columns[i]
    }
    new.name <- required.columns[i]
    if(!new.name %in% colnames(data)){
      if(!old.name %in% colnames(data)){
        warning(glue("Required column {new.name} not found or mapped. Adding an empty column with this name."))
        # we need this column even if it is all empty
        data <- data %>%
          mutate(!!(new.name) := NA)
      } else {
        # rename the old column name with the new
        data <- data %>%
          rename_at(vars(all_of(old.name)), ~new.name)
      }
    }
    
    cst.reference <- cst.reference %>%
      mutate(field = replace(field, field == old.name, new.name))
    
    cst.reference <- cst.reference %>%
      filter(field %in% required.columns | derived)
  }
  
  
  
  data <- data %>% dplyr::select(-setdiff(colnames(data), required.columns))
  
  list(data = data, cst.reference = cst.reference)
}



# This makes the symptom, comorbidity and treatment reference table

#' @export
#' @keywords internal
process.symptoms.comorbidities.treatments <- function(data.dict.file, 
                                                      yn.field.type = "radio", 
                                                      treatment.field.categories = "treatment",
                                                      comorbidity.categories = "comorbidities",
                                                      admission.symptom.field.categories = "admission_signs_and_symptoms"){
  
  d.dict <- read_csv(data.dict.file, col_types = cols(), progress= FALSE) %>%
    # filter(is.na(`Field Annotation`) | `Field Annotation` != "@HIDDEN") %>%
    dplyr::select(`Variable / Field Name`,`Form Name`, `Field Type`, `Field Label`) %>%
    dplyr::rename(field.name = `Variable / Field Name`, form.name = `Form Name`, field.type = `Field Type`, field.label = `Field Label`)
  
  
  comorbidities.colnames <- d.dict %>% filter(form.name %in% comorbidity.categories & field.type == yn.field.type) %>% pull(field.name)
  
  admission.symptoms.colnames <- d.dict %>% 
    filter(form.name %in% admission.symptom.field.categories &
             field.type == yn.field.type &
             str_detect(field.name, "ceoccur") ) %>%
    pull(field.name)
  
  treatment.colnames <- d.dict %>% filter(form.name %in% treatment.field.categories & 
                                            field.type == yn.field.type &  
                                            field.name != "oxygen_cmdose" &
                                            field.label != "Would you like to add another antibiotic?") %>% pull(field.name)
  
  comorbidities.labels <- d.dict %>%
    filter(form.name %in% comorbidity.categories & field.type == yn.field.type) %>%
    pull(field.label) %>%
    map_chr(function(x) {
      if(startsWith(x, "<")){
        temp <- str_replace(x, '\".*\"', "")
        
        str_match(temp, '<acronym title=>(.*)</acronym>')[,2]
      } else {
        x
      }
    }) %>%
    map_chr(function(x) str_split_fixed(x, " <i>\\(", Inf)[1]) %>%
    map_chr(function(x) str_split_fixed(x, " \\(", Inf)[1]) %>%
    map_chr(function(x) sub("\\s+$", "", x))
  
  comorbidities <- tibble(field = comorbidities.colnames, label = comorbidities.labels, derived = FALSE)
  
  comorbidities <- bind_rows(comorbidities, tibble(field = "pregnancy", label = "Pregnancy", derived = TRUE))
  comorbidities <- comorbidities %>% bind_rows(list(field = "liver.disease", label = "Liver disease", derived = TRUE)) %>%
    filter(field != "mildliver" & field != "modliver")
  comorbidities <- comorbidities %>% bind_rows(list(field = "diabetes", label = "Diabetes", derived = TRUE))  %>%
    filter(field != "diabetes_mhyn" & field != "diabetes_mhyn_2" & field != "diabetescom_mhyn_2")
  
  comorbidities <- comorbidities %>% mutate(type = "comorbidity")
  
  admission.symptoms.labels <- d.dict %>%
    filter(form.name %in% admission.symptom.field.categories &
             field.type == yn.field.type &
             str_detect(field.name, "ceoccur") ) %>%
    pull(field.label) %>%
    map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
    map_chr(function(x) sub("\\s+$", "", x)) 
  
  admission.symptoms <- tibble(field = admission.symptoms.colnames, label = admission.symptoms.labels, derived = FALSE)
  
  admission.symptoms <- admission.symptoms %>% bind_rows(list(field = "shortness.breath", label = "Shortness of breath", derived = TRUE)) %>%
    filter(field != "shortbreath_ceoccur_v2" & field != "lowerchest_ceoccur_v2")
  
  admission.symptoms <- admission.symptoms %>% bind_rows(list(field = "cough.nosputum", label = "Cough (no sputum)", derived = TRUE)) %>%
    bind_rows(list(field = "cough.sputum", label = "Cough (with sputum)", derived = TRUE)) %>%
    bind_rows(list(field = "cough.bloodysputum", label = "Cough (bloody sputum / haemoptysis)", derived = TRUE)) %>%
    filter(field != "cough_ceoccur_v2" & field != "coughsput_ceoccur_v2" & field !="coughhb_ceoccur_v2")
  
  admission.symptoms <- admission.symptoms %>% mutate(type = "symptom")
  
  treatment.labels <- d.dict %>%
    filter(form.name %in% treatment.field.categories & 
             field.type == yn.field.type & 
             field.name != "oxygen_cmdose" &
             field.label != "Would you like to add another antibiotic?") %>%
    pull(field.label) %>%
    # str_match(pattern = "6\\.[0-9]+[\\.]?[0-9]?[\\.]?\\s(.*)") %>%
    map_chr(function(x) {
      if(startsWith(x, "<")){
        temp <- str_replace(x, '\".*\"', "")
        
        str_match(temp, '<acronym title=>(.*)</acronym>')[,2]
      } else {
        x
      }
    }) %>%
    map_chr(function(x) str_split_fixed(x, " <i>\\(", Inf)[1]) %>%
    map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
    # I don't know why you can't figure this out nicely. Do it later.
    map_chr(function(x) sub("\\s+$", "", x)) %>%
    map_chr(function(x) sub("\\?+$", "", x)) %>%
    map_chr(function(x) sub("\\s+$", "", x)) 
  
  
  treatments <- tibble(field = treatment.colnames, label = treatment.labels, type = "treatment", derived = FALSE)
  
  bind_rows(comorbidities, admission.symptoms, treatments)
}


# Function to check for text that looks like it means SARS-CoV-2. Lots of typos in these fields. This may catch other coronaviruses.


#' @export
#' @keywords internal
probable.cov.freetext <- function(text){
  str_detect(text, "[sS][aA][rR][sS]") |
    str_detect(text, "[cC][oO]r?[vV]") |
    str_detect(text, "[nN][cC][oO][cC]") |
    str_detect(text, "[nN][cC][vV][oO]") |
    str_detect(text, "[cC][oO][iI][vV][dD]") |
    str_detect(text, "[cC][vV][iI][dD]") |
    str_detect(text, "[cC][oO][vV][oO][iI][dD]") |
    str_detect(text, "[cC]or?n?o?n[ao]vir[iu]s") |
    str_detect(text, "[cC]OR?N?O?N[AO]VIR[IU]S") |
    str_detect(text, "[cC]or?n?o?n[ao]\\s[Vv]ir[iu]s") |
    str_detect(text, "[cC]OR?N?O?N[AO]\\sVIR[IU]S") |
    str_detect(text, "[Cc][Oo][Rr][Oo][Nn][Aa]")
}

#' @export
#' @keywords internal
pre.nest.sanity.checks <- function(data, message.output.file = NULL, check.early.dates = TRUE, verbose = FALSE){
  if(verbose) {
    if(check.early.dates){
      cat("Setting future dates and implausibly distant past dates to NA...\n")
    } else {
      cat("Setting future dates to NA...\n")
    }
  }
  date.columns <- c("dsstdat", "daily_dsstdat", "daily_lbdat", "hostdat", "cestdat", "dsstdtc")
  
  for(dc in c(date.columns, "agedat")){
    data <- data %>% mutate_at(vars(all_of(dc)), function(x) parse_date_time(x, orders = c("ymd", "dmy", "mdy")))
  }
  
  
  for(dc in date.columns){
    data <- data %>% mutate_at(vars(all_of(dc)), .funs = ~pcareful.date.check(.,
                                                                              subjid = subjid,
                                                                              colname = dc,
                                                                              check.early = check.early.dates,
                                                                              message.out.file = message.output.file))
  }
  
  for(dc in "agedat"){
    data <- data %>% mutate_at(vars(all_of(dc)), .funs = ~pcareful.date.check(.,
                                                                              subjid = subjid,
                                                                              colname = dc,
                                                                              check.early = FALSE,
                                                                              message.out.file = message.output.file))
  }
  
  data <- data %>%
    mutate_at(c(date.columns, "agedat"), function(x) as.Date(x, origin = "1970-01-01"))
  
  
  # Now, some fields need to be numerical even if the data dictionary does not think they are.
  # These are just the ones of those that we _currently use_. Others should be added as required.
  
  if(verbose) cat("Manually adjusting some fields...\n")
  
  data <- data %>%
    mutate(daily_fio2_lborres = map2_dbl(subjid, daily_fio2_lborres, function(x, y) careful.as.numeric(y, x, "daily_fio2_lborres", message.output.file))) %>%
    mutate(age_estimateyears = map2_dbl(subjid, age_estimateyears, function(x, y) careful.as.numeric(y, x, "age_estimateyears", message.output.file))) %>%
    mutate(hodur = map2_dbl(subjid, hodur, function(x, y) careful.as.numeric(y, x, "hodur", message.output.file))) %>%
    mutate(invasive_prdur = map2_dbl(subjid, invasive_prdur, function(x, y) careful.as.numeric(y, x, "invasive_prdur", message.output.file)))
  
  # Replace the fractional ages
  
  data <- data %>%  mutate(age_estimateyears = map2_dbl(age_estimateyears, subjid, function(x, y){
    careful.fractional.age(x, y, "age_estimateyears", message.output.file)
  }))
  
  data
}

#' @export
#' @keywords internal
process.data <- function(data,
                         cst.reference,
                         embargo.limit = today(),
                         verbose = F){
  
  
  admission.symptoms <- cst.reference %>% filter(type == "symptom")
  comorbidities <- cst.reference %>% filter(type == "comorbidity")
  treatments <- cst.reference %>% filter(type == "treatment")
  
  patient.data <- data
  
  # Group liver disease categories
  
  patient.data <- patient.data %>%
    mutate(liver.disease = pmap_dbl(list(mildliver, modliver, liver_mhyn), function(mild, moderate, any){
      if(is.na(mild) & is.na(moderate) & is.na(any)){
        NA
      } else if(!is.na(any)){
        any
      } else if(is.na(mild)){
        moderate
      } else if(is.na(moderate)){
        mild
      } else if(mild == 1 | moderate == 1){
        1
      } else if(mild == 2 & moderate == 2){
        2
      } else {
        3
      }
    }))
  
  
  # Group diabetes categories
  patient.data <- patient.data %>%
    mutate(diabetes = pmap_dbl(list(diabetes_mhyn, diabetes_mhyn_2, diabetescom_mhyn), function(diab1, diab2, diab1c){
      if(is.na(diab1) & is.na(diab2) & is.na(diab1c)){
        NA
      } else if((!is.na(diab2) & diab2 == 3) | ((!is.na(diab1) & diab1 == 3) & (!is.na(diab1c) & diab1c == 3))){
        NA
      } else if( (!is.na(diab1) & diab1 == 1) | (!is.na(diab1c) & diab1c == 1) | (!is.na(diab2) & (diab2 == 1 | diab2 == 4))){
        1
      } else {
        2
      }
    }))
  
  # Cough records may have not been entered coherently, and are recoded to be mutually exclusive.
  # Someone with a cough with sputum does not have a cough without it
  
  patient.data <- patient.data %>%
    mutate(cough.cols = pmap(list(cough_ceoccur_v2_2, cough_ceoccur_v2, coughsput_ceoccur_v2, coughhb_ceoccur_v2), function(c2, c1.c, c1.s, c1.b){
      if(is.na(c2) & any(is.na(c(c1.c, c1.s, c1.b)))){
        cough.nosputum <- NA
        cough.sputum <- NA
        cough.bloodysputum <- NA
      } else if(!is.na(c2)){
        if(c2 == 4){
          cough.nosputum <- NA
          cough.sputum <- NA
          cough.bloodysputum <- NA
        } else if(c2 == 0){
          cough.nosputum <- 2
          cough.sputum <- 2
          cough.bloodysputum <- 2
        } else if(c2 == 1){
          cough.nosputum <- 1
          cough.sputum <- 2
          cough.bloodysputum <- 2
        } else if(c2 == 2){
          cough.nosputum <- 2
          cough.sputum <- 1
          cough.bloodysputum <- 2
        } else {
          cough.nosputum <- 2
          cough.sputum <- 2
          cough.bloodysputum <- 1
        } 
      } else {
        if(any(c(c1.c, c1.s, c1.b) == 3) | any(is.na(c(c1.c, c1.s, c1.b)))){
          cough.nosputum <- NA
          cough.sputum <- NA
          cough.bloodysputum <- NA
        } else if(all(c(c1.c, c1.s, c1.b) == 2)){
          cough.nosputum <- 2
          cough.sputum <- 2
          cough.bloodysputum <- 2
        } else if(c1.s == 1){
          cough.nosputum <- 2
          if(c1.b == 1){
            cough.sputum <- 2
            cough.bloodysputum <- 1
          } else {
            cough.sputum <- 1
            cough.bloodysputum <- 2
          }
        } else if(c1.b == 1) {
          cough.nosputum <- 2
          cough.sputum <- 2
          cough.bloodysputum <- 1
        } else {
          cough.nosputum <- c1.c
          cough.sputum <- 2
          cough.bloodysputum <- 2
        }
      }
      list(cough.sputum = cough.sputum, cough.nosputum = cough.nosputum, cough.bloodysputum = cough.bloodysputum)
    })) %>%
    bind_cols(., bind_rows(!!!.$cough.cols)) %>%
    dplyr::select(-cough.cols) %>%
    mutate(cough.any = pmap_dbl(list(cough.nosputum, cough.sputum, cough.bloodysputum), function(x,y,z){
      if(is.na(x)){
        NA
      } else {
        if(all(c(x,y,z) == 2)){
          2
        } else {
          1
        }
      }
    }))
  
  # Group shortness of breath categories
  
  patient.data <- patient.data %>%
    mutate(shortness.breath = map2_dbl(shortbreath_ceoccur_v2, lowerchest_ceoccur_v2, function(adult, paed){
      if(is.na(adult) & is.na(paed)){
        NA
      } else if(is.na(adult)){
        paed
      } else if(is.na(paed)){
        adult
      } else if(adult == 1 | paed == 1){
        1
      } else if(adult == 2 & paed == 2){
        2
      } else {
        2
      }
    }))
  
  
  # Add new columns with more self-explanatory names as needed
  
  if(verbose) cat("Adding new columns...\n")
  
  patient.data <- patient.data %>%
    # Consolidated age is the exact age at enrolment if this is present. Otherwise it is taken from the estimated age column.
    dplyr::mutate(consolidated.age = pmap_dbl(list(age_estimateyears, agedat, dsstdat), function(ageest, dob, doa){
      if(is.na(dob)){
        ageest
      } else {
        floor(decimal_date(doa) - decimal_date(dob))
      }
    })) %>%
    # Age groups in five and ten year incerements
    dplyr::mutate(agegp5 = cut(consolidated.age, c(seq(0,90,by = 5),120), right = FALSE)) %>%
    dplyr::mutate(agegp5 = fct_relabel(agegp5, function(a){
      # make nicer labels
      temp <- substr(a, 2, nchar(a) -1 )
      newlabels <- map_chr(temp, function(x){
        components <- as.numeric(str_split_fixed(x, ",", Inf))
        components[2] <- components[2] - 1
        paste(components, collapse = "-")
      })
      str_replace(newlabels, "90-119", "90+")
    })) %>%
    dplyr::mutate(agegp10 = cut(consolidated.age, c(seq(0,70,by = 10),120), right = FALSE)) %>%
    dplyr::mutate(agegp10 = fct_relabel(agegp10, function(a){
      # make nicer labels
      temp <- substr(a, 2, nchar(a) -1 )
      newlabels <- map_chr(temp, function(x){
        components <- as.numeric(str_split_fixed(x, ",", Inf))
        components[2] <- components[2] - 1
        paste(components, collapse = "-")
      })
      str_replace(newlabels, "70-119", "70+")
    }))
  
  patient.data <- patient.data %>%
    mutate(pregnancy = pmap_dbl(list(pregyn_rptestcd, sex, consolidated.age), function(preg, sx, age){
      if(is.na(preg)){
        # use the same rules as the UK data dictionary
        if(!is.na(sx) & sx == 1){
          2
        } else if(!is.na(age) & (age < 12 | age > 55)){
          2
        } else {
          3
        }
      } else if(preg == 999){
        2
      } else if(preg == 998) {
        3
      } else if(preg == 0){
        2
      } else {
        preg
      }
    }))
  
  patient.data <- patient.data %>%
    # check if symptoms, comorbidities and treatments were actually recorded
    dplyr::mutate(symptoms.recorded = pmap_lgl(list(!!!rlang::parse_exprs(admission.symptoms$field)), ~any(!is.na(c(...))))) %>%
    dplyr::mutate(comorbidities.recorded = pmap_lgl(list(!!!rlang::parse_exprs(comorbidities$field)), ~any(!is.na(c(...))))) %>%
    dplyr::mutate(treatments.recorded = map_lgl(events, function(x){
      temp <- x %>% mutate(tr = pmap_lgl(list(!!!rlang::parse_exprs(treatments$field)), ~any(!is.na(c(...)))))
      any(temp$tr)
    }))
  
  
  patient.data <- patient.data %>%
    # exit date is whenever the patient leaves the site. @todo look at linking up patients moving between sites
    dplyr::mutate(exit.date = map2_chr(subjid, events, function(y, x){
      outcome.rows <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsstdtc))
      if(nrow(outcome.rows) == 0){
        return(NA)
      } else {
        if(nrow(outcome.rows) > 1) {
          warning(glue("Multiple exit dates for patient {y}"))
        }
        return(outcome.rows  %>% slice(nrow(outcome.rows)) %>% pull(dsstdtc) %>% as.character())
      }
    })) %>%
    dplyr::mutate(exit.date = ymd(exit.date)) %>%
    # exit code is the reason for leaving the site.
    dplyr::mutate(exit.code = map2_chr(subjid, events, function(y,x){
      outcome.rows <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm))
      if(nrow(outcome.rows) == 0){
        return(NA)
      } else {
        return(switch(outcome.rows %>% pull(dsterm) %>% as.character(),
                      "1" = "discharge",
                      "2" = "hospitalisation",
                      "3" = "transfer",
                      "4" = "death",
                      "5" = "transfer.palliative",
                      "6" = "unknown"))
      }
    })) %>%
    dplyr::mutate(exit.code = factor(exit.code, levels = c("discharge", "hospitalisation", "transfer", "death", "transfer.palliative", "unknown"))) %>%
    # censorship occurs if the patient is still in site
    dplyr::mutate(censored = map2_lgl(subjid, events, function(y, x){
      if(x %>% pull(redcap_event_name) %>% startsWith("discharge") %>% any() %>% not()){
        # still in site
        return(TRUE)
      } else {
        temp <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm))
        if(nrow(temp) == 0){
          return(TRUE)
        } else {
          return(temp %>% pull(dsterm) %>% is.na() %>% any())
        }
      }
    }))
  
  patient.data <- patient.data %>%
    # outcome is death, discharge or other for transfers etc
    dplyr::mutate(outcome = map2_chr(censored, events, function(x, y){
      if(x){
        return("censored")
      } else {
        return(switch(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsterm) %>% as.character(),
                      "1" = "discharge",
                      "4" = "death",
                      NA))
      }
    })) %>%
    dplyr::mutate(outcome.date.known = map2_dbl(outcome, events, function(x, y){
      if(!(x %in% c("discharge", "death"))){
        return(2)
      } else {
        return(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtcyn))
      }
    })) %>%
    dplyr::mutate(outcome.date = map2_chr(outcome, events, function(x, y){
      if(!(x %in% c("discharge", "death"))){
        return(NA)
      } else {
        if(length(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtc) %>% as.character()) > 1){
          stop("Multiple outcome dates?")
        }
        return(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtc) %>% as.character())
      }
    })) %>%
    # oh for map_date!!!
    dplyr::mutate(outcome.date = ymd(outcome.date)) %>%
    # Death and discharge dates are NA if the patient is not dead/discharged
    dplyr::mutate(death.date = map2_chr(outcome, outcome.date, function(x,y){
      if(is.na(y)){
        NA
      }
      ifelse(x=="death", as.character(y), NA)
    })) %>%
    dplyr::mutate(death.date = ymd(death.date)) %>%
    dplyr::mutate(discharge.date = map2_chr(outcome, outcome.date, function(x,y){
      if(is.na(y)){
        NA
      }
      ifelse(x=="discharge", as.character(y), NA)
    }))  %>%
    dplyr::mutate(discharge.date = ymd(discharge.date))
  
  patient.data <- patient.data %>%
    dplyr::mutate(ICU.start.date = map_chr(events, function(x) as.character(extract.named.column.from.events(x, "icu_hostdat", TRUE)))) %>%
    dplyr::mutate(ICU.start.date = ymd(ICU.start.date)) %>%
    dplyr::mutate(ICU.end.date = map_chr(events, function(x) as.character(extract.named.column.from.events(x, "icu_hoendat", TRUE)))) %>%
    dplyr::mutate(ICU.end.date = ymd(ICU.end.date)) %>%
    dplyr::mutate(ICU.duration = map_dbl(events, function(x) extract.named.column.from.events(x, "hodur", TRUE))) %>%
    dplyr::mutate(IMV.duration  = map_dbl(events, function(x) extract.named.column.from.events(x, "invasive_prdur", TRUE))) %>%
    # these are just for the sake of having more self-explanatory column names
    dplyr::mutate(admission.date = hostdat) %>%
    dplyr::mutate(enrolment.date = dsstdat) %>%
    dplyr::mutate(onset.date = cestdat) %>%
    # start.date is either the admission date or the date of symptom onset, whichever is _later_ - hospital cases are counted from disease onset
    dplyr::mutate(start.date = map2_chr(admission.date, onset.date, function(x,y){
      suppressWarnings(as.character(max(x,y, na.rm = T)))
    })) %>%
    dplyr::mutate(start.date = ymd(start.date))
  
  patient.data <- patient.data %>%
    dplyr::mutate(antiviral.any = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmyn", TRUE) )) %>%
    dplyr::mutate(antiviral.Ribavirin = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___1", TRUE) )) %>%
    dplyr::mutate(antiviral.Lopinavir.Ritonvir = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___2", TRUE) )) %>%
    dplyr::mutate(antiviral.Interferon.alpha = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___3", TRUE) )) %>%
    dplyr::mutate(antiviral.Interferon.beta = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___4", TRUE) )) %>%
    dplyr::mutate(antiviral.Neuraminidase.inhibitors = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___5", TRUE) )) %>%
    dplyr::mutate(antiviral.other = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___6", TRUE) ))   %>%
    dplyr::mutate(antiviral.freetext = map_chr(events, function(x) extract.named.column.from.events(x, "antiviral_cmtype", TRUE) )) %>%
    dplyr::mutate(antibiotic.any = map_dbl(events, function(x) extract.named.column.from.events(x, "antibiotic_cmyn", TRUE) )) %>%
    dplyr::mutate(antifungal.any = map_dbl(events, function(x) extract.named.column.from.events(x, "antifung_cmyn", TRUE) )) %>%
    dplyr::mutate(steroid.any = map_dbl(events, function(x) extract.named.column.from.events(x, "corticost_cmyn", TRUE) ))
  
  ###### Date wrangling ######
  
  if(verbose) cat("Wrangling dates for treatment modalities...\n")
  
  patient.data <- patient.data %>%
    # NIV
    mutate(NIMV.cols  = map(events, function(el){
      process.event.dates(el, "noninvasive_proccur", "daily_noninvasive_prtrt")
    })) %>%
    mutate(NIMV.cols = map(NIMV.cols, function(x){
      names(x) <- glue("NIMV.{names(x)}")
      x
    })) %>%
    { bind_cols(., bind_rows(!!!.$NIMV.cols)) } %>%
    dplyr::select(-NIMV.cols) %>%
    # IMV
    mutate(IMV.cols  = map2(subjid, events, function(id, el){
      process.event.dates(el, "invasive_proccur", "daily_invasive_prtrt")
    })) %>%
    mutate(IMV.cols = map(IMV.cols, function(x){
      names(x) <- glue("IMV.{names(x)}")
      x
    })) %>%
    { bind_cols(., bind_rows(!!!.$IMV.cols)) } %>%
    dplyr::select(-IMV.cols) %>%
    # ECMO
    mutate(ECMO.cols  = map2(subjid, events, function(id, el){
      process.event.dates(el, "extracorp_prtrt", "daily_ecmo_prtrt")
    })) %>%
    mutate(ECMO.cols = map(ECMO.cols, function(x){
      names(x) <- glue("ECMO.{names(x)}")
      x
    })) %>%
    { bind_cols(., bind_rows(!!!.$ECMO.cols)) } %>%
    dplyr::select(-ECMO.cols) %>%
    # ICU - we already have this information incompletely from icu_hostdat, icu_hoendat and hostdur,
    # so these columns are "ICU2" and we consolidate later
    mutate(ICU.cols  = map(events, function(el){
      process.event.dates(el, "icu_hoterm", "daily_hoterm")
    })) %>%
    mutate(ICU.cols = map(ICU.cols, function(x){
      names(x) <- glue("ICU2.{names(x)}")
      x
    })) %>%
    { bind_cols(., bind_rows(!!!.$ICU.cols)) } %>%
    dplyr::select(-ICU.cols) %>%
    # RRT
    mutate(RRT.cols  = map(events, function(el){
      process.event.dates(el, "rrt_prtrt", "daily_rrt_cmtrt")
    })) %>%
    mutate(RRT.cols = map(RRT.cols, function(x){
      names(x) <- glue("RRT.{names(x)}")
      x
    })) %>%
    # Inotrope
    { bind_cols(., bind_rows(!!!.$RRT.cols)) } %>%
    dplyr::select(-RRT.cols) %>%
    mutate(Inotrope.cols  = map(events, function(el){
      process.event.dates(el, "inotrop_cmtrt", "daily_inotrope_cmyn")
    })) %>%
    mutate(Inotrope.cols = map(Inotrope.cols, function(x){
      names(x) <- glue("Inotrope.{names(x)}")
      x
    })) %>%
    { bind_cols(., bind_rows(!!!.$Inotrope.cols)) } %>%
    dplyr::select(-Inotrope.cols)
  
  # O2 ever - more complex
  
  patient.data <- patient.data %>%
    mutate(O2.ever = map_lgl(events, function(x){
      
      x2 <- x %>% filter(!is.na(daily_fio2_lborres) | !is.na(daily_nasaloxy_cmtrt) | !is.na(daily_nasaloxy_cmtrt))
      if(nrow(x2) == 0){
        NA
      } else {
        x2 <- x2 %>% mutate(O2.ever = daily_fio2_lborres > .21 | daily_nasaloxy_cmtrt == 1 | oxygen_cmoccur == 1)
        any(x2$O2.ever, na.rm = T)
      }
    }))
  
  # ICU.start.data and ICU.end.date hae values from daily sheets but omit some
  # values from the outcome sheet. Where available, the outcome sheet ispreferred.
  
  patient.data <- patient.data %>%
    # ICU.ever is either ICU2.ever or non-NA icu_hoterm. At the moment, NA ICU2.evers lead to NA ICU.evers in the absence of a icu_hoterm.
    # It's possible that they should be FALSE instead
    mutate(ICU.ever = !is.na(ICU.start.date) | ICU2.ever) %>%
    # mutate(ICU.ever2 = !is.na(ICU.start.date) | !(is.na(ICU2.ever)  | !ICU2.ever)) %>%
    mutate(ICU.start.date = replace(ICU.start.date, is.na(ICU.start.date), ICU2.start.date[which(is.na(ICU.start.date))])) %>%
    mutate(ICU.end.date = replace(ICU.end.date, is.na(ICU.end.date), ICU2.end.date[which(is.na(ICU.end.date))])) %>%
    mutate(ICU.multiple.periods = ICU2.multiple.periods)
  
  # if we can get those from the daily forms then we can get this
  patient.data$ICU.duration[is.na(patient.data$ICU.duration) == TRUE] <-
    as.numeric(difftime(
      patient.data$ICU.end.date[is.na(patient.data$ICU.duration) == TRUE],
      patient.data$ICU.start.date[is.na(patient.data$ICU.duration) == TRUE],
      unit="days"
    ))
  
  # drop the ICU2 columns
  
  patient.data <- patient.data %>% dplyr::select(-starts_with("ICU2"))
  
  ###### Calculation of time periods #####
  
  patient.data <- patient.data %>%
    dplyr::mutate(NIMV.duration = map2_dbl(NIMV.end.date, NIMV.start.date, function(x,y){
      as.numeric(difftime(x, y,  unit="days"))
    })) %>%
    dplyr::mutate(admission.to.exit = as.numeric(difftime(exit.date, admission.date,  unit="days")),
                  onset.to.admission = as.numeric(difftime(admission.date, onset.date, unit="days")),
                  start.to.exit = as.numeric(difftime(exit.date, start.date,  unit="days"))) %>%
    dplyr::mutate(admission.to.censored = map2_dbl(admission.to.exit, admission.date, function(x,y){
      if(is.na(x)){
        as.numeric(difftime(embargo.limit, y,  unit="days"))
      }
      else {
        NA
      }
    })) %>%
    dplyr::mutate(start.to.censored = map2_dbl(admission.to.exit, start.date, function(x,y){
      if(is.na(x)){
        as.numeric(difftime(embargo.limit, y,  unit="days"))
      }
      else {
        NA
      }
    })) %>%
    dplyr::mutate(admission.to.death = pmap_dbl(list(exit.code, exit.date, admission.date), function(x, y, z){
      if(!is.na(x) & x == "death"){
        as.numeric(difftime(y, z,  unit="days"))
      } else {
        NA
      }
    })) %>%
    dplyr::mutate(start.to.death = pmap_dbl(list(exit.code, exit.date, start.date), function(x, y, z){
      if(!is.na(x) & x == "death"){
        as.numeric(difftime(y, z,  unit="days"))
      } else {
        NA
      }
    })) %>%
    dplyr::mutate(admission.to.discharge = pmap_dbl(list(exit.code, exit.date, admission.date), function(x, y, z){
      if(!is.na(x) & x == "discharge"){
        as.numeric(difftime(y, z,  unit="days"))
      } else {
        NA
      }
    })) %>%
    dplyr::mutate(start.to.discharge = pmap_dbl(list(exit.code, exit.date, start.date), function(x, y, z){
      if(!is.na(x) & x == "discharge"){
        as.numeric(difftime(y, z,  unit="days"))
      } else {
        NA
      }
    })) %>%
    mutate(admission.to.ICU = as.numeric(difftime(ICU.start.date, admission.date, unit="days")),
           admission.to.IMV = as.numeric(difftime(IMV.start.date, admission.date, unit="days")),
           admission.to.NIMV = as.numeric(difftime(NIMV.start.date, admission.date, unit="days")),
           start.to.ICU = as.numeric(difftime(ICU.start.date, start.date, unit="days")),
           start.to.IMV = as.numeric(difftime(IMV.start.date, start.date, unit="days")),
           start.to.NIMV = as.numeric(difftime(NIMV.start.date, start.date, unit="days")))
  
  ##### Untangling COVID test fields #####
  
  if(verbose) cat("Untangling SARS-CoV-2 test results...\n")
  
  patient.data <- patient.data %>%
    mutate(cov.test.result =
             map_dbl(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "corna_mbcat"))) %>%
    mutate(cov.test.organism =
             map_dbl(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "corna_mbcaty"))) %>%
    mutate(cov.other.organism.freetext =
             map_chr(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "coronaother_mborres"))) %>%
    mutate(any.test.result =
             map(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "mborres"))) %>%
    mutate(any.test.freetext =
             map(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "mbtestcd")))
  
  # IMPORTANT: At the moment "positive.COV19.test" refers to "evidence for a positive test". FALSE is not evidence for a negative test.
  
  patient.data <- patient.data %>%
    mutate(positive.COV19.test = pmap_lgl(list(subjid, cov.test.result, cov.test.organism, cov.other.organism.freetext, any.test.result, any.test.freetext),
                                          function(id, ctest, cpos, cfreetext, anytest, anyfreetext){
                                            cpos.covlikely = probable.cov.freetext(cfreetext)
                                            anytest2 <- as.numeric(anytest)
                                            anyfreetext2 <- anyfreetext[which(!is.na(anytest))]
                                            if((ctest %in% c(1,2)) & ((!is.na(cpos) & cpos == 1) | (cpos == 888 & !is.na(cfreetext) & cpos.covlikely))){
                                              # You have COVID if:
                                              # 1) Your SARS-CoV-2 test result is a weak or strong positive for SARS-CoV-2
                                              # OR 2) Your SARS-CoV-2 test result is a weak or strong positive for "another virus" but the free text says that virus is actually SARS-CoV-2(!)
                                              return(TRUE)
                                            } else if(ctest == 0 | is.na(ctest) | (ctest  %in% c(1,2) & (is.na(cpos) | cpos == 2 | (!is.na(cpos.covlikely) & !cpos.covlikely)))){
                                              # Patients get here if:
                                              # 1) They have a negative SARS-CoV-2 test
                                              # OR 2) They have an NA entry for SARS-CoV-2 test result
                                              # OR 3) They have a weak or strong positive test for a coronavirus other than SARS-CoV-2 AND
                                              #   3A) The entry for virus species is NA
                                              #   OR 3B) The entry for virus species is "MERS"
                                              #   OR 3C) The free text is missing or says that virus is not SARS-CoV-2
                                              if(length(anyfreetext) > 0){
                                                # This examines the free text from the daily form, looking for COVID-like words
                                                otherpos.covlikely = map_lgl(anyfreetext2, probable.cov.freetext)
                                                if(any(!is.na(otherpos.covlikely) & !is.na(anytest2) & otherpos.covlikely & anytest2 == 1)){
                                                  # words found and the test was positive
                                                  return(TRUE)
                                                } else {
                                                  # words not found or the test was negative
                                                  return(FALSE)
                                                }
                                              }
                                            } else {
                                              return(FALSE)
                                            }
                                          }))
  
  
  # Minimal table for the unebargoed data
  
  unembargoed.data <- patient.data %>% dplyr::select(subjid, start.date, admission.date, outcome)
  
  # Impose the embargo
  
  if(verbose) cat("Imposing the embargo...\n")
  
  patient.data <-  patient.data %>%
    filter(dsstdat <= embargo.limit)
  
  # Temporary fix! #
  
  patient.data <- patient.data %>% filter(!(is.na(exit.code) & !is.na(exit.date)))
  
  list(unembargoed.data = unembargoed.data, detailed.data = patient.data)
  
}

