# #
# d.file <- "/Users/mdhall/Nexus365/Emmanuelle Dankwa - COVID Reports/data/Data/2020-04-20/CoVEOT_DATA_2020-04-20_0714.csv"
# d.dict.file <- "/Users/mdhall/Nexus365/Emmanuelle Dankwa - COVID Reports/data/Site List & Data Dictionaries/CoVEOT_DataDictionary_2020-04-10.csv"
# c.table <- "/Users/mdhall/ISARIC.COVID.reports/required_columns.csv"
# s.list <- "/Users/mdhall/ISARIC.COVID.reports/site_list.csv"
# verbose <- TRUE
# ref.date <- today()
# embargo.length <- 0
# message.out.file <- "messages.csv"
# source.name <- "test"
# #
#
# import.and.process.data(d.file, d.dict.file, c.table, s.list, "test", "messages.csv", verbose = TRUE)

#' Import data from Redcap .csv output for processing
#' @param data.file Path of the data file
#' @param data.dict.file Path of the data dictionary file
#' @param column.table.file Path of the column translation file (default required_columns.csv)
#' @param source.name String identifier for the data source (optional)
#' @param message.out.file Path of a file to store messages about fields that were overwritten in the data cleaning process. If not given, this file is not generated
#' @param embargo.length Length of the data embargo in days; patients enrolled less than this period before \code{ref.date} will be excluded from reports. Default 0.
#' @param ref.date Date to be taken as the date of the report. Default is today's date.
#' @param verbose Flag for verbose output
#'
#' @import readr glue lubridate magrittr purrr stringr tidyr forcats
#' @export import.and.process.data
#'
import.and.process.data <- function(data.file,
                                    data.dict.file,
                                    column.table.file = "required_columns.csv",
                                    site.list,
                                    source.name = NA,
                                    message.out.file = NULL,
                                    embargo.length = 0,
                                    ref.date = today(),
                                    verbose = F){

  column.table <- read_csv(column.table.file)
  site.list <- read_csv(site.list.file)

  raw.data <- import.patient.data(data.file, data.dict.file, source.name, verbose)

  cst.reference <- process.symptoms.comorbidities.treatments(data.dict.file)

  temp <- rename.and.drop.columns(raw.data, column.table, cst.reference)
  raw.data <- temp$data
  cst.reference <- temp$cst.reference

  raw.data <- raw.data %>% mutate(site.number = substr(redcap_data_access_group, 1, 3))

  raw.data <- raw.data %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number)

  if(!is.null(message.out.file)){
    write("oldvalue,new.value,column.name,subject.id,reason", file = message.out.file, append = FALSE)
  }

  raw.data <- pre.nest.sanity.checks(raw.data, message.out.file, verbose)

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
  patient.data.output$embargo.limit <- ref.date - embargo.length

  patient.data.output

}

#' Generate a PDF report from the data
#' @param patient.data.output List output from \code{import.and.process.data}
#' @param file.name Path to a PDF file for the report

#' @import rmarkdown psych ggplot2 fitdistrplus boot survival tibble grid gridExtra ggupset viridis
#' @export generate.report
#'

generate.report <- function(patient.data.output, file.name){
  patient.data <- patient.data.output$detailed.data
  unembargoed.data <- patient.data.output$unembargoed.data
  countries.and.sites <- patient.data.output$countries.and.sites
  cst.reference <- patient.data.output$cst.reference

  embargo.limit <- patient.data.output$embargo.limit

  admission.symptoms <- cst.reference %>% filter(type == "symptom")
  comorbidities <- cst.reference %>% filter(type == "comorbidity")
  treatments <- cst.reference %>% filter(type == "treatment")

  de <- d.e(patient.data, unembargoed.data, embargo.limit, comorbidities, admission.symptoms, treatments)

  render('markdown/COV-report.Rmd',output_file=file.name)

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
      cat(glue("Importing data from source {source.name}...\n"))
    } else {
      cat("Importing data...\n")
    }
  }

  data.dict <- read_csv(data.dict.file)

  column.types <- data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)

  data <- read_csv(data.file, guess_max = 100000)

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
rename.and.drop.columns <- function(data, column.table, cst.reference){

  for(i in 1:nrow(column.table)){
    old.name <- column.table$redcap.column.name[i]
    new.name <- column.table$report.column.name[i]
    if(!new.name %in% colnames(data)){
      if(!old.name %in% colnames(data)){
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
  }

  data <- data %>% dplyr::select(-setdiff(colnames(data), column.table$report.column.name))

  list(data = data, cst.reference = cst.reference)
}



# This makes the symptom, comorbidity and treatment reference table

#' @export
#' @keywords internal
process.symptoms.comorbidities.treatments <- function(data.dict.file){

  d.dict <- read_csv(data.dict.file) %>%
    dplyr::select(`Variable / Field Name`,`Form Name`, `Field Type`, `Field Label`) %>%
    dplyr::rename(field.name = `Variable / Field Name`, form.name = `Form Name`, field.type = `Field Type`, field.label = `Field Label`)


  comorbidities.colnames <- d.dict %>% filter(form.name == "comorbidities" & field.type == "radio") %>% pull(field.name)
  admission.symptoms.colnames <- d.dict %>% filter(form.name == "admission_signs_and_symptoms" & startsWith(field.label, "4") & field.type == "radio" &  field.name != "bleed_ceterm_v2") %>% pull(field.name)
  treatment.colnames <- d.dict %>% filter(form.name == "treatment" & field.type == "radio" & field.label != "Would you like to add another antibiotic?") %>% pull(field.name)

  comorbidities.labels <- d.dict %>%
    filter(form.name == "comorbidities" & field.type == "radio") %>%
    pull(field.label) %>%
    str_match(pattern = "4b\\.\\s?[0-9]+\\.\\s(.*)") %>%
    as_tibble() %>%
    pull(2) %>%
    map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
    map_chr(function(x) sub("\\s+$", "", x))

  comorbidities.labels[1] <- "Chronic cardiac disease"
  comorbidities.labels[18] <- "Other"

  comorbidities <- tibble(field = comorbidities.colnames, label = comorbidities.labels)

  comorbidities <- bind_rows(comorbidities, tibble(field = "pregnancy", label = "Pregnancy"))
  comorbidities <- comorbidities %>% bind_rows(list(field = "liver.disease", label = "Liver disease")) %>%
    filter(field != "mildliver" & field != "modliver")
  comorbidities <- comorbidities %>% bind_rows(list(field = "diabetes", label = "Diabetes")) %>%
    filter(field != "diabetes_mhyn" & field != "diabetescom_mhyn")

  comorbidities <- comorbidities %>% mutate(type = "comorbidity")

  admission.symptoms.labels <- d.dict %>%
    filter(form.name == "admission_signs_and_symptoms" &
             startsWith(field.label, "4") &
             field.type == "radio" &
             field.name != "bleed_ceterm_v2" ) %>%
    pull(field.label) %>%
    str_match(pattern = "4a\\.[0-9]+\\.[\\.]?[0-9]?\\s(.*)") %>%
    as_tibble() %>%
    pull(2) %>%
    map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
    map_chr(function(x) sub("\\s+$", "", x))

  admission.symptoms.labels[2] <- "Cough: no sputum"

  admission.symptoms <- tibble(field = admission.symptoms.colnames, label = admission.symptoms.labels)

  admission.symptoms <- admission.symptoms %>% bind_rows(list(field = "cough.nosputum", label = "Cough (no sputum)")) %>%
    bind_rows(list(field = "cough.sputum", label = "Cough (with sputum)")) %>%
    bind_rows(list(field = "cough.bloodysputum", label = "Cough (bloody sputum / haemoptysis)")) %>%
    filter(field != "cough_ceoccur_v2" & field != "coughsput_ceoccur_v2" & field !="coughhb_ceoccur_v2")

  admission.symptoms <- admission.symptoms %>% bind_rows(list(field = "shortness.breath", label = "Shortness of breath")) %>%
    filter(field != "shortbreath_ceoccur_v2" & field != "lowerchest_ceoccur_v2")

  admission.symptoms <- admission.symptoms %>% mutate(type = "symptom")

  treatment.labels <- d.dict %>%
    filter(form.name == "treatment" & field.type == "radio" & field.label != "Would you like to add another antibiotic?") %>%
    pull(field.label) %>%
    str_match(pattern = "6\\.[0-9]+[\\.]?[0-9]?[\\.]?\\s(.*)") %>%
    as_tibble() %>%
    pull(2) %>%
    map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
    # I don't know why you can't figure this out nicely. Do it later.
    map_chr(function(x) sub("\\s+$", "", x)) %>%
    map_chr(function(x) sub("\\?+$", "", x)) %>%
    map_chr(function(x) sub("\\s+$", "", x))

  treatment.labels[9] <- "Inhaled nitric oxide"
  treatment.labels[10] <- "Tracheostomy"
  treatment.labels[14] <- "Other"

  treatments <- tibble(field = treatment.colnames, label = treatment.labels, type = "treatment")

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


pre.nest.sanity.checks <- function(data, message.output.file = NULL, verbose = FALSE){
  if(verbose) cat("Setting future dates to NA...\n")

  date.columns <- c("dsstdat", "daily_dsstdat", "daily_lbdat", "hostdat", "cestdat", "dsstdtc")

  for(dc in date.columns){
    data <- data %>% mutate_at(vars(all_of(dc)), .funs = ~pcareful.date.check(.,
                                                                              subjid = subjid,
                                                                              colname = dc,
                                                                              check.early = TRUE,
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
    mutate(diabetes = pmap_dbl(list(diabetes_mhyn, diabetescom_mhyn, diabetes_mhyn), function(simple, complex, any){
      if(is.na(simple) & is.na(complex) & is.na(any)){
        NA
      } else if(!is.na(any)){
        # any is from RAPID currently and should not overlap with the others
        any
      } else if(is.na(simple)){
        complex
      } else if(is.na(complex)){
        simple
      } else if(simple == 1 | complex == 1){
        1
      } else if(simple == 2 & complex == 2){
        2
      } else {
        2
      }
    }))

  # Cough records may have not been entered coherently, and are recoded to be mutually exclusive.
  # Someone with a cough with sputum does not have a cough without it

  patient.data <- patient.data %>%
    mutate(cough.cols = pmap(list(cough_ceoccur_v2, coughsput_ceoccur_v2, coughhb_ceoccur_v2), function(x,y,z){

      if(any(c(x,y,z) == 3) | any(is.na(c(x,y,z)))){
        cough.nosputum <- NA
        cough.sputum <- NA
        cough.bloodysputum <- NA
      } else if(all(c(x,y,z) == 2)){
        cough.nosputum <- 2
        cough.sputum <- 2
        cough.bloodysputum <- 2
      } else if(y == 1){
        cough.nosputum <- 2
        if(z == 1){
          cough.sputum <- 2
          cough.bloodysputum <- 1
        } else {
          cough.sputum <- 1
          cough.bloodysputum <- 2
        }
      } else if(z == 1) {
        cough.nosputum <- 2
        cough.sputum <- 2
        cough.bloodysputum <- 1
      } else {
        cough.nosputum <- x
        cough.sputum <- 2
        cough.bloodysputum <- 2
      }
      list(cough.sputum = cough.sputum, cough.nosputum = cough.nosputum, cough.bloodysputum = cough.bloodysputum)
    })) %>%
    { bind_cols(., bind_rows(!!!.$cough.cols)) } %>%
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
        # censored until today
        as.numeric(difftime(ref.date, y,  unit="days"))
      }
      else {
        NA
      }
    })) %>%
    dplyr::mutate(start.to.censored = map2_dbl(admission.to.exit, start.date, function(x,y){
      if(is.na(x)){
        as.numeric(difftime(ref.date, y,  unit="days"))
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
                                              # OR 3)They have a weak or strong positive test for SARS-CoV-2 AND
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

  unembargoed.data <- patient.data %>% dplyr::select(subjid, Country, site.name, start.date, admission.date, outcome)

  # Data for the by-country summary

  countries.and.sites <-  unembargoed.data %>%
    group_by(Country, site.name) %>%
    dplyr::summarise(n.sites = 1) %>%
    dplyr::summarise(n.sites = sum(n.sites)) %>%
    filter(!is.na(Country))

  # Impose the embargo

  if(verbose) cat("Imposing the embargo...\n")


  patient.data <-  patient.data %>%
    filter(dsstdat <= embargo.limit)

  # Save to disk

  # Remove cases with problematic outcome code-date matches

  # Temporary fix! #

  patient.data<- patient.data %>% filter(!(is.na(exit.code) & !is.na(exit.date)))

  # Detach the events table

  # patient.data <- patient.data %>% dplyr::select(-events)

  list(unembargoed.data = unembargoed.data, detailed.data = patient.data, countries.and.sites = countries.and.sites)

}

