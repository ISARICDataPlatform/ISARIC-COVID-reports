
library(viridis)
library(ggupset)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(magrittr)
library(binom)
library(plyr)
library(glue)
library(fitdistrplus)
library(lubridate)
library(grid)
library(magrittr)
library(binom)

# flags for inclusion of the two data files

use.uk.data <- FALSE
use.row.data <- TRUE

if(!use.uk.data & !use.row.data){
  stop("No data to be imported")
}

# read the data dictionary to get lists of columns for symptoms at admission, comorbidities, and treatments

d.dict <- read_csv(glue("{data.path}/{data.dict.file}")) %>%
  dplyr::select(`Variable / Field Name`,`Form Name`, `Field Type`, `Field Label`) %>%
  dplyr::rename(field.name = `Variable / Field Name`, form.name = `Form Name`, field.type = `Field Type`, field.label = `Field Label`)

comorbidities.colnames <- d.dict %>% filter(form.name == "comorbidities" & field.type == "radio") %>% pull(field.name)
admission.symptoms.colnames <- d.dict %>% filter(form.name == "admission_signs_and_symptoms" & startsWith(field.label, "4")) %>% pull(field.name)
treatment.colnames <- d.dict %>% filter(form.name == "treatment" & field.type == "radio" & field.label != "Would you like to add another antibiotic?") %>% pull(field.name)

# COMORBIDITIES

comorbidities.labels <- d.dict %>% 
  filter(form.name == "comorbidities" & field.type == "radio") %>% 
  pull(field.label) %>%
  str_match(pattern = "4b\\.[0-9]+\\.\\s(.*)") %>%
  as_tibble() %>%
  pull(2) %>%
  map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
  map_chr(function(x) sub("\\s+$", "", x))

# At some point, farting around with regexes is more trouble than its worth

comorbidities.labels[1] <- "Chronic cardiac disease"
comorbidities.labels[18] <- "Other"

# SYMPTOMS

admission.symptoms.labels <- d.dict %>% 
  filter(form.name == "admission_signs_and_symptoms" & startsWith(field.label, "4")) %>% 
  pull(field.label) %>%
  str_match(pattern = "4a\\.[0-9]+\\.[\\.]?[0-9]?\\s(.*)") %>%
  as_tibble() %>%
  pull(2) %>%
  map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
  map_chr(function(x) sub("\\s+$", "", x)) 

admission.symptoms.labels[26] <- "Bleeding (other)"

# TREATMENTS

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

treatment.labels[8] <- "Inhaled nitric oxide"
treatment.labels[9] <- "Tracheostomy" 
treatment.labels[14] <- "Other"

# Lookup tables for the nice labels used in graphs

comorbidities <- tibble(field = comorbidities.colnames, label = comorbidities.labels)
admission.symptoms <- tibble(field = admission.symptoms.colnames, label = admission.symptoms.labels)
treatments <- tibble(field = treatment.colnames, label = treatment.labels)

# List of sites

site.list <- read_csv(glue("{data.path}/{site.list}")) %>% 
  dplyr::mutate(site.number = map_chr(`Site Number`, function(x) substr(x, 1, 3))) %>%
  dplyr::mutate(site.name = map_chr(`Site Number_1`, function(x) substr(x, 5, nchar(x)))) %>%
  dplyr::select(site.number, site.name, Country)


if(use.uk.data){
  uk.data <- read_csv(glue("{data.path}/{uk.data.file}"), guess_max = 10000) %>%
    # some fields are all-numerical in some files but not others. But using col_types is a faff for this many columns. This is a hack for now. @todo
    dplyr::mutate_at(vars(ends_with("orres")), as.character) %>%
    dplyr::mutate(Country = "UK")
} else {
  uk.data <- NULL
}

if(use.row.data){
  row.data <- read_csv(glue("{data.path}/{row.data.file}"), guess_max = 10000) %>% 
    # some fields are all-numerical in some files but not others. But using col_types is a faff for this many columns. This is a hack for now. @todo
    dplyr::mutate_at(vars(ends_with("orres")), as.character) %>%
    # different column names for some comorbidities
    dplyr::rename(chrincard = chroniccard_mhyn, 
                  modliv = modliver_mhyn, 
                  mildliver = mildliv_mhyn, 
                  chronichaemo_mhyn = chronhaemo_mhyn, 
                  diabetescom_mhyn = diabetiscomp_mhyn,
                  rheumatologic_mhyn = rheumatology_mhyr,
                  icu_hoendat = hoendat) %>%
    dplyr::mutate(site.number = map_chr(redcap_data_access_group, function(x) substr(x, 1, 3))) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    add_column(agedat = NA)
} else {
  row.data <- NULL
}

raw.data <- bind_rows(uk.data, row.data) %>%
  dplyr::mutate(dsstdat = ymd(dsstdat), 
                agedat = ymd(agedat), 
                daily_dsstdat = ymd(daily_dsstdat), 
                daily_lbdat = ymd(daily_lbdat),
                hostdat = ymd(hostdat),
                cestdat = ymd(cestdat),
                dsstdtc = ymd(dsstdtc))

# Demographic data is in the first row

demog.data <- raw.data %>% group_by(subjid) %>% slice(1) %>% ungroup()

# Clinical data is in subsequent rows but also _sometimes_ in the first row. So the events column still contains a copy of the first row.

event.data <- raw.data %>% group_by(subjid) %>% nest() %>% dplyr::rename(events = data) %>% ungroup() %>% ungroup()

patient.data <- demog.data %>% left_join(event.data)

# Add new columns with more self-explanatory names as needed

patient.data <- patient.data %>%
  # exit date is whenever the patient leaves the site. @todo look at linking up patients moving between sites
  dplyr::mutate(exit.date = map_chr(events, function(x){
    outcome.rows <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsstdtc)) 
    if(nrow(outcome.rows) == 0){
      return(NA)
    } else if(nrow(outcome.rows) > 1) {
      stop("Multiple exit dates?")
    } else {
      return(outcome.rows %>% slice(1) %>% pull(dsstdtc) %>% as.character())
    }
  })) %>%
  dplyr::mutate(exit.date = ymd(exit.date)) %>%
  # exit code is the reason for leaving the site. Unsure what "hospitalisation" means but it's yet to appear in actual data
  dplyr::mutate(exit.code = map_chr(events, function(x){
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
  # censorship occurs if either the patient is still in site or is moved offsite without a death or discharge code
  dplyr::mutate(censored = map_lgl(events, function(x){
    if(x %>% pull(redcap_event_name) %>% startsWith("discharge") %>% any() %>% not()){
      # still in site
      return(TRUE)
    } else {
      # Anything other than discharge or death is "censored"
      temp <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) 
      if(nrow(temp) == 0){
        return(TRUE)
      } else {
        return(temp %>% pull(dsterm)%>% match(c(1,4)) %>% is.na() %>% any())
      }
    }
  })) %>%
  # outcome is just death or discharge
  dplyr::mutate(outcome = map2_chr(censored, events, function(x, y){
    if(x){
      return("censored")
    } else {
      return(switch(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsterm) %>% as.character(),
                    "1" = "discharge",
                    "4" = "death"))
    }
  })) %>%
  dplyr::mutate(outcome.date.known = map2_dbl(censored, events, function(x, y){
    if(x){
      return(2)
    } else {
      return(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtcyn))
    }
  }))  %>%
  dplyr::mutate(outcome.date = map2_chr(censored, events, function(x, y){
    if(x){
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
  dplyr::mutate(discharge.date = ymd(discharge.date)) %>%
  # Consolidated age is the exact age at enrollment if this is present. Otherwise it is taken from the estimated age column. 
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
    temp <- str_replace(temp, ",", "-")
    str_replace(temp, "90-120", "90+")
    
  })) %>%
  dplyr::mutate(agegp10 = cut(consolidated.age, c(seq(0,70,by = 10),120), right = FALSE)) %>%
  dplyr::mutate(agegp10 = fct_relabel(agegp10, function(a){
    # make nicer labels
    temp <- substr(a, 2, nchar(a) -1 )
    temp <- str_replace(temp, ",", "-")
    str_replace(temp, "70-120", "70+")
    
  })) %>%
  # these are just for the sake of having more self-explanatory column names
  dplyr::mutate(admission.date = hostdat) %>%
  dplyr::mutate(enrollment.date = dsstdat) %>%
  dplyr::mutate(onset.date = cestdat) %>%
  dplyr::mutate(ICU.admission.date = icu_hostdat) %>%
  dplyr::mutate(ICU.discharge.date = icu_hoendat) %>%
  dplyr::mutate(ICU.duration = hodur) %>%
  dplyr::mutate(IMV.duration  = invasive_prdur)

compareNA <- function(v1,v2) {
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# More complicated date wrangling (IMV, NIMV)

other.dates <- map(1:nrow(patient.data), function(i){
  id = patient.data$subjid[i]
  events.tibble <- patient.data$events[i][[1]]
  
  # the NIMV field for the daily form is daily_noninvasive_prtrt
  
  ever.NIMV <- patient.data$noninvasive_proccur[i] == 1
  
  # rows in events that have an entry for NIMV
  
  NIMV.rows <- events.tibble %>% filter(!is.na(daily_noninvasive_prtrt)) %>% dplyr::select(daily_dsstdat, daily_noninvasive_prtrt)
  
  # multiple.NIMV.periods indicates that the patient had more than one continuous episode of NIMV. Currently there are none of these. 
  # They are not currently coherently handled, just flagged
  
  if(nrow(NIMV.rows) == 0){
    # no reference to NIMV
    NIMV.start.date <- NA
    NIMV.end.date <- NA
    multiple.NIMV.periods <- NA
  } else if(!any(NIMV.rows$daily_noninvasive_prtrt == 1)){
    # no "yes" to NIMV
    NIMV.start.date <- NA
    NIMV.end.date <- NA
    multiple.NIMV.periods <- NA
  } else {
    NIMV.start.date <- NIMV.rows %>% filter(daily_noninvasive_prtrt == 1) %>% slice(1) %>% pull(daily_dsstdat)
    NIMV.last.date <- NIMV.rows %>% filter(daily_noninvasive_prtrt == 1) %>% slice(n()) %>% pull(daily_dsstdat)
    if(NIMV.last.date == NIMV.rows %>% slice(n()) %>% pull(daily_dsstdat)){
      # Patient was on NIMV at last report
      NIMV.end.date <- NA
    } else {
      # They were off NIMV at the next report
      next.report.row <- max(which(NIMV.rows$daily_dsstdat == NIMV.last.date)) + 1
      NIMV.end.date <- NIMV.rows$daily_dsstdat[next.report.row]
    }
    if(nrow(NIMV.rows)<=2){
      multiple.NIMV.periods <- F
    } else {
      # we are looking for the number of instances of 2 then 1. If this is more than 1, or more than 0 with the first report on NIMV, 
      # then the patient went on NIMV multiple times
      temp <- map_dbl(2:nrow(NIMV.rows), function(x)  NIMV.rows$daily_noninvasive_prtrt[x] - NIMV.rows$daily_noninvasive_prtrt[x-1] )
      multiple.NIMV.periods <- length(which(temp == -1)) < 1 | (length(which(temp == -1)) > 0 &  NIMV.rows$daily_noninvasive_prtrt[1] == 1)
    }
  }
  
  # the IMV field for the daily form is daily_invasive_prtrt

  ever.IMV <- patient.data$daily_invasive_prtrt[i] == 1
  
  # rows in events that have an entry for NIMV
  
  IMV.rows <- events.tibble %>% filter(!is.na(daily_invasive_prtrt)) %>% dplyr::select(daily_dsstdat, daily_invasive_prtrt)
  
  # multiple.IMV.periods indicates that the patient had more than one continuous episode of IMV. Currently there are none of these. 
  # They are not currently coherently handled, just flagged
  
  if(nrow(IMV.rows) == 0){
    # no reference to IMV
    IMV.start.date <- NA
    IMV.end.date <- NA
    multiple.IMV.periods <- NA
  } else if(!any(IMV.rows$daily_invasive_prtrt == 1)){
    # no "yes" to NIMV
    IMV.start.date <- NA
    IMV.end.date <- NA
    multiple.IMV.periods <- NA
  } else {
    IMV.start.date <- IMV.rows %>% filter(daily_invasive_prtrt == 1) %>% slice(1) %>% pull(daily_dsstdat)
    IMV.last.date <- IMV.rows %>% filter(daily_invasive_prtrt == 1) %>% slice(n()) %>% pull(daily_dsstdat)
    if(IMV.last.date == IMV.rows %>% slice(n()) %>% pull(daily_dsstdat)){
      # Patient was on IMV at last report
      IMV.end.date <- NA
    } else {
      # They were off IMV at the next report
      next.report.row <- max(which(IMV.rows$daily_dsstdat == IMV.last.date)) + 1
      IMV.end.date <- IMV.rows$daily_dsstdat[next.report.row]
    }
    if(nrow(IMV.rows)<=2){
      multiple.IMV.periods <- F
    } else {
      # we are looking for the number of instances of 2 then 1. If this is more than 1, or more than 0 with the first report on IMV, 
      # then the patient went on IMV multiple times
      temp <- map_dbl(2:nrow(IMV.rows), function(x)  IMV.rows$daily_invasive_prtrt[x] - IMV.rows$daily_invasive_prtrt[x-1] )
      multiple.IMV.periods <- length(which(temp == -1)) > 1 | (length(which(temp == -1)) > 0 &  IMV.rows$daily_invasive_prtrt[1] == 1)
    }
  }
  
  list(subjid = id, ever.IMV = ever.IMV, IMV.start.date = IMV.start.date, IMV.end.date = IMV.end.date, multiple.IMV.periods = multiple.IMV.periods,
       ever.NIMV = ever.NIMV, NIMV.start.date = NIMV.start.date, NIMV.end.date = NIMV.end.date, multiple.NIMV.periods = multiple.NIMV.periods)
}) %>% bind_rows()

patient.data <- patient.data %>% left_join(other.dates)

# @todo this script needs to be more aware of the date of the dataset

ref.date = today()

# calculation of time periods @todo NIMV, IMV

patient.data <- patient.data %>%
  dplyr::mutate(admission.to.exit = as.numeric(difftime(exit.date, hostdat,  unit="days")),
                onset.to.admission = as.numeric(difftime(hostdat, cestdat, unit="days"))) %>%
  dplyr::mutate(admission.to.censored = map2_dbl(admission.to.exit, hostdat, function(x,y){
    if(is.na(x)){
      as.numeric(difftime(ref.date, y,  unit="days"))}
    else{
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.death = pmap_dbl(list(dsstdtcyn, dsstdtc, admission.date), function(x, y, z){
    if(compareNA(4,x )){
      as.numeric(difftime(y, z,  unit="days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.recovery = pmap_dbl(list(dsstdtcyn, dsstdtc, admission.date), function(x, y, z){
    if(compareNA(1, x)){
      as.numeric(difftime(y, z,  unit="days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.ICU = map2_dbl(ICU.admission.date, admission.date, function(x,y){
    as.numeric(difftime(x, y,  unit="days"))
  })) %>%
  dplyr::mutate(NIMV.duration = map2_dbl(NIMV.end.date, NIMV.start.date, function(x,y){
    as.numeric(difftime(x, y,  unit="days"))
  }))

# save RDA @todo change this to keep only relevant columns

save(patient.data, file=glue("patient_data_{today()}.rda"))


##### GRAPH FUNCTIONS ##### 


# Age pyramid

age.pyramid <- function(data){
  
  data2 <- data %>%
    group_by(agegp5, sex, outcome) %>%
    dplyr::summarise(count = n()) %>%
    ungroup() %>%
    filter(!is.na(sex) & !is.na(agegp5)) %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("death", "censored", "discharge"))) %>%
    dplyr::mutate(count = map2_dbl(count, sex, function(c, s){
      if(s == 1){
        -c
      } else {
        c
      }
    })) %>%
    dplyr::mutate(sex = map_chr(sex, function(s){
      c("M", "F")[s]
    })) 
  
  # this is to get the axes right (maximum the same in both directions)
  
  max.count = data2 %>% group_by(agegp5, sex) %>% dplyr::summarise(sac = sum(abs(count))) %>% pull(sac) %>% max()
  
  ggplot() + geom_bar(data = (data2 %>% filter(sex == "M")), aes(x=agegp5, y=count, fill = outcome), stat = "identity", col = "black") +
    geom_bar(data = data2 %>% filter(sex == "F"), aes(x=agegp5, y=count, fill = outcome),  stat = "identity", col = "black") +
    coord_flip(clip = 'off') +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Death", "Censored", "Discharge")) +
    xlab("Age group") +
    ylab("Count") +
    scale_x_discrete(drop = "F") +
    scale_y_continuous(
      # currently in hard-coded increments of 5. @todo make this better
      breaks = seq(-(ceiling(max(abs(data2$count))/5)*5), ceiling(max(abs(data2$count))/5)*5, by = 5),
      labels = as.character(c(rev(seq(5, ceiling(max(abs(data2$count))/5)*5, by = 5)), 0, seq(5, ceiling(max(abs(data2$count))/5)*5, by= 5))),
      limits = c(-1.1*max.count, 1.1*max.count)) +
    annotation_custom(
      grob = textGrob(label = "Males", hjust = 0.5, gp = gpar(cex = 1.5)),
      ymin = -max(data2$count)*1.1/2,      
      ymax = -max(data2$count)*1.1/2,
      xmin = length(levels(data2$agegp5))+1.5 ,         
      xmax = length(levels(data2$agegp5))+1.5) +
    annotation_custom(
      grob = textGrob(label = "Females", hjust = 0.5, gp = gpar(cex = 1.5)),
      ymin = max(data2$count)*1.1/2,      
      ymax = max(data2$count)*1.1/2,
      xmin = length(levels(data2$agegp5))+1.5,         
      xmax = length(levels(data2$agegp5))+1.5) +
    theme(plot.margin=unit(c(30,5,5,5.5,5.5),"pt"))
  
}

# Distribution of sites by country

sites.by.country <- function(data){
  data2 <- data %>%
    group_by(Country, redcap_data_access_group) %>%
    dplyr::summarise(n.sites = 1) %>%
    dplyr::summarise(n.sites = sum(n.sites))
  
  ggplot(data2) + geom_col(aes(x = Country, y = n.sites), col = "black", fill = "deepskyblue3") +
    theme_bw() +
    xlab("Country") +
    ylab("Sites") 
}

# Distribution of patients and outcomes by country

outcomes.by.country <- function(data){
  data2 <- data %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("death", "censored", "discharge")))
  
  ggplot(data2) + geom_bar(aes(x = Country, fill = outcome), col = "black") +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Death", "Censored", "Discharge")) +
    xlab("Country") +
    ylab("Cases") 
}

# Outcomes by epi-week

outcomes.by.admission.date <- function(data){
  data2 <- data %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("death", "censored", "discharge")))
  ggplot(data2) + geom_bar(aes(x = epiweek(hostdat), fill = outcome), col = "black", width = 0.95) +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Death", "Censored", "Discharge")) +
    scale_x_continuous(breaks = seq(min(epiweek(data2$hostdat), na.rm = TRUE), max(epiweek(data2$hostdat), na.rm = TRUE), by=2)) +
    xlab("Epidemiological week (2020)") +
    ylab("Cases") 
}

# Comorbidities upset plot (max.comorbidities is the n to list; this will be the n most frequent)

comorbidities.upset <- function(data, max.comorbidities){
  
  # just the comorbidity columns
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(comorbidities$field)) 
  
  n.comorb <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.comorb+1), names_to = "Condition", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        NA
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        NA
      }
    })) 
  
  # get the most common
  
  most.common <- data2 %>%        
    group_by(Condition) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    ungroup() %>%
    filter(Condition != "other_mhyn") %>%
    arrange(desc(Present)) %>%
    slice(1:max.comorbidities) %>%
    pull(Condition)
  
  data3 <- data %>%
    dplyr::select(subjid, one_of(most.common)) %>%
    pivot_longer(2:(length(most.common)+1), names_to = "Condition", values_to = "Present") %>%
    left_join(comorbidities, by = c("Condition" = "field")) %>%
    dplyr::select(-Condition) %>%
    filter(!is.na(Present)) %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        NA
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        NA
      }
    })) %>%
    group_by(subjid) %>%
    dplyr::summarise(Conditions = list(label), Presence = list(Present)) %>%
    dplyr::mutate(conditions.present = map2(Conditions, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Conditions, -Presence)
  
  ggplot(data3, aes(x = conditions.present)) + 
    geom_bar(fill = "indianred3", col = "black") + 
    theme_bw() +
    xlab("Comorbidities present at admission") +
    ylab("Count") +
    scale_x_upset() 
}

# Symptoms upset plot (max.symptoms is the n to list; this will be the n most frequent)

symptoms.upset <- function(data, max.symptoms){
  
  # just the symptom columns
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms$field)) 
  
  n.comorb <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.comorb+1), names_to = "Condition", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        NA
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        NA
      }
    })) 
  
  # find the most common
  
  most.common <- data2 %>%        
    group_by(Condition) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    ungroup() %>%
    filter(Condition != "other_mhyn") %>%
    arrange(desc(Present)) %>%
    slice(1:max.symptoms) %>%
    pull(Condition)
  
  data3 <- data %>%
    dplyr::select(subjid, one_of(most.common)) %>%
    pivot_longer(2:(length(most.common)+1), names_to = "Condition", values_to = "Present") %>%
    left_join(admission.symptoms, by = c("Condition" = "field")) %>%
    dplyr::select(-Condition) %>%
    filter(!is.na(Present)) %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        NA
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        NA
      }
    })) %>%
    group_by(subjid) %>%
    dplyr::summarise(Conditions = list(label), Presence = list(Present)) %>%
    dplyr::mutate(conditions.present = map2(Conditions, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Conditions, -Presence)
  
  ggplot(data3, aes(x = conditions.present)) + 
    geom_bar(fill = "deepskyblue3", col = "black") + 
    theme_bw() +
    xlab("Symptoms present at admission") +
    ylab("Count") +
    scale_x_upset() 
}

# Prevalence of symptoms and comortbidities

comorbidity.symptom.prevalence <- function(data){
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms$field), one_of(comorbidities$field)) 
  
  nconds <- ncol(data2) - 1
  
  combined.labeller <- bind_rows(comorbidities %>% add_column(type = "Comorbidities"), 
                                 admission.symptoms %>% add_column(type = "Symptoms at\nadmission"))
  
  data2 <- data2 %>%
    pivot_longer(2:(nconds + 1), names_to = "Condition", values_to = "Present") %>%
    group_by(Condition) %>%
    filter(!is.na(Present)) %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        NA
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        NA
      }
    })) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    left_join(combined.labeller, by = c("Condition" = "field")) %>%
    dplyr::select(-Condition) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(type, prop.yes) %>%
    dplyr::mutate(Condition = as_factor(label)) %>%
    pivot_longer(c(prop.yes, prop.no), names_to = "affected", values_to = "Proportion") %>%
    dplyr::mutate(affected = map_lgl(affected, function(x) x == "prop.yes")) %>%
    dplyr::mutate(typepresent = glue("{type}_{affected}")) %>%
    filter(label != "Other")
  
  
  ggplot(data2) + 
    geom_col(aes(x = Condition, y = Proportion, fill = affected), col = "black") +
    facet_wrap(~type, scales = "free") +
    theme_bw() + 
    coord_flip() + 
    ylim(0, 1) +
    scale_fill_brewer(palette = "Paired", name = "Condition\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
}

# Raw proportions of patients undergoing each treatment

treatment.use.plot <- function(data){
  
  treatment.columns <- map(1:nrow(data), function(i){
    data$events[i][[1]] %>% 
      filter(startsWith(redcap_event_name, "dischargeoutcome")) %>%
      dplyr::select( one_of(treatments$field)) %>%
      add_column(subjid = data$subjid[i]) %>%
      slice(1)
  }) %>% bind_rows()
  
  
  data2 <- data %>% 
    dplyr::select(-one_of(treatments$field)) %>%
    left_join(treatment.columns, by="subjid") %>%
    dplyr::select(subjid, one_of(treatments$field))
  
  ntr <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(ntr + 1), names_to = "Treatment", values_to = "Present") %>%
    group_by(Treatment) %>%
    filter(!is.na(Present)) %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        NA
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        NA
      }
    })) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    left_join(treatments, by = c("Treatment" = "field")) %>%
    dplyr::select(-Treatment) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(prop.yes) %>%
    dplyr::mutate(Condition = as_factor(label)) %>%
    pivot_longer(c(prop.yes, prop.no), names_to = "treated", values_to = "Proportion") %>%
    dplyr::mutate(affected = map_lgl(treated, function(x) x == "prop.yes")) 
  
  ggplot(data2) + 
    geom_col(aes(x = Condition, y = Proportion, fill = affected), col = "black") +
    theme_bw() + 
    coord_flip() + 
    ylim(0, 1) +
    scale_fill_brewer(palette = "Paired", name = "Condition\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
}

# "modified KM plot" with death and 

modified.km.plot <- function(data){
  
  total.patients <- nrow(data)
  
  data2 <- data %>%
    filter(outcome != "censored" & !is.na(outcome.date)) %>%
    dplyr::select(subjid, hostdat, outcome.date, outcome, admission.to.exit) 
  
  timeline <- map(0:max(data2$admission.to.exit), function(x){
    outcome.date <- data2 %>% filter(admission.to.exit <= x)
    prop.dead <- nrow(outcome.date %>% filter(outcome == "death"))/total.patients
    prop.discharged <- nrow(outcome.date %>% filter(outcome == "discharge"))/total.patients
    list(day = x, prop.dead = prop.dead, prop.discharged = prop.discharged)
  }) %>% bind_rows() %>%
    dplyr::mutate(prop.not.discharged = 1-prop.discharged) %>%
    dplyr::select(-prop.discharged)
  
  
  
  
  final.dead <- timeline %>%  pull(prop.dead) %>% max()
  final.not.discharged <- timeline %>% pull(prop.not.discharged) %>% min()
  
  interpolation.line <- final.dead + (1-(final.not.discharged+final.dead))*(final.dead/(final.dead + (1-final.not.discharged)))
  
  timeline <- timeline %>%
    add_column(interpolation = interpolation.line) %>%
    pivot_longer(2:4, names_to = "stat", values_to = "value") %>%
    dplyr::mutate(stat = factor(stat, levels = c("prop.dead", "prop.not.discharged", "interpolation")))
  
  ggplot(timeline) + 
    geom_line(aes(x= day, y=value, col = stat, linetype = stat), size =0.75) +
    theme_bw() +
    scale_colour_manual(values = c("#e41a1c", "#377eb8", "black"), name = "Statistic", labels = c("Death", "Discharge", "Interpolated\nfatality risk")) +
    scale_linetype_manual(values = c("solid", "solid", "dashed"),  guide = F) +
    xlab("Days after admission") +
    ylab("Cumulative probability")
  
}

# hospital fataility ratio plot

hospital.fatality.ratio <- function(data) {
  
  # Method from https://doi.org/10.2807/1560-7917.ES.2020.25.3.2000044
  # Only uses individuals who have either died or been discharged
  
  row_n <- nrow(data)
  for (i in 1:row_n) {
    events <- data$events[i]
    detail <- events[[1]]
    detail$dsterm[is.na(detail$dsterm) == TRUE] <- 0
    detail$Recovered.Date <- -500
    detail$Recovered.Date[detail$dsterm == 1] <- detail$dsstdtc[detail$dsterm == 1]
    detail$Died.Date <- -500
    detail$Died.Date[detail$dsterm == 4] <- detail$dsstdtc[detail$dsterm == 4]  
    
    recovered <- as.Date(
      max(detail$Recovered.Date, na.rm = TRUE),
      origin = "1970-01-01"
    )
    died <- as.Date(
      max(detail$Died.Date, na.rm = TRUE),
      origin = "1970-01-01"
    )
    recovered[recovered == "1968-08-19"] <- NA
    died[died == "1968-08-19"] <- NA
    out <- data.frame(death.date = died, discharge.date = recovered)
    # This is a PID made for this table and does not correlate with other
    # patient IDs
    if (i == 1) {
      outcome <- out
    } else {
      outcome <- rbind(outcome, out, deparse.level = 1)
    }
  }
  
  
  Dc_date <- outcome$discharge.date
  Died_date <- outcome$death.date
  
  # Identify first and last events
  
  first <- min(Dc_date, Died_date, na.rm = TRUE)
  last <- max(Dc_date, Died_date, na.rm = TRUE)
  diff <- last - first
  
  # Plot to start after first event
  
  d.0 <- first
  
  for (i in 0:diff) {
    
    d.i <- d.0 + i
    date <- d.0 + i
    disch <- sum(Dc_date == date, na.rm=TRUE)
    died <- sum(Died_date == date, na.rm=TRUE)
    
    db.i <- data.frame(Row = i, Date = date, Dc = disch, Died = died)
    db.i$Dc[is.na(db.i$Dc) == TRUE] <- 0
    db.i$Died[is.na(db.i$Died) == TRUE] <- 0
    
    db.i$Dc_c <- 0
    db.i$Died_c <- 0
    
    if (i == 0) {
      db <- db.i
      db$Dc_c <- db$Dc
      db$Died_c <- db$Died
    } else {  
      db <- rbind(db, db.i, deparse.level = 0)
      db$Dc_c[i + 1] <- db$Dc_c[i] + db$Dc[i + 1]
      db$Died_c[i + 1] <- db$Died_c[i] + db$Died[i + 1]
    }
  }
  
  db$Events <- db$Dc_c + db$Died_c
  
  # Discard rows before the first patients with events
  
  db <- subset(db, Events > 0)
  
  # hospital fatality risk = (fatal cases)/(fatal cases+recovered cases)
  
  db$Hfr <- db$Died_c / db$Events
  bino <- binom.confint(
    db$Died_c, 
    db$Events, 
    conf.level = .95, 
    methods = "exact"
  )
  
  db <- cbind(db, bino, deparse.level = 0)
  
  
  line <- geom_line(
    data = db, 
    stat = "identity", 
    aes(x = Date, y = Hfr), 
    colour = "blue",
    size = 1
  ) 
  shade <- geom_ribbon(
    fill = 'lightblue',
    data = db,
    stat = "identity", 
    aes(x = Date, ymin = lower, ymax = upper),  
    linetype = 2,
    alpha = 0.5
  )
  yaxis <- scale_y_continuous(
    name = "Hospital fatality ratio", 
    limits = c(0, 1)
  )
  plt <- ggplot(data = db) +
    line +
    shade +
    yaxis + theme_bw()
  
  return(plt)
  
}

# The following not currently in use

## Violin plot by sex ####

violin.sex.func <- function(data){
  
  # Analysis to be run on only entries with either admission.to.exit or admission.to.censored 
  
  data2 <- data %>% filter(!is.na(admission.to.exit) | !is.na(admission.to.censored))
  
  # This is to include dates for individuals still in hospital
  
  data2 <- data2 %>% 
    mutate(length.of.stay = map2_dbl(admission.to.exit, admission.to.censored, function(x,y){
      max(x, y, na.rm = T)
    }))
  
  data2$sex <- revalue(as.factor(data2$sex), c('1' = 'Male', '2' = 'Female'))
  
  vd <- tibble(Sex = data2$sex, length.of.stay = abs(data2$length.of.stay) )
  
  # by sex
  
  x <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) + geom_violin(trim=FALSE)+ geom_boxplot(width=0.1, fill="white")  +
    labs(title=" ", x="Sex", y = "Length of hospital stay") + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + 
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(x)
}



#violin.sex.func(patient.data)
### Violin age ####



violin.age.func <- function(data){
  
  # Analysis to be run on only entries with either admission.to.exit or admission.to.censored 
  
  data2 <- data %>% filter(!is.na(admission.to.exit) | !is.na(admission.to.censored))
  
  data2 <- data2 %>%
    mutate(length.of.stay = pmax(admission.to.exit, admission.to.censored, na.rm = T))
  
  
  vdx<- tibble(subjid = data2$subjid, Age = data2$agegp10, length_of_stay = abs(data2$length.of.stay) )

  # remove NAs (@todo for now?)
  
  vdx <- vdx %>% filter(!is.na(agegp10))
  
  vd2 <- ggplot(vdx, aes(x = Age, y = length_of_stay, fill=Age)) + geom_violin(trim=FALSE)+ #geom_boxplot(width=0.1, fill="white")  +
    labs(title="  ", x="Age group", y = "Length of hospital stay") + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + 
    scale_fill_discrete(drop = F) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(vd2)
  
}


########### Distribution plots ############

#### Replace zeros with 0.5 (half a day) #######

replacezeros <- function(admit.discharge){
  
  for (i in 1: length(admit.discharge)){
    
    if (admit.discharge[i]==0){
      admit.discharge[i] <- 0.5
    }
  }
  
  return(admit.discharge) 
}




adm.outcome.func <- function(data){
  
  data2 <- data %>% filter(!is.na(admission.to.exit) | !is.na(admission.to.censored))
  
  data2 <- data2 %>% 
    mutate(length.of.stay = map2_dbl(admission.to.exit, admission.to.censored, function(x,y){
      max(x, y, na.rm = T)
    }))
  
  admit.discharge <- data2$length.of.stay
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge <- replacezeros(admit.discharge)
  
  pos.cens <- which(data2$censored == 'TRUE')
  
  
  left <- c(admit.discharge)
  right <- replace(admit.discharge, pos.cens, values=NA )
  censored_df <- data.frame(left, right)
  fit <- fitdistcens(censored_df, dist = 'gamma')
  t <- data.frame(x = admit.discharge)
  
  plot <- ggplot(data = t) + 
    #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+    
    geom_line(aes(x=t$x, y=dgamma(t$x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
    labs(y = 'Density', x = 'Time (in days) from admission to death or recovery', title = '')
  
  return( plot)
  
}







########## Onset to admission #####


onset.adm.func <- function(data){
  
  admit.discharge <- data$onset.to.admission
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge.2 <- replacezeros(admit.discharge)
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  # Plot 
  
  library(ggplot2)
  t <- data.frame(x=admit.discharge)
  plot <- ggplot(data = t) + 
    #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+    
    geom_line(aes(x=t$x, y=dgamma(t$x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
    labs(y = 'Density', x = 'Time from symptom onset to admission', title = ' ')
  
  return(plot)
}







########## Survival plot ######


surv_plot_func <- function(data){
  
  
  data2 <- data %>% filter(!is.na(admission.to.exit) | !is.na(admission.to.censored))
  
  data2 <- data2 %>% 
    mutate(length.of.stay = map2_dbl(admission.to.exit, admission.to.censored, function(x,y){
      max(x, y, na.rm = T)
    }))
  
  
  data2$sex <- revalue(as.factor(data2$sex), c('1' = 'Male', '2' = 'Female'))
  
  vdy <- tibble(sex = data2$sex, length.of.stay = abs(data2$length.of.stay), Censored = data2$censored )
  
  
  fit <- survfit(Surv(length.of.stay, Censored) ~ sex, data = vdy)
  #print(fit)
  plot <- ggsurvplot(fit,
                     pval = T, conf.int = T,
                     risk.table = F, # Add risk table
                     # risk.table.col = "strata", # Change risk table color by groups
                     linetype = "strata", # Change line type by groups
                     #surv.median.line = "hv", # Specify median survival
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c('#D2691E', '#BA55D3'),
                     legend.labs = 
                       c("Male", "Female"), title = (main = ' '), ylab = '1 - Probability of hospital exit' , legend = c(0.8, 0.9))
  return(plot)
}

# Upset plot for treatments @todo add maximum parameter?


treatment.upset <- function(data) {
  row_n <- nrow(data)
  for (i in 1:row_n) {
    events <- data$events[i]
    detail <- events[[1]]
    detail <- subset(
      detail, 
      select = c(dsterm,
                 antiviral_cmyn, antiviral_cmtrt, 
                 antibiotic_cmyn, 
                 corticost_cmyn, corticost_cmroute,
                 antifung_cmyn)
    )
    detail$Pid_special <- i   
    # This is a PID made for this table and does not correlate with other
    # patient IDs
    if (i == 1) {
      details <- detail
    } else {
      details <- rbind(details, detail, deparse.level = 1)
    }
    
  }
  
  # If no dsterm result then not the form that will have treatment details
  details$All_NA <- 1
  details$All_NA[is.na(details$dsterm) == FALSE] <- 0
  details <- subset(details, All_NA == 0)
  
  # Separate steroids according to route
  
  details$Oral_Steroid <- 
    details$Intravenous_Steroid <- 
    details$Inhaled_Steroid <- 
    0
  details$Oral_Steroid[details$corticost_cmroute == 1] <- 1
  details$Intravenous_Steroid[details$corticost_cmroute == 2] <- 1
  details$Inhaled_Steroid[details$corticost_cmroute == 3] <- 1
  
  # 1 is Yes, set anything else to 0 (No)
  details$antiviral_cmyn[details$antiviral_cmyn != 1] <- 0
  details$antibiotic_cmyn[details$antibiotic_cmyn != 1] <- 0
  details$antifung_cmyn[details$antifung_cmyn != 1] <- 0
  details$corticost_cmyn[details$corticost_cmyn != 1] <- 0
  details$Antiviral <- details$antiviral_cmyn
  details$Antibiotic <- details$antibiotic_cmyn
  details$Antifungal <- details$antifung_cmyn
  details$Corticosteroid <- details$corticost_cmyn
  
  # Plot 1 - not separating according to type of antiviral
  treatments <- details %>%
    dplyr::select(
      Pid_special, 
      Antiviral, 
      Antibiotic, 
      Antifungal, 
      Oral_Steroid, Intravenous_Steroid, Inhaled_Steroid
    ) %>%
    pivot_longer(2:7, names_to = "Treatment", values_to = "Present") %>%
    mutate(Present = as.logical(Present)) %>%
    group_by(Pid_special) %>%
    dplyr::summarise(Treatments = list(Treatment), Presence = list(Present)) %>%
    mutate(treatments.used = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  
  p <- ggplot(treatments, aes(x = treatments.used)) + 
    geom_bar(fill = "chartreuse4", col = "black") + 
    theme_bw() +
    xlab("Treatments used during hospital admission") +
    ylab("Count") +
    scale_x_upset() 
  
  
  return(p)
}

######### Timeline plot ##############
# @todo add ICU. Add IMV.

status.by.time.after.admission <- function(data){
  
  data2 <- data %>%
    dplyr::mutate(status = map_chr(exit.code, function(x){
      ifelse(is.na(x), "censored", x)
    })) %>%
    dplyr::mutate(status = factor(status)) 
  
  timings.wrangle <- data2 %>%
    dplyr::select(subjid,
                  status,
                  # Admit.ICU,
                  # Dur.ICU,
                  censored,
                  admission.to.exit) %>%
    dplyr::mutate(hospital.start = 0) %>%
    dplyr::mutate(hospital.end = admission.to.exit) %>%
    # mutate(ICU.start = Admit.ICU) %>%
    # mutate(ICU.end = Admit.ICU + Dur.ICU) %>%
    dplyr::select(subjid,  ends_with("start"), ends_with("end"), censored, status) %>%
    filter(hospital.end >= 0 | is.na(hospital.end))
  # mutate(ever.ICU = !is.na(ICU.start)) 
  
  overall.start <- 0
  overall.end <- max(timings.wrangle$hospital.end, na.rm = T)

  # this generates a table of the status of every patient on every day
  
  complete.timeline <- map(1:nrow(timings.wrangle), function(pat.no){
    times <- map(overall.start:overall.end, function(day){
      # if(!timings.wrangle$ever.ICU[pat.no]){
      if(is.na(timings.wrangle$hospital.end[pat.no])){
        "Admitted"
      } else if(day < timings.wrangle$hospital.end[pat.no]){
        "Admitted"
      } else if(timings.wrangle$status[pat.no] == "death"){
        "Died"
      } else if(timings.wrangle$status[pat.no] == "discharge"){
        "Discharged"
      } else {
        "Transferred"
      }
      # } else {
      # if(day >= timings.wrangle$ICU.start[pat.no] & day < timings.wrangle$ICU.end[pat.no]){
      #   "ICU"
      # } else if(timings.wrangle$Censored[pat.no]){
      #   if(day >= timings.wrangle$ICU.end[pat.no]){
      #     "Censored"
      #   } else {
      #     "Admitted"
      #   }
      # } else if(day < timings.wrangle$hospital.end[pat.no]){
      #   "Admitted"
      # } else if(timings.wrangle$Died[pat.no]){
      #   "Dead"
      # } else {
      #   "Discharged"
      # }
      # }
    })
    names(times) <- glue::glue("day_{overall.start:overall.end}")
    times$subjid <- timings.wrangle$subjid[pat.no]
    times
  }) %>%
    bind_rows()
  
  n.days <- ncol(complete.timeline) - 1
  
  complete.timeline <- complete.timeline %>%
    pivot_longer(1:n.days, names_to = "day", values_to = "status") %>%
    dplyr::select(subjid, day, status) %>%
    dplyr::mutate(day = map_dbl(day, function(x) as.numeric(str_split_fixed(x, "_", 2)[2]))) %>%
    dplyr::mutate(status = factor(status, levels = c("Died", "Admitted", "Transferred", "Discharged"))) %>%
    ungroup() 
  
  ggplot(complete.timeline) + geom_bar(aes(x = day, fill = status), position = "fill", col = "black") +
    scale_fill_brewer(palette = "Set1", name  = "Status", drop = F) + 
    theme_bw() + 
    xlab("Days relative to admission") +
    ylab("Proportion")
}

