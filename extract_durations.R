#### SUMMARY: This script processes raw data from the ISARIC data base and produces refined data with fields as specified in the functions below ##

## Run entire script ##


## Import dataset ##

#################  UK ###############################


data <- read.csv('CCPUKSARI_DATA_2020-03-17_1211-1.csv', 
                 header = T, na.strings = c("", " ", "NA"))

# Rename field #
names(data)[1] <- 'subjid'



######## FUNCTIONS #############


## Function for returning a row for each unique case ##

unique.func <- function(data){
  
  nrows <- nrow(data)
  all.rows <- c(1:nrows)
  dup.positions <- which(duplicated(data$subjid))
  unique.rows <- all.rows[-(dup.positions)]
  
  return(unique.rows) 
  
}



## Function to include outcome and outcome dates ##

outcome.func <- function(data, data.2){
  
  ## Which rows in 'data' have dsstdtcyn entries ##
  outcome_positions <- which(!is.na(data$dsstdtcyn))
  
  ## Pick up the IDs to which they correspond ##
  IDs_outcome <- data[outcome_positions ,"subjid"]
  
  ## Pick up the actual outcomes ##
  outcomes <- data$dsterm[outcome_positions]
  
  ## Pick up the outcome indicator ##
  
  outcome_ind <- data$dsstdtcyn[outcome_positions]
  
  
  ## Pick up the dates ##
  outcome_dates <- data$dsstdtc[outcome_positions]
  
  
  ## Pick up the positions of those IDs in data.2 which have outcomes
  positions <- which(data.2$subjid %in% IDs_outcome)
  
  ## Fill in outcomes and dates at the given positions 
  data.2$dsterm[positions] <- outcomes
  data.2$dsstdtc[positions] <- outcome_dates
  data.2$dsstdtcyn[positions] <- outcome_ind
  
  return(data.2)
  
}



## Function for age calculation, given date of enrolment (dsstdat) and date of birth (agedat) [Add to data frame] ##

age.func <- function(data.2){
  
  birth_dates <- as.Date(data.2$agedat, tryFormats = "%d/%m/%Y") # dates of birth
  doe <- as.Date(data.2$dsstdat, tryFormats = "%d/%m/%Y")  # date of enrolment
  
  data.2$age <- floor(as.numeric(difftime(doe, birth_dates, unit="weeks"))/52.25)
  
  return(data.2)
}



## This function compares elements - returns TRUE when elements are the same (caters for NAs)##

compareNA <- function(v1,v2) {
  
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}




## Function to generate time from onset to admission, discharge, hospitalization e.t.c. ##

gen.times <- function(data.2, ref.date = '17/03/2020'){
  
  
  Admit.Discharge.any <- c()   # Discharge any  (admission to discharge)
  Onset.Hosp <- c()             # Onset on hospitalization
  Admit.Censored.any <- c()    # Censored for admission to discharge (any)
  
  
  
  data.2$Admit.Death  <- c(rep(NA, nrow(data.2)))          # Admission to death
  data.2$Admit.Recovered <- c(rep(NA, nrow(data.2)))
  #data.2$Status <- c(rep(NA, nrow(data.2)))
  a <- c()
  
  # Discharge any
  
  admit <- as.Date(data.2$hostdat, tryFormats = "%d/%m/%Y")  # hospitalization/admission dates
  discharge <- as.Date(data.2$dsstdtc, tryFormats = "%d/%m/%Y")
  onset <- as.Date(data.2$cestdat, tryFormats = "%d/%m/%Y")
  outcome.dates <- as.Date(data.2$dsstdtc, tryFormats = "%d/%m/%Y" )
  
  data.2$Admit.Discharge.any <- as.numeric(difftime(discharge, admit,  unit="days")) # Discharge any
  
  data.2$Onset.Hosp <- as.numeric(difftime(admit, onset, unit="days"))  # Onset hosp
  
  ref.date.2 <- as.Date(ref.date, tryFormats = "%d/%m/%Y") 
  
  for (i in 1:nrow(data.2)){
    
    # Censored any, if admit.disc.any is NA, then case has not yet been discharged and is censored; take difference between current date ad ref date, otherwise NA
    
    if(is.na(data.2$Admit.Discharge.any[i]) == 'TRUE'){
      
      data.2$Admit.Censored.any[i] <- as.numeric(difftime(ref.date.2, admit[i],  unit="days"))}else{
        
        data.2$Admit.Censored.any[i] <- NA 
      }
    
    if(compareNA(4, data.2$dsstdtcyn[i])=='TRUE'){
      
      data.2$Admit.Death[i] <- as.numeric(difftime(outcome.dates[i], admit[i],  unit="days"))
      
    }else{
      
      if(compareNA(1, data.2$dsstdtcyn[i])=='TRUE'){
        
        data.2$Admit.Recovered[i] <- as.numeric(difftime(outcome.dates[i], admit[i],  unit="days"))
        
      }
      
    }
    
  }
  
  return(data.2)
  
}




gen.times.2 <- function(data.2, ref.date = Sys.Date()){
  
  
  Admit.hospexit.any <- c()   # Discharge any  (admission to discharge)
  Onset.Hosp <- c()             # Onset on hospitalization
  Admit.Censored <- c()    # Censored for admission to discharge (any)
  Admit.ICU <- c()       # Admission to ICU
  Dur.ICU <- c()        # Duration of ICU
  Admit.Death <- c()   # Admission to death
  Admit.Recov <- c()    # Admission to recovery
  IMV.Dur <- c()    # Admission to invasive ventilation
  ECMO.Dur <- c()   # Admission to ECMO
  Inotrope.Dur <- c()     # Admission to inotropes/vasopressors;
  status <- c()   # status
  
  
  # Set up dates
  
  data.2$Admit.Death <- c(rep(NA, nrow(data.2)))
  
  data.2$Admit.Recov <- c(rep(NA, nrow(data.2)))
  
  
  
  admit <- as.Date(data.2$hostdat, tryFormats = "%d/%m/%Y")  # hospitalization/admission dates
  outcome.dates <- as.Date(data.2$dsstdtc, tryFormats = "%d/%m/%Y")
  onset <- as.Date(data.2$cestdat, tryFormats = "%d/%m/%Y")
  icu.entry.dates <- as.Date(data.2$icu_hostdat, tryFormats = "%d/%m/%Y" )
  icu.discharge.dates  <- as.Date(data.2$icu_hoendat, tryFormats = "%d/%m/%Y" )
  imv.dur <- data.2$invasive_prdur
  ecmo.dur <-  data.2$excorp_prdur
  inotrope.dur <- data.2$inotrope_cmdur
  
  
  
  # Outcome any
  
  data.2$Admit.hospexit.any <- as.numeric(difftime(outcome.dates, admit,  unit="days")) # Discharge any (any outcome; death or recovery)
  
  # Onset to hosp
  
  data.2$Onset.Hosp <- as.numeric(difftime(admit, onset, unit="days"))  # Onset hosp
  
  ref.date.2 <- as.Date(ref.date, tryFormats = "%d/%m/%Y") 
  
  # Admit.ICU
  
  data.2$Admit.ICU <- as.numeric(difftime(icu.entry.dates, admit,  unit="days")) # Admission to ICU
  
  
  # Dur ICU
  
  data.2$Dur.ICU <-  as.numeric(difftime(icu.discharge.dates, icu.entry.dates,  unit="days"))
  
  
  # IMV dur
  
  data.2$IMV.Dur <- imv.dur
  
  
  # ECMO Duration
  
  data.2$ECMO.Dur <- ecmo.dur
  
  
  # Vaso duration
  
  data.2$Inotrope.Dur <- inotrope.dur
  
  
  
  for (i in 1:nrow(data.2)){
    
    # Censored any, if admit.disc.any is NA, then case has not yet been discharged and is censored; take difference between current date ad ref date, otherwise NA
    
    if(is.na(data.2$Admit.hospexit.any[i]) == 'TRUE'){
      
      data.2$Censored[i] <- 1    # if no exit date, individual is censored
      
      #data.2$Admit.Outcome.any[i] <- as.numeric(difftime(ref.date.2, admit[i],  unit="days"))
      
      data.2$Admit.Censored[i] <- as.numeric(difftime(ref.date.2, admit[i],  unit="days"))}else{
        
        data.2$Censored[i] <- 0    # if exit date available, individual is not censored
        
        data.2$Admit.Censored[i] <- NA 
      }
    
    if((compareNA(4, data.2$dsterm[i]))=='TRUE'){
      
      data.2$status[i] <- 1 ;   # code for deaths
      
      data.2$Admit.Death[i] <- as.numeric(difftime(outcome.dates[i], admit[i],  unit="days"))
      
    }else{
      
      if(compareNA(1, data.2$dsterm[i])=='TRUE'){
      
        
        data.2$status[i] <- 2    # coode for recoveries
        
        data.2$Admit.Recov[i] <- as.numeric(difftime(outcome.dates[i], admit[i],  unit="days"))
        
      }else{
        data.2$status[i] <- 0
      }
      
    }
    
  }
  
  return(data.2)
  
}


gen.times.2(data.2)


# Processing data (function definitions above) #

# Select columns of interest (not change in name of dataframe) #

process.data <- function(data){
  
  
  data.2 <- data[unique.func(data), c(1, 11, 12, 13:18, 26, 27, 28, 64, 65, 88, 95, 96, 104:128, 133:148, 265, 266, 268, 269, 323:338, 
                                      380:390)]
  
  data.2 <- gen.times.2(age.func(outcome.func(data, data.2)))
  
  return(data.2)
  
}







### Data with desirable fields and values ####


data.uk <- process.data(data)
rownames(data.uk) <- NULL


write.csv(data.uk, file= 'data.uk.csv')


# UK: all 181 rows, for COV positive
#data.uk[data.uk$coriona_ieorres2==1 || data.uk$corona_ieorres==1, ]



































#### SUMMARY: This script processes raw data from the ISARIC data base and produces refined data with fields as specified in the functions below ##

## Run entire script ##


## Import dataset ##

############################### Int'l data ############################

data <- read.csv('ISARICnCoV_DATA_2020-03-17_1215.csv', header = T, na.strings = c("", " ", "NA"))



# Rename field #
names(data)[1] <- 'subjid'


######## FUNCTIONS #############


## Function for returning a row for each unique case ##

unique.func <- function(data){
  
  nrows <- nrow(data)
  all.rows <- c(1:nrows)
  dup.positions <- which(duplicated(data$subjid))
  unique.rows <- all.rows[-(dup.positions)]
  
  return(unique.rows) 
  
}




## Function to include outcome and outcome dates ##

outcome.func <- function(data, data.2){
  
  ## Which rows in 'data' have dsstdtcyn entries ##
  outcome_positions <- which(!is.na(data$dsstdtcyn))
  
  ## Pick up the IDs to which they correspond ##
  IDs_outcome <- data[outcome_positions ,"subjid"]
  
  ## Pick up the actual outcomes ##
  outcomes <- data$dsterm[outcome_positions]
  
  ## Pick up the outcome indicator ##
  
  outcome_ind <- data$dsstdtcyn[outcome_positions]
  
  
  ## Pick up the dates ##
  outcome_dates <- data$dsstdtc[outcome_positions]
  
  
  ## Pick up the positions of those IDs in data.2 which have outcomes
  positions <- which(data.2$subjid %in% IDs_outcome)
  
  ## Fill in outcomes and dates at the given positions 
  data.2$dsterm[positions] <- outcomes
  data.2$dsstdtc[positions] <- outcome_dates
  data.2$dsstdtcyn[positions] <- outcome_ind
  
  return(data.2)
  
}



## Function for rounding ages

age.func.2 <- function(data.2){
  
  
  data.2$age <- ceiling(data.2$age_estimateyears)
  
  return(data.2)
}



## This function compares elements - returns TRUE when elements are the same (caters for NAs)##

compareNA <- function(v1,v2) {
  
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}




## Function to generate time from onset to admission, discharge, hospitalization e.t.c. ##

gen.times <- function(data.2, ref.date = Sys.Date()){
  
  
  Admit.hospexit.any <- c()   # Discharge any  (admission to discharge)
  Onset.Hosp <- c()             # Onset on hospitalization
  Admit.Censored.any <- c()    # Censored for admission to discharge (any)
  Admit.ICU <- c()       # Admission to ICU
  Dur.ICU <- c()        # Duration of ICU
  Admit.Death <- c()   # Admission to death
  Admit.Recov <- c()    # Admission to recovery
  IMV.Dur <- c()    # Admission to invasive ventilation
  ECMO.Dur <- c()   # Admission to ECMO
  Inotrope.Dur <- c()     # Admission to inotropes/vasopressors;
  status <- c()   # status
  
  
  # Set up dates
  
  
  data.2$Admit.Death <- c(rep(NA, nrow(data.2)))
  
  data.2$Admit.Recov <- c(rep(NA, nrow(data.2)))
  
  
  
  admit <- as.Date(data.2$hostdat, tryFormats = "%d/%m/%Y")  # hospitalization/admission dates
  outcome.dates <- as.Date(data.2$dsstdtc, tryFormats = "%d/%m/%Y")
  onset <- as.Date(data.2$cestdat, tryFormats = "%d/%m/%Y")
  icu.entry.dates <- as.Date(data.2$icu_hostdat, tryFormats = "%d/%m/%Y" )
  icu.discharge.dates  <- as.Date(data.2$hoendat, tryFormats = "%d/%m/%Y" )
  imv.dur <- data.2$invasive_prdur
  inotrope.start <- as.Date(data.2$inotrope_cmstdat, tryFormats = "%d/%m/%Y" )
  inotrope.end <- as.Date(data.2$inotrope_cmendat, tryFormats = "%d/%m/%Y" )
  
  
  
  # Outcome any
  
  data.2$Admit.hospexit.any <- as.numeric(difftime(outcome.dates, admit,  unit="days")) # Discharge any (any outcome; death or recovery)
  
  # Onset to hosp
  
  data.2$Onset.Hosp <- as.numeric(difftime(admit, onset, unit="days"))  # Onset hosp
  
  ref.date.2 <- as.Date(ref.date, tryFormats = "%d/%m/%Y") 
  
  # Admit.ICU
  
  data.2$Admit.ICU <- as.numeric(difftime(icu.entry.dates, admit,  unit="days")) # Admission to ICU
  
  
  # Dur ICU
  
  data.2$Dur.ICU <-  as.numeric(difftime(icu.discharge.dates, icu.entry.dates,  unit="days"))
  
  
  # IMV dur
  
  data.2$IMV.Dur <- imv.dur
  
  
  # Vaso duration
  
  data.2$Inotrope.Dur <- as.numeric(difftime(inotrope.end, inotrope.start,  unit="days"))
  
  
  
  
  for (i in 1:nrow(data.2)){
    
    # Censored any, if admit.disc.any is NA, then case has not yet been discharged and is censored; take difference between current date ad ref date, otherwise NA
    
    if(is.na(data.2$Admit.hospexit.any[i]) == 'TRUE'){
      
      data.2$Censored[i] <- 1    # if no exit date, individual is censored
      
      #data.2$Admit.Outcome.any[i] <- as.numeric(difftime(ref.date.2, admit[i],  unit="days"))
      
      data.2$Admit.Censored[i] <- as.numeric(difftime(ref.date.2, admit[i],  unit="days"))}else{
        
        data.2$Censored[i] <- 0    # if exit date available, individual is not censored
        
        data.2$Admit.Censored[i] <- NA 
      }
    
    if(compareNA(4, data.2$dsterm[i])=='TRUE'){
      
      data.2$status[i] <- 1    # code for deaths
      
      data.2$Admit.Death[i] <- as.numeric(difftime(outcome.dates[i], admit[i],  unit="days"))
      
      
    }else{
      
      if(compareNA(1, data.2$dsterm[i])=='TRUE'){
        
        data.2$status[i] <- 2    # coode for recovered
        
        data.2$Admit.Recov[i] <- as.numeric(difftime(outcome.dates[i], admit[i],  unit="days"))
        
      }else{
        data.2$status[i] <- 0
      }
      
    }
    
  }
  
  return(data.2)
  
}


# For surv. ana, convert admission to x and duration times ti numbers for all and indicate whether censored or not 

# Processing data (function definitions above) #


process.data <- function(data){
  
  data.2 <- data[unique.func(data), c(1, 18, 20, 27, 28,  82:83, 109, 113, 122:156, 304:319, 332:334, 340:341)]
  
  data.2 <- gen.times(age.func.2(outcome.func(data, data.2)))
  
  return(data.2)
  
}


### Data with desirable fields and values ####


data.intl <- process.data(data)

write.csv(data.intl, file = 'data.intl.csv')



# Notes:

# For Covid-19, data$corona_ieorres == 1 ; suspected or proven COVID-19 cases
# nrow(data.2) # 70
#sum(data$corona_ieorres==1, na.rm= T) # 60
# 60 COVID cases, 10 non-COVID








######################## Merge both #######################


# To add more column columns, rename columns in data.uk to match those of the data.intl (or vice-versa).

comcol_ind <- which(names(data.uk) %in% names(data.intl))

com.names <- names(data.uk[comcol_ind])

data <- rbind(data.intl[, com.names], data.uk[, com.names])
row.names(data) <- NULL

dim(data)
head(data)
