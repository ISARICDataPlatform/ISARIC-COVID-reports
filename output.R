############ SUMMARY: Script for data summaries ###############


############################################################################

# Main function d.e (dynamic.estimates) calculates estimates and confidence intervals, to be incorporated into report. #
d.e <- function(data, ...){
  
  # Summaries 
  
  N.cases <- nrow(data)      # total
  N.var <- ncol(data)  # number of variables
  N.sites <- length(unique(data$site.name)) # number of sites 
  N.countries = length(unique(data$Country)) # number of countries
  median.age <- median(data$age_estimateyears, na.rm = T) # median age (observed)
  min.age <- ceiling(min(data$age_estimateyears, na.rm=T)) # minimum age
  max.age <- ceiling(max(data$age_estimateyears, na.rm=T)) # maximum age
  N.males <- summary(as.factor(data$sex))[[1]] # males-count
  N.females <- summary(as.factor(data$sex))[[2]] # females-count
  N.sex.unknown <- N.cases - N.males - N.females # unknown-count
  N.censored <- summary(as.factor(data$outcome))[[1]]  # censored-count
  N.deaths <- summary(as.factor(data$outcome))[[2]]    # deaths-count
  N.recoveries <- summary(as.factor(data$outcome))[[3]]   # recoveries -count
  N.outcomes <- N.deaths+N.recoveries         # outcomes-count (deaths+recoveries)
  #N.ICU <- sum(!is.na(data$Admit.ICU))       # ICU-admissions-count
  N.healthworkers <- summary(as.factor(patient.data$healthwork_erterm))[[1]]
  
  # Distribution estimates
  
  mean.adm.to.outcome <-  round(fit.summary.gamma(adm.outcome.func(data)$fit)$m, 1)
  adm.outcome.lower <-   round(fit.summary.gamma(adm.outcome.func(data)$fit)$lower.m, 1)
  adm.outcome.upper <-   round(fit.summary.gamma(adm.outcome.func(data)$fit)$upper.m, 1)
  
  mean.onset.to.adm <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$m, 1)
  mean.onset.to.adm.lower <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$lower.m, 1)
  mean.onset.to.adm.upper <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$upper.m, 1)
  
  var.onset.to.adm <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$v, 1)
  var.onset.to.adm.lower <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$lower.v, 1)
  var.onset.to.adm.upper <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$upper.v, 1)
  
  
  # CFR
  
  cfr <- round(casefat2(data)$cfr, 2)
  cfr.lower <-  round(casefat2(data)$lcfr, 2)
  cfr.upper <-   round(casefat2(data)$ucfr, 2)
  
  
  # HFR
  db <- hospital.fatality.ratio(data)$db
  hfr <- round(db[nrow(db), 'mean'], 2)
  hfr.upper <- round(db[nrow(db), 'upper'], 2)
  hfr.lower <- round(db[nrow(db), 'lower'], 2)
  
  
  return(list(N.cases = N.cases,
              N.var = N.var,
              N.sites = N.sites,
              N.countries = N.countries,
              median.age = median.age,
              min.age = min.age,
              max.age = max.age,
              N.males = N.males,
              N.females = N.females,
              N.sex.unknown = N.sex.unknown,
              N.censored = N.censored,
              N.deaths = N.deaths,
              N.recoveries = N.recoveries,
              N.outcomes = N.outcomes,
              N.healthworkers = N.healthworkers,
              
              mean.adm.to.outcome =  mean.adm.to.outcome,
              adm.outcome.lower =  adm.outcome.lower,
              adm.outcome.upper = adm.outcome.upper,
              
              mean.onset.to.adm = mean.onset.to.adm ,
              mean.onset.to.adm.lower =   mean.onset.to.adm.lower,
              mean.onset.to.adm.upper = mean.onset.to.adm.upper,
              
              var.onset.to.adm = var.onset.to.adm,
              var.onset.to.adm.lower  = var.onset.to.adm.lower ,
              var.onset.to.adm.upper = var.onset.to.adm.upper,
              
              cfr = cfr,
              cfr.lower = cfr.lower,
              cfr.upper = cfr.upper,
             
              hfr = hfr,
              hfr.lower = hfr.lower,
              hfr.upper =  hfr.upper
              #N.ICU = N.ICU
  ))
  
}

de <- d.e(patient.data)
