############ SUMMARY: Script for data summaries ###############


############################################################################

# Main function d.e (dynamic.estimates) calculates estimates and confidence intervals, to be incorporated into report. #
d.e <- function(data, ...){
  
  # Summaries 
  
  N.cases <- nrow(data)      # total
  N.var <- ncol(data)  # number of variables
  N.sites <- length(unique(data$site.name)) # number of sites 
  N.countries <- length(unique(data$Country)) # number of countries
  median.age <- median(data$age_estimateyears, na.rm = T) # median age (observed)
  mean.age <-  mean(data$age_estimateyears, na.rm = T)  # mean age
  sd.age <- sd(data$age_estimateyears, na.rm = T)
  
  min.age <- ceiling(min(data$age_estimateyears, na.rm=T)) # minimum age
  max.age <- ceiling(max(data$age_estimateyears, na.rm=T)) # maximum age
  
  # ages by sex
  m <- data[data$sex=='1', ]
  f <-  data[data$sex=='2', ]
  
  m.age.mean <- mean(m$age_estimateyears, na.rm=T)
  m.age.sd <- sd(m$age_estimateyears, na.rm=T)
  f.age.mean <- mean(f$age_estimateyears, na.rm=T)
  f.age.sd <-  sd(f$age_estimateyears, na.rm=T)
  
  
  N.males <- summary(as.factor(data$sex))[[1]] # males-count
  N.females <- summary(as.factor(data$sex))[[2]] # females-count
  N.sex.unknown <- N.cases - N.males - N.females # unknown-count
  N.censored <- summary(as.factor(data$outcome))[[1]]  # censored-count
  N.deaths <- summary(as.factor(data$outcome))[[2]]    # deaths-count
  N.recoveries <- summary(as.factor(data$outcome))[[3]]   # recoveries -count
  N.outcomes <- N.deaths+N.recoveries         # outcomes-count (deaths+recoveries)
  #N.ICU <- sum(!is.na(data$Admit.ICU))       # ICU-admissions-count
  N.healthworkers <- summary(as.factor(patient.data$healthwork_erterm))[[1]]
  
  eth.1 <- N.cases - summary(as.factor(data$ethnic___1))[[1]]
  eth.2 <- N.cases - summary(as.factor(data$ethnic___2))[[1]]
  eth.3 <- N.cases - summary(as.factor(data$ethnic___3))[[1]]
  eth.4 <- N.cases - summary(as.factor(data$ethnic___4))[[1]]
  eth.5 <- N.cases - summary(as.factor(data$ethnic___5))[[1]]
  eth.6 <- N.cases - summary(as.factor(data$ethnic___6))[[1]]
  eth.7 <- N.cases - summary(as.factor(data$ethnic___7))[[1]]
  eth.8 <- N.cases - summary(as.factor(data$ethnic___8))[[1]]
  eth.9 <- N.cases - summary(as.factor(data$ethnic___9))[[1]]
  eth.10 <-N.cases -  summary(as.factor(data$ethnic___10))[[1]]
  eth.NA <- N.cases - sum(eth.1, eth.2, eth.3, eth.4, eth.5, eth.6, eth.7, eth.8, eth.9, eth.10)
  
  
  # Distribution estimates
  
  # Admission to outcome
  
  mean.adm.to.outcome <-  round(fit.summary.gamma(adm.outcome.func(data)$fit)$m, 1)
  adm.outcome.lower <-   round(fit.summary.gamma(adm.outcome.func(data)$fit)$lower.m, 1)
  adm.outcome.upper <-   round(fit.summary.gamma(adm.outcome.func(data)$fit)$upper.m, 1)
  
  
  
  # Onset to admission
  
  # sd.onset.to.adm <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$v), 1)
  # sd.onset.to.adm.lower <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$lower.v), 1)
  # sd.onset.to.adm.upper <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$upper.v), 1)
  # 
  
  mean.onset.to.adm <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$m, 1)
  mean.onset.to.adm.lower <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$lower.m, 1)
  mean.onset.to.adm.upper <-  round(fit.summary.gamma(onset.adm.func(data)$fit)$upper.m, 1)
  

  
  # Distribution estimates - OBSERVED
  
  # Admission to outcome
  
  x <- adm.outcome.func(data)$obs
  x_mean <- mean(x, na.rm = T)
  x_sd <- sd(x, na.rm = T)
  
  obs.mean.adm.outcome <- round(x_mean, 1)
  obs.mean.adm.outcome.lower <- round( x_mean-1.96*(x_sd/sqrt(length(x))), 1)
  obs.mean.adm.outcome.upper <- round( x_mean+1.96*(x_sd/sqrt(length(x))), 1)
  obs.sd.adm.outcome <- round(x_sd, 2)
  
  
  # Onset to admission
  
  y <- onset.adm.func(data)$obs
  y_mean <- sd(y, na.rm = T)
  y_sd <- mean(y, na.rm = T)
  
  obs.mean.onset.adm <- round(y_mean, 1)
  obs.mean.onset.adm.lower <- round( y_mean - 1.96*(x_sd/sqrt(length(y))), 1)
  obs.mean.onset.adm.upper <-  round( y_mean + 1.96*(x_sd/sqrt(length(y))), 1)
  obs.sd.onset.adm <- round(y_sd, 2)
  
  
  # Admission to ICU (not fitted due to low count)
  
  a.icu <- data$admission.to.ICU
  a.icu <- round.zeros(abs(a.icu[!is.na(a.icu)]))
  
  
  # ICU duration 
  
  d.icu <- data$ICU.duration
  d.icu <- round.zeros(abs(d.icu[!is.na(d.icu)]))
  
  
  
  # Admission to IMV
  
  a.imv <- patient.data$admission.to.IMV
  a.imv <- round.zeros(abs(a.imv[!is.na(a.imv)]))
  
  
  # IMV duration
  
  d.imv <- data$IMV.duration
  d.imv <- round.zeros(abs(d.imv[!is.na(d.imv)]))
  
  
  # Admission to NIMV
  
  a.nimv <- patient.data$admission.to.NIMV
  a.nimv <- round.zeros(abs(a.nimv[!is.na(a.nimv)]))
  
  
  # NIMV duration
  d.nimv <- patient.data$NIMV.duration
  d.nimv <- round.zeros(abs(d.nimv[!is.na(d.nimv)]))
  
  
  # CFR
  
  cfr <- round(casefat2(data)$cfr, 2)
  cfr.lower <-  round(casefat2(data)$lcfr, 2)
  cfr.upper <-   round(casefat2(data)$ucfr, 2)
  
  
  # HFR
  db <- hospital.fatality.ratio(data)$db
  hfr <- round(db[nrow(db), 'mean'], 2)
  hfr.upper <- round(db[nrow(db), 'upper'], 2)
  hfr.lower <- round(db[nrow(db), 'lower'], 2)
  
  # Treatments data
  df <- treatment.upset(data)$df
  n.treat <- df$All
  p.none <- 100 * df$None / df$All 
  p.none <- paste(sprintf("%.1f", p.none))
  p.abx <- 100 * df$Abx / df$All 
  p.abx <- paste(sprintf("%.1f", p.abx))
  p.av <- 100 * df$Av / df$All
  p.av <- paste(sprintf("%.1f", p.av))
  
  # Symptoms 
  
  s.dat <- symptom.prevalence(data)$data3
  
  # Comorbidities
  
  c.dat <- comorbidity.prevalence(data)$data3
  
  # Treatments
  
  t.dat <- treatment.use.plot(data)$data3
  
  
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
              
              m.age.mean =  m.age.mean,
              m.age.sd  =   m.age.sd,
              f.age.mean = f.age.mean,
              f.age.sd = f.age.sd,
              
              eth.1 = eth.1,
              eth.2 = eth.2,
              eth.3 = eth.3,
              eth.4 = eth.4,
              eth.5 = eth.5,
              eth.6 = eth.6,
              eth.7 = eth.7,
              eth.8 = eth.8,
              eth.9 = eth.9,
              eth.10 = eth.10,
              eth.NA = eth.NA,
              
              mean.adm.to.outcome =  mean.adm.to.outcome,
              adm.outcome.lower =  adm.outcome.lower,
              adm.outcome.upper = adm.outcome.upper,
              
              mean.onset.to.adm = mean.onset.to.adm ,
              mean.onset.to.adm.lower =   mean.onset.to.adm.lower,
              mean.onset.to.adm.upper = mean.onset.to.adm.upper,
              
              # sd.onset.to.adm = sd.onset.to.adm,
              # sd.onset.to.adm.lower  = sd.onset.to.adm.lower ,
              # sd.onset.to.adm.upper = sd.onset.to.adm.upper,
          
              obs.mean.adm.outcome =  obs.mean.adm.outcome,
              obs.mean.adm.outcome.lower = obs.mean.adm.outcome.lower,
              obs.mean.adm.outcome.upper = obs.mean.adm.outcome.upper,
              obs.sd.adm.outcome  = obs.sd.adm.outcome,
              
              obs.mean.onset.adm =  obs.mean.onset.adm,
              obs.mean.onset.adm.lower = obs.mean.onset.adm.lower,
              obs.mean.onset.adm.upper = obs.mean.onset.adm.upper,
              obs.sd.onset.adm = obs.sd.onset.adm,
              
              a.icu = a.icu,
              d.icu = d.icu,
              
              a.imv = a.imv,
              d.imv = d.imv,
              
              a.nimv = a.nimv,
              d.nimv = d.nimv,
              
              
              cfr = cfr,
              cfr.lower = cfr.lower,
              cfr.upper = cfr.upper,
             
              hfr = hfr,
              hfr.lower = hfr.lower,
              hfr.upper =  hfr.upper,
              #N.ICU = N.ICU
              
              n.treat = n.treat,
              p.none = p.none,
              p.abx = p.abx,
              p.av = p.av,
              
              s.dat = s.dat,
              c.dat = c.dat,
              t.dat = t.dat
  ))
  
  
}

de <- d.e(patient.data)
