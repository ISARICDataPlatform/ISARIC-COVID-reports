############ SUMMARY: Script for data summaries ###############


############################################################################

# Main function d.e (dynamic.estimates) calculates estimates and confidence intervals, to be incorporated into report. #
d.e <- function(data, datafull, ...){
  
  # Summaries 
  
  N.cases <- nrow(data)      # total embargoed
  N.cases.full <- nrow(datafull)
  N.var <- ncol(data)  # number of variables
  N.sites.full <- length(unique(datafull$site.name)) # number of sites 
  N.countries.full <- length(unique(datafull$Country)) # number of countries
  median.age <- median(data$age_estimateyears, na.rm = T) # median age (observed)
  mean.age <-  mean(data$age_estimateyears, na.rm = T)  # mean age
  sd.age <- sd(data$age_estimateyears, na.rm = T)
  
  min.age <- ceiling(min(data$age_estimateyears, na.rm=T)) # minimum age
  max.age <- ceiling(max(data$age_estimateyears, na.rm=T)) # maximum age
  
  embargo.length <- 14
  embargo.limit <- format(today() - embargo.length, "%d %B %Y")
  
  
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
  
  
  # Distribution estimates
  
  # Admission to outcome
  
  adm.out.func.1 <- adm.outcome.func(data)
  gamma.adm.outcome.fit <- fit.summary.gamma(adm.out.func.1$fit)
  
  mean.adm.to.outcome <-  round(gamma.adm.outcome.fit$m, 1)
  adm.outcome.lower <-   round(gamma.adm.outcome.fit$lower.m, 1)
  adm.outcome.upper <-   round(gamma.adm.outcome.fit$upper.m, 1)
  median.adm.to.outcome <- round(gamma.adm.outcome.fit$bmed$t0, 1)
  
  
  # Onset to admission
  
  # sd.onset.to.adm <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$v), 1)
  # sd.onset.to.adm.lower <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$lower.v), 1)
  # sd.onset.to.adm.upper <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$upper.v), 1)
  # 
  
  onset.adm.fn.1 <- onset.adm.func(data)
  gamma.onset.adm.fit <- fit.summary.gamma(onset.adm.fn.1$fit)
  
  
  mean.onset.to.adm <-  round(gamma.onset.adm.fit$m, 1)
  mean.onset.to.adm.lower <-  round(gamma.onset.adm.fit$lower.m, 1)
  mean.onset.to.adm.upper <-  round(gamma.onset.adm.fit$upper.m, 1)
  median.onset.to.adm <- round(gamma.onset.adm.fit$bmed$t0, 1)

  
  # Distribution estimates - OBSERVED
  
  # Admission to outcome
  
  x <- adm.out.func.1$obs
  x_mean <- mean(x, na.rm = T)
  x_sd <- sd(x, na.rm = T)
  
  obs.mean.adm.outcome <- round(x_mean, 1)
  obs.mean.adm.outcome.lower <- round( x_mean-1.96*(x_sd/sqrt(length(x))), 1)
  obs.mean.adm.outcome.upper <- round( x_mean+1.96*(x_sd/sqrt(length(x))), 1)
  obs.sd.adm.outcome <- round(x_sd, 2)
  cases.full.adm.outcome <- length(adm.outcome.func(patient.data)$obs)
  
  # Onset to admission
  
  y <- onset.adm.fn.1$obs
  y_mean <- sd(y, na.rm = T)
  y_sd <- mean(y, na.rm = T)
  
  obs.mean.onset.adm <- round(y_mean, 1)
  obs.mean.onset.adm.lower <- round( y_mean - 1.96*(y_sd/sqrt(length(y))), 1)
  obs.mean.onset.adm.upper <-  round( y_mean + 1.96*(y_sd/sqrt(length(y))), 1)
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
  cfr.lower <-round(casefat2(data)$lcfr, 2)
  cfr.upper <-  round(casefat2(data)$ucfr, 2)
  
  
  # HFR
  db <- hospital.fatality.ratio(data)$db
  hfr <- round(db[nrow(db), 'mean'], 2)
  hfr.upper <- round(db[nrow(db), 'upper'], 2)
  hfr.lower <- round(db[nrow(db), 'lower'], 2)
  
  # Treatments data
  df <- treatment.upset.numbers(data)
  n.treat <- df$All
  p.none <- 100 * df$None / df$All 
  p.none <- paste(sprintf("%.1f", p.none))
  p.abx <- 100 * df$Abx / df$All 
  p.abx <- paste(sprintf("%.1f", p.abx))
  p.av <- 100 * df$Av / df$All
  p.av <- paste(sprintf("%.1f", p.av))
  
  n.o2 <- df$O2
  p.o2 <- 100 * df$O2 / df$All
  p.o2 <- paste(sprintf("%.1f", p.o2))
  
  # Note, proportions of ventilation have denominator O2
  p.NIV <- 100 * df$NIV / df$O2
  p.NIV <- paste(sprintf("%.1f", p.NIV))
  p.In.Ven <- 100 * df$Inv.ven / df$O2
  p.In.Ven <- paste(sprintf("%.1f", p.In.Ven))
  
  
  # Symptoms 
  
  s.dat <- symptom.prevalence(data)$data3
  
  # Comorbidities
  
  c.dat <- comorbidity.prevalence(data)$data3
  
  # Treatments
  
  t.dat <- treatment.use.plot(data)$data3
  
  # Cough
  
  cough_pre <- summary(as.factor(patient.data$cough))[[2]]
  cough_abs <- summary(as.factor(patient.data$cough))[[1]]
  cough_unk <- summary(as.factor(patient.data$cough))[[3]]
  
  # p-value
  
  #pval <- surv.plot.func(patient.data)$pval
  
  # Sum
  
  #surv.sum <- sum(table(surv.plot.func(data)$df$sex, surv.plot.func(data)$df$event))
  
  
  return(list(N.cases = N.cases,
              N.cases.full = N.cases.full,
              N.var = N.var,
              N.sites.full = N.sites.full,
              N.countries.full = N.countries.full,
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
              embargo.limit = embargo.limit,
              
              m.age.mean =  m.age.mean,
              m.age.sd  =   m.age.sd,
              f.age.mean = f.age.mean,
              f.age.sd = f.age.sd,
              
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
              cases.full.adm.outcome = cases.full.adm.outcome, 
              
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
              
              n.o2 = n.o2,
              p.o2 = p.o2,
              p.NIV = p.NIV,
              p.In.Ven = p.In.Ven,
              
              s.dat = s.dat,
              c.dat = c.dat,
              t.dat = t.dat,
              
              cough_pre = cough_pre,
              cough_abs = cough_abs,
              cough_unk = cough_unk
              
              
              #surv.sum = surv.sum
  ))
  
  
}
print("HHHSHGDGHSHRHRS")
de <- d.e(patient.data, unembargoed.data)

save(de, file = glue("{code.path}/report_input_data_{today()}.rda"))
