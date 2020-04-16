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
  median.age <- round(median(data$age_estimateyears, na.rm = T), 1) # median age (observed)
  mean.age <-  mean(data$age_estimateyears, na.rm = T)  # mean age
  sd.age <- sd(data$age_estimateyears, na.rm = T)
  
  min.age <- ceiling(min(data$age_estimateyears, na.rm=T)) # minimum age
  max.age <- ceiling(max(data$age_estimateyears, na.rm=T)) # maximum age
  
  transfer.outcome <- sum(summary(as.factor(data$exit.code))[['transfer']],  summary(as.factor(data$exit.code))[['transfer.palliative']])
  unk.outcome <-  sum(summary(as.factor(data$exit.code))[['hospitalisation']], summary(as.factor(data$exit.code))[['unknown']]) # 'Hospitalisation' entries mostly mean the data collection wasn't completed 
  
  
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
  N.healthworkers <- summary(as.factor(data$healthwork_erterm))[[1]]
  
  
  
  # Outcome by age and sex
  
  age.out.tab <- table( data$agegp10, data$outcome)
  sex.out.tab <- table(data$sex, data$outcome)
  
  age.out.tab2 <-  table( data$agegp10, data$exit.code)
  sex.out.tab2 <- table(data$sex, data$exit.code)
  
  
  # COV status
  
  cov.19.confirmed <- summary(data$positive.COV19.test)[['TRUE']]
  cov.19.suspected <- summary(data$positive.COV19.test)[['FALSE']]
  
  # Evers
  
  IMV.pr <- sum(data$IMV.ever=='TRUE', na.rm=T)
  IMV.ab <- sum(data$IMV.ever=='FALSE', na.rm=T)
  IMV.un <- N.cases -IMV.pr - IMV.ab
  
  
  NIMV.pr <- sum(data$NIMV.ever=='TRUE', na.rm=T)
  NIMV.ab <- sum(data$NIMV.ever=='FALSE', na.rm=T)
  NIMV.un <- N.cases - NIMV.pr - NIMV.ab
  
  o2.pr <- sum(data$O2.ever=='TRUE', na.rm=T)
  o2.ab <- sum(data$O2.ever=='FALSE', na.rm=T)
  o2.un <- N.cases - o2.pr - o2.ab
  
  ECMO.pr <- sum(data$ECMO.ever=='TRUE', na.rm=T)
  ECMO.ab <-  sum(data$ECMO.ever=='FALSE', na.rm=T)
  ECMO.un <-  N.cases -  ECMO.pr -   ECMO.ab 
  
  # Distribution estimates - EXPECTED 
  
  # Admission to outcome
  
  adm.out.func.1 <- adm.outcome(data)
  adm.out.summ <- fit.summary.gamma(adm.out.func.1$fit)
  
  adm.to.outcome <-  round(adm.out.summ$m, 1)
  adm.outcome.l <-   round(adm.out.summ$lower.m, 1)
  adm.outcome.u <-   round(adm.out.summ$upper.m, 1)
  
  # Onset to admission
  
  # sd.onset.to.adm <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$v), 1)
  # sd.onset.to.adm.lower <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$lower.v), 1)
  # sd.onset.to.adm.upper <-  round(sqrt(fit.summary.gamma(onset.adm.func(data)$fit)$upper.v), 1)
  # 
  
  onset.adm.fn.1 <- onset.adm(data)
  onset.adm.summ <- fit.summary.gamma(onset.adm.fn.1$fit)
  
  
  onset.to.adm <-  round(onset.adm.summ$m, 1)
  onset.to.adm.l <-  round(onset.adm.summ$lower.m, 1)
  onset.to.adm.u <-  round(onset.adm.summ$upper.m, 1)
  
  
  
  # Admission to ICU
  
  adm.to.icu.1 <- adm.to.icu(data)
  adm.icu.summ <-  fit.summary.gamma(adm.to.icu.1$fit)
  
  
  adm.icu <- round(adm.icu.summ$m, 1)
  adm.icu.l <- round(adm.icu.summ$lower.m, 1)
  adm.icu.u <- round(adm.icu.summ$upper.m, 1)
  
  
  
  # Duration of ICU (more censored cases than cases with outcomes; causing mle error)
  
  
  # dur.icu.1 <- dur.icu(data)
  # dur.icu.summ <- fit.summary.gamma(dur.icu.1$fit)
  
  
  # dur.icu <- round( dur.icu.summ$m, 1)
  # dur.icu.u <- round(dur.icu.summ$lower.m, 1)
  #  dur.icu.l <- round(dur.icu.summ$upper.m, 1)
  
  
  
  # Admission to IMV
  
  adm.imv.1 <- adm.to.imv(data)
  adm.imv.summ <- fit.summary.gamma(adm.imv.1$fit)
  
  
  adm.imv <- round(adm.imv.summ$m, 1)
  adm.imv.l <- round(adm.imv.summ$lower.m, 1)
  adm.imv.u <- round(adm.imv.summ$upper.m, 1)
  
  
  
  
  # # Duration of IMV (more censored cases than cases with outcomes; causing mle error)
  # 
  # 
  # dur.imv.1 <- dur.imv(data)
  # dur.imv.summ <- fit.summary.gamma(dur.imv.1$fit)
  # 
  # 
  # dur.imv <- round(dur.imv.summ$m, 1)
  # dur.imv.l <- round(dur.imv.summ$lower.m, 1)
  # dur.imv.u <- round(dur.imv.summ$upper.m, 1)
  # 
  
  # Admission to NIV
  
  adm.niv.1 <- adm.to.niv(data)
  adm.niv.summ <- fit.summary.gamma(adm.niv.1$fit)
  
  
  adm.niv <- round(adm.niv.summ$m, 1)
  adm.niv.l <- round(adm.niv.summ$lower.m, 1)
  adm.niv.u <- round(adm.niv.summ$upper.m, 1)
  
  
  
  
  
  # Duration of NIV
  
  dur.niv.1 <- dur.niv(data)
  dur.niv.summ <- fit.summary.gamma(dur.niv.1$fit)
  
  
  dur.niv <- round(dur.niv.summ$m, 1)
  dur.niv.l <- round(dur.niv.summ$lower.m, 1)
  dur.niv.u <- round(dur.niv.summ$upper.m, 1)
  
  
  
  
  
  
  # Distribution estimates - OBSERVED
  
  # Admission to outcome
  
  x <- adm.out.func.1$obs
  x_mean <- mean(x, na.rm = T)
  x_sd <- sd(x, na.rm = T)
  x_median <- median(x, na.rm = T)
  
  obs.mean.adm.outcome <- round(x_mean, 1)
  obs.mean.adm.outcome.lower <- round( x_mean-1.96*(x_sd/sqrt(length(x))), 1)
  obs.mean.adm.outcome.upper <- round( x_mean+1.96*(x_sd/sqrt(length(x))), 1)
  obs.sd.adm.outcome <- round(x_sd, 1)
  cases.full.adm.outcome <- length(adm.outcome(patient.data)$obs)
  obs.median.adm.outcome <- round(x_median, 1)
  obs.iqr.adm.outcome <- round(IQR(x, na.rm = T), 1)
  
  # Onset to admission
  
  y <- onset.adm.fn.1$obs
  y_mean <- sd(y, na.rm = T)
  y_sd <- mean(y, na.rm = T)
  y_median <- median(y, na.rm = T)
  
  obs.mean.onset.adm <- round(y_mean, 1)
  obs.mean.onset.adm.lower <- round( y_mean - 1.96*(y_sd/sqrt(length(y))), 1)
  obs.mean.onset.adm.upper <-  round( y_mean + 1.96*(y_sd/sqrt(length(y))), 1)
  obs.sd.onset.adm <- round(y_sd, 1)
  obs.median.onset.adm <- round(y_median, 1)
  obs.iqr.onset.adm <- round(IQR(y, na.rm = T), 1)
  
  
  # Admission to ICU (not fitted due to low count)
  
  a.icu <- data$admission.to.ICU
  a.icu <- round.zeros(abs(a.icu[!is.na(a.icu)]))
  
  
  # ICU duration 
  
  d.icu <- data$ICU.duration
  d.icu <- round.zeros(abs(d.icu[!is.na(d.icu)]))
  
  
  
  # Admission to IMV
  
  a.imv <- data$admission.to.IMV
  a.imv <- round.zeros(abs(a.imv[!is.na(a.imv)]))
  
  
  # IMV duration
  
  d.imv <- data$IMV.duration
  d.imv <- round.zeros(abs(d.imv[!is.na(d.imv)]))
  
  
  # Admission to NIMV
  
  a.nimv <- data$admission.to.NIMV
  a.nimv <- round.zeros(abs(a.nimv[!is.na(a.nimv)]))
  
  
  # NIMV duration
  d.nimv <- data$NIMV.duration
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
  df <- make.props.treats(data)
  n.treat <- df$N
  p.abx <- paste(sprintf("%.1f", 100 * df$n.abx / df$N))
  p.av <- paste(sprintf("%.1f", 100 * df$n.av / df$N))
  p.o2 <- paste(sprintf("%.1f", 100 * df$n.O2 / df$N))
  # Note, proportions of ventilation have denominator O2
  p.NIV <- paste(sprintf("%.1f", 100 * df$n.NIV / df$n.O2))
  p.In.Ven <- paste(sprintf("%.1f", 100 * df$n.IMV / df$n.O2))
  n.o2 <- df$n.O2
  
  # ICU treatments
  df <- get_icu_pts(data) %>%
    make.props.treats()
  icu.n.treat <- df$N
  icu.p.abx <- paste(sprintf("%.1f", 100 * df$n.abx / df$N))
  icu.p.av <- paste(sprintf("%.1f", 100 * df$n.av / df$N))
  icu.p.o2 <- paste(sprintf("%.1f", 100 * df$n.O2 / df$N))
  # Note, proportions of ventilation have denominator O2
  icu.p.NIV <- paste(sprintf("%.1f", 100 * df$n.NIV / df$n.O2))
  icu.p.In.Ven <- paste(sprintf("%.1f", 100 * df$n.IMV / df$n.O2))
  # Putcomes for ICU patient
  icu.d <- get_icu_pts(data)
  N.icu.censored <- summary(as.factor(icu.d$outcome))[[1]]  # censored-count
  N.icu.deaths <- summary(as.factor(icu.d$outcome))[[2]]    # deaths-count
  N.icu.recoveries <- summary(as.factor(icu.d$outcome))[[3]]   # recoveries -count
  N.icu.NA <- summary(as.factor(icu.d$outcome))[[4]]      # ICU NA
  # outcomes 
  
  # Symptoms 
  
  s.dat <- symptom.prev.calc(data)
  
  # Comorbidities
  
  c.dat <- comorb.prev.calc(data)
  
  # Treatments
  
  t.dat <- treatment.use.calc(data)
  
  # Cough
  
  cough_pre <- patient.data %>% filter(cough.any == 1) %>% nrow()
  cough_abs <- patient.data %>% filter(cough.any == 2) %>% nrow()
  cough_unk <- patient.data %>% filter(is.na(cough.any)) %>% nrow()
  
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
              
              
              m.age.mean =  m.age.mean,
              m.age.sd  =   m.age.sd,
              f.age.mean = f.age.mean,
              f.age.sd = f.age.sd,
              
              transfer.outcome = transfer.outcome,
              unk.outcome = unk.outcome,
              
              cov.19.confirmed =  cov.19.confirmed,
              cov.19.suspected =  cov.19.suspected,
              
              age.out.tab = age.out.tab,
              age.out.tab2 = age.out.tab2,
              
              sex.out.tab = sex.out.tab,
              sex.out.tab2 = sex.out.tab2,
              
              IMV.pr = IMV.pr,
              IMV.ab = IMV.ab,
              IMV.un = IMV.un,
              
              NIMV.pr = NIMV.pr,
              NIMV.ab = NIMV.ab,
              NIMV.un = NIMV.un,
              
              
              o2.pr = o2.pr,
              o2.ab = o2.ab,
              o2.un = o2.un,
              
              
              ECMO.pr = ECMO.pr,
              ECMO.ab = ECMO.ab,
              ECMO.un = ECMO.un,
              
              
              adm.to.outcome =  adm.to.outcome,            # admission to outcome
              adm.outcome.l =  adm.outcome.l,
              adm.outcome.u = adm.outcome.u,
              
              
              onset.to.adm = onset.to.adm ,             # onset to admission
              onset.to.adm.lower =   onset.to.adm.l,
              onset.to.adm.u = onset.to.adm.u,
              
              
              adm.icu = adm.icu,                   # admission to ICU
              adm.icu.l  =adm.icu.l,
              adm.icu.u = adm.icu.u,
              
              #  
              # dur.icu =dur.icu,                     # Duration of ICU
              # dur.icu.l  =adm.icu.l,
              # dur.icu.u =dur.icu.u,
              
              adm.niv = adm.niv,                    # Admission to NIV         
              adm.niv.l =adm.niv.l,
              adm.niv.u = adm.niv.u,
              
              dur.niv =dur.niv,                    # NIV duration
              dur.niv.l =dur.niv.l,
              dur.niv.u =dur.niv.u,
              
              
              adm.imv = adm.imv,                    # Admission to IMV     
              adm.imv.l =adm.imv.l,
              adm.imv.u = adm.imv.u,
              # 
              # dur.imv =dur.imv,                    # IMV duration
              # dur.imv.l =dur.imv.l,
              # dur.imv.u =dur.imv.u,
              # 
              
              # sd.onset.to.adm = sd.onset.to.adm,
              # sd.onset.to.adm.lower  = sd.onset.to.adm.lower ,
              # sd.onset.to.adm.upper = sd.onset.to.adm.upper,
              
              obs.mean.adm.outcome =  obs.mean.adm.outcome,
              obs.mean.adm.outcome.lower = obs.mean.adm.outcome.lower,
              obs.mean.adm.outcome.upper = obs.mean.adm.outcome.upper,
              obs.sd.adm.outcome  = obs.sd.adm.outcome,
              cases.full.adm.outcome = cases.full.adm.outcome, 
              obs.median.adm.outcome = obs.median.adm.outcome,
              obs.iqr.adm.outcome =   obs.iqr.adm.outcome,
              
              obs.mean.onset.adm =  obs.mean.onset.adm,
              obs.mean.onset.adm.lower = obs.mean.onset.adm.lower,
              obs.mean.onset.adm.upper = obs.mean.onset.adm.upper,
              obs.sd.onset.adm = obs.sd.onset.adm,
              obs.median.onset.adm =  obs.median.onset.adm,
              obs.iqr.onset.adm =   obs.iqr.onset.adm,
              
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
              # p.none = p.none,
              p.abx = p.abx,
              p.av = p.av,
              
              n.o2 = n.o2,
              p.o2 = p.o2,
              p.NIV = p.NIV,
              p.In.Ven = p.In.Ven,
              
              icu.n.treat = icu.n.treat,
              icu.p.abx = icu.p.abx,
              icu.p.av = icu.p.av,
              icu.p.o2 = icu.p.o2,
              icu.p.NIV = icu.p.NIV,
              icu.p.In.Ven = icu.p.In.Ven,
              
              N.icu.deaths = N.icu.deaths,
              N.icu.censored = N.icu.censored,
              N.icu.recoveries = N.icu.recoveries,
              N.icu.NA =   N.icu.NA,
              
              s.dat = s.dat,
              c.dat = c.dat,
              t.dat = t.dat,
              
              cough_pre = cough_pre,
              cough_abs = cough_abs,
              cough_unk = cough_unk
              
              
              #surv.sum = surv.sum
  ))
  
  
}

de <- d.e(patient.data, unembargoed.data)

save(de, file = glue("{code.path}/report_input_data_{ref.date}.rda"))

#save(de, file = glue("{code.path}/report_input_data_sa_{today()}.rda"))
