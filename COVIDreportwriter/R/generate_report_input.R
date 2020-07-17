############ SUMMARY: Script for data summaries ###############


############################################################################

#' @export
#' @keywords internal
# Main function d.e (dynamic.estimates) calculates estimates and confidence intervals, to be incorporated into report. #
d.e <- function(data, datafull, embargo.limit, comorbidities, admission.symptoms, treatments, site.name, embargo.length, ...){
  
  
  # Summaries
  
  N.cases <- nrow(data)      # total embargoed
  N.cases.full <- nrow(datafull)
  N.var <- ncol(data)  # number of variables
  # N.sites.full <- length(unique(datafull$site.name)) # number of sites
  # N.countries.full <- length(unique(datafull$Country)) # number of countries
  # 
  
  transfer.outcome <- sum(summary(data$exit.code)[['transfer']],  summary(data$exit.code)[['transfer.palliative']])
  unk.outcome <-  sum(summary(data$exit.code)[['hospitalisation']], summary(data$exit.code)[['unknown']]) # 'Hospitalisation' entries mostly mean the data collection wasn't completed
  

  
  N.censored <- data %>% filter(outcome == "censored") %>% nrow()    # censored-count
  N.deaths <- data %>% filter(outcome == "death") %>% nrow()    # deaths-count
  N.recoveries <- data %>% filter(outcome == "recovery") %>% nrow()   # recoveries -count
  N.outcomes <- N.deaths+N.recoveries         # outcomes-count (deaths+recoveries)
  #N.ICU <- sum(!is.na(data$Admit.ICU))       # ICU-admissions-count
  N.healthworkers <- data %>% filter(healthwork_erterm == 1) %>% nrow()
  
  
  
  # Some sites do not have access to sex and/or age data
  if(!all(is.na(data$sex))){ # if data on sex is available
    
    N.males <- summary(as.factor(data$sex))[[1]] # males-count
    N.females <- summary(as.factor(data$sex))[[2]] # females-count
    N.sex.unknown <- N.cases - N.males - N.females # unknown-count
    
    # ages by sex
    m <- data[data$sex=='1', ]
    f <-  data[data$sex=='2', ]
    
    # Outcome by sex
    sex.out.tab <- table(data$sex, data$outcome)
    sex.out.tab2 <- table(data$sex, data$exit.code)
    
  }else{
    N.males <- NA
    N.females <- NA
    N.sex.unknown <- NA
    m <- NA
    f <- NA
    
    sex.out.tab <- matrix(nrow=3, ncol = 3)
    sex.out.tab2 <- matrix(nrow=3, ncol = 6)
  }
  
  
  if(!all(is.na(data$age_estimateyears))){  # if age data is available
    
    median.age <- round(median(data$consolidated.age, na.rm = T), 1) # median age (observed)
    mean.age <-  mean(data$consolidated.age, na.rm = T)  # mean age
    sd.age <- sd(data$consolidated.age, na.rm = T)
    
    min.age <- ceiling(min(data$consolidated.age, na.rm=T)) # minimum age
    max.age <- ceiling(max(data$consolidated.age, na.rm=T)) # maximum age
    
    m.age.mean <- mean(m$consolidated.age, na.rm=T)
    m.age.sd <- sd(m$consolidated.age, na.rm=T)
    f.age.mean <- mean(f$consolidated.age, na.rm=T)
    f.age.sd <-  sd(f$consolidated.age, na.rm=T)
    
    age.out.tab <- table(data$agegp10, data$outcome)
    age.out.tab2<-  table(data$agegp10, data$exit.code)
    
    # Age group
    
    age.summ <- summary(data$agegp10)
    
    c0 <- age.summ[[1]]
    c10 <- age.summ[[2]]
    c20 <- age.summ[[3]]
    c30 <- age.summ[[4]]
    c40 <- age.summ[[5]]
    c50 <- age.summ[[6]]
    c60 <- age.summ[[7]]
    c70 <- age.summ[[8]]
    cunk <- N.cases - sum(c0, c10, c20, c30, c40, c50, c60, c70)
    
    
  } else {
    median.age <- NA
    mean.age <- NA
    sd.age <- NA
    
    min.age <- NA
    max.age <- NA
    m.age.mean <- NA
    m.age.sd <- NA
    f.age.mean <- NA
    f.age.sd <- NA
    
    age.out.tab <- matrix(nrow=8,ncol = 3)
    age.out.tab2<- matrix(nrow = 8, ncol = 6)
    
    c0 <- NA
    c10 <- NA
    c20 <- NA
    c30 <- NA
    c40 <- NA
    c50 <- NA
    c60 <-NA
    c70 <- NA
    cunk <- NA
    
    
  }
  
  
  
  # COV status
  
  cov.19.confirmed <- data  %>% filter(positive.COV19.test) %>% nrow()
  cov.19.suspected <- data  %>% filter(!positive.COV19.test) %>% nrow()
  
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
  
  adm.out.func.1 <- adm.outcome(data, embargo.limit)
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
  
  if(!is.null(adm.to.icu.1$fit)){
    adm.icu.summ <-  fit.summary.gamma(adm.to.icu.1$fit)
    
    
    adm.icu <- round(adm.icu.summ$m, 1)
    adm.icu.l <- round(adm.icu.summ$lower.m, 1)
    adm.icu.u <- round(adm.icu.summ$upper.m, 1)
    
  } else {
    adm.icu <- NA
    adm.icu.l <- NA
    adm.icu.u <- NA
  }
  
  
  
  # Duration of ICU (more censored cases than cases with outcomes; causing mle error)
  
  
  # dur.icu.1 <- dur.icu(data)
  # dur.icu.summ <- fit.summary.gamma(dur.icu.1$fit)
  
  
  # dur.icu <- round( dur.icu.summ$m, 1)
  # dur.icu.u <- round(dur.icu.summ$lower.m, 1)
  #  dur.icu.l <- round(dur.icu.summ$upper.m, 1)
  
  
  
  # Admission to IMV
  
  
  adm.imv.1 <- adm.to.imv(data)
  
  if(!is.null(adm.imv.1$fit)){
    adm.imv.summ <- fit.summary.gamma(adm.imv.1$fit)
    
    adm.imv <- round(adm.imv.summ$m, 1)
    adm.imv.l <- round(adm.imv.summ$lower.m, 1)
    adm.imv.u <- round(adm.imv.summ$upper.m, 1)
  } else {
    adm.imv <- NA
    adm.imv.l <- NA
    adm.imv.u <- NA
  }
  
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
  
  if(!is.null(adm.niv.1$fit)){
    adm.niv.summ <- fit.summary.gamma(adm.niv.1$fit)
    
    adm.niv <- round(adm.niv.summ$m, 1)
    adm.niv.l <- round(adm.niv.summ$lower.m, 1)
    adm.niv.u <- round(adm.niv.summ$upper.m, 1)
  } else {
    adm.niv <- NA
    adm.niv.l <- NA
    adm.niv.u <- NA
  }
  
  
  
  
  # Duration of NIV
  
  dur.niv.1 <- dur.niv(data)
  if(!is.null(dur.niv.1$fit)){
    dur.niv.summ <- fit.summary.gamma(dur.niv.1$fit)
    
    dur.niv <- round(dur.niv.summ$m, 1)
    dur.niv.l <- round(dur.niv.summ$lower.m, 1)
    dur.niv.u <- round(dur.niv.summ$upper.m, 1)
    
  } else {
    dur.niv <- NA
    dur.niv.l <- NA
    dur.niv.u <- NA
  }
  
  
  
  
  
  
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
  cases.full.adm.outcome <- length(adm.out.func.1$obs)
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
  if(length(a.icu) > 0 & any(!is.na(a.icu))){
    a.icu <- round.zeros(abs(a.icu[!is.na(a.icu)]))
  }
  
  
  
  # ICU duration
  
  d.icu <- data$ICU.duration
  if(length(d.icu) > 0 & any(!is.na(d.icu))){
    d.icu <- round.zeros(abs(d.icu[!is.na(d.icu)]))
  }
  
  
  # Admission to IMV
  
  a.imv <- data$admission.to.IMV
  if(length(a.imv) > 0 & any(!is.na(a.imv))){
    a.imv <- round.zeros(abs(a.imv[!is.na(a.imv)]))
  }
  
  # IMV duration
  
  d.imv <- data$IMV.duration
  if(length(d.imv) > 0 & any(!is.na(d.imv))){
    d.imv <- round.zeros(abs(d.imv[!is.na(d.imv)]))
  }
  
  # Admission to NIMV
  
  a.nimv <- data$admission.to.NIMV
  if(length(a.nimv) > 0 & any(!is.na(a.nimv))){
    a.nimv <- round.zeros(abs(a.nimv[!is.na(a.nimv)]))
  }
  
  # NIMV duration
  d.nimv <- data$NIMV.duration
  if(length(d.nimv) > 0 & any(!is.na(d.nimv))){
    d.nimv <- round.zeros(abs(d.nimv[!is.na(d.nimv)]))
  }

  # CFR
  
  temp <-casefat2(data, embargo.limit)
  if(!is.null(temp)){
    cfr <- round(temp$cfr,  2)
  } else {
    cfr <- NA
  }

  # cfr.lower <-round(casefat2(data, embargo.limit)$lcfr, 2)
  # cfr.upper <-  round(casefat2(data, embargo.limit)$ucfr, 2)
  # 
  # # CFR for ICU/non-ICU
  # 
  # cfr.icu <-  icu.cfr.func(data, embargo.limit)$cfr.icu
  # cfr.icu.l <- icu.cfr.func(data, embargo.limit)$cfr.icu.l
  # cfr.icu.u <- icu.cfr.func(data, embargo.limit)$cfr.icu.u
  # 
  # cfr.non.icu <-  icu.cfr.func(data, embargo.limit)$cfr.non.icu
  # cfr.non.icu.l <- icu.cfr.func(data, embargo.limit)$cfr.non.icu.l
  # cfr.non.icu.u <- icu.cfr.func(data, embargo.limit)$cfr.non.icu.u
  # 

  # # HFR
  # db <- hospital.fatality.ratio(data)$db
  # hfr <- round(db[nrow(db), 'mean'], 2)
  # hfr.upper <- round(db[nrow(db), 'upper'], 2)
  # hfr.lower <- round(db[nrow(db), 'lower'], 2)

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
  # Outcomes for ICU patients
  icu.d <- get_icu_pts(data)
  N.icu.censored = icu.d %>% filter(outcome == "censored") %>% nrow()   # censored-count
  N.icu.deaths <- icu.d %>% filter(outcome == "death") %>% nrow()   # deaths-count
  N.icu.recoveries <- icu.d %>% filter(outcome == "discharge") %>% nrow()   # recoveries -count
  N.icu.NA <- icu.d %>% filter(is.na(outcome)) %>% nrow()      # ICU NA
  # outcomes
  
  # Cough
  
  cough_pre <- data %>% filter(cough.any == 1) %>% nrow()
  cough_abs <- data %>% filter(cough.any == 2) %>% nrow()
  cough_unk <- data %>% filter(is.na(cough.any)) %>% nrow()
  combined_cough <- tibble(Condition = 'cough_combined', present = as.integer(cough_pre),
                           absent = as.integer(cough_abs), unknown = as.integer(cough_unk), label = 'Cough',
                           derived = TRUE, type = "symptom")
  
  
  # Symptoms 
  # Remove cough row & add combined cough variable
  # s.dat <- symptom.prev.calc(data, admission.symptoms)%>% filter(!grepl("Cough", label))
  # 
  
  # # Symptoms 
  # # Remove cough row & add combined cough variable
  # s.dat <- rbind(s.dat, combined_cough)
  # s.dat <- s.dat %>% 
  #   mutate(present = as.numeric(present)) %>% 
  #   mutate(absent = as.numeric(absent)) %>% 
  #   mutate(unknown = as.numeric(unknown))
  # 
  # 
  


  s.dat <-  rbind(symptom.prev.calc(data, admission.symptoms)%>% filter(!grepl("Cough", label)), combined_cough)%>%
    mutate(present = as.numeric(present)) %>%
    mutate(absent = as.numeric(absent)) %>%
    mutate(unknown = as.numeric(unknown))
  s.dat <- s.dat[order(-s.dat$present), ]
  
  
  # Comorbidities
  
  c.dat <- comorb.prev.calc(data, comorbidities)%>% 
    mutate(present = as.numeric(present)) %>% 
    mutate(absent = as.numeric(absent)) %>% 
    mutate(unknown = as.numeric(unknown))
  c.dat <- c.dat[order(-c.dat$present), ] 
  
  # Treatments
  
  oxy.treatments <- data.frame(Condition = c('ecmo', 'imv', 'niv', 'o2'),
                               present = c(ECMO.pr,IMV.pr, NIMV.pr, o2.pr),
                               absent = c(ECMO.ab,IMV.ab, NIMV.ab, o2.ab),
                               unknown = c(ECMO.un,IMV.un, NIMV.un, o2.un),
                               label = c('Extracorporeal membrane oxygenation (ECMO)',
                                         'Invasive ventilation',
                                         'Non-invasive ventilation',
                                         'Oxygen therapy'),
                               
                               derived = rep(TRUE, 4),
                               type = rep('treatment', 4))
  
  
  t.dat <- rbind(treatment.use.calc(data, treatments)%>% filter(!grepl("vasive|support|Oxygen", label)), oxy.treatments)%>% 
    mutate(present = as.numeric(present)) %>% 
    mutate(absent = as.numeric(absent)) %>% 
    mutate(unknown = as.numeric(unknown))
  t.dat <- t.dat[order(-t.dat$present), ] 
  
  # Steroid
  
  steroid.total.n = sum(data$steroid.any == 1, na.rm = TRUE)
  steroid.total.N = sum(data$steroid.any == 2, na.rm = TRUE) + steroid.total.n
  steroid.total.percent = sprintf("%3.1f", 100 * steroid.total.n / steroid.total.N)
  steroid.data <- data %>%
    mutate(imv.no.na = if_else(is.na(IMV.ever), FALSE, IMV.ever)) %>%
    mutate(level = factor(
      if_else(
        imv.no.na == TRUE,
        1,
        if_else(O2.ever == TRUE, 2, 3)
      ),
      levels = c(1:3),
      labels = c("IMV", "Other oxygen", "No oxygen")
    )) %>%
    filter(!is.na(level)) %>%
    group_by(level) %>%
    summarise(
      steroid.yes = sum(steroid.any == 1, na.rm = TRUE),
      steroid.no = sum(steroid.any == 2, na.rm = TRUE)
    ) %>%
    mutate(total = steroid.yes + steroid.no)
  steroid.imv.n = steroid.data$steroid.yes[steroid.data$level == "IMV"]
  steroid.imv.N = steroid.data$steroid.no[steroid.data$level == "IMV"] + steroid.imv.n
  steroid.imv.percent = sprintf("%3.1f", 100 * steroid.imv.n / steroid.imv.N)
  steroid.o2.n = steroid.data$steroid.yes[steroid.data$level == "Other oxygen"]
  steroid.o2.N = steroid.data$steroid.no[steroid.data$level == "Other oxygen"] + steroid.o2.n
  steroid.o2.percent = sprintf("%3.1f", 100 * steroid.o2.n / steroid.o2.N)
  steroid.noo2.n = steroid.data$steroid.yes[steroid.data$level == "No oxygen"]
  steroid.noo2.N = steroid.data$steroid.no[steroid.data$level == "No oxygen"] + steroid.noo2.n
  steroid.noo2.percent = sprintf("%3.1f", 100 * steroid.noo2.n / steroid.noo2.N)
  steroid.data2 <- data %>%
    filter(start.date >= as.Date("2020-06-16")) %>%
    mutate(imv.no.na = if_else(is.na(IMV.ever), FALSE, IMV.ever)) %>%
    mutate(level = factor(
      if_else(
        imv.no.na == TRUE,
        1,
        if_else(O2.ever == TRUE, 2, 3)
      ),
      levels = c(1:3),
      labels = c("IMV", "Other oxygen", "No oxygen")
    )) %>%
    filter(!is.na(level)) %>%
    group_by(level) %>%
    summarise(
      steroid.yes = sum(steroid.any == 1, na.rm = TRUE),
      steroid.no = sum(steroid.any == 2, na.rm = TRUE)
    ) %>%
    mutate(total = steroid.yes + steroid.no)
  steroid.imv.n.jun16 = steroid.data2$steroid.yes[steroid.data2$level == "IMV"]
  steroid.imv.N.jun16 = steroid.data2$steroid.no[steroid.data2$level == "IMV"] + steroid.imv.n.jun16
  steroid.imv.percent.jun16 = sprintf("%3.1f", 100 * steroid.imv.n.jun16 / steroid.imv.N.jun16)
  steroid.o2.n.jun16 = steroid.data2$steroid.yes[steroid.data2$level == "Other oxygen"]
  steroid.o2.N.jun16 = steroid.data2$steroid.no[steroid.data2$level == "Other oxygen"] + steroid.o2.n.jun16
  steroid.o2.percent.jun16 = sprintf("%3.1f", 100 * steroid.o2.n.jun16 / steroid.o2.N.jun16)
  steroid.noo2.n.jun16 = steroid.data2$steroid.yes[steroid.data2$level == "No oxygen"]
  steroid.noo2.N.jun16 = steroid.data2$steroid.no[steroid.data2$level == "No oxygen"] + steroid.noo2.n.jun16
  steroid.noo2.percent.jun16 = sprintf("%3.1f", 100 * steroid.noo2.n.jun16 / steroid.noo2.N.jun16)
  
  
  # p-value
  
  #pval <- surv.plot.func(patient.data)$pval
  
  # Sum
  
  #surv.sum <- sum(table(surv.plot.func(data)$df$sex, surv.plot.func(data)$df$event))

  return(list(site.name = site.name,
              embargo.length = embargo.length,
              
              N.cases = N.cases,
              N.cases.full = N.cases.full,
              N.var = N.var,
              # N.sites.full = N.sites.full,
              # N.countries.full = N.countries.full,
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
              
              c0 = c0,
              c10 =  c10,
              c20 =  c20,
              c30 = c30,
              c40 =  c40,
              c50 =  c50,
              c60 =  c60,
              c70 =  c70,
              cunk = cunk,
              
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
              # cfr.lower = cfr.lower,
              # cfr.upper = cfr.upper,
              # 
              # 
              # cfr.icu = cfr.icu,
              # cfr.icu.l = cfr.icu.l,
              # cfr.icu.u = cfr.icu.u,
              # 
              # 
              # cfr.non.icu = cfr.non.icu,
              # cfr.non.icu.l = cfr.non.icu.l,
              # cfr.non.icu.u = cfr.non.icu.u,
              
              # hfr = hfr,
              # hfr.lower = hfr.lower,
              # hfr.upper =  hfr.upper,
              # #N.ICU = N.ICU
              
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
              cough_unk = cough_unk,
              
              
              #surv.sum = surv.sum
              
              steroid.total.n = steroid.total.n,
              steroid.total.N = steroid.total.N,
              steroid.total.percent = steroid.total.percent,
              steroid.imv.n = steroid.imv.n,
              steroid.imv.N = steroid.imv.N,
              steroid.imv.percent = steroid.imv.percent,
              steroid.o2.n = steroid.o2.n,
              steroid.o2.N = steroid.o2.N,
              steroid.o2.percent = steroid.o2.percent,
              steroid.noo2.n = steroid.noo2.n,
              steroid.noo2.N = steroid.noo2.N,
              steroid.noo2.percent = steroid.noo2.percent,
              steroid.imv.n.jun16 = steroid.imv.n.jun16,
              steroid.imv.N.jun16 = steroid.imv.N.jun16,
              steroid.imv.percent.jun16 = steroid.imv.percent.jun16,
              steroid.o2.n.jun16 = steroid.o2.n.jun16,
              steroid.o2.N.jun16 = steroid.o2.N.jun16,
              steroid.o2.percent.jun16 = steroid.o2.percent.jun16,
              steroid.noo2.n.jun16 = steroid.noo2.n.jun16,
              steroid.noo2.N.jun16 = steroid.noo2.N.jun16,
              steroid.noo2.percent.jun16 = steroid.noo2.percent.jun16
                
  ))
  
  
}


