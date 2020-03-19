
library(survival)
library(survminer)
library(ggplot2)
library(glue)
library(plyr)

## Violin plot by sex ####

violin_sex_func <- function(dat2 = data){
  
  # Analysis to be run on only entries with either Admit.hospexit.any date or Admit.censored dates
  
  dat2 <- dat2[!(is.na(dat2$Admit.hospexit.any|dat2$Admit.Censored)), ] 
  dim(dat2)
  
  #length(age_groups)      
  # This is to include  dates for individuals still in hospital
  dat2$length.of.stay <- rep(NA, nrow(dat2))
  
  
  for(i in 1: nrow(dat2)){
    
    dat2$length.of.stay[i] <- max(dat2$Admit.hospexit.any[i], dat2$Admit.Censored[i], na.rm = T)
  }
  
  
  dat2$sex <- revalue(as.factor(dat2$sex), c('1' = 'Male', '2' = 'Female'))
  
  vd <- data.frame(Sex = dat2$sex, length.of.stay = abs(dat2$length.of.stay) )
  
  #head(vd)
  
  library(ggplot2)
  
  # by sex
  
  x <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) + geom_violin(trim=FALSE)+ geom_boxplot(width=0.1, fill="white")  +
    labs(title=" ", x="Sex", y = "Days from admission to discharge") + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + 
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(x)
}


#violin_sex_func(data)




### Violin age ####



violin_age_func <- function(dat2 = data){
  
  # Analysis to be run on only entries with either Admit.hospexit.any date or Admit.censored dates
  
  dat2 <- dat2[!(is.na(dat2$Admit.hospexit.any|dat2$Admit.Censored)), ] 
  dim(dat2)
  
  #length(age_groups)      
  # This is to include  dates for individuals still in hospital
  dat2$length.of.stay <- rep(NA, nrow(dat2))
  dat2$age.group <- rep(NA, nrow(dat2))
  dat2$age <- abs(dat2$age)
  
  
  for(i in 1: nrow(dat2)){
    
    dat2$length.of.stay[i] <- max(dat2$Admit.hospexit.any[i], dat2$Admit.Censored[i], na.rm = T)
    
    if(is.na(dat2$age[i])  == 'TRUE'){
      dat2$age.group[i] <- 'Unknown' 
    }else{
      if(dat2$age[i] >=10 & dat2$age[i] <= 19){
        dat2$age.group[i] <- '10-19'
      }else{
        if(dat2$age[i] >=20 & dat2$age[i] <= 29){
          dat2$age.group[i] <- '20-29'
        }else{
          if(dat2$age[i] >=30 & dat2$age[i] <= 39){
            dat2$age.group[i] <- '30-39'
          }else{
            if(dat2$age[i] >=40 & dat2$age[i] <= 49){
              dat2$age.group[i] <- '40-49'
            }else{
              if(dat2$age[i] >=50 & dat2$age[i] <= 59){
                dat2$age.group[i] <- '50-59'
              }else{
                if(dat2$age[i] >=60 & dat2$age[i] <= 69){
                  dat2$age.group[i] <- '60-69'
                }else{
                  if(dat2$age[i] >=70){
                    dat2$age.group[i] <- '70+'
                  }else{
                    if(dat2$age[i] >=0 & dat2$age[i] <= 9){
                      dat2$age.group[i] <- '0-9'
                    } 
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  
  vd<- data.frame(Age = dat2$age.group, length_of_stay = abs(dat2$length.of.stay) )
  
  
  
  vd2 <- ggplot(vd, aes(x = Age, y = length_of_stay, fill=Age)) + geom_violin(trim=FALSE)+ #geom_boxplot(width=0.1, fill="white")  +
    labs(title="  ", x="Age group", y = "Days from admission to discharge") + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + 
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(vd2)
  
}



#violin_age_func(data)





###### Distribution plots ####


#### replace zeros with 0.5 #######

replacezeros <- function(admit.discharge){
  
  for (i in 1: length(admit.discharge)){
    
    if (admit.discharge[i]==0){
      admit.discharge[i] <- 0.5
    }
  }
  
  return(admit.discharge) 
}





########## Onset to admission #####


onset.adm.func <- function(dat2 = data){
  
  library(fitdistrplus) 
  
  admit.discharge <- dat2$Onset.Hosp
  
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  
  admit.discharge.2 <- replacezeros(admit.discharge)
  
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  # Plot 
  
  library(ggplot2)
  
  t <- data.frame(x=admit.discharge)
  
  plot <- ggplot(data = t) + 
    geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+    
    geom_line(aes(x=t$x, y=dgamma(t$x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="black", size = 2) +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
    labs(y = 'Density', x = 'Days', title = 'Time from symptom onset to admission')
  
  return(plot)
  
  
}


#onset.adm.func(data)





########## Admission to outcome (any) ############



adm.outcome.func <- function(dat2 = data){
  
  admit.discharge <- dat2$Admit.hospexit.any
  
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  
  admit.discharge <- replacezeros(admit.discharge)
  
  
  
  censored <- abs(dat2$Admit.Censored)
  
  censored <- replacezeros(censored[!(is.na(censored))])
  
  
  left <- c(admit.discharge, censored)
  right <- c(admit.discharge, rep(NA, length(censored)))
  
  censored_df <- data.frame(left, right)
  
  fit <- fitdistcens(censored_df, dist = 'gamma')
  
  
  join <- c(admit.discharge, censored)
  
  t <- data.frame(x = join)
  
  
  plot <- ggplot(data = t) + 
    geom_histogram(data = as.data.frame(join), aes(x=join, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+    
    geom_line(aes(x=t$x, y=dgamma(t$x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="black", size = 2) +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
    labs(y = 'Density', x = 'Days', title = 'Time from admission to outcome (any)')
  
  return(plot)
  
  
}





#adm.outcome.func(data)





########## Survival plot ######


surv_plot_func <- function(f = data){
  
  a <- f[!(is.na(f$Admit.hospexit.any | f$Admit.Censored)), ] 
  
  #length(age_groups)      
  # This is to include  dates for individuals still in hospital
  a$length.of.stay <- rep(NA, nrow(a))
  
  
  for(i in 1:nrow(a)){
    
    a$length.of.stay[i] <- max(a$Admit.hospexit.any[i], a$Admit.Censored[i], na.rm = T)
  }
  
  
  # Omit entry 160 (unknown sex)
  
  a <- a[-160, ]
  
  
  # Make positive
  
  
  a$length.of.stay <- abs(a$length.of.stay)
  
  
  a$sex <- revalue(as.factor(a$sex), c('1' = 'Male', '2' = 'Female'))
  
  
  fit <- survfit(Surv(length.of.stay, Censored) ~ sex, data = a)
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


#surv_plot_func(f = df)





######### Timeline plot ##############


status.by.time.after.admission <- function(data){
  
  data2 <- data %>%
    dplyr::mutate(status = map_chr(status, function(x){
      switch(as.character(x),
             "0" = "Censored",
             "1" = "Died",
             "2" = "Recovered")
    })) %>%
    dplyr::mutate(status = factor(status)) %>%
    dplyr::mutate(Died = status == "Died")
  
  
  timings.wrangle <- data2 %>%
    dplyr::select(subjid,
                  Died,
                  # Admit.ICU,
                  # Dur.ICU,
                  Censored,
                  Admit.hospexit.any) %>%
    dplyr::mutate(hospital.start = 0) %>%
    dplyr::mutate(hospital.end = Admit.hospexit.any) %>%
    # mutate(ICU.start = Admit.ICU) %>%
    # mutate(ICU.end = Admit.ICU + Dur.ICU) %>%
    dplyr::select(subjid,  ends_with("start"), ends_with("end"), Censored, Died) %>%
    dplyr::mutate(last.date = map2_dbl(Censored, hospital.end, function(c, h){
      ifelse(!c, h, NA)
    })) #%>%
  # mutate(ever.ICU = !is.na(ICU.start)) 
  
  overall.start <- 0
  overall.end <- max(timings.wrangle$last.date, na.rm = T)
  
  
  complete.timeline <- map(1:nrow(timings.wrangle), function(pat.no){
    times <- map(overall.start:overall.end, function(day){
      # if(!timings.wrangle$ever.ICU[pat.no]){
      if(timings.wrangle$Censored[pat.no]){
        "Censored"
      } else if(day < timings.wrangle$hospital.end[pat.no]){
        "Admitted"
      } else if(timings.wrangle$Died[pat.no]){
        "Dead"
      } else {
        "Discharged"
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
    dplyr::mutate(status = factor(status, levels = c("Censored", "Dead", "Admitted", "Discharged"))) %>%
    ungroup() 
  
  ggplot(complete.timeline) + geom_bar(aes(x = day, fill = status), position = "fill") +
    scale_fill_brewer(palette = "Set3", name  = "Status", drop = F) + 
    theme_bw() + 
    xlab("Days relative to admission") +
    ylab("Proportion")
}






























