##### GRAPH FUNCTIONS ##### 


# Age pyramid

age.pyramid <- function(data, ...){
  
  data2 <- data %>%
    group_by(agegp5, sex, outcome) %>%
    dplyr::summarise(count = n()) %>%
    ungroup() %>%
    filter(!is.na(sex) & !is.na(agegp5)) %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("discharge", "censored","death"))) %>%
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
  
  plot.breaks <- seq(-(ceiling(max.count/10)*10), ceiling(max.count/10)*10, by = 10)
  plot.labels <- as.character(c(rev(seq(10, ceiling(max.count/10)*10, by = 10)), 0, seq(10, ceiling(max.count/10)*10, by= 10)))
  
  ggplot() + geom_bar(data = (data2 %>% filter(sex == "M")), aes(x=agegp5, y=count, fill = outcome), stat = "identity", col = "black") +
    geom_bar(data = data2 %>% filter(sex == "F"), aes(x=agegp5, y=count, fill = outcome),  stat = "identity", col = "black") +
    coord_flip(clip = 'off') +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Discharge", "Ongoing care", "Death")) +
    xlab("Age group") +
    ylab("Count") +
    scale_x_discrete(drop = "F") +
    scale_y_continuous(
      # currently in hard-coded increments of 5. @todo make this better
      breaks = plot.breaks,
      labels = plot.labels,
      limits = c(-1.1*max.count, 1.1*max.count)) +
    annotation_custom(
      grob = textGrob(label = "Males", hjust = 0.5, gp = gpar(cex = 1.5)),
      ymin = -max(data2$count)*1.1/2,      
      ymax = -max(data2$count)*1.1/2,
      xmin = length(levels(data2$agegp5))+1.5 ,         
      xmax = length(levels(data2$agegp5))+1.5) +
    annotation_custom(
      grob = textGrob(label = "Females", hjust = 0.4, gp = gpar(cex = 1.5)),
      ymin = max(data2$count)*1.1/2,      
      ymax = max(data2$count)*1.1/2,
      xmin = length(levels(data2$agegp5))+1.5,         
      xmax = length(levels(data2$agegp5))+1.5) +
    theme(plot.margin=unit(c(30,5,5,5.5,5.5),"pt"))
  
}

# Distribution of sites by country

sites.by.country <- function(data, ...){
  data2 <- data %>%
    group_by(Country, site.name) %>%
    dplyr::summarise(n.sites = 1) %>%
    dplyr::summarise(n.sites = sum(n.sites)) %>%
    filter(!is.na(Country))
  
  
  ggplot(data2) + geom_col(aes(x = Country, y = n.sites), col = "black", fill = "deepskyblue3") +
    theme_bw() +
    xlab("Country") +
    ylab("Sites") + theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    geom_text(aes(x=Country, y=n.sites + 3, label=n.sites), size=4)
}

# Distribution of patients and outcomes by country

outcomes.by.country <- function(data, ...){
  data2 <- data %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("discharge", "censored","death")))  %>%
    filter(!is.na(Country))
  
  ggplot(data2) + geom_bar(aes(x = Country, fill = outcome), col = "black") +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Discharge", "Ongoing care", "Death")) +
    xlab("Country") +
    ylab("Cases") + theme(axis.text.x = element_text(angle = 45, hjust=1))
}

# Outcomes by epi-week

outcomes.by.admission.date <- function(data, ...){
  data2 <- data %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("discharge", "censored", "death"))) %>%
    mutate(two.digit.epiweek = map_chr(start.date, function(x){
      ew <- epiweek(x)
      ifelse(nchar(as.character(ew))==1, glue("0{as.character(ew)}"), as.character(ew))
    })) %>%
    filter(!is.na(hostdat))
  ggplot(data2) + geom_bar(aes(x = two.digit.epiweek, fill = outcome), col = "black", width = 0.95) +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Discharge", "Ongoing care", "Death")) +
    # scale_x_continuous(breaks = seq(min(epiweek(data2$hostdat), na.rm = TRUE), max(epiweek(data2$hostdat), na.rm = TRUE), by=2)) +
    xlab("Epidemiological week of admission/symptom onset (2020)") +
    ylab("Cases") 
}

# Comorbidities upset plot (max.comorbidities is the n to list; this will be the n most frequent)

comorbidities.upset <- function(data, max.comorbidities, ...){
  
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
  
  top.n.conditions.tbl <- data %>%
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
  
  other.conditions.tbl <- data %>%
    dplyr::select(subjid, one_of(comorbidities %>% filter(!(field %in% most.common)) %>% pull(field)))
  
  #other.conditions.tbl <- data %>%
  #  dplyr::select(subjid, one_of(comorbidities$field) & !one_of(most.common)) 
  
  other.conditions.tbl <- other.conditions.tbl%>%
    pivot_longer(2:(ncol(other.conditions.tbl)), names_to = "Condition", values_to = "Present") %>%
    group_by(subjid) %>%
    dplyr::summarise(Present = any(Present == 1)) %>%
    add_column(label = "Any other") %>%
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
  
  # just get rid of individuals from the top n table that have NAs in the other table
  
  label.order = c(unique(comorbidities %>% filter(field %in% most.common ) %>% pull(label)), "Any other")  
  
  top.n.conditions.tbl <- top.n.conditions.tbl %>%
    filter(subjid %in% other.conditions.tbl$subjid) %>%
    left_join(other.conditions.tbl, by = "subjid") %>%
    mutate(conditions.present = map2(conditions.present.x, conditions.present.y, function(a,b){
      c(a,b)
    })) %>%
    dplyr::select(-conditions.present.x, -conditions.present.y)
  
  
  ggplot(top.n.conditions.tbl, aes(x = conditions.present)) + 
    geom_bar(aes(y=..count../sum(..count..)), fill = "indianred3", col = "black") + 
    theme_bw() +
    xlab("Comorbidities present at admission") +
    ylab("Proportion of patients") +
    scale_x_upset() 
}

# Symptoms upset plot (max.symptoms is the n to list; this will be the n most frequent)


symptoms.upset <- function(data, max.symptoms, ...){
  
  
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
    arrange(desc(Present)) %>%
    slice(1:max.symptoms) %>%
    pull(Condition)
  
  top.n.symptoms.tbl <- data %>%
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
  
  #other.conditions.tbl <- data %>%
  #dplyr::select(subjid, one_of(admission.symptoms$field) & !one_of(most.common)) 
  
  other.conditions.tbl <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms %>% filter(!(field %in% most.common)) %>% pull(field)))
  
  other.conditions.tbl <- other.conditions.tbl%>%
    pivot_longer(2:(ncol(other.conditions.tbl)), names_to = "Condition", values_to = "Present") %>%
    group_by(subjid) %>%
    dplyr::summarise(Present = any(Present == 1)) %>%
    add_column(label = "Any other") %>%
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
  
  # just get rid of individuals from the top n table that have NAs in the other table
  
  top.n.symptoms.tbl <- top.n.symptoms.tbl %>%
    filter(subjid %in% other.conditions.tbl$subjid) %>%
    left_join(other.conditions.tbl, by = "subjid") %>%
    mutate(conditions.present = map2(conditions.present.x, conditions.present.y, function(a,b){
      c(a,b)
    })) %>%
    dplyr::select(-conditions.present.x, -conditions.present.y)
  
  
  ggplot(top.n.symptoms.tbl, aes(x = conditions.present)) + 
    geom_bar(aes(y=..count../sum(..count..)), fill = "deepskyblue3", col = "black") + 
    theme_bw() +
    xlab("Symptoms present at admission") +
    ylab("Proportion of patients") +
    scale_x_upset() 
}

# Prevalence of symptoms

symptom.prev.calc <- function(data){
  data2 <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms$field)) 
  
  nconds <- ncol(data2) - 1
  
  data3 <- data2 %>%
    pivot_longer(2:(nconds + 1), names_to = "Condition", values_to = "Present") %>%
    group_by(Condition) %>%
    dplyr::mutate(Present = map_chr(Present, function(x){
      if(is.na(x)){
        "unknown"
      } else if(x == 1){
        "present"
      } else if(x == 2){
        "absent"
      } else {
        "unknown"
      }
    })) %>%
    group_by(Condition) %>%
    dplyr::summarise(present = sum(Present == "present"), absent = sum(Present == "absent"), unknown = sum(Present == "unknown")) %>%
    dplyr::left_join(admission.symptoms, by = c("Condition" = "field"))
  
  return(data3)
  
}


symptom.prevalence.plot <- function(data, ...){
  data2 <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms$field)) 
  
  nconds <- ncol(data2) - 1
  
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
    left_join(admission.symptoms, by = c("Condition" = "field")) %>%
    dplyr::select(-Condition) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(prop.yes) %>%
    dplyr::mutate(Condition = as_factor(label)) %>%
    pivot_longer(c(prop.yes, prop.no), names_to = "affected", values_to = "Proportion") %>%
    dplyr::mutate(affected = map_lgl(affected, function(x) x == "prop.yes")) %>%
    filter(label != "Other")
  
  plt <- ggplot(data2) + 
    geom_col(aes(x = Condition, y = Proportion, fill = affected), col = "black") +
    theme_bw() + 
    coord_flip() + 
    ylim(0, 1) +
    scale_fill_manual(values = c("deepskyblue1", "deepskyblue4"), name = "Symptom\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt

}

# Prevalence of comorbidities

comorb.prev.calc <- function(data){
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(comorbidities$field)) 
  
  nconds <- ncol(data2) - 1
  
  data3 <- data2 %>%
    pivot_longer(2:(nconds + 1), names_to = "Condition", values_to = "Present") %>%
    group_by(Condition) %>%
    dplyr::mutate(Present = map_chr(Present, function(x){
      if(is.na(x)){
        "unknown"
      } else if(x == 1){
        "present"
      } else if(x == 2){
        "absent"
      } else {
        "unknown"
      }
    })) %>%
    group_by(Condition) %>%
    dplyr::summarise(present = sum(Present == "present"), absent = sum(Present == "absent"), unknown = sum(Present == "unknown"))

  return(data3)
  
}


comorbidity.prevalence.plot <- function(data, ...){
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(comorbidities$field)) 
  
  nconds <- ncol(data2) - 1

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
    left_join(comorbidities, by = c("Condition" = "field")) %>%
    dplyr::select(-Condition) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(prop.yes) %>%
    dplyr::mutate(Condition = as_factor(label)) %>%
    pivot_longer(c(prop.yes, prop.no), names_to = "affected", values_to = "Proportion") %>%
    dplyr::mutate(affected = map_lgl(affected, function(x) x == "prop.yes")) %>%
    filter(label != "Other")
  
  
  plt <- ggplot(data2) + 
    geom_col(aes(x = Condition, y = Proportion, fill = affected), col = "black") +
    theme_bw() + 
    coord_flip() + 
    ylim(0, 1) +
    scale_fill_manual(values = c("indianred1", "indianred4"), name = "Comorbidity\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}


# Raw proportions of patients undergoing each treatment

treatment.use.calc <- function(data){
  
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

  data3 <- data2 %>%
    pivot_longer(2:(ntr + 1), names_to = "Condition", values_to = "Present") %>%
    group_by(Condition) %>%
    dplyr::mutate(Present = map_chr(Present, function(x){
      if(is.na(x)){
        "unknown"
      } else if(x == 1){
        "present"
      } else if(x == 2){
        "absent"
      } else {
        "unknown"
      }
    })) %>%
    group_by(Condition) %>%
    dplyr::summarise(present = sum(Present == "present"), absent = sum(Present == "absent"), unknown = sum(Present == "unknown"))

  return(data3)
}

treatment.use.plot <- function(data, ...){
  
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
  
  plt<-  ggplot(data2) + 
    geom_col(aes(x = Condition, y = Proportion, fill = affected), col = "black") +
    theme_bw() + 
    coord_flip() + 
    ylim(0, 1) +
    scale_fill_manual(values = c("chartreuse2", "chartreuse4"), name = "Treatment", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
}

## "modified KM plot" ##


modified.km.plot <- function(data, ...) {
  
  
  # Method: Ghani et ql. 2005:  https://doi.org/10.1093/aje/kwi230
  
  # Exclude rows which no entries for length of stay
  
  data2 <- data %>% filter(!is.na(start.to.exit) | !is.na(start.to.censored))
  
  #data2 <- data2 %>% 
  #mutate(length.of.stay = map2_dbl(start.to.exit, admission.to.censored, function(x,y){
  
  data2 <- data2 %>% 
    mutate(length.of.stay = map2_dbl(start.to.exit, start.to.censored, function(x,y){
      
      max(x, y, na.rm = T)
    }))
  
  # c$pstate is cumulative incidence function for each endpoint
  c <- casefat2(data)$c
  Fd <- c$pstate[,which(c$states=="death")] # death
  Fr <- c$pstate[,which(c$states=="discharge")] # recovery
  cfr <- casefat2(data)$cfr
  
  
  # Plot
  df <- data.frame(day = rep(c$time,3), value = c(1-Fr, Fd, rep(cfr,length(Fd))), 
                   status =factor(c(rep('discharge', length(Fd)), rep('death', length(Fd)), rep('cfr', length(Fd))),
                                  levels = c("death", "discharge", "cfr")
                   ))
  ggplot(data = df)+
    geom_line(aes(x=day, y = value, col = status, linetype = status), size=0.75)+
    theme_bw()+ xlim(0, 20) +
    scale_colour_manual(values = c("indianred",  "green", "black"), name = "Legend", labels = c( "Deaths", "Recoveries","Case\n fatality ratio")) +
    scale_linetype_manual(values = c( "solid", "solid", "dashed" ),  guide = F) +
    xlab("Days after admission") +
    ylab("Cumulative probability")
  
}



modified.km.plot.1 <- function(data, ...){
  
  total.patients <- nrow(data)
  
  data2 <- data %>%
    filter(outcome != "censored" & !is.na(outcome.date)) %>%
    dplyr::select(subjid, hostdat, outcome.date, outcome, admission.to.exit) 
  
  timeline <- map(0:max(data2$admission.to.exit, na.rm = T), function(x){
    outcome.date <- data2 %>% filter(admission.to.exit <= x)
    prop.dead <- nrow(outcome.date %>% filter(outcome == "death"))/total.patients
    prop.discharged <- nrow(outcome.date %>% filter(outcome == "discharge"))/total.patients
    list(day = x, prop.dead = prop.dead, prop.discharged = prop.discharged)
  }) %>% bind_rows() %>%
    dplyr::mutate(prop.not.discharged = 1-prop.discharged)
  
  
  
  final.dead <- timeline %>%  pull(prop.dead) %>% max()
  final.discharged <- timeline %>% pull(prop.discharged) %>% max()
  final.not.discharged <- timeline %>% pull(prop.not.discharged) %>% min()
  
  interpolation.line <- final.dead + (1-(final.discharged+final.dead))*(final.dead/(final.dead + final.discharged))
  
  timeline <- timeline %>%
    add_column(interpolation = interpolation.line) %>%
    select(-prop.discharged) %>%
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




hospital.fatality.ratio <- function(data){
  library(binom)
  # Method from https://doi.org/10.2807/1560-7917.ES.2020.25.3.2000044
  # Only uses individuals who have either died or been discharged
  Dc_date <- data$discharge.date
  Died_date <- data$death.date
  # Identify first and last events
  first <- min(Dc_date, Died_date, na.rm = TRUE)
  # Set start date as 1 March 2020
  first <- as.Date("2020-03-01")
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
    ylab("Hospital fatality ratio") + 
    theme_bw() + 
    theme(
      axis.ticks.x = element_blank(), 
      axis.text.x = element_blank(), 
      axis.title.x = element_blank(),
      plot.margin = unit(c(1,1,3,2), "lines")
    ) + 
    coord_cartesian(
      xlim = c(first, last), 
      ylim = c(0, 1), 
      default = TRUE, clip = "off"
    )
  
  # Make data table to go at bottom
  number_rows <- as.integer(as.numeric(1 + (last - first) / 5)) 
  rows <- 1:number_rows
  dates <- as.Date((rows -1) * 5, origin = "2020-03-01")
  dt <- data.frame(rows = rows, dates = dates)
  dt <- merge(dt,  db, by.x = "dates", by.y = "Date")
  dt$Discharged <- dt$Dc_c
  dt_Died <- dt$Died_c
  dt <- subset(dt, select = c(dates, Discharged, Died))
  
  # I've not been able to get geom_text outside the plotting area
  
  for (i in 1:number_rows) {
    print_date <- format(dt$date[i], format = "%d %b")
    plt <- plt + 
      annotate("text", x = dt$date[i], y = -.1, label = print_date) +
      annotate("text", x = dt$date[i], y = -.15, label = paste(dt$Discharged[i])) +
      annotate("text", x = dt$date[i], y = -.2, label = paste(dt$Died[i]))
  }
  
  plt <- plt +
    annotate("text", x = first - 3, y = -.1, label = "Date") +
    annotate("text", x = first - 3, y = -.15, label = "Discharged") +
    annotate("text", x = first - 3, y = -.2, label = "Died")
  
  return(list(plt=plt, db=db))
  
}

#### Function to round 0 days to 0.5 (half a day) #######

round.zeros <- function(x){
  
  for (i in 1: length(x)){
    
    if (x[i]==0){
      x[i] <- 0.5
    }
  }
  
  return(x) 
}

## Violin plot by sex ####

violin.sex.func <- function(data, ...){
  
  # Analysis to be run on only entries with admission.to.exit entries
  
  data2 <- data %>% filter(!is.na(start.to.exit))      #| !is.na(admission.to.censored))
  
  # This is to include dates for individuals still in hospital
  
  data2 <- data2 %>% 
    mutate(length.of.stay = abs(round.zeros(start.to.exit)))  %>%#, admission.to.censored, function(x,y){
    #     max(x, y, na.rm = T)
    #   })) %>%
    mutate(sex = map_chr(sex, function(x)  c('Male', 'Female')[x])) %>%
    mutate(sex = factor(sex, levels = c("Male", "Female")))
  
  data2 <- data2 %>%
    filter(!is.na(sex))
  
  
  vd <- tibble(Sex = data2$sex, length.of.stay = data2$length.of.stay )
  
  # by sex
  
  x <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) + 
    geom_violin(trim=FALSE)+ 
    geom_boxplot(width=0.1, fill="white")  +
    scale_fill_discrete(drop = F) +
    labs(title=" ", x="Sex", y = "Length of hospital stay") + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12) 
    ) +  #+ ylim(0, max(length(vd$length.of.stay)))
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(x)
}


### Violin age ####



violin.age.func <- function(data, ...){
  
  # Analysis to be run on only entries with admission.to.exit entries
  
  data2 <- data %>% filter(!is.na(start.to.exit))      #| !is.na(admission.to.censored))
  
  data2 <- data2 %>% 
    mutate(length.of.stay = abs(round.zeros(start.to.exit)))
  
  vdx<- tibble(subjid = data2$subjid, Age = data2$agegp10, length_of_stay = abs(data2$length.of.stay) )
  
  # remove NAs (@todo for now?)
  
  vdx <- vdx %>% filter(!is.na(Age))
  
  vd2 <- ggplot(vdx, aes(x = Age, y = length_of_stay, fill=Age)) + 
    geom_violin(trim=FALSE)+ geom_boxplot(width=0.05, fill="white")  +
    labs(title="  ", x="Age group", y = "Length of hospital stay") + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + #ylim(0, length(0, max(vdx$length_of_stay))+5) +
    scale_fill_discrete(drop = F) +
    scale_x_discrete(drop = F) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(vd2)
  
}



# Upset plot for treatments @todo add maximum parameter?

treatment.upset.prep <- function(data, ...) {
  details <- subset(
    data, 
    select = c(subjid,
               antibiotic.any,
               antiviral.any,
               antifungal.any,
               steroid.any,
               NIMV.ever, IMV.ever, O2.ever
    )
  )
  # Do not plot if all NA - likely just not had outcome form
  col_from <- 2
  col_to <- ncol(details) - 1  # -1 as O2.ever is never NA
  details$allna <- 1
  for (i in col_from:col_to) {
    details$allna[is.na(details[, i]) == FALSE] <- 0
  }
  details <- subset(details, allna == 0)
  # 1 is Yes, set anything else to 0 (No)
  for (i in 2:5) {
    details[, i][details[, i] != 1 | is.na(details[, i]) == TRUE] <- 0
  }
  # Any O2 therapy - will include data from daily forms
  details$O2.ever[details$NIMV.ever == TRUE] <- TRUE
  details$O2.ever[details$IMV.ever == TRUE] <- TRUE
  
  return(details)
}

treatment.upset <- function(data, ...) {
  details <- treatment.upset.prep(data)
  treatments <- details %>%
    dplyr::select(
      subjid, 
      antiviral.any, 
      antibiotic.any, 
      antifungal.any, 
      steroid.any,
      O2.ever
    ) %>%
    pivot_longer(2:6, names_to = "Treatment", values_to = "Present") %>%
    mutate(Present = as.logical(Present)) 
  # Change labels
  treatments$Treatment[treatments$Treatment == "O2.ever"] <- 
    "Oxygen supplementation"
  treatments$Treatment[treatments$Treatment == "antiviral.any"] <- 
    "Antiviral"
  treatments$Treatment[treatments$Treatment == "antibiotic.any"] <- 
    "Antibiotic"
  treatments$Treatment[treatments$Treatment == "antifungal.any"] <- 
    "Antifungal"
  treatments$Treatment[treatments$Treatment == "steroid.any"] <- 
    "Corticosteroid"
  treatments <- treatments %>%
    group_by(subjid) %>%
    dplyr::summarise(Treatments = list(Treatment), Presence = list(Present)) %>%
    mutate(treatments.used = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  p <- ggplot(treatments, aes(x = treatments.used)) + 
    geom_bar(aes(y=..count../sum(..count..)), fill = "chartreuse3", col = "black") + 
    theme_bw() +
    xlab("Treatments used during hospital admission") +
    ylab("Proportion of patients") +
    scale_x_upset() 
  
  return(p)
}

treatment.upset.ventilation <- function(data, ...) {
  # A second plot for types of ventilation. This one will use the whole dataset
  
  vd <- subset(data, select = c(subjid, O2.ever, NIMV.ever, IMV.ever, ECMO.ever))
  vd$allna <- 1
  vd$allna[vd$O2.ever == TRUE] <- 0
  vd$allna[is.na(vd$NIMV.ever) == FALSE] <- 0
  vd$allna[is.na(vd$IMV.ever) == FALSE] <- 0
  vd$allna[is.na(vd$ECMO.ever) == FALSE] <- 0
  vd <- subset(vd, allna == 0)
  vd$O2.ever[vd$NIMV.ever == TRUE] <- TRUE
  vd$O2.ever[vd$IMV.ever == TRUE] <- TRUE
  # 1 is Yes, set anything else to 0 (No)
  for (i in 2:5) {
    vd[, i][vd[, i] != TRUE | is.na(vd[, i]) == TRUE] <- FALSE
  }
  treatments <- vd %>%
    dplyr::select(
      subjid, 
      O2.ever, NIMV.ever, IMV.ever, ECMO.ever
    ) %>%
    pivot_longer(2:4, names_to = "Treatment", values_to = "Present") %>%
    mutate(Present = as.logical(Present)) 
  # Change labels
  treatments$Treatment[treatments$Treatment == "O2.ever"] <- 
    "Any oxygen supplementation"
  treatments$Treatment[treatments$Treatment == "NIMV.ever"] <- 
    "Non-invasive ventilation"
  treatments$Treatment[treatments$Treatment == "IMV.ever"] <- 
    "Invasive ventilation"
  treatments$Treatment[treatments$Treatment == "ECMO.ever"] <- 
    "ECMO"
  treatments <- treatments %>%
    group_by(subjid) %>%
    dplyr::summarise(Treatments = list(Treatment), Presence = list(Present)) %>%
    mutate(treatments.used = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence) 
  vent.plt <- ggplot(treatments, aes(x = treatments.used)) + 
    geom_bar(aes(y=..count../sum(..count..)), fill = "blue", col = "black") + 
    theme_bw() +
    xlab("Oxygen therapies used during hospital admission") +
    ylab("Proportion of patients") +
    scale_x_upset() 
  
  return(vent.plt)
}  

treatment.upset.numbers <- function(data, ...) {
  details <- treatment.upset.prep(data)
  # Counts
  N.treat <- nrow(details)
  N.abx <- sum(details$antibiotic.any, na.rm = FALSE)
  N.av <- sum(details$antiviral.any, na.rm = FALSE)
  details = details %>%
    rowwise() %>%
    mutate(Any = max(antiviral.any, antibiotic.any, antifungal.any, steroid.any))
  details$None <- 1 - details$Any
  N.none <- sum(details$None, na.rm = FALSE)
  # Oxygen data now from whole dataset, not just those with completed records
  N.O2 <- sum(data$O2.ever, na.rm = TRUE)
  N.NIV <- sum(data$NIMV.ever, na.rm = TRUE)
  N.inv.vent <- sum(data$IMV.ever, na.rm = TRUE)
  df = data.frame(
    All = N.treat, 
    Abx = N.abx, 
    Av = N.av, 
    None = N.none, 
    O2 = N.O2,
    NIV = N.NIV,
    Inv.ven <- N.inv.vent
  )
  return(df)
}


######### Timeline plot ##############
# @todo add ICU. Add IMV.

status.by.time.after.admission <- function(data, ...){
  
  data2 <- data %>%
    dplyr::mutate(status = map_chr(exit.code, function(x){
      ifelse(is.na(x), "censored", x)
    })) %>%
    dplyr::mutate(status = factor(status)) 
  
  timings.wrangle <- data2 %>%
    dplyr::select(subjid,
                  status,
                  start.to.ICU,
                  ICU.duration,
                  censored,
                  start.to.exit) %>%
    dplyr::mutate(hospital.start = 0) %>%
    dplyr::mutate(hospital.end = start.to.exit) %>%
    mutate(ICU.start = start.to.ICU) %>%
    mutate(ICU.end = start.to.ICU + ICU.duration) %>%
    dplyr::select(subjid,  ends_with("start"), ends_with("end"), censored, status) %>%
    filter(hospital.end >= 0 | is.na(hospital.end))  %>%
    mutate(ever.ICU = !is.na(ICU.start)) %>%
    # If hospital end is known but ICU end is not, impossible to resolve
    filter(!(!is.na(hospital.end) & is.na(ICU.end) & ever.ICU))
  
  overall.start <- 0
  overall.end <- max(timings.wrangle$hospital.end, na.rm = T)
  
  # this generates a table of the status of every patient on every day
  
  complete.timeline <- map(1:nrow(timings.wrangle), function(pat.no){
    times <- map(overall.start:overall.end, function(day){
      if(!timings.wrangle$ever.ICU[pat.no]){
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
      } else {
        if(is.na(timings.wrangle$hospital.end[pat.no])){
          if(day >= timings.wrangle$ICU.start[pat.no] & (is.na(timings.wrangle$ICU.end[pat.no] | day < timings.wrangle$ICU.end[pat.no] ))){
            "ICU"
          } else {
            "Admitted"
          }
        } else {
          if(day >= timings.wrangle$ICU.start[pat.no] &  day < timings.wrangle$ICU.end[pat.no]){
            "ICU"
          } else if(day < timings.wrangle$hospital.end[pat.no]){
            "Admitted"
          } else if(timings.wrangle$status[pat.no] == "death"){
            "Died"
          } else if(timings.wrangle$status[pat.no] == "discharge"){
            "Discharged"
          } else {
            "Transferred"
          }
        }
      }
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
    dplyr::mutate(status = factor(status, levels = rev(c("Died", "ICU", "Admitted", "Transferred", "Discharged")))) %>%
    ungroup() 
  
  ggplot(complete.timeline) + geom_bar(aes(x = day, fill = status), position = "fill") +
    scale_fill_brewer(palette = "Set3", name  = "Status", drop = F) + 
    theme_bw() + 
    xlab("Days relative to admission") +
    ylab("Proportion")
}

antiviral.use.upset <- function(data, ...){
  
  antiviral.mapper <- function(x){
    switch(x,
           "antiviral.Ribavirin" = "Ribavirin",
           "antiviral.Lopinavir.Ritonvir" = "Lopinavir/Ritonvir",
           "antiviral.Interferon.alpha" = "Interferon-alpha",
           "antiviral.Interferon.beta" = "Interferon-beta",
           "antiviral.Neuraminidase.inhibitors" =  "Neuraminidase inhibitor",
           "antiviral.other" = "Other antiviral")
  }
  
  data2 <- data %>%
    filter(data.source == "UK") %>%
    dplyr::select(subjid, starts_with("antiviral.")) %>%
    
    pivot_longer(2:7, names_to = "antiviral", values_to = "value") %>%
    mutate(antiviral = map_chr(antiviral, antiviral.mapper)) %>%
    filter(!is.na(value)) %>%
    mutate(value = as.logical(value)) %>%
    group_by(subjid) %>%
    dplyr::summarise(antivirals = list(antiviral), values = list(value)) %>%
    dplyr::mutate(antivirals.used = map2(antivirals, values, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-antivirals, -values)
  
  
  
  ggplot(data2, aes(x = antivirals.used)) + 
    geom_bar(aes(y=..count../sum(..count..)), fill = "deepskyblue3", col = "black") + 
    theme_bw() +
    xlab("Antivirals used") +
    ylab("Proportion of patients") +
    scale_x_upset() 
  
}

##############################################################################
# Function below calculates mean and variance estimates and confidence intervals (by bootstrap) for the following time-based distributions.
# Onset to admission
# Admission to outcome (death/ recovery)
# Admission to ICU; ICU duration
# Admission to NIMV; NIMV duration
# Admission to IMV; IMV duration


# For bootstrap
samp.mean <- function(x, i) {mean(x[i])} 
samp.var <- function(x, i){var(x[i])}
samp.median <- function(x,i){median(x[i])}

fit.summary.gamma <- function(fit){
  
  m <- fit$estimate[['shape']]/fit$estimate[['rate']]       # mean
  v <- fit$estimate[['shape']]/(fit$estimate[['rate']])^2   # variance
  
  set.seed(101)
  # Sample
  X = rgamma(1e3, shape = fit$estimate[['shape']], rate = fit$estimate[['rate']] )
  # Bootstrap (mean)
  bm <- boot(data = X , statistic = samp.mean, R=1000)
  # CI
  lower.m <-  boot.ci(bm, type = 'bca')$bca[4]       # lower bound of confidence interval for mean
  upper.m <-  boot.ci(bm, type = 'bca')$bca[5]       # upper bound of confidence interval for mean
  # Bootstrap (variance)
  bv <- boot(data = X, statistic = samp.var, R=1000 )
  # CI
  lower.v <- boot.ci(bv, type = 'bca')$bca[4]       # lower bound of confidence interval for mean
  upper.v <- boot.ci(bv, type = 'bca')$bca[5]       # upper bound of confidence interval for mean
  # Bootstrap (median)
  bmed <- boot(data = X, statistic = samp.median, R=1000 )
  # CI
  lower.med <- boot.ci(bmed, type = 'bca')$bca[4]       # lower bound of confidence interval for mean
  upper.med <- boot.ci(bmed, type = 'bca')$bca[5]       # upper bound of confidence interval for mean
  
  
  return(list(m=m, lower.m = lower.m, upper.m = upper.m,  v=v, 
              lower.v = lower.v, upper.v = upper.v, bmed = bmed, lower.med = lower.med,
              upper.med = upper.med))
  
}



casefat2 <-  function(data, conf=0.95){
  
  # Function for the estimation of the case fatality ratio based on the nonparametric KM-like method by
  # Ghani et ql. 2005:  https://doi.org/10.1093/aje/kwi230
  
  #############################################################
  
  # Modify data
  
  # Exclude rows which no entries for length of stay
  
  data2 <- data %>% filter(!is.na(start.to.exit) | !is.na(admission.to.censored))
  data2 <- data2 %>% 
    mutate(length.of.stay = map2_dbl(start.to.exit, admission.to.censored, function(x,y){
      max(x, y, na.rm = T)
    }))
  
  
  t <- abs(data2$length.of.stay)  # time
  f <- as.factor(data2$outcome)   # status
  
  ###############################################################
  # CFR calculation
  
  c = survfit(Surv(t, f)~1)
  di = which(c$states=="death")  # deaths
  ri = which(c$states=="discharge")  # recoveries
  
  
  # c$pstate is cumulative incidence function for each endpoint
  theta1 = max(c$pstate[,di])
  theta2 = max(c$pstate[,ri])
  
  cfr = theta1/(theta1+theta2)
  
  ###############################################################
  # Survivor function and variance for combined endpoint 
  
  f.end <- revalue(f, c(death = 'endpoint', discharge = 'endpoint'))
  
  c0 = survfit(Surv(t, f.end)~1)
  si = which(c0$states!="endpoint")
  S0 = c0$pstate[,si]
  V0 = (c0$std.err[,si])^2
  n = length(V0)
  if(V0[n]<1E-12){
    V0[n]=V0[n-1]
  }
  nrisk = c0$n.risk[,si]
  
  
  # hazard contributions for each endpoint
  h1 = c$n.event[,di]/nrisk
  h2 = c$n.event[,ri]/nrisk
  
  
  ###############################################################
  # Variances
  
  # Greenwood-like method
  M = diag(V0)
  for(j in 2:nrow(M)){
    for(k in 1:(j-1)){
      M[j, k] = V0[k]*S0[j]/S0[k]
      M[k, j] = M[j, k]
    }
  }
  
  v1 = as.numeric(sum((S0)^2*h1/pmax(nrisk, 1)) + (h1 %*% M %*% h1))
  v2 = as.numeric(sum((S0)^2*h2/pmax(nrisk, 1)) + (h2 %*% M %*% h2))
  cov12 = as.numeric((h1 %*% M %*% h2))
  
  secfr = sqrt((theta2^2*v1 + theta1^2*v2 - 2*theta1*theta2*cov12))/(theta1+theta2)^2
  
  
  
  ###############################################################
  # logit scale for CI
  
  lc = log(cfr/(1-cfr))
  sel = sqrt(v1/theta1^2 + v2/theta2^2 - 2*cov12/(theta1*theta2))
  alpha = (1-conf)/2
  za = -qnorm(alpha)
  llc = lc-za*sel
  ulc = lc+za*sel
  lcfr = exp(llc)/(1+exp(llc))
  ucfr = exp(ulc)/(1+exp(ulc))
  
  
  
  ###############################################################
  
  # Two simple methods
  Nt = c$n
  Nd = sum(c$n.event[,di])
  Nr = sum(c$n.event[,ri])
  
  e1 = Nd/Nt
  see1 = sqrt(e1*(1-e1)/Nt)
  a1 = Nd+0.5
  b1 = Nt-Nd+0.5
  le1 = qbeta(0.025, shape1=a1, shape2=b1)
  ue1 = qbeta(0.975, shape1=a1, shape2=b1)
  
  
  e2 = Nd/(Nd+Nr)
  see2 = sqrt(e2*(1-e2)/(Nd+Nr))
  a2 = Nd+0.5
  b2 = Nr+0.5
  alpha = (1-conf)/2
  le2 = qbeta(alpha, shape1=a2, shape2=b2)
  ue2 = qbeta(1-alpha, shape1=a2, shape2=b2)
  
  
  ###############################################################
  
  return(list(cfr=cfr, secfr=secfr, lcfr = lcfr, ucfr = ucfr, c=c,
              e1=e1, see1=see1, le1=le1, ue1=ue1,
              e2=e2, see2=see2, le2=le2, ue2=ue2))
  
}



########### Distribution plots ############






########### Admission to outcome #########


adm.outcome.func <- function(data){
  
  data2 <- data %>% filter(!is.na(start.to.exit) | !is.na(admission.to.censored))
  
  data2 <- data2 %>% 
    mutate(length.of.stay = map2_dbl(start.to.exit, admission.to.censored, function(x,y){
      max(x, y, na.rm = T)
    }))
  
  admit.discharge <- data2$length.of.stay
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge <- round.zeros(admit.discharge)
  
  pos.cens <- which(data2$censored == 'TRUE')
  
  left <- c(admit.discharge)
  right <- replace(admit.discharge, pos.cens, values=NA )
  censored_df <- data.frame(left, right)
  fit <- fitdistcens(censored_df, dist = 'gamma')
  t <- data.frame(x = admit.discharge)
  
  obs <- right[!(is.na(right))] # cases with completed duration days.
  
  
  plt <- ggplot(data = t) + 
    #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+    
    geom_line(aes(x=t$x, y=dgamma(t$x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
    labs(y = 'Density', x = 'Time (in days) from admission to death or recovery', title = '')
  
  return(list(plt=plt, fit=fit, obs = obs))
  
}

adm.outcome.plot <- function(data, ...){
  adm.outcome.func(data)$plt
}

########## Onset to admission #####


onset.adm.func <- function(data){
  
  admit.discharge <- data$onset.to.admission
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge.2 <- round.zeros(admit.discharge)
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  obs <-  admit.discharge.2  # record observed values for reporting
  
  # Plot 
  
  t <- data.frame(x=admit.discharge)
  plt <- ggplot(data = t) + 
    #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+    
    geom_line(aes(x=t$x, y=dgamma(t$x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + xlim(0, 30) + geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
    labs(y = 'Density', x = 'Time (in days) from symptom onset to admission', title = ' ')
  
  return(list(plt=plt, fit=fit, obs = obs))
  
  
}

onset.adm.plot <- function(data, ...){
  onset.adm.func(data)$plt
}


# Cumulative recruitment by outcome

recruitment.dat.plot <- function(data, embargo.limit, ...) {
  data$outcome.count <- 0
  data$outcome.count[data$outcome != "censored"] <- 1
  data$censored.count <- 0
  data$censored.count[data$outcome == "censored"] <- 1
  
  from <- min(data$start.date, na.rm = TRUE)
  to <- max(data$start.date, na.rm = TRUE)
  
  plt.d <- data.frame(d = from:to)
  plt.d$date <- as.Date(plt.d$d, origin = "1970-01-01")
  plt.d$outcome <- 0
  plt.d$censored <- 0
  
  for (i in from:to) {
    plt.d$outcome[plt.d$date == i] <- 
      sum(data$outcome.count[data$start.date == i], na.rm = TRUE)
    plt.d$censored[plt.d$date == i] <- 
      sum(data$censored.count[data$start.date == i], na.rm = TRUE)    
  }
  plt.d$out.c <- cumsum(plt.d$outcome)
  plt.d$cen.c <- cumsum(plt.d$censored)
  
  xmin <- as.Date("2020-02-01")
  plt.d <- subset(plt.d, date >= xmin)
  
  p <- ggplot(data = plt.d, aes(x = date)) +
    geom_line(aes(y = out.c, colour = "Outcome recorded"), size = 1.5) +
    geom_line(aes(y = cen.c, colour = "Follow-up ongoing"), size = 1.5) +
    geom_vline(xintercept = embargo.limit, linetype = "dashed") +
    theme_bw() + 
    theme(legend.title=element_blank(), legend.position="top") +
    xlab("Admission date") +
    xlim(xmin, to) +
    ylab("Cumulative recruitment")
  
  return(p)
}



######### Survival plot ######


# surv.plot.func <- function(data, ...){
#   
#   data2 <- data %>% dplyr::filter(!is.na(start.to.exit) | !is.na(admission.to.censored))
#   data2 <- data2 %>% 
#     dplyr::mutate(length.of.stay = map2_dbl(start.to.exit, admission.to.censored, function(x,y){
#       max(x, y, na.rm = T)
#     })) %>%
#     dplyr::mutate(length.of.stay = abs(length.of.stay))
#     
#   data2$sex <- data2$sex[!is.na(data2$sex)]  #plyr::revalue(as.factor(data2$sex), c('1' = 'Male', '2' = 'Female'))
#    
#   data2$event <- as.factor(as.numeric(data2$censored)) #True (1) = censored (no event), false (0) = not censored (experienced event)
#   
#   # Changing to event code to match Surv specifications 
#   # codes swapped to obtain actual discharge probabilities. #1 - event , 2 - no event 
#   
#   data2$event <- as.numeric(data2$event)    
# 
#   
#   df <- data.frame(data2$sex, data2$length.of.stay, data2$event)
#   names(df) <- c('sex', 'length.of.stay', 'event')
#   
#   #df <- data2 %>% dplyr::select(sex, length.of.stay, event) %>%
#    # mutate(length.of.stay = abs(length.of.stay)) %>%
#    # data.frame()
#   
#   fit <- survival::survfit(Surv(length.of.stay, event) ~ sex, data = df)
#   
#   plt <- survminer::ggsurvplot(fit,
#                     pval = T, pval.coord = c(0, 0.03), conf.int = T,
#                     risk.table = F, # Add risk table
#                     # risk.table.col = "strata", # Change risk table color by groups
#                     linetype = "strata", # Change line type by groups
#                     #surv.median.line = "hv", # Specify median survival
#                     ggtheme = theme_bw(), # Change ggplot2 theme
#                     lengend.labs = c('Male', 'Female'),
#                     palette = c('#D2691E', '#BA55D3'),
#                     legend.labs = c("Male", "Female"), title = (main = ' '), ylab = 'Cumulative probability (of hospital exit)' , xlab = 'Time (in days) from admission', legend = c(0.8, 0.9))
#   
#  # pval <- round(surv_pvalue(fit)$pval, 2)
#   
#   return(list(plt=plt, df=df))
#   
#   #return(list(plt = plt, pval=pval))
#   
# }









