##### GRAPH FUNCTIONS #####


##### Age pyramid ####

#' @title 
#' Plot patient demographics by outcome.
#' @description 
#' Plots the age and sex distribution of patients according to clinical outcome.
#' 
#' @export age.pyramid
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}.. This should be a dataframe which includes columns for age group, sex, and outcome of patients. See `Details'.
#' @return  Bar plot of the age (in intervals of four years) and sex (male/female) of patients, plotted according to clinical outcome (discharge/death/ongoing care).
#'
#' @details 
#' The columns of \code{data} for age group, sex and outcome should be named "agegp5", "sex" and "outcome" respectively and formatted as follows:
#' the variable "sex" should be numeric with values 1 and 2 for males and females respectively;
#' the variable "agegp5" should be a factor with levels 0-4, 5-9, 10-14, ...., 90+;
#' and the variable `outcome' should be a factor with levels'
#' `discharge', 
#' `censored' 
#' and `death'; in this case,
#' `censored' patients are those for whom clinical care is ongoing.

age.pyramid <- function(data, ...){
  
  if( all(is.na(data$consolidated.age)) | all(is.na(data$sex))){
    plt <- insufficient.data.plot()
  }else{
    
    data2 <- data %>%
      filter(!is.na(outcome)) %>%
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
    
    order.of.magnitude <- ceiling(log10(max.count))
    
    if(as.numeric(substr(as.character(max.count), 1, 1)) > 5){
      tick.increment <- 10^(order.of.magnitude-1)
    } else {
      tick.increment <- 10^(order.of.magnitude-1)/2
    }
    
    plot.breaks <- seq(-(ceiling(max.count/tick.increment)*tick.increment), ceiling(max.count/tick.increment)*tick.increment, by = tick.increment)
    plot.labels <- as.character(c(rev(seq(tick.increment, ceiling(max.count/tick.increment)*tick.increment, by = tick.increment)),
                                  0,
                                  seq(tick.increment, ceiling(max.count/tick.increment)*tick.increment, by= tick.increment)))
    
    
    plt <- ggplot() + geom_bar(data = (data2 %>% filter(sex == "M")), aes(x=agegp5, y=count, fill = outcome), stat = "identity") +
      geom_bar(data = data2 %>% filter(sex == "F"), aes(x=agegp5, y=count, fill = outcome),  stat = "identity") +
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
        ymin = -max.count/2,
        ymax = -max.count/2,
        xmin = length(levels(data2$agegp5))+1.5 ,
        xmax = length(levels(data2$agegp5))+1.5) +
      annotation_custom(
        grob = textGrob(label = "Females", hjust = 0.4, gp = gpar(cex = 1.5)),
        ymin = max.count/2,
        ymax = max.count/2,
        xmin = length(levels(data2$agegp5))+1.5,
        xmax = length(levels(data2$agegp5))+1.5) +
      theme(plot.margin=unit(c(30,5,5,5.5,5.5),"pt"))
    
  }
  
  plt
  
}
##### Distribution of sites by country #####


#' Plot the number of sites by country
#'
#' Plots the number of sites by country
#'
#' @export sites.by.country
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}.. This should be a dataframe which includes columns for the country and site associated with each patient. See `Details'.
#'
#' The columns of \code{data} containing country and site names should be named "Country" and "site.name" respectively.
#'
#' @return  Bar plot showing the number of sites per country. Actual counts are printed on top of each bar.
sites.by.country <- function(data, ...){
  data2 <- data %>%
    group_by(Country, site.name) %>%
    dplyr::summarise(n.sites = 1) %>%
    dplyr::summarise(n.sites = sum(n.sites)) %>%
    filter(!is.na(Country))
  
  nudge <- max(data2$n.sites)/30
  
  ggplot(data2) + geom_col(aes(x = Country, y = n.sites), fill = "deepskyblue3") +
    theme_bw() +
    xlab("Country") +
    ylab("Sites") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    geom_text(aes(x=Country, y=n.sites + nudge, label=n.sites), size=4)
}


##### Distribution of patients and outcomes by country #####


#' Plot the distribution of patients by country and outcome
#'
#' Plots the distribution of patients by country and outcome.
#'
#' @export outcomes.by.country
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}.. This should be a dataframe which includes columns for the country and outcome associated with each patient. See `Details'.
#' @return  Bar plot showing the number of patients per country and by outcome (discharge/ongoing care/death). Actual counts of the total number of patients for each country are printed on top of each bar.
outcomes.by.country <- function(data, include.uk = TRUE, ...){
  data2 <- data %>%
    filter(!is.na(outcome)) %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("discharge", "censored","death")))  %>%
    filter(!is.na(Country)) %>%
    mutate(uk = Country == "UK")
    
  if(!include.uk){
    data2 <- data2 %>% filter(!uk)
  }
  
  data2 <- data2 %>%
    group_by(Country, outcome, uk) %>%
    summarise(count = n()) %>%
    ungroup()

  
  data3 <- data %>%
    filter(!is.na(outcome)) %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("discharge", "censored","death")))  %>%
    filter(!is.na(Country)) %>%
    group_by(Country) %>%
    summarise(count = n()) %>%
    mutate(uk = Country == "UK")
  
  nudge <- max(data3$count)/1500
  
  plot1 <- ggplot() +
    geom_text(data = data3 %>% filter(!uk), aes(x=Country, y= count + nudge, label=count), size=2) +
    geom_col(data = data2 %>% filter(!uk), aes(x = Country, y=count,  fill = outcome)) +
    theme_bw() +
    scale_x_discrete(expand = c(0,1.5))+
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Discharge", "Ongoing care", "Death"), guide = F) +
    # facet_wrap (~ uk, scales = "free") +
    # xlab("Country") +
    ylab("Cases") +
    # coord_fixed(ratio = 0.23) +
    theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.x=element_blank())
  nudge2 <- max(data3$count)/30
  
  
  plot2 <- ggplot() +
    geom_text(data = data3 %>% filter(uk), aes(x=Country, y= count + nudge2, label=count), size=2) +
    geom_col(data = data2%>% filter(uk), aes(x = Country, y=count,  fill = outcome), width=1) +
    theme_bw() +
    scale_x_discrete(expand = c(0,1.5))+
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Discharge", "Ongoing care", "Death")) +
    # facet_wrap (~ uk, scales = "free") +
    # xlab("Country") +
    ylab("Cases") +
    # coord_fixed(ratio = 0.1) +
    theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.y=element_blank(), axis.title.x=element_blank())
  
  
  out <- plot_grid(plot1, plot2, ncol = , align = "h", rel_widths = c(1.8,1))
  
  out <- add_sub(out, "Country", vpadding=grid::unit(0,"lines"),y=5, x=0.5, vjust=4.5)
}

##### Outcomes by epi-week #####


#' Plot weekly admission counts
#'
#' Plots patient numbers and outcomes by epidemiological week (of 2020) of admission (or, for patients infected in hospital, of symptom onset).
#'
#' @export outcomes.by.admission.date
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}.. This should be a dataframe which includes columns for the date of admission and  outcome for each patient. See `Details'.
#' @return  Bar plot showing the number of patients per country and by outcome (discharge/ongoincg care/death). Bars are annotated with counts.
#'
outcomes.by.admission.date <- function(data, embargo.limit, ...){
  data2 <- data %>%
    filter(!is.na(outcome)) %>%
    dplyr::mutate(outcome = factor(outcome, levels = c("discharge", "censored", "death"))) %>%
    mutate(epiweek = map_dbl(start.date, function(x){
      ew <- epiweek(x)
    })) %>%
    mutate(two.digit.epiweek = map_chr(epiweek, function(x){
      ifelse(nchar(as.character(x))==1, glue("0{as.character(x)}"), as.character(x))
    })) %>%
    filter(!is.na(admission.date)) %>%
    filter(epiweek(start.date) <= epiweek(embargo.limit))
  
  ew.labels <- map_chr(min(data2$epiweek):max(data2$epiweek), function(x)ifelse(nchar(as.character(x))==1, glue("0{as.character(x)}"), as.character(x)))
  
  data2 <- data2 %>%
    mutate(two.digit.epiweek = factor(two.digit.epiweek, levels = ew.labels))
  
  peak.cases <- data2 %>% group_by(two.digit.epiweek) %>% dplyr::summarise(count = n()) %>% pull(count) %>% max()
  
  ggplot(data2) + geom_bar(aes(x = two.digit.epiweek, fill = outcome), width = 0.95) +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F", labels = c("Discharge", "Ongoing care", "Death")) +
    # scale_x_continuous(breaks = seq(min(epiweek(data2$hostdat), na.rm = TRUE), max(epiweek(data2), na.rm = TRUE), by=2)) +
    xlab("Epidemiological week of admission/symptom onset (2020)") +
    ylab("Cases") +
    ylim(c(0,peak.cases)) +
    scale_x_discrete(drop = F) +
    annotate(geom = "text", label = "*", x = max(data2$epiweek) - min(data2$epiweek) + 1,
             y = nrow(data2 %>% filter(two.digit.epiweek == max(data2$epiweek))), size =15)
}

##### Comorbidities upset plot #####

#' Plot prevalence of combinations of comorbidities.
#'
#'  Plots the distribution of combinations of the most common comorbidities, amongst all patients for whom these data were recorded.
#'
#' @export comorbidities.upset
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#' @param max.comorbidities The \code{max.comorbidities} most frequent comorbidities will be included in the upset plot. Defaults to 4.
#'
#' @return UpSet plot showing the frequency of combinations of the top \code{max.comorbidities} comorbidities.
#' Filled and empty circles below the x-axis of the plot indicate the presence or absence of each comorbidity.
#' The `Any other' category in the upset plot contains all remaining comorbidities which are not included in the top \code{max.comorbidities} comorbidities, as well as any other comorbidities recorded as free text by clinical staff.
#'
#' @examples
#' comorbidities.upset(data = patient.data, max.comorbidities = 4)
comorbidities.upset <- function(data, max.comorbidities, comorbidities, ...){
  # (max.comorbidities is the n to list; this will be the n most frequent)
  # just the comorbidity columns
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(comorbidities$field))
  
  n.comorb <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.comorb+1), names_to = "Condition", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
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
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
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
    # filter(!is.na(Present)) %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
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
    geom_bar(aes(y=..count../sum(..count..)), fill = "indianred3") +
    theme_bw() +
    xlab("Conditions present at admission") +
    ylab("Proportion of patients") +
    scale_x_upset()
}

##### Symptoms upset plot #####

#' Plot prevalence of combinations of symptoms.
#'
#'  Plots the distribution of combinations of the most common symptoms on admission, amongst all patients for whom these data were recorded.
#'
#' @export symptoms.upset
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#' @param max.symptoms The \code{max.symptoms} most frequent symptoms will be included in the plot. Defaults to 4.
#'
#' @return UpSet plot showing the frequency of combinations of the top \code{max.symptoms} symptoms.
#' Filled and empty circles below the x-axis of the plot indicate the presence and absence respectively of each symptom.
#' The `Any other' category in the upset plot contains all remaining comorbidities which are not included in the top \code{max.symptoms} symptoms.
#'
#' @examples
#' symptoms.upset(data = patient.data, max.symptoms = 4)
symptoms.upset <- function(data, max.symptoms, admission.symptoms, ...){
  # (max.symptoms is the n to list; this will be the n most frequent)
  
  # just the symptom columns
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms$field))
  
  n.symp <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.symp+1), names_to = "Condition", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
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
    # filter(!is.na(Present)) %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
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
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
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
    geom_bar(aes(y=..count../sum(..count..)), fill = "deepskyblue3") +
    theme_bw() +
    xlab("Symptoms present at admission") +
    ylab("Proportion of patients") +
    scale_x_upset()
}

#' @export
#' @keywords internal

symptom.prev.calc <- function(data, admission.symptoms){
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

##### Prevalence of symptoms heatmap #####
#'  Plot pairwise symptom prevalance.
#'
#'  Plots a heatmap for prevalance of pairwise combinations of symptoms.
#'  The pairwise prevalence proportions are caculated amongst patients with recorded presence or absence of both symptoms.
#' @export symptom.heatmap
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Heatmap showing the proportion of patients for each pairwise combination of symptoms.
symptom.heatmap <- function(data, admission.symptoms, ...){
  
  data2 <- data %>%
    dplyr::select(subjid, one_of(admission.symptoms$field))
  
  
  phi.correlation <- function(c1, c2){
    if(c1 == c2){
      return(1)
    } else {
      restricted.df <- data2 %>% dplyr::select_at(c(c1, c2))
      
      restricted.df <- restricted.df %>%
        filter((!!sym(c1) != 3) & (!!sym(c2) != 3) & !is.na(!!sym(c1)) & !is.na(!!sym(c2))) %>%
        mutate(!!sym(c1) := (!!sym(c1) == 1)) %>%
        mutate(!!sym(c2) := !!sym(c2) == 1)
      
      twobytwo <- table(restricted.df[[c1]], restricted.df[[c2]])
      # print(twobytwo)
      
      if(nrow(twobytwo) == 2 & ncol(twobytwo) == 2){
        return(phi(twobytwo))
      } else {
        return(NA)
      }
      
      
    }
  }
  
  combinations.tibble <- tibble(cond1 = rep(admission.symptoms$field, length(admission.symptoms$field)),
                                cond2 = rep(admission.symptoms$field, each = length(admission.symptoms$field))) %>%
    mutate(phi.correlation = map2_dbl(cond1, cond2, phi.correlation)) %>%
    left_join(admission.symptoms, by=c("cond1" = "field"), suffix = c(".x", ".y")) %>%
    left_join(admission.symptoms, by=c("cond2" = "field"), suffix = c(".x", ".y"))
  
  fct.order <- c("Runny nose",
                 "Sore throat",
                 "Ear pain",
                 "Diarrhoea",
                 "Vomiting / Nausea",
                 "Abdominal pain",
                 "Joint pain",
                 "Muscle aches",
                 "Fatigue / Malaise",
                 "Headache",
                 "Shortness of breath",
                 "History of fever",
                 "Wheezing",
                 "Cough (no sputum)",
                 "Cough (with sputum)",
                 "Cough (bloody sputum / haemoptysis)",
                 "Chest pain",
                 "Lymphadenopathy",
                 "Disturbance or loss of taste",
                 "Disturbance or loss of smell",
                 "Conjunctivitis",
                 "Bleeding",
                 "Skin ulcers",
                 "Skin rash",
                 "Seizures",
                 "Altered consciousness / confusion"
  )
  
  combinations.tibble.2 <- combinations.tibble %>%
    mutate(label.x = factor(label.x, levels = fct.order)) %>%
    mutate(label.y = factor(label.y, levels = fct.order))
  
  
  ggplot(combinations.tibble.2) +
    geom_tile(aes(x=label.x, y=label.y, fill=phi.correlation)) +
    scale_fill_gradient2(low = "deepskyblue3", mid = "white", high = "indianred3",
                         name = "phi coefficient", limits = c(-1,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_fixed()
  
}

##### Barplot for the prevalence of symptoms #####

#'  Plot distribution of symptom prevalance.
#'
#'  Plots the proportion of patients presenting with each symptom at admission.
#'
#'  Note that the denominators used in the computation of proportions may differ by symptom as symptom information
#'  may be incomplete for some patients.
#'
#' @export symptom.prevalence.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Barplot showing the proportion of patients reporting each symptom.
#' Bars are annotated with a fraction representing the number of patients presenting with a symptom
#' over the number of patients for whom presence or absence of that symptom was recorded.
symptom.prevalence.plot <- function(data, admission.symptoms, ...){
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
    dplyr::mutate(Absent = Total - Present) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(prop.yes) %>%
    dplyr::mutate(Condition = as_factor(label)) %>%
    dplyr::select(Condition, Present, Absent) %>%
    pivot_longer(c(Present, Absent), names_to = "affected", values_to = "Count") %>%
    dplyr::mutate(affected = map_lgl(affected, function(x) x == "Present")) %>%
    filter(Condition != "Other") %>%
    group_by(Condition) %>%
    mutate(total = sum(Count)) %>%
    ungroup() %>%
    mutate(Proportion = Count/total) %>%
    mutate(label = glue("{Count}/{total}")) %>%
    dplyr::select(-total)
  
  plt <- ggplot(data2) +
    geom_col(aes(x = Condition, y = Proportion, fill = affected)) +
    geom_text(data = data2 %>% filter(affected), aes(x=Condition, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Symptom") +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("deepskyblue1", "deepskyblue4"), name = "Symptom\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}

# Prevalence of comorbidities
#' @export
#' @keywords internal


comorb.prev.calc <- function(data, comorbidities){
  
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
    dplyr::summarise(present = sum(Present == "present"), absent = sum(Present == "absent"), unknown = sum(Present == "unknown")) %>%
    dplyr::left_join(comorbidities, by = c("Condition" = "field"))
  
  return(data3)
  
}


##### Barplot for the prevalence of comorbidities #####

#'  Plot distribution of comorbidity prevalance.
#'
#'  Plots the proportion of patients reporting each comorbidity at admission.
#'
#'  Note that the denominators used in the computation of proportions may differ by comorbidity as
#'  information on the presence or absence of some comorbidities may be missing/incomplete for some patients.
#' @export comorbidity.prevalence.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Barplot showing the proportion of patients reporting each symptom.
#' Bars are annotated with a fraction representing the number of patients reporting a comorbidity
#' over the number of patients for whom presence or absence of that comorbidity was recorded.
comorbidity.prevalence.plot <- function(data, comorbidities, ...){
  
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
    dplyr::mutate(Absent = Total - Present) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(prop.yes) %>%
    dplyr::mutate(Condition = as_factor(label)) %>%
    dplyr::select(Condition, Present, Absent) %>%
    pivot_longer(c(Present, Absent), names_to = "affected", values_to = "Count") %>%
    dplyr::mutate(affected = map_lgl(affected, function(x) x == "Present")) %>%
    filter(Condition != "Other") %>%
    group_by(Condition) %>%
    mutate(total = sum(Count)) %>%
    ungroup() %>%
    mutate(Proportion = Count/total) %>%
    mutate(label = glue("{Count}/{total}")) %>%
    dplyr::select(-total)
  
  plt <- ggplot(data2) +
    geom_col(aes(x = Condition, y = Proportion, fill = affected)) +
    geom_text(data = data2 %>% filter(affected), aes(x=Condition, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("indianred1", "indianred4"), name = "Condition\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}



# Raw proportions of patients undergoing each treatment
#' @export
#' @keywords internal


treatment.use.calc <- function(data, treatments){
  
  treatment.columns <- map(1:nrow(data), function(i){
    data$events[i][[1]] %>%
      filter(startsWith(redcap_event_name, "discharge")) %>%
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
    dplyr::summarise(present = sum(Present == "present"), absent = sum(Present == "absent"), unknown = sum(Present == "unknown")) %>%
    dplyr::left_join(treatments, by = c("Condition" = "field"))
  
  return(data3)
}


##### Treatment use plot #####

#'  Plot distribution of treatments used.
#'
#'  Plots the proportion of patients given each treatment during clinical care.
#'
#'  Note that the denominators used in the computation of proportions may differ by treatment as
#'  information on treatment given may be missing/incomplete for some patients.
#' @export treatment.use.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Barplot showing the proportion of patients given each treatment.
#' Bars are annotated with a fraction representing the number of patients given a treatment
#' over the number of patients for whom presence or absence of that treatment was recorded.
treatment.use.plot <- function(data, treatments, ...){
  
  treatment.columns <- map(1:nrow(data), function(i){
    data$events[i][[1]] %>%
      filter(startsWith(redcap_event_name, "discharge")) %>%
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
    dplyr::mutate(Absent = Total - Present) %>%
    dplyr::mutate(prop.yes = Present/Total) %>%
    dplyr::mutate(prop.no = 1-prop.yes) %>%
    arrange(prop.yes) %>%
    dplyr::mutate(Treatment = as_factor(label)) %>%
    pivot_longer(c(Present, Absent), names_to = "treated", values_to = "Count") %>%
    dplyr::mutate(affected = map_lgl(treated, function(x) x == "Present"))  %>%
    group_by(Treatment) %>%
    mutate(total = sum(Count)) %>%
    ungroup() %>%
    mutate(Proportion = Count/total) %>%
    mutate(label = glue("{Count}/{total}")) %>%
    dplyr::select(-total)
  
  plt<-  ggplot(data2) +
    geom_col(aes(x = Treatment, y = Proportion, fill = affected)) +
    geom_text(data = data2 %>% filter(affected), aes(x=Treatment, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("chartreuse2", "chartreuse4"), name = "Treatment", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  plt
}


# Upset plot for treatments @todo add maximum parameter?
#' @export
#' @keywords internal


treatment.upset.prep <- function(data, ...) {
  details <- subset(
    data,
    select = c(subjid, outcome,
               antibiotic.any, antiviral.any, antifungal.any, steroid.any,
               NIMV.ever, IMV.ever, O2.ever,
               RRT.ever, Inotrope.ever
    )
  )
  # Do not plot if no outcome (i.e. censored) or if all treatments are NA
  details <- details %>%
    filter(!is.na(outcome)) %>%
    filter(!outcome == "censored")
  details$AllNa <- 1
  details$AllNa[is.na(details$antibiotic.any) == FALSE |
                  is.na(details$antiviral.any) == FALSE |
                  is.na(details$antifungal.any) == FALSE |
                  is.na(details$steroid.any) == FALSE |
                  is.na(details$NIMV.ever) == FALSE |
                  is.na(details$IMV.ever) == FALSE |
                  is.na(details$O2.ever) == FALSE |
                  is.na(details$RRT.ever) == FALSE |
                  is.na(details$Inotrope.ever) == FALSE] <- 0
  details <- details %>%
    filter(AllNa == 0) %>%
    dplyr::select(-AllNa, - outcome)
  # If ventilated then had O2 therapy
  details$O2.ever[details$NIMV.ever == TRUE] <- TRUE
  details$O2.ever[details$IMV.ever == TRUE] <- TRUE
  # 1 is Yes, set anything else to 0 (No)
  for (i in 2:10) {
    details[, i][details[, i] != 1 | is.na(details[, i]) == TRUE] <- 0
  }
  #Create "any antimicrobial"
  details <- details %>%
    mutate(any.antimicrobial = pmax(antibiotic.any, antiviral.any, antifungal.any))
  return(details)
}

#' @export
#' @keywords internal

make.props.treats <- function(data, ...){
  details <- treatment.upset.prep(data)
  details$All <- 1
  data2 <- data.frame(
    N = sum(details$All),
    n.abx = sum(details$antibiotic.any),
    n.av = sum(details$antiviral.any),
    n.antif = sum(details$antifungal.any),
    n.steroid = sum(details$steroid.any),
    n.O2 = sum(details$O2.ever),
    n.NIV = sum(details$NIMV.ever),
    n.IMV = sum(details$IMV.ever),
    n.RRT = sum(details$RRT.ever),
    n.inotrope = sum(details$Inotrope.ever)
  )
  return(data2)
}

# df <- make.props.treats(patient.data)

## "Modified KM plot" ##     - increase xlim

#'  Plot case fatality ratio (CFR) and survival functions for deaths and recovery.
#'
#'  Plots the proportion of deaths and recoveries over time as well as a non-parametric estimate for the CFR using
#'  an adapted Kaplan-Meier method. See `Details'.
#'
#'  The CFR and survival functions for death and recovery are estimated using a nonparametric Kaplan-Meier-based method proposed by Ghani et al. (2005).
#'  This method estimates the CFR with the formula a/(a+b), where a and b are the values of the cumulative incidence function for deaths and recoveries respectively,
#'  estimated at the last observed time point. See `References' for details.
#' @export modified.km.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Plot of the survival functions for deaths and recoveries and a line indicating the CFR estimate.
#'
#' @references
#' A. C. Ghani, C. A. Donnelly, D. R. Cox, J. T. Griffin, C. Fraser, T. H. Lam, L. M. Ho, W. S. Chan, R. M. Anderson, A. J. Hedley, G. M. Leung (2005).
#' Methods for Estimating the Case Fatality Ratio for a Novel, Emerging Infectious Disease, *American Journal of Epidemiology*, **162**(5), 479-486.
#' [doi:10.1093/aje/kwi230](doi:10.1093/aje/kwi230).
modified.km.plot <- function(data, embargo.limit, ...) {
  
  
  # Method: Ghani et ql. 2005:  https://doi.org/10.1093/aje/kwi230
  
  # Exclude rows which no entries for length of stay
  
  # c$pstate is cumulative incidence function for each endpoint
  c <- casefat2(data, embargo.limit)$c
  Fd <- c$pstate[,which(c$states=="death")] # death
  Fr <- c$pstate[,which(c$states=="discharge")] # recovery
  cfr <- casefat2(data, embargo.limit)$cfr
  
  
  # Plot
  df <- data.frame(day = rep(c$time,3), value = c(1-Fr, Fd, rep(cfr,length(Fd))),
                   status =factor(c(rep('discharge', length(Fd)), rep('death', length(Fd)), rep('cfr', length(Fd))),
                                  levels = c("death", "discharge", "cfr")
                   ))
  
  
  ggplot(data = df)+
    geom_line(aes(x=day, y = value, col = status, linetype = status), size=0.75)+
    geom_ribbon(data = df %>% filter(status == "death"), aes(x=day, ymin = 0, ymax = value), fill ="indianred", alpha = 0.66)+
    geom_ribbon(data = df %>% filter(status == "discharge"), aes(x=day, ymin = value, ymax = 1), fill ="springgreen4", alpha = 0.66)+
    theme_bw()+ xlim(0, 50) +
    scale_colour_manual(values = c("indianred",  "springgreen4", "black"), name = "Legend", labels = c( "Deaths", "Discharges","Case fatality\nratio")) +
    scale_linetype_manual(values = c( "solid", "solid", "dashed" ),  guide = F) +
    xlab("Days after admission") +
    ylab("Cumulative probability") +
    ylim(c(0,1))
  
}


#' @export
#' @keywords internal

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



##### Treatment upset plot #####

#' Plot frequency of combinations of treatments.
#'
#'  Plots the distribution of combinations of the 5 most common treatments administered during hospital stay,
#'  across all patients with completed hospital stay and recorded treatment data.
#' @export treatment.upset
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  UpSet plot showing the frequency of combinations of the 5 most common treatments.
#' Filled and empty circles below the x-axis of the plot indicate treatments that were and were not administered respectively.
treatment.upset <- function(data, ...) {
  details <- treatment.upset.prep(data)
  treatments2 <- details %>%
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
  treatments2$Treatment[treatments2$Treatment == "O2.ever"] <-
    "Any oxygen provision"
  treatments2$Treatment[treatments2$Treatment == "antiviral.any"] <-
    "Antiviral"
  treatments2$Treatment[treatments2$Treatment == "antibiotic.any"] <-
    "Antibiotic"
  treatments2$Treatment[treatments2$Treatment == "antifungal.any"] <-
    "Antifungal"
  treatments2$Treatment[treatments2$Treatment == "steroid.any"] <-
    "Corticosteroid"
  treatments2 <- treatments2 %>%
    group_by(subjid) %>%
    dplyr::summarise(Treatments = list(Treatment), Presence = list(Present)) %>%
    mutate(treatments.used = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  p <- ggplot(treatments2, aes(x = treatments.used)) +
    geom_bar(aes(y=..count../sum(..count..)), fill = "chartreuse3") +
    theme_bw() +
    xlab("Treatments used during hospital admission") +
    ylab("Proportion of patients") +
    scale_x_upset()
  
  return(p)
}

#' @export
#' @keywords internal

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
  treatments2 <- vd %>%
    dplyr::select(
      subjid,
      O2.ever, NIMV.ever, IMV.ever, ECMO.ever
    ) %>%
    pivot_longer(2:4, names_to = "Treatment", values_to = "Present") %>%
    mutate(Present = as.logical(Present))
  # Change labels
  treatments2$Treatment[treatments2$Treatment == "O2.ever"] <-
    "Any oxygen supplementation"
  treatments2$Treatment[treatments2$Treatment == "NIMV.ever"] <-
    "Non-invasive ventilation"
  treatments2$Treatment[treatments2$Treatment == "IMV.ever"] <-
    "Invasive ventilation"
  treatments2$Treatment[treatments2$Treatment == "ECMO.ever"] <-
    "ECMO"
  treatments2 <- treatments2 %>%
    group_by(subjid) %>%
    dplyr::summarise(Treatments = list(Treatment), Presence = list(Present)) %>%
    mutate(treatments.used = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  vent.plt <- ggplot(treatments2, aes(x = treatments.used)) +
    geom_bar(aes(y=..count../sum(..count..)), fill = "blue") +
    theme_bw() +
    xlab("Oxygen therapies used during hospital admission") +
    ylab("Proportion of patients") +
    scale_x_upset()
  
  return(vent.plt)
}

#' @export
#' @keywords internal

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

#' @export
#' @keywords internal

plot_outcome_saturations <- function(data, ...) {
  # oxy_vsorresu is  1, Room air|2, Oxygen therapy|3, N/A
  df <- patient.data  %>%
    dplyr::select(oxy_vsorresu, oxy_vsorres, outcome)
  # oxy_vsorres is a string
  df$oxy_vsorres <- destring(df$oxy_vsorres, keep = "0-9.-")
  # Drop values of SpO2 < 20%, NA SpO2, those not on room air, and those with
  # no outcome
  df <- df %>%
    filter(!is.na(oxy_vsorres)) %>%
    filter(oxy_vsorres > 20) %>%
    filter(oxy_vsorresu == 1) %>%
    filter(!is.na(outcome))
  df$SpO2_admission_ra <- 0
  thr <- c(20, 75, 80, 85, 88, 90, 92, 94, 96, 98)
  for (i in thr) {
    df$SpO2_admission_ra[df$oxy_vsorres >= i] <- i
  }
  df$SpO2_admission_ra <- factor(
    df$SpO2_admission_ra,
    levels = thr,
    labels = c("<75", "75-", "80-", "85-", "88-", "90-", "92-", "94-", "96-", "98-100")
  )
  df$Died <- df$Discharged <- df$Censored <- 0
  df$Died[df$outcome == "death"] <- 1
  df$Discharged[df$outcome == "discharge"] <- 1
  df$Censored[df$outcome == "censored"] <- 1
  df <- df %>%
    dplyr::select(SpO2_admission_ra, Died, Discharged, Censored) %>%
    group_by(SpO2_admission_ra) %>%
    summarise(Died = sum(Died), Discharged = sum(Discharged), Censored = sum(Censored))
  df$Tot <- df$Died + df$Discharged + df$Censored
  df$Died <- df$Died / df$Tot
  df$Discharged <- df$Discharged / df$Tot
  df$Censored <- df$Censored / df$Tot
  df$Censored <- df$Censored + df$Died
  df$Discharged <- df$Discharged + df$Censored
  
  p <- ggplot(data = df) +
    geom_col(aes(x = SpO2_admission_ra, y = Discharged, fill = "Discharged")) +
    geom_col(aes(x = SpO2_admission_ra, y = Censored, fill = "Ongoing care")) +
    geom_col(aes(x = SpO2_admission_ra, y = Died, fill = "Died")) +
    scale_fill_brewer(palette = "Dark2", name  = "Status", drop = F,
                      breaks = c("Discharged", "Ongoing care", "Died")) +
    geom_text(aes(x = SpO2_admission_ra, y = 1.1, label = Tot), size = 3) +
    geom_text(aes(x = -0, y = 1.1, label = "n ="), size = 3) +
    theme_bw() + theme(plot.margin = margin(1, 0, 0, 0, unit = "cm")) +
    xlab("Oxygen saturation (%) in room air on admission") +
    ylab("Proportion") +
    coord_cartesian(xlim = c(1, 10), ylim = c(0, 1), clip = "off")
  
  return(p)
}

#' @export
#' @keywords internal

plot_nosocomial <- function(data, ...){
  data2 <- data %>%
    filter(!is.na(admission.date) & !is.na(onset.date)) %>%
    dplyr::select(subjid, onset.date, admission.date) %>%
    mutate(at.07.days = onset.date >= admission.date + 7, at.14.days = onset.date >= admission.date + 14) %>%
    pivot_longer(4:5) %>%
    group_by(name) %>%
    summarise(perc = sum(value)/n())
  
  ggplot(data2) +
    geom_col(aes(name, perc*100), fill = "orange3") +
    scale_x_discrete(labels = c("At least 7 days", "At least 14 days"), name = "Time of symptom onset after admission") +
    ylab("Percentage of patients") +
    theme_bw()
  
}


######### Timeline plot ##############
# @todo add ICU. Add IMV.
# @todo rename 'ongoing care'.

#' Plot timelines by patients' status.
#'
#' Plots the distribution of patients' status by number of days after admission. Seven statuses are considered: `Discharge', 
#' `Transferred',
#'  `Unknown', 
#'  `Ongoing care', 
#'  `Ward', 
#'  `ICU' and 
#'  `Death'. 
#'  See `Details'.
#'
#'  Patients with `Unknown' status have left the site at the time of report but have unknown outcomes due to missing data.
#'  Patients with `Transferred' status have been transferred to another health facility by the time of the report.
#'  Patients still on site at the time of report appear in the`Ongoing care' category for days which are in the future at that time.
#'  (For example, a patient admitted 7 days before the date of report and still on site at report would be categorised as `ongoing care' for days 8 and later.)
#'  The black line in the plot marks the end of 14 days; due to the cut-off, only a small number of patients appear in the `ongoing care' category left of this line.
#' @export status.by.time.after.admission
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Plot showing the proportion of patients in each category over time. Each status has been assigned a different colour code to enable easy differentiation.
status.by.time.after.admission <- function(data, ...){
  
  data2 <- data %>%
    dplyr::mutate(final.status = map_chr(exit.code, function(x){
      ifelse(is.na(x), "censored", as.character(x))
    })) %>%
    dplyr::mutate(final.status = map_chr(final.status, function(x){
      switch(x,
             "censored" = "censored",
             "death" = "death",
             "discharge" = "discharge",
             "hospitalisation" = "unknown",
             "transfer" = "transfer",
             "transfer.palliative" = "transfer",
             "unknown" = "unknown")}
    )) %>%
    dplyr::mutate(final.status = factor(final.status)) %>%
    filter(!is.na(admission.date))
  
  timings.wrangle <- data2 %>%
    dplyr::select(subjid,
                  final.status,
                  start.to.ICU,
                  start.to.exit,
                  start.to.censored,
                  ICU.duration,
                  censored) %>%
    dplyr::mutate(hospital.start = 0) %>%
    dplyr::mutate(hospital.end = start.to.exit) %>%
    mutate(ICU.start = start.to.ICU) %>%
    mutate(ICU.end = start.to.ICU + ICU.duration) %>%
    mutate(censored.date = start.to.censored) %>%
    dplyr::select(subjid,  ends_with("start"), ends_with("end"), censored.date,  censored, final.status) %>%
    filter(hospital.end >= 0 | is.na(hospital.end))  %>%
    mutate(ever.ICU = !is.na(ICU.start)) %>%
    # If hospital end is known but ICU end is not, impossible to resolve
    filter(!(!is.na(hospital.end) & is.na(ICU.end) & ever.ICU)) %>%
    mutate(last.date = pmax(hospital.end, ICU.end, censored.date, na.rm = T))
  
  overall.start <- 0
  overall.end <- quantile(timings.wrangle$hospital.end, 0.99, na.rm = T)
  
  # this generates a table of the status of every patient on every day
  
  complete.timeline <- map(1:nrow(timings.wrangle), function(pat.no){
    times <- map(overall.start:overall.end, function(day){
      if(!timings.wrangle$ever.ICU[pat.no]){
        if(!timings.wrangle$censored[pat.no] & is.na(timings.wrangle$hospital.end[pat.no])){
          # this happens with an exit code but no exit date. We don't know what happened after admission
          "unknown"
        } else if(timings.wrangle$censored[pat.no]){
          if(day <= timings.wrangle$censored.date[pat.no]){
            "Ward"
          } else {
            "Censored"
          }
        } else {
          if(day <= timings.wrangle$hospital.end[pat.no]){
            "Ward"
          } else {
            as.character(timings.wrangle$final.status[pat.no])
          }
        }
      } else {
        if(!timings.wrangle$censored[pat.no] & is.na(timings.wrangle$hospital.end[pat.no])){
          # this happens with an exit code but no exit date. We don't know what happened after ICU admission or ICU exit if recorded
          if(day <= timings.wrangle$ICU.start[pat.no]){
            "Ward"
          } else if(!is.na(timings.wrangle$ICU.end[pat.no]) & day <= timings.wrangle$ICU.end[pat.no]){
            "ICU"
          } else {
            "unknown"
          }
        } else if(timings.wrangle$censored[pat.no]){
          if(day <= timings.wrangle$censored.date[pat.no]){
            if(day <= timings.wrangle$ICU.start[pat.no]) {
              "Ward"
            } else if(is.na(timings.wrangle$ICU.end[pat.no]) | day <= timings.wrangle$ICU.end[pat.no]) {
              "ICU"
            } else {
              "Ward"
            }
          } else {
            "Censored"
          }
        } else {
          if(day <= timings.wrangle$hospital.end[pat.no]){
            if(day <= timings.wrangle$ICU.start[pat.no]) {
              "Ward"
            } else if(is.na(timings.wrangle$ICU.end[pat.no]) | day <= timings.wrangle$ICU.end[pat.no]) {
              "ICU"
            } else {
              "Ward"
            }
          } else {
            as.character(timings.wrangle$final.status[pat.no])
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
  
  complete.timeline.2 <- complete.timeline %>%
    pivot_longer(all_of(1:n.days), names_to = "day", values_to = "status") %>%
    dplyr::select(subjid, day, status) %>%
    dplyr::mutate(day = map_dbl(day, function(x) as.numeric(str_split_fixed(x, "_", 2)[2]))) %>%
    dplyr::mutate(status = factor(status, levels = c("discharge", "transfer","unknown", "Censored", "Ward", "ICU", "death"))) %>%
    ungroup()
  
  ggplot(complete.timeline.2) + geom_bar(aes(x = day, fill = status), position = "fill") +
    scale_fill_brewer(palette = "Dark2", name  = "Status", drop = F, labels = c("Discharged", "Transferred","Unknown", "Ongoing care", "Ward", "ICU", "Death")) +
    theme_bw() +
    xlab("Days relative to admission") +
    annotate(geom = "segment", x = 14.5, xend = 14.5, y = 0, yend = 1) +
    ylab("Proportion")
}

#' @export
#' @keywords internal

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
    geom_bar(aes(y=..count../sum(..count..)), fill = "deepskyblue3") +
    theme_bw() +
    xlab("Antivirals used") +
    ylab("Proportion of patients") +
    scale_x_upset()
  
}


# Cumulative recruitment by outcome

# Cumulative recruitment by outcome #

#' Plot cumulative recruitment of patients.
#'
#' Plots the cumulative recruitment of patients, separated by whether follow-up is ongoing or an outcome has been recorded.
#' @export recruitment.dat.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#' @param embargo.limit The cut-off date for inclusion in the report. Patients recruited after \code{embargo.limit} are not considered in the analysis.
#' Set  \code{embargo.limit} to the date of the report if all patients are to be considered.
#'
#' this date have not been included.
#'
#' @return  Plot showing the cumulative number of patients in the study. One line plots the cumulative of patients for whom follow-up has been recorded
#' while the other line captures patients for whom follow-up is ongoing. The first dashed black line indicates the \code{embargo.limit}.
#'  The second black line is the cut-off date for the next report, assuming that reports are issued weekly.
recruitment.dat.plot <- function(data, embargo.limit, ...) {
  data <- data %>% filter(admission.date <= today())
  
  data$outcome.count <- 0
  data$outcome.count[data$outcome != "censored"] <- 1
  data$censored.count <- 0
  data$censored.count[data$outcome == "censored"] <- 1
  
  from <- min(data$admission.date, na.rm = TRUE)
  to <- max(data$admission.date, na.rm = TRUE)
  
  plt.d <- data.frame(d = from:to)
  plt.d$date <- as.Date(plt.d$d, origin = "1970-01-01")
  
  counts.tbl <- data %>%
    group_by(admission.date) %>%
    summarise(outcome = sum(outcome.count, na.rm = T), censored = sum(censored.count, na.rm = T))
  
  plt.d <- plt.d %>%
    left_join(counts.tbl, by=c("date" = "admission.date"))  %>%
    filter(!is.na(outcome) & !is.na(censored))
  
  # for (i in from:to) {
  #   plt.d$outcome[plt.d$date == i] <-
  #     sum(data$outcome.count[data$start.date == i], na.rm = TRUE)
  #   plt.d$censored[plt.d$date == i] <-
  #     sum(data$censored.count[data$start.date == i], na.rm = TRUE)
  # }
  plt.d$out.c <- cumsum(plt.d$outcome)
  plt.d$cen.c <- cumsum(plt.d$censored)
  
  xmin <- as.Date("2020-02-01")
  plt.d <- subset(plt.d, date >= xmin)
  
  p <- ggplot(data = plt.d, aes(x = date)) +
    geom_line(aes(y = out.c, colour = "Outcome recorded"), size = 1) +
    geom_line(aes(y = cen.c, colour = "Follow-up ongoing"), size = 1) +
    geom_vline(xintercept = embargo.limit, linetype = "dashed") +
    geom_vline(xintercept = embargo.limit + 7, linetype = "dashed") +
    theme_bw() +
    theme(legend.title=element_blank(), legend.position="top") +
    xlab("Admission date") +
    xlim(xmin, to) +
    ylab("Cumulative recruitment")
  
  return(p)
}



### ICU plots ----------------------------------------------------------------

#' @export
#' @keywords internal

get_icu_pts <- function(patient.data, ...) {
  data <- patient.data %>%
    filter(ICU.ever == 1)
  
  calc_dur <- function(duration, start, end, mult) {
    data$calc.duration <- difftime(end, start, unit = "days")
    # remove if <0 or if multiple periods
    data$calc.duration[data$calc.duration < 0 | mult == TRUE] <- NA
    duration[is.na(duration) == TRUE] <-
      as.numeric(data$calc.duration[is.na(duration) == TRUE])
    return(duration)
  }
  data$ICU.duration <- calc_dur(data$ICU.duration, data$ICU.start.date,
                                data$ICU.end.date, data$ICU.multiple.periods)
  data$IMV.duration <- calc_dur(data$IMV.duration, data$IMV.start.date,
                                data$IMV.end.date, data$IMV.multiple.periods)
  
  return(data)
}


##### Treatment upset plot for ICU  #####

#' Plot frequency of combinations of Intensive Care Unit (ICU) and High Dependency Unit (HDU) treatments.
#'
#' Plots the distribution of combinations of treatments administered during ICU/HDU stay
#' @export treatment.use.plot.icu
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  UpSet plot showing the frequency of combinations of ICU/HDU treatments.
#' Filled and empty circles below the x-axis of the plot indicate treatments that were and were not
#' administered respectively.
treatment.use.plot.icu <- function(data, treatments,...){
  d <- get_icu_pts(data)
  #Overriding colour from treatment.use.plot, therefore suppressMessages
  p <- suppressMessages(
    treatment.use.plot(d, treatments) +
      scale_fill_manual(
        values = c("darkorchid2", "darkorchid4"),
        name = "Treatment", labels = c("No", "Yes")
      )
  )
  return(p)
}

#' @export
#' @keywords internal

icu.treatment.upset.prep <- function(data, ...) {
  d <- get_icu_pts(data)
  details <- subset(
    d,
    select = c(subjid,
               antibiotic.any,
               antiviral.any,
               antifungal.any,
               steroid.any,
               NIMV.ever, IMV.ever, RRT.ever, Inotrope.ever, O2.ever
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
  for (i in 2:10) {
    details[, i][details[, i] != 1 | is.na(details[, i]) == TRUE] <- 0
  }
  # Anyone who is ventilated will be set to O2 therapy = yes
  details$O2.ever[details$NIMV.ever == 1 | details$IMV.ever == 1] <- 1
  details <- details %>%
    mutate(any.antimicrobial =
             pmax(antibiotic.any, antiviral.any, antifungal.any)) %>%
    dplyr::select(-allna)
  return(details)
}

#' @export
#' @keywords internal

icu.treatment.upset <- function(data, ...) {
  details <- data %>%
    get_icu_pts() %>%
    treatment.upset.prep()
  treatments2 <- details %>%
    dplyr::select(
      subjid,
      any.antimicrobial,
      steroid.any,
      O2.ever,
      IMV.ever,
      RRT.ever,
      Inotrope.ever
    ) %>%
    pivot_longer(2:7, names_to = "Treatment", values_to = "Present") %>%
    mutate(Present = as.logical(Present))
  # Change labels
  treatments2$Treatment[treatments2$Treatment == "O2.ever"] <-
    "Any oxygen provision"
  treatments2$Treatment[treatments2$Treatment == "any.antimicrobial"] <-
    "Any antimicrobials"
  treatments2$Treatment[treatments2$Treatment == "IMV.ever"] <-
    "Invasive ventilation"
  treatments2$Treatment[treatments2$Treatment == "RRT.ever"] <-
    "Renal replacement therapy"
  treatments2$Treatment[treatments2$Treatment == "steroid.any"] <-
    "Corticosteroid"
  treatments2$Treatment[treatments2$Treatment == "Inotrope.ever"] <-
    "Inotropes"
  treatments2 <- treatments2 %>%
    group_by(subjid) %>%
    dplyr::summarise(Treatments = list(Treatment), Presence = list(Present)) %>%
    mutate(treatments.used = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  p <- ggplot(treatments2, aes(x = treatments.used)) +
    geom_bar(aes(y=..count../sum(..count..)), fill = "darkorchid4") +
    theme_bw() +
    xlab("Treatments used") +
    ylab("Proportion of patients \n admitted to intensive care") +
    scale_x_upset()
  
  return(p)
}


# ICU violin plot #


#' Plot lengths of hospital stay for patients admitted into Intensive Care Unit (ICU)/High Dependency Unit (HDU).
#'
#' Plots the distribution of lengths of stay for patients who were admitted to ICU/HDU: the distribution of the total length of hospital stay for this group is plotted,
#' as well as the length of stay within ICU/HDU.
#' @export icu.violin.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return Violin plots (with box plots) showing the distribution of the total length of hospital stay for patients who were admitted to ICU/HDU and the
#' distribution of the lengths of stay within ICU/HDU. The coloured areas of the plot indicate the kernel probability density of the observed data
#' and the box plots show the median and interquartile range of the lengths of stay.
icu.violin.plot  <- function(data, ref.date, ...){
  data <- get_icu_pts(data)
  # Use available data for each measure
 # dur <- data$start.to.exit
  
  data <- data %>% filter(start.to.exit < as.numeric(as.Date(today()) - as.Date("2019-12-01"))) %>% 
                filter(ICU.duration < as.numeric(as.Date(today()) - as.Date("2019-12-01"))) 
  
  dur <- data$start.to.exit
  dur <- dur[which(dur>=0)]  # Exclude negative times
  d <- data.frame(dur = dur)
  d$type <- 1
  
  dur <- data$ICU.duration
  dur <- dur[which(dur>=0)]
  d.2 <- data.frame(dur)
  d.2$type <- 2
  
  d <- rbind(d, d.2, deparse.level = 1) %>%
    filter(!is.na(dur))
  d$type <- factor(d$type, levels = c(1, 2), labels = c("Total hospital stay", "ICU"))
  
  p <- ggplot(data = d, aes(x = type, y = dur, fill = type)) +
    geom_violin(trim = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = c("darkorchid2", "darkorchid4")) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    labs(title = " ", x = "Location", y = "Length of stay (days)") +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                      colour = "grey"),
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                      colour = "grey"),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
  
  return(p)
}






##############################################################################
# Function below calculates mean and variance estimates and confidence intervals (by bootstrap) for the following time-based distributions.
# Onset to admission
# Admission to outcome (death/ recovery)
# Admission to ICU; ICU duration
# Admission to NIMV; NIMV duration
# Admission to IMV; IMV duration


# For bootstrap
#' @export
#' @keywords internal

samp.mean <- function(x, i) {mean(x[i])}

#' @export
#' @keywords internal

samp.var <- function(x, i){var(x[i])}

#' @export
#' @keywords internal

samp.median <- function(x,i){median(x[i])}

#' @export
#' @keywords internal

fit.summary.gamma <- function(fit){

  if(is.character(fit)){
    
    return(list(m=NA, lower.m = NA, upper.m = NA,  v=NA,
                lower.v = NA, upper.v = NA, bmed = NA, lower.med = NA,
                upper.med = NA))
    
  }else{
    
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
    lower.v <- boot.ci(bv, type = 'bca')$bca[4]       # lower bound of confidence interval for variance
    upper.v <- boot.ci(bv, type = 'bca')$bca[5]       # upper bound of confidence interval for variance
    # Bootstrap (median)
    bmed <- boot(data = X, statistic = samp.median, R=1000 )
    # CI
    lower.med <- boot.ci(bmed, type = 'bca')$bca[4]       # lower bound of confidence interval for variance
    upper.med <- boot.ci(bmed, type = 'bca')$bca[5]       # upper bound of confidence interval for variance
    
    
    return(list(m=m, lower.m = lower.m, upper.m = upper.m,  v=v,
                lower.v = lower.v, upper.v = upper.v, bmed = bmed, lower.med = lower.med,
                upper.med = upper.med))
    
  }
  
  
}


# @todo set filter for lengths of stay to automatically remove potentially wrong entries > 150 days

#' @export
#' @import dplyr
#' @keywords internal

casefat2 <-  function(data, embargo.limit, conf=0.95){
  
  # Function for the estimation of the case fatality ratio based on the nonparametric KM-like method by
  # Ghani et ql. 2005:  https://doi.org/10.1093/aje/kwi230
  
  #############################################################
  
  # Modify data
  
  # Exclude rows which no entries for length of stay
  
  data2 <- data %>% dplyr::filter(!is.na(start.to.exit) | !is.na(admission.to.censored))
  data2 <- data2 %>%
    mutate(length.of.stay =  map2_dbl(abs(start.to.exit), abs(admission.to.censored), function(x,y){
      max(x, y, na.rm = T)
    })) %>% filter(!is.na(outcome))  %>%
    filter(length.of.stay < as.numeric(as.Date(embargo.limit) - as.Date("2019-12-01")))
  
  
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
  
  f.end <- fct_recode(f, endpoint = 'death', endpoint = 'discharge')
  
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



# Calcualate cfr for ICU and non-ICU cases

#' @export
#' @import dplyr
#' @keywords internal

icu.cfr.func <- function(data, embargo.limit){
  
  icu.cases <- data %>% dplyr::filter(ICU.ever == 'TRUE')
  
  non.icu.cases <- data %>% dplyr::filter(is.na(ICU.ever) | ICU.ever == 'FALSE')
  
  cfr.icu.list <- casefat2(icu.cases, embargo.limit)
  cfr.icu <- cfr.icu.list$cfr
  cfr.icu.l <- cfr.icu.list$lcfr
  cfr.icu.u <- cfr.icu.list$ucfr
  
  cfr.non.icu.list <- casefat2(non.icu.cases, embargo.limit)
  cfr.non.icu <- cfr.non.icu.list$cfr
  cfr.non.icu.l <- cfr.non.icu.list$lcfr
  cfr.non.icu.u <- cfr.non.icu.list$ucfr
  
  return(list(cfr.icu = cfr.icu, cfr.icu.l = cfr.icu.l, cfr.icu.u = cfr.icu.u, cfr.non.icu = cfr.non.icu, cfr.non.icu.l = cfr.non.icu.l,
              cfr.non.icu.u = cfr.non.icu.u ))
  
}


########### Distribution plots ############



#### Function to round 0 days to 0.5 (half a day) #######

#' @export round.zeros
#' @keywords internal

round.zeros <- function(x) ifelse (!is.na(x) & x==0, 0.5, x)

## Violin plot by sex (length of hospital stay by sex) ####

#' Plot lengths of hospital stay by sex
#'
#' Plots the distribution of lengths of stay for males and females on the same graph. Only cases with reported outcomes (i.e. death/discharge) are considered.
#' @export violin.sex.func
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return Violin plots (with box plots) showing the distribution of the total length of hospital stay by sex. The coloured areas of the plot indicate the
#'  kernel probability density of the observed data and the box plots show the median and interquartile range of the lengths of stay for each sex.
violin.sex.func <- function(data, embargo.limit, ...){
  
  
  if(all(is.na(data$sex))){
    plt <- insufficient.data.plot()
  }else{
    
    # Analysis to be run on only cases with admission.to.exit entries & sex entries (i.e. cases with completed outcomes)
    
    
    data2 <- data %>% filter(!is.na(start.to.exit)) %>% filter(!is.na(sex) & sex!=3) %>% filter(start.to.exit > 0) # Exclude negative values for length of stay - indication of issue with data entry
    
    data2 <- data2%>%
      mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))  %>%
      mutate(sex = map_chr(sex, function(x)  c('Male', 'Female')[x])) %>%
      mutate(sex = factor(sex, levels = c("Male", "Female")))  %>%
      filter(length.of.stay < as.numeric(as.Date(embargo.limit) - as.Date("2019-12-01")))
    
    
    vd <- tibble(Sex = data2$sex, length.of.stay = data2$length.of.stay )
    
    # by sex
    
    plt <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) +
      geom_violin(trim=F)+
      geom_boxplot(width=0.1, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
      scale_fill_viridis(drop = F, discrete = "true", option = "magma", begin = 0.25, end = 0.75) +
      labs(title=" ", x="Sex", y = "Length of hospital stay") +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  ylim(c(0,max(vd$length.of.stay))) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
    
    
  }
  
  return(plt)
}


### Violin sex plots by outcomes ###

#' @export
#' @keywords internal

violin.sex.func.discharge <- function(data, ...){
  
  # Analysis to be run on only cases with admission.to.exit entries & sex entries (i.e. cases with completed outcomes)
  
  
  data2 <- data %>% filter(!is.na(start.to.exit)) %>% filter(!is.na(sex) & sex!=3)%>% filter(outcome == 'discharge')
  
  
  data2 <- data2%>%
    mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))  %>%
    mutate(sex = map_chr(sex, function(x)  c('Male', 'Female')[x])) %>%
    mutate(sex = factor(sex, levels = c("Male", "Female")))
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  
  vd <- tibble(Sex = data2$sex, length.of.stay = data2$length.of.stay )
  
  # by sex
  
  plt <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) +
    geom_violin(trim=TRUE)+
    geom_boxplot(width=0.1, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    scale_fill_viridis(drop = F, discrete = "true", option = "magma", begin = 0.25, end = 0.75) +
    labs(title=" ", x="Sex", y = "Length of hospital stay") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +  #+ ylim(0, max(length(vd$length.of.stay)))
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(plt)
}

#' @export
#' @keywords internal
violin.sex.func.death <- function(data, ...){
  
  # Analysis to be run on only cases with admission.to.exit entries & sex entries (i.e. cases with completed outcomes)
  
  
  data2 <- data %>% filter(!is.na(start.to.exit)) %>% filter(!is.na(sex) & sex!=3)%>% filter(outcome == 'death')
  
  
  data2 <- data2%>%
    mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))  %>%
    mutate(sex = map_chr(sex, function(x)  c('Male', 'Female')[x])) %>%
    mutate(sex = factor(sex, levels = c("Male", "Female")))
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  
  vd <- tibble(Sex = data2$sex, length.of.stay = data2$length.of.stay )
  
  # by sex
  
  plt <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) +
    geom_violin(trim=TRUE)+
    geom_boxplot(width=0.1, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    scale_fill_viridis(drop = F, discrete = "true", option = "magma", begin = 0.25, end = 0.75) +
    labs(title=" ", x="Sex", y = "Length of hospital stay") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +  #+ ylim(0, max(length(vd$length.of.stay)))
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(plt)
}

#' @export
#' @keywords internal
violin.sex.func.hospital <- function(data, ...){
  
  # Analysis to be run on only cases with admission.to.exit entries & sex entries (i.e. cases with completed outcomes)
  
  
  data2 <- data %>% filter(!is.na(start.to.exit) & start.to.exit > 0) %>% filter(!is.na(sex) & sex!=3)%>% filter(outcome == 'censored')
  
  
  data2 <- data2%>%
    mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))  %>%
    mutate(sex = map_chr(sex, function(x)  c('Male', 'Female')[x])) %>%
    mutate(sex = factor(sex, levels = c("Male", "Female")))
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  
  vd <- tibble(Sex = data2$sex, length.of.stay = data2$length.of.stay )
  
  # by sex
  
  plt <- ggplot(vd, aes(x = Sex, y = length.of.stay, fill=Sex)) +
    geom_violin(trim=TRUE)+
    geom_boxplot(width=0.1, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    scale_fill_viridis(drop = F, discrete = "true", option = "magma", begin = 0.25, end = 0.75) +
    labs(title=" ", x="Sex", y = "Length of hospital stay") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +  ylim(c(0,100)) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(plt)
}


### Violin age ####

#' Plot lengths of hospital stay by age group
#'
#' Plots the distribution of lengths of stay by age group. Only cases with reported outcomes (i.e. death/discharge) are considered.
#' @export violin.age.func
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return Violin plots (with box plots) showing the distribution of the total length by age group. Age is plotted in  10-year intervals: 0-9, 10-19, \dots, 70+.
#' The coloured areas of the plot indicate the kernel probability density of the observed data and the box plots show the
#' median and interquartile range of the lengths of hospital stay for each age group.
violin.age.func <- function(data, embargo.limit,...){
  
  
  if(all(is.na(data$consolidated.age))){
    plt <- insufficient.data.plot()
  } else {
    
    # Analysis to be run on only entries with start.to.exit entries
    
    data2 <- data %>% filter(!is.na(start.to.exit)) %>% filter(!is.na(agegp10)) %>% filter(start.to.exit > 0) # Exclude negative values for length of stay - indication of issue with data entry
    
    data2 <- data2 %>%
      mutate(length.of.stay = map_dbl(start.to.exit, round.zeros)) %>%
      filter(length.of.stay < as.numeric(as.Date(embargo.limit) - as.Date("2019-12-01")))
    
    
    vdx<- tibble(subjid = data2$subjid, Age = data2$agegp10, length_of_stay = data2$length.of.stay )
    
    
    plt <- ggplot(vdx, aes(x = Age, y = length_of_stay, fill=Age)) +
      geom_violin(trim=F) +
      geom_boxplot(width=0.05, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
      labs(title="  ", x="Age group", y = "Length of hospital stay") +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) + #ylim(0, length(0, max(vdx$length_of_stay))+5) +
      scale_fill_viridis(option = "magma", discrete = T, drop = F, begin = 0.4, end = 1) +
      scale_x_discrete(drop = F) +
      ylim(c(0,max(vdx$length_of_stay))) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
    
    
  }
  
  
  return(plt)
  
  
}



### Violin age plots by outcomes ###
#' @export
#' @keywords internal

violin.age.func.discharge <- function(data, ...){
  
  # Analysis to be run on only entries with start.to.exit entries
  
  data2 <- data %>% filter(!is.na(start.to.exit)) %>% filter(!is.na(agegp10)) %>% filter(outcome == 'discharge')
  
  data2 <- data2 %>%
    mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  
  
  vdx<- tibble(subjid = data2$subjid, Age = data2$agegp10, length_of_stay = data2$length.of.stay )
  
  # remove NAs (@todo for now?)
  
  # vdx <- vdx %>% filter(!is.na(Age))
  
  plt <- ggplot(vdx, aes(x = Age, y = length_of_stay, fill=Age)) +
    geom_violin(trim=F) +
    geom_boxplot(width=0.05, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    labs(title="  ", x="Age group", y = "Length of hospital stay") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + #ylim(0, length(0, max(vdx$length_of_stay))+5) +
    scale_fill_viridis(option = "magma", discrete = T, drop = F, begin = 0.4, end = 1) +
    scale_x_discrete(drop = F) +
    ylim(c(0,40)) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(plt)
  
}

#' @export
#' @keywords internal

violin.age.func.death <- function(data, ...){
  
  # Analysis to be run on only entries with start.to.exit entries
  
  data2 <- data %>% filter(!is.na(start.to.exit) & start.to.exit > 0) %>% filter(!is.na(agegp10)) %>% filter(outcome == "death")
  
  data2 <- data2 %>%
    mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  
  vdx<- tibble(subjid = data2$subjid, Age = data2$agegp10, length_of_stay = data2$length.of.stay )
  
  plt <- ggplot(vdx, aes(x = Age, y = length_of_stay, fill=Age)) +
    geom_violin(trim=FALSE) +
    geom_boxplot(width=0.05, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    labs(title="  ", x="Age group", y = "Length of hospital stay") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + #ylim(0, length(0, max(vdx$length_of_stay))+5) +
    scale_fill_viridis(option = "magma", discrete = T, drop = F, begin = 0.4, end = 1) +
    scale_x_discrete(drop = F) +
    ylim(c(0,40)) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(plt)
  
}

#' @export
#' @keywords internal

violin.age.func.hospital <- function(data, ...){
  
  # Analysis to be run on only entries with start.to.exit entries
  
  data2 <- data %>% filter(!is.na(start.to.exit & start.to.exit > 0)) %>% filter(!is.na(agegp10)) %>% filter(outcome == "censored")
  
  data2 <- data2 %>%
    mutate(length.of.stay = map_dbl(start.to.exit, round.zeros))
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  
  vdx<- tibble(subjid = data2$subjid, Age = data2$agegp10, length_of_stay = data2$length.of.stay )
  
  plt <- ggplot(vdx, aes(x = Age, y = length_of_stay, fill=Age)) +
    geom_violin(trim=F) +
    geom_boxplot(width=0.05, fill="white", outlier.shape = 21, outlier.fill = "white", outlier.size = 1.5)  +
    labs(title="  ", x="Age group", y = "Length of hospital stay") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + #ylim(0, length(0, max(vdx$length_of_stay))+5) +
    scale_fill_viridis(option = "magma", discrete = T, drop = F, begin = 0.4, end = 1) +
    scale_x_discrete(drop = F) +
    ylim(c(0,40)) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  return(plt)
  
}





########### Admission to outcome (accounting for censorship) #########

#' @export
#' @keywords internal
adm.outcome <- function(data, embargo.limit, plt = F){
  
  data2 <- data %>% filter(!is.na(start.to.exit) | !is.na(start.to.censored))
  
  data2 <- data2 %>%
    mutate(length.of.stay = map2_dbl(start.to.exit, start.to.censored, function(x,y){
      max(x, y, na.rm = T)
    })) %>%
    filter(length.of.stay < as.numeric(as.Date(embargo.limit) - as.Date("2019-12-01")))
  
  
  # Exclude negative values for length of stay - indication of issue with data entry
  data2 <- data2[-c(which(data2$length.of.stay < 0)), ]
  
  admit.discharge <- data2$length.of.stay
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge <- map_dbl(admit.discharge, round.zeros)
  
  pos.cens <- which(data2$censored == 'TRUE')
  
  left <- c(admit.discharge)
  right <- replace(admit.discharge, pos.cens, values=NA )
  censored_df <- data.frame(left, right)
  obs <- right[!(is.na(right))] # cases with completed duration days.
  
  if(sum(is.na(right)) > 0.8*length(right)){
    fit <- "Cannot be computed, insufficient data"
  }else{
    
    fit <- fitdistcens(censored_df, dist = 'gamma')
    
  }
  
  if(!is.character(fit) & plt == T){
    
    t <- data.frame(x = admit.discharge)
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) from admission to death or recovery', title = '') + xlim(c(0,max(admit.discharge) - 20))
    
    return(list(plt=plt, fit=fit, obs = obs))
    
  }else{
    
    plt <- insufficient.data.plot()
    return(list(plt = plt, fit=fit, obs = obs))
  }
  
}





##  Admission to outcome plot (accounting for censorship) ####

#' @title Plot distribution of time (in days) from admission to an outcome.
#
#' @description Plots a Gamma distribution fit to the lengths of hospital stay (in days) from admission to an outcome - either death or discharge, accounting for unobserved outcomes. See `Details'.
#'
#' @details
#' The estimates of the Gamma distribution were fitted to the observed data were obtained by a maximum likelihood estimation
#' procedure implemented in the \code{\link[fitdistrplus]{fitdistcens}} function in the \code{fitdistrplus} package. The lengths of stay for patients with unobserved outcomes were treated as interval censored data.
#' @export adm.outcome.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return Plot of the Gamma distribution fit to lengths of hospital stay. The black dashed line indicates the position of the estimated mean of the Gamma distribution.
#' (Note that the expected mean is different from the *observed mean* of lengths of hospital stay, which is estimated using records from patients with observed outcomes only.)
#' @references
#' Delignette-Muller, M. L., & Dutang, C. (2015).
#' fitdistrplus: An R package for fitting distributions. *Journal of statistical software*, **64**(4), 1-34.

adm.outcome.plot <- function(data, embargo.limit, ...){
  adm.outcome(data, embargo.limit, plt=T)$plt
}

########## Onset to admission #####

#' @export
#' @keywords internal
onset.adm <- function(data, plt = F){
  
  admit.discharge <- data$onset.to.admission
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge.2 <- map_dbl(admit.discharge, round.zeros)
  # admit.discharge.2 <- admit.discharge.2[-which(admit.discharge.2>160)]
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  obs <-  admit.discharge.2  # record observed values for reporting
  
  # Plot
  
  
  if (plt==T){
    t <- data.frame(x=admit.discharge)
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) + xlim(0, 30) + geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) from symptom onset to admission', title = ' ')
    
    return(list(plt=plt, fit=fit, obs = obs))
  }else{
    return(list(fit=fit, obs = obs))
  }
  
}


#' @title
#' Plot distribution of time (in days) from symptom onset to admission.
#
#' @description
#' Plots a Gamma distribution fit to durations (in days) from symptom onset to admission. This includes only patients with
#' complete records on the time (in days) between symptom onset and admission.
#'
#' @details
#' The estimates of the Gamma distribution were fitted to the observed data were obtained by a maximum likelihood estimation
#' procedure implemented in the \code{\link[fitdistrplus]{fitdistcens}} package in the \code{fitdistrplus} package.
#' @export onset.adm.plot
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return Plot of the Gamma distribution fit to lengths of hospital stay. The black dashed line indicates the position of the estimated mean of the Gamma distribution.
#' @references
#' Delignette-Muller, M. L., & Dutang, C. (2015).
#' fitdistrplus: An R package for fitting distributions. *Journal of statistical software*, **64**(4), 1-34.
onset.adm.plot <- function(data,...){
  onset.adm(data, plt=T)$plt
}


# Function to calculate NIMV durations for all cases with reported NIMV.start dates.
# Durations are calculated for cases that are still in NIMV by  today() (i.e. date of the data).
#' @export
#' @keywords internal
calculate.durations <- function(data){
  durs <- c()
  cens <- c()
  
  for(i in 1:nrow(data)){
    if(!is.na(data$event.end.date[i])){
      durs[i] <- data$event.end.date[i] - data$event.start.date[i]
      cens[i] <- 0                # if end.date is available, then consider as not censored
    }else{
      durs[i] <- today() - data$event.start.date[i]  # if end.date is unreported, use reference date (ie. data pull date) as reference
      cens[i] <- 1                # if end.date is not reported, then consider as censored
    }
  }
  return(list(durs = durs, cens = cens))
}



######## Admission to NIV ############
#' @export
#' @keywords internal
adm.to.niv <- function(data,plt = F,...){
  
  
  data2 <- data %>% filter(!is.na(admission.to.NIMV))
  
  admit.discharge <- data2$admission.to.NIMV
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge.2 <- map_dbl(admit.discharge, round.zeros)
  
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  obs <-  admit.discharge.2  # record observed values for reporting
  
  # Plot
  
  if(plt == T){
    
    t <- data.frame(x = admit.discharge)
    
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) from admission to NIV', title = '')
    
    return(list(plt=plt, fit=fit, obs = obs))
    
  }else{
    return(list(fit=fit, obs = obs))
  }
  
}



#adm.to.niv(patient.data)

#' @export
#' @keywords internal
adm.to.niv.plot <- function(data,...){
  adm.to.niv(data, plt = T)$plt
}


####### Duration of NIV ###########

#' @export
#' @keywords internal
dur.niv <- function(data,plt = F, ...){
  data2 <- data %>% filter(!is.na(NIMV.start.date)) %>% mutate(event.start.date = NIMV.start.date) %>% mutate(event.end.date = NIMV.end.date)
  
  temp <- calculate.durations(data2)
  
  data2 <- data2  %>% mutate(event.duration = abs(temp$durs)) %>%
    mutate(event.duration = map_dbl(event.duration, function(x) round.zeros(x))) %>%
    mutate(event.censoring = temp$cens)
  
  
  left <- data2$event.duration   # all duration dates
  pos.cens <- which(data2$event.censoring == 1) # select positions for censored cases
  right <-  replace(left, pos.cens, values=NA )
  censored_df <- data.frame(left, right)
  pos.n.cens <- which(data2$event.censoring == 0)
  obs <- left[pos.n.cens]
  
  
  if(sum(is.na(right)) > !sum(is.na(right))){
    fit <- "Cannot be computed, insufficient data"
  }else{
    
    fit <- fitdistcens(censored_df, dist = 'gamma')
    
  }
  
  # Plt
  
  if(!is.character(fit) & plt == T){
    
    t <- data.frame(x = left)
    
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Duration of NIV (in days)', title = '')
    
    return(list(plt=plt, fit=fit, obs = obs))
    
  }else{
    
    plt <- insufficient.data.plot()
    return(list(plt = plt, fit=fit, obs = obs))
    
  }
}

#' @export
#' @keywords internal
dur.niv.plot <-  function(data,  ...){
  dur.niv(data, plt=T)$plt
}



######## Admission to ICU #######

#' @export
#' @keywords internal
adm.to.icu <- function(data, plt = F,...){
  
  data2 <- data %>% filter(!is.na(admission.to.ICU))
  
  admit.discharge <- data2$admission.to.ICU
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge.2 <- map_dbl(admit.discharge, round.zeros)
  
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  obs <-  admit.discharge.2  # record observed values for reporting
  
  # Plot
  
  if(plt==T){
    
    t <- data.frame(x = admit.discharge)
    
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) from admission to ICU', title = '')
    
    return(list(plt=plt, fit=fit, obs = obs))
  }else{
    return(list(fit=fit, obs = obs))
  }
  
}

#' @export
#' @keywords internal
adm.to.icu.plot <- function(data,...){
  adm.to.icu(data, plt=T)$plt
}


####### Duration of ICU #########
#' @export
#' @keywords internal
dur.icu <- function(data, plt = F, ...) {
  
  data2 <- data %>% filter(!is.na(ICU.admission.date)) %>% mutate(event.start.date = ICU.admission.date) %>% mutate(event.end.date = ICU.discharge.date)
  
  temp <- calculate.durations(data2)
  
  data2 <- data2  %>% mutate(event.duration = abs(temp$durs)) %>%
    mutate(event.duration = map_dbl(event.duration, function(x) round.zeros(x))) %>%
    mutate(event.censoring = temp$cens)

  mutate(event.duration = map_dbl(durs, function(x) calculate.durations(x)$durs)) %>%
    mutate(event.duration = map_dbl(event.duration, function(x) abs(round.zeros(x)))) %>%
    mutate(event.censoring = map_dbl(cens, calculate.durations(x)$cens))
  
  left <- data2$event.duration   # all duration dates
  pos.cens <- which(data2$event.censoring == 1) # select positions for censored cases
  right <-  replace(left, pos.cens, values=NA )
  censored_df <- data.frame(left, right)
  
  pos.n.cens <- which(data2$event.censoring == 0)
  obs <- left[pos.n.cens]
  
  if(sum(is.na(right)) > !sum(is.na(right))){
    
    fit <- "Cannot be computed, insufficient data"
  }else{
    
    fit <- fitdistcens(censored_df, dist = 'gamma')
  }
  
  
  if(!is.character(fit) & plt==T){
    
    
    t <- data.frame(x = left)
    
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) spent in ICU', title = '')
    
    return(list(plt=plt, fit=fit, obs = obs))
    
  }else{
    
    plt <- insufficient.data.plot()
    return(list(plt = plt, fit=fit, obs = obs))
    
  }
}


#' @export
#' @keywords internal
dur.icu.plot <- function(data,...){
  dur.icu(data, plt=T)$plt
}




############## Admission to IMV #######################

#' @export
#' @keywords internal
adm.to.imv <- function(data, plt = F, ...){
  
  data2 <- data %>% filter(!is.na(admission.to.IMV))
  
  admit.discharge <- data2$admission.to.IMV
  admit.discharge <- abs(admit.discharge[!(is.na(admit.discharge))])
  admit.discharge.2 <- map_dbl(admit.discharge, round.zeros)
  
  fit <- fitdist(admit.discharge.2, dist = 'gamma', method = 'mle')
  
  obs <-  admit.discharge.2  # record observed values for reporting
  
  # Plot
  
  if(plt == T){
    
    t <- data.frame(x = admit.discharge)
    
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) from admission to IMV', title = '')
    
    return(list(plt=plt, fit=fit, obs = obs))
    
  }else{
    return(list(fit=fit, obs = obs))
  }
}

#' @export
#' @keywords internal
adm.to.imv.plot <- function(data,...){
  adm.to.imv(data, plt=T)$plt
}


################ Duration of IMV ####################

#' @export
#' @keywords internal
dur.imv <- function(data, plt=F, ...) {
  
  data2 <- data %>% filter(!is.na(IMV.start.date)) %>% mutate(event.start.date = IMV.start.date) %>% mutate(event.end.date = IMV.end.date)
  
  temp <- calculate.durations(data2)
  
  data2 <- data2  %>% mutate(event.duration = abs(temp$durs)) %>%
    mutate(event.duration = map_dbl(event.duration, function(x) round.zeros(x))) %>%
    mutate(event.censoring = temp$cens)
  
  
  left <- data2$event.duration   # all duration dates
  pos.cens <- which(data2$event.censoring == 1) # select positions for censored cases
  right <-  replace(left, pos.cens, values=NA )
  censored_df <- data.frame(left, right)
  pos.n.cens <- which(data2$event.censoring == 0)
  obs <- left[pos.n.cens]
  
  
  if(sum(is.na(right)) > !sum(is.na(right))){
    fit <- "Cannot be computed, insufficient data"
  }else{
    
    fit <- fitdistcens(censored_df, dist = 'gamma')
    
    
  }
  
  
  
  if(!is.character(fit) & plt ==T){
    
    t <- data.frame(x = left)
    
    plt <- ggplot(data = t) +
      #geom_histogram(data = as.data.frame(admit.discharge), aes(x=admit.discharge, y=..density..), binwidth = 1,  color = 'white', fill = 'blue', alpha = 0.8)+
      geom_line(aes(x=x, y=dgamma(x,fit$estimate[["shape"]], fit$estimate[["rate"]])), color="blue", size = 1.1) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +  geom_vline(xintercept = fit.summary.gamma(fit)$m, linetype = 'dashed') +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Time (in days) spent receiving IMV', title = '')
    
    return(list(plt=plt, fit=fit, obs = obs))
  }else{
    
    plt <- insufficient.data.plot()
    return(list(plt = plt, fit=fit, obs = obs))
  }
  
}


#' @export
#' @keywords internal
dur.imv.plot <- function(data,...){
  dur.imv(data, plt=T)$plt
}


### Functions for admission observations / results etc.

#' @export plot.by.age.make.zeroandone
#' @keywords internal
plot.by.age.make.zeroandone <- function(var, ...) {
  var[var == 2] <- 0
  var[var == 3] <- NA
  
  return(var)
  
}

#' @export plot.prop.by.age
#' @keywords internal
plot.prop.by.age <- function(data, var, name, ymax = 1, sz = 750, ...) {
  data2 <- data
  summ <- data2 %>%
    add_column(a = var) %>%
    filter(!is.na(a)) %>%
    group_by(AgeGrp) %>%
    dplyr::summarise(
      All = sum(All, na.rm = TRUE),
      v = sum(a, na.rm = TRUE)
    )
  d <- binom.confint(summ$v, summ$All, conf.level = .95, method = "exact")
  d$X <- summ$AgeGrp
  d$lbl <- paste(d$x, d$n, sep = "/\n", collapse = NULL)
  censored.lbl <- paste("-", d$n, sep = "/\n", collapse = NULL)
  d$lbl[d$x <= 5] <- censored.lbl[d$x <= 5]
  d$size <- d$n / sz
  xlabs <- c(
    "<10",
    "10-",
    "20-",
    "30-",
    "40-",
    "50-",
    "60-",
    "70-",
    expression(phantom(x) >= 80)
  )
  N <- paste("N = ", sum(summ$All), sep = "", collapse = NULL)
  pts <- geom_point(
    data = d,
    aes(x = d$X, y = mean),
    shape = "square",
    size = d$size,
    colour = "navy"
  )
  lines <- geom_linerange(
    data = d,
    aes(x = X, ymin = lower, ymax = upper),
    colour = "#000000",
    show.legend = FALSE
  )
  xa <- scale_x_discrete(
    name = "Age group (years)",
    labels = xlabs
  )
  ya <- scale_y_continuous(
    name = name,
    limits = c(0, ymax)
  )
  #  lbls <- geom_text(
  #    data = d,
  #    aes(x = X, y = ymax, label = lbl),
  #    size = 2
  #  )
  p <- ggplot() +
    pts +
    lines +
    #    lbls +
    xa + ya +
    theme_bw() + theme(axis.text = element_text(size = 6)) +
    labs(title = N)
  
  return(p)
  
}

#' @export plot.by.age.grouping
#' @keywords internal
plot.by.age.grouping <- function(data, ...) {
  data$Age <- data$consolidated.age
  data <- data %>%
    dplyr::select(-consolidated.age) %>%
    filter(!is.na(Age))
  data$AgeGrp <- 0
  thr <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 999)
  for (i in thr) data$AgeGrp[data$Age >= i] <- i
  data$AgeGrp[is.na(data$Age) == TRUE] <- 999
  data$AgeGrp <- factor(
    data$AgeGrp,
    levels = thr,
    labels = c("<10", "10-", "20-", "30-", "40-","50-","60-", "70-", ">=80", "NR")
  )
  
  return(data)
  
}





##  Plot comorbidities by age  ####

#' @title Plot the prevalence of comorbidities by age group.
#
#' @description Plots the prevalence of seven comorbidities (asthma, malignancy, HIV, obesity, diabetes mellitus, dementia, and smoking), stratified  by age group.
#'
#' @export comorb.by.age
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return A plot for each comorbidity, showing the 95% binomial confidence interval (represented by a black line) 
#' for the proportion of patients in each age group with that comorbidity. The width of the boxes gives an indication--in relative terms-- 
#' of the number of people in each age group for whom information on that comorbidity is available. On top of each plot, N (the number of individuals 
#' whose records are included in the plot) is printed (this varies between plots due to data completeness).
comorb.by.age <- function(data, ...) {
  
  if(all(is.na(data$consolidated.age)) | all(is.na(data$sex))){
    p <- insufficient.data.plot()
  }else{
    
    df <- data %>%
      dplyr::select(subjid, consolidated.age, 
                    asthma_mhyn, malignantneo_mhyn, aidshiv_mhyn, obesity_mhyn,
                    diabetes, dementia_mhyn, smoking_mhyn,
                    start.to.exit, sex
      )
    # chroniccard_mhyn is all NA in patient.data
    # chrincard does not exist in most datasets.
    # hypertension_mhyn does not exist in some datasets.
    # Therefore chronic cardiac disease currently omitted.
    df <- plot.by.age.grouping(df)
    for (i in 2: ncol(df) - 5) df[, i] <- plot.by.age.make.zeroandone(df[, i])
    # Coding for smoking differs between datasets. For some, 3 = unknown, for
    # others 3 = former.
    df$CurrentSmoke <- 0
    df$CurrentSmoke[df$smoking_mhyn == 1] <- 1
    df$CurrentSmoke[is.na(df$smoking_mhyn) == TRUE] <- NA
    df$All <- 1
    size <- nrow(df) / 20
    ylimit <- .4
    # ylimit may need to go up when adding cardiac comorbidities and hypertension
    # back in
    
    pa <- plot.prop.by.age(df, df$asthma_mhyn,
                           "Proportion with\nasthma", ymax = ylimit, sz = size)
    pb <- plot.prop.by.age(df, df$malignantneo_mhyn,
                           "Proportion with\nmalignancy", ymax = ylimit, sz = size)
    pc <- plot.prop.by.age(df, df$obesity_mhyn,
                           "Proportion with\nobesity", ymax = ylimit, sz = size)
    pd <- plot.prop.by.age(df, df$diabetes,
                           "Proportion with\ndiabetes mellitus", ymax = ylimit, sz = size)
    pe <- plot.prop.by.age(df, df$dementia_mhyn,
                           "Proportion with\ndementia", ymax = ylimit, sz = size)
  # Chronic cardiac disease omitted as described above.
    # pf <- plot.prop.by.age(df, df$chrincard,
   #                        "Proportion with\nchronic cardiac disease", ymax = ylimit, sz = size)
    # Most have missing for hypertension - leave out until resolved
  #  pg <- plot.prop.by.age(df, df$hypertension_mhyn,
  #                         "Proportion with\nhypertension", ymax = ylimit, sz = size)
    ph <- plot.prop.by.age(df, df$CurrentSmoke,
                           "Proportion who\ncurrently smoke", ymax = ylimit, sz = size)
    
  #  p <- arrangeGrob(pa, pb, pc, pd, pe, pf, pg, ph, ncol = 2)
    p <- arrangeGrob(pa, pb, pc, pd, pe, ph, ncol = 2)
    
  }
  
  return(p)
  
}


##  Plot symptoms by age  ####

#' @title Plot the prevalence of symptoms by age group.
#
#' @description Plots the prevalence of five comorbidities (fever, cough, shortness of breath, confusion, and gastrointestinal symptoms), stratified  by age group.
#'
#' @export sx.by.age
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return A plot for each symptom, showing the 95% binomial confidence interval (represented by a black line) 
#' for the proportion of patients in each age group who presented that symptom. The width of the boxes gives an indication--in relative terms-- 
#' of the number of people in each age group for whom information on that symptom is available. On top of each plot, N (the number of individuals 
#' whose records are included in the plot) is printed (this varies between plots due to data completeness).
sx.by.age <- function(data, admission.symptoms, ...) {
  
  if( all(is.na(data$consolidated.age)) | all(is.na(data$sex))){
    p <- insufficient.data.plot()
  }else{
    
    
    df <- data %>%
      dplyr::select(subjid, consolidated.age,
                    one_of(admission.symptoms$field), start.to.exit, sex
      )
    df <- plot.by.age.grouping(df)
    for (i in 2: ncol(df) - 4) df[, i] <- plot.by.age.make.zeroandone(df[, i])
    df$All <- 1
    if(length(intersect(colnames(df), c("cough.bloodysputum", "cough.nosputum", 
                                        "cough.sputum") )) != 0){
      df$Cough <- pmax(df$cough.bloodysputum, df$cough.nosputum, df$cough.sputum,
                       na.rm = TRUE)
    } else {
      df$Cough <- NA 
    }
    if(length(intersect(colnames(df), c("wheeze_ceoccur_v2", "df$shortness.breath") )) != 0){
      df$Low.Resp <- pmax(df$wheeze_ceoccur_v2, df$shortness.breath,
                          na.rm = TRUE)
    } else {
      df$Low.Resp <- NA 
    }
    if(length(intersect(colnames(df), c("sorethroat_ceoccur_v2", "runnynose_ceoccur_v2", 
                                        "earpain_ceoccur_v2") )) != 0){
      df$Upper.Resp <- pmax(df$sorethroat_ceoccur_v2, df$runnynose_ceoccur_v2, 
                            df$earpain_ceoccur_v2,
                            na.rm = TRUE)
    } else {
      df$Upper.Resp <- NA 
    }
    if(length(intersect(colnames(df), c("abdopain_ceoccur_v2", "vomit_ceoccur_v2", 
                                        "diarrhoea_ceoccur_v2") )) != 0){
      df$GI <- pmax(df$abdopain_ceoccur_v2, df$vomit_ceoccur_v2,
                    df$diarrhoea_ceoccur_v2, na.rm = TRUE)
    } else {
      df$GI <- NA
    }
    if(length(intersect(colnames(df), c("confusion_ceoccur_v2", "seizures_cecoccur_v2") )) != 0){
      df$Neuro <- pmax(df$confusion_ceoccur_v2, df$seizures_cecoccur_v2,
                       na.rm = TRUE)
    } else {
      df$Neuro <- NA 
    }    
    if(length(intersect(colnames(df), c("myalgia_ceoccur_v2", "jointpain_ceoccur_v2", 
                                        "fatigue_ceoccur_v2", "headache_ceoccur_v2") )) != 0){
      df$Const <- pmax(df$myalgia_ceoccur_v2, df$jointpain_ceoccur_v2, df$fatigue_ceoccur_v2,
                       df$headache_ceoccur_v2,
                       na.rm = TRUE)
    } else {
      df$Const <- NA 
    }    
    if(length(intersect(colnames(df), c("rash_ceoccur_v2", "conjunct_ceoccur_v2",
                                        "skinulcers_ceoccur_v2", "lymp_ceoccur_v2", 
                                        "bleed_ceoccur_v2") )) != 0){
      df$Systemic <- pmax(df$rash_ceoccur_v2, df$conjunct_ceoccur_v2,
                          df$skinulcers_ceoccur_v2, df$lymp_ceoccur_v2, df$bleed_ceoccur_v2,
                          na.rm = TRUE)
    } else {
      df$Systemic <- NA 
    }       
    size <- nrow(df) / 20
    
    pa <- plot.prop.by.age(df, df$fever_ceoccur_v2, "\nFever", sz = size)
    pb <- plot.prop.by.age(df, df$Cough, "\nCough", sz = size)
    pc <- plot.prop.by.age(df, df$Low.Resp, "Lower respiratory\nsymptoms", sz = size)
    pd <- plot.prop.by.age(df, df$Upper.Resp, "Upper respiratory\nsymptoms", sz = size)
    pe <- plot.prop.by.age(df, df$GI, "Gastrointestinal\nsymptoms", sz = size)
    pf <- plot.prop.by.age(df, df$Neuro, "Neurological\nsymptoms", sz = size)
    pg <- plot.prop.by.age(df, df$Const, "Constitutional\nsymptoms", sz = size)
    
    p <- arrangeGrob(pa, pb, pc, pd, pe, pf, pg, ncol = 2)
  }
  return(p)
  
}

#' @export plot.bw.by.age
#' @keywords internal
plot.bw.by.age <- function(data, var, name, ...) {
  data2 <- data
  summ <- data2 %>%
    add_column(v = var) %>%
    filter(!is.na(v)) %>%
    filter(!is.na(AgeGrp)) %>%
    group_by(AgeGrp) %>%
    mutate(lq = quantile(v, .25, na.rm = TRUE)) %>%
    mutate(uq = quantile(v, .75, na.rm = TRUE)) %>%
    mutate(iqr = uq - lq) %>%
    mutate(droplow = lq - 1.5 * iqr) %>%
    mutate(drophigh = uq + 1.5 * iqr) %>%
    filter(v > droplow) %>%
    filter(v < drophigh)
  N <- paste("N = ", nrow(summ), sep = "", collapse = NULL)
  
  xlabs <- c(
    "<10",
    "10-",
    "20-",
    "30-",
    "40-",
    "50-",
    "60-",
    "70-",
    expression(phantom(x) >= 80)
  )
  xa <- scale_x_discrete(
    name = "Age group (years)",
    labels = xlabs
  )
  ya <- scale_y_continuous(
    name = name
  )
  p <- ggplot(data = summ, aes(AgeGrp, v)) +
    geom_boxplot(outlier.shape = NA) +
    xa + ya +
    theme_bw() + theme(axis.text = element_text(size = 6)) +
    labs(title = N)
  
  return(p)
  
}

##  Plot vital signs by age  ####

#' @title Box plots for observations at hospital by age group.
#
#' @description Plots patients' data on five vital signs (respiratory rate, heart rate, systolic blood pressure and temperature) as well as the oxygen staturation in room air (%), by age group. 
#' Respiratory rate is recorded in breaths per minute, heart rate in beats per minute, systolic blood pressure in mmHg and temperature in degree Celsius.   
#' @export signs.by.age
#' @param data \code{detailed.data}, a component of the \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#' @return A plot for each observation, showing box plots by age group. On top of each plot, N (the number of individuals 
#' whose records are included in the plot) is printed (this varies between plots due to data completeness).
signs.by.age <- function(data, ...) {
  
  if( all(is.na(data$consolidated.age)) | all(is.na(data$sex))){
    p <- insufficient.data.plot()
  }else{
    
    df <- data %>%
      dplyr::select(subjid, consolidated.age, agedat, start.date, agegp10,
                    rr_vsorres, oxy_vsorresu, oxy_vsorres, hr_vsorres,
                    sysbp_vsorres, temp_vsorres, temp_vsorresu, start.to.exit,
                    sex
      )
    df <- plot.by.age.grouping(df)
    df$RR <- as.numeric(df$rr_vsorres)
    df$SpO2_roomair <- as.numeric(df$oxy_vsorres)
    df$SpO2_roomair[df$oxy_vsorresu != 1] <- NA
    df$HR <- as.numeric(df$hr_vsorres)
    df$SBP <- as.numeric(df$sysbp_vsorres)
    df$Temp <- as.numeric(df$temp_vsorres)
    # In case anyone goes Fahrenheit
    if (max(df$temp_vsorresu, na.rm = TRUE) >= 2) {
      df$temp_vsorresu[is.na(df$temp_vsorresu) == TRUE] <- 0
      df$Temp[df$temp_vsorresu == 2 & is.na(df$Temp) == FALSE] <-
        (df$Temp[df$temp_vsorresu == 2 & is.na(df$Temp) == FALSE] - 32) * 5 / 9
    }
    df$SpO2_roomair[df$SpO2_roomair < 0 | df$SpO2_roomair > 100] <- NA
    
    pa_name <- expression("Respiratory rate " ("min." ^ -1))
    pa <- plot.bw.by.age(df, df$RR, pa_name)
    pb_name <- expression("O" [2] * " saturation in room air (%)")
    pb <- plot.bw.by.age(df, df$SpO2_roomair, pb_name)
    pc_name <- expression("Heart rate " ("min." ^ -1))
    pc <- plot.bw.by.age(df, df$HR, pc_name)
    pd <- plot.bw.by.age(df, df$SBP, "Systolic blood pressure (mmHg)")
    pe_name <- expression("Temperature " (degree*C))
    pe <- plot.bw.by.age(df, df$Temp, pe_name)
    
    p <- arrangeGrob(pa, pb, pc, pd, pe, ncol = 2)
  }
  return(p)
  
}

##  Plot vital signs by age  ####

#' @title Box plots for laboratory results within 24 hours of hospital presentation. 
#
#' @description Plots the following laboratory results by age group: WCC (10^{9}/L), Lymphocytes (10^{9}/L), Neutrophilis (10^{9}/L), Urea (mmol/L), CRP (mg/L), 
#' Prothrombin time (s), APTT (s), Bilirubin (\eqn{\mu} mol/L) and ALT (units/L).
#'
#' @export blood.results.by.age
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return A plot for each laboratory result, showing box plots by age group. On top of each plot, N (the number of individuals 
#' whose records are included in the plot) is printed (this varies between plots due to data completeness).
blood.results.by.age <- function(data, ...) {
  
  if( all(is.na(data$consolidated.age)) | all(is.na(data$sex))){
    p<- insufficient.data.plot()
  }else{
    
    # Use a loop to collect data to avoid problems of data too large to process
    for (i in 1:nrow(data)) {
      p <- data$events[i][[1]] %>%
        dplyr::select(dsstdat, daily_crp_lborres,
                      daily_bun_lborres, daily_bun_lborresu,
                      daily_wbc_lborres, daily_wbc_lborresu, daily_aptt_lborres,
                      daily_pt_lborres, daily_lymp_lborres, daily_neutro_lborres,
                      ddimer_lborres, daily_alt_lborres, daily_bil_lborres,
                      daily_bil_lborresu, daily_ast_lborres
        ) %>%
        add_column(
          subjid = data$subjid[i],
          consolidated.age = data$consolidated.age[i],
          agedat = data$agedat[i],
          start.date = data$start.date[i],
          agegp10 = data$agegp10[i],
        ) %>%
        filter(dsstdat <= start.date + 1)
      if (i == 1) {
        df <- p
      } else {
        df <- bind_rows(df, p)
      }
    }
    df <- plot.by.age.grouping(df)
    #Make numeric and convert units
    df$CRP <- as.numeric(df$daily_crp_lborres)
    df$Urea <- as.numeric(df$daily_bun_lborres)
    # df$Urea[df$daily_bun_lborresu == 2 & is.na(df$Urea) == FALSE] includes some
    # lines where df$Urea = NA. Why?
    df$uc <- 1
    df$uc[df$daily_bun_lborresu != 2] <- 0
    df$uc[is.na(df$Urea) == TRUE] <- 0
    df$Urea[df$uc == 1] <- .1665 * df$Urea[df$uc == 1]
    # Units for WCC are equivalent
    df$WCC <- as.numeric(df$daily_wbc_lborres)
    df$APTT <- as.numeric(df$daily_aptt_lborres)
    df$PT <- as.numeric(df$daily_pt_lborres)
    df$Lcyte <- as.numeric(df$daily_lymp_lborres)
    df$Neut <- as.numeric(df$daily_neutro_lborres)
    df$Ddimer <- as.numeric(df$ddimer_lborres)
    df$Bili <- as.numeric(df$daily_bil_lborres)
    #df$Bili[df$daily_bil_lborresu == 2 & is.na(df$Bili) == FALSE] includes NA ??
    df$bc <- 1
    df$bc[df$daily_bil_lborresu != 2] <- 0
    df$bc[is.na(df$Bili) == TRUE] <- 0
    df$Bili[df$bc == 1] <- df$Bili[df$bc == 1] * 17.1
    df$ALT <- as.numeric(df$daily_alt_lborres)
    df$AST <- as.numeric(df$daily_ast_lborres)
    
    # WCC, lcyte, neut all vary over orders of magnitude
    df$WCC[df$WCC > 100 & is.na(df$WCC) == FALSE] <-
      df$WCC[df$WCC > 100 & is.na(df$WCC) == FALSE] / 1000
    df$Lcyte[df$Lcyte > 100 & is.na(df$Lcyte) == FALSE] <-
      df$Lcyte[df$Lcyte > 100 & is.na(df$Lcyte) == FALSE] / 1000
    df$Neut[df$Neut > 100 & is.na(df$Neut) == FALSE] <-
      df$Neut[df$Neut > 100 & is.na(df$Neut) == FALSE] / 1000
    
    pa_name <- expression("WCC " (10 ^ 9 * " /L"))
    pa <- plot.bw.by.age(df, df$WCC, pa_name)
    pb_name <- expression("Lymphocytes " (10 ^ 9 * "/L"))
    pb <- plot.bw.by.age(df, df$Lcyte, pb_name)
    pc_name <- expression("Neutrophils " (10 ^ 9 * " /L"))
    pc <- plot.bw.by.age(df, df$Neut, pc_name)
    pd <- plot.bw.by.age(df, df$Urea, "Urea (mmol/L)")
    pe <- plot.bw.by.age(df, df$CRP, "CRP (mg/L)")
    pf <- plot.bw.by.age(df, df$PT, "Prothrombin time (s)")
    pg <- plot.bw.by.age(df, df$APTT, "APTT (s)")
    # To include D-dimer when enough data
    #  ph <- plot.bw.by.age(df, df$Ddimer, "D-dimer (mg/L)")
    pj_name <- expression("Bilirubin (" * mu * "mol/L)")
    pj <- plot.bw.by.age(df, df$Bili, pj_name)
    pl <- plot.bw.by.age(df, df$ALT, "ALT (units/L)")
    
    # Omit AST as N much lower than for ALT
    
    p <- arrangeGrob(pa, pb, pc, pd, pe, pf, pg, pj, pl, ncol = 2)
    
  }
  return(p)
  
}

#' @export insufficient.data.plot
#' @keywords internal

insufficient.data.plot <- function(){
  ggplot() + annotate(geom="text", label = "Insufficient data to display this plot", x=0, y=0) + theme_void()
  
}




