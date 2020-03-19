#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(viridis)
library(ggupset)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(binom)
library(Cairo)
library(survminer)
library(survival)

server <- function(input, output) {
  countries <- c("Country A", "Country B", "Country C")
  
  alldata <- read_csv("dat2_updated.csv") %>%
    add_column(location = sample(x = countries, size = 400, prob = c(3, 2, 1), replace = TRUE)) %>%
    mutate(status = map2_chr(Died, Censored, function(d, c){
      if(c){
        "Censored"
      } else if(d){
        "Died"
      } else {
        "Recovered"
      }
      
    })) %>%
    mutate(status = factor(status)) %>%
    add_column(onset = round(30 - rexp(400, 0.5)))
  
  site.counts <- tibble(country = countries, count = rpois(3, 100))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  mapin <- st_read("Local_Authority_Districts_December_2017_Super_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84/Local_Authority_Districts_December_2017_Super_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")
  mapin2 <- st_transform(mapin, "+proj=longlat +datum=WGS84")
  
  mapin2$cases <- rpois(nrow(mapin2), 100)
  
  timings.wrangle <- alldata %>%
    select(ID,
           Died,
           Onset.Admission,
           Admission_to_ICU,
           ICU_Duration,
           Admission_to_NIMV,
           NIMV_Duration,
           Admission_to_IMV,
           IMV_Duration,
           Censored,
           Admission_to_discharge) %>%
    mutate(symptoms.start = -Onset.Admission) %>%
    mutate(symptoms.end = 0) %>%
    mutate(hospital.start = 0) %>%
    mutate(hospital.end = Admission_to_discharge) %>%
    mutate(ICU.start = Admission_to_ICU) %>%
    mutate(NIMV.start = Admission_to_NIMV) %>%
    mutate(IMV.start = Admission_to_IMV) %>%
    mutate(ICU.end = Admission_to_ICU + ICU_Duration) %>%
    mutate(NIMV.end = Admission_to_NIMV + NIMV_Duration) %>%
    mutate(IMV.end = Admission_to_IMV + IMV_Duration)  %>%
    mutate(IMV.start = map2_dbl(IMV.start, ICU.start, function(x, y) ifelse(is.na(y), NA, x))) %>%
    mutate(NIMV.start = map2_dbl(NIMV.start, ICU.start, function(x, y) ifelse(is.na(y), NA, x))) %>%
    mutate(IMV.end = map2_dbl(IMV.end, ICU.start, function(x, y) ifelse(is.na(y), NA, x))) %>%
    mutate(NIMV.end = map2_dbl(NIMV.end, ICU.start, function(x, y) ifelse(is.na(y), NA, x))) %>%
    mutate(IMV.start = map2_dbl(ICU.start, IMV.start, function(x,y){
      if(is.na(y)) {
        NA
      } else {
        max(x, y)
      }
    })) %>%
    mutate(NIMV.start = map2_dbl(ICU.start, NIMV.start, function(x,y){
      if(is.na(y)) {
        NA
      } else {
        max(x, y)
      }
    }))  %>%
    mutate(IMV.end = map2_dbl(ICU.end, IMV.end, function(x,y){
      if(is.na(y)) {
        NA
      } else {
        min(x, y)
      }
    })) %>%
    mutate(NIMV.end = map2_dbl(ICU.end, NIMV.end, function(x,y){
      if(is.na(y)) {
        NA
      } else {
        min(x, y)
      }
    })) %>%
    select(ID,  ends_with("start"), ends_with("end"), Censored, Died) %>%
    mutate(last.date = pmap_dbl(list(Censored, hospital.end, ICU.end, IMV.end, NIMV.end), function(c, h, i, m, n){
      if(!c){
        h
      } else {
        min(i,m,n)
      }
    })) %>%
    mutate(ever.ICU = !is.na(ICU.start)) %>%
    mutate(ever.IMV = !is.na(IMV.start)) %>%
    mutate(ever.NIMV = !is.na(NIMV.start))
  
  overall.start <- min(timings.wrangle$symptoms.start, na.rm = T)
  overall.end <- max(timings.wrangle$last.date, na.rm = T)
  
  complete.timeline <- map(1:400, function(patid){
    times <- map(overall.start:overall.end, function(day){
      if(day < 0){
        if(is.na(timings.wrangle$symptoms.start[patid])){
          "Unknown"
        } else if(day < timings.wrangle$symptoms.start[patid]){
          "Presymptomatic"
        } else {
          "Symptomatic"
        }
      } else {
        if(!timings.wrangle$ever.ICU[patid]){
          if(timings.wrangle$Censored[patid]){
            "Censored"
          } else if(day < timings.wrangle$hospital.end[patid]){
            "Admitted"
          } else if(timings.wrangle$Died[patid]){
            "Dead"
          } else {
            "Discharged"
          }
        } else if(!timings.wrangle$ever.IMV[patid] & !timings.wrangle$ever.NIMV[patid]){
          if(day >= timings.wrangle$ICU.start[patid] & day < timings.wrangle$ICU.end[patid]){
            "ICU"
          } else if(timings.wrangle$Censored[patid]){
            if(day >= timings.wrangle$ICU.end[patid]){
              "Censored"
            } else {
              "Admitted"
            }
          } else if(day < timings.wrangle$hospital.end[patid]){
            "Admitted"
          } else if(timings.wrangle$Died[patid]){
            "Dead"
          } else {
            "Discharged"
          }
        } else if(timings.wrangle$ever.IMV[patid]){
          if(day < timings.wrangle$IMV.start[patid] & day < timings.wrangle$IMV.end[patid]){
            "IMV"
          }else if(day >= timings.wrangle$ICU.start[patid] & day < timings.wrangle$ICU.end[patid]){
            "ICU"
          } else if(timings.wrangle$Censored[patid]){
            if(day >= timings.wrangle$ICU.end[patid] & day >= timings.wrangle$IMV.end[patid]){
              "Censored"
            } else {
              "Admitted"
            }
          } else if(day < timings.wrangle$hospital.end[patid]){
            "Admitted"
          } else if(timings.wrangle$Died[patid]){
            "Dead"
          } else {
            "Discharged"
          }
        } else {
          if(day >= timings.wrangle$NIMV.start[patid] & day < timings.wrangle$NIMV.end[patid]){
            "NIMV"
          }else if(day >= timings.wrangle$ICU.start[patid] & day < timings.wrangle$ICU.end[patid]){
            "ICU"
          } else if(timings.wrangle$Censored[patid]){
            if(day >= timings.wrangle$ICU.end[patid] & day >= timings.wrangle$NIMV.end[patid]){
              "Censored"
            } else {
              "Admitted"
            }
          } else if(day < timings.wrangle$hospital.end[patid]){
            "Admitted"
          } else if(timings.wrangle$Died[patid]){
            "Dead"
          } else {
            "Discharged"
          }
        }
      }
    })
    names(times) <- glue("day_{overall.start:overall.end}")
    times$ID <- patid
    times
  }) %>%
    bind_rows() %>%
    pivot_longer(1:70, names_to = "day", values_to = "status") %>%
    select(ID, day, status) %>%
    mutate(day = map_dbl(day, function(x) as.numeric(str_split_fixed(x, "_", 2)[2]))) %>%
    # filter(day >= 0) %>%
    mutate(status = factor(status, levels = c("Censored", "Dead", "IMV", "NIMV", "ICU", "Admitted", "Discharged", "Pre-symptomatic", "Symptomatic", "Unknown"))) %>%
    ungroup() %>%
    left_join(alldata %>% select(ID, location))
  
  
  
  output$plot1 <- renderPlot({
    data <- reactive({
      alldata %>%
        filter(location %in% input$countries) %>%
        mutate(agegp = cut(Age, breaks = seq(0, 120, 5))) %>%
        group_by(agegp, Sex, status) %>%
        dplyr::summarise(count = n()) %>%
        ungroup() %>%
        filter(Sex != 0 ) %>%
        mutate(status = factor(status)) %>%
        mutate(count = map2_dbl(count, Sex, function(c, s){
          if(s == 1){
            -c
          } else {
            c
          }
        })) %>%
        mutate(gender = map_chr(Sex, function(s){
          c("M", "F")[s]
        })) %>%
        mutate(agegp = fct_relabel(agegp, function(a){
          temp <- substr(a, 2, nchar(a) -1 )
          str_replace(temp, ",", "-")
          
        }))
    })
    ggplot() + geom_bar(data = (data() %>% filter(Sex == 1)), aes(x=agegp, y=count, fill = status), stat = "identity", col = "black") +
      geom_bar(data = data() %>% filter(Sex == 2), aes(x=agegp, y=count, fill = status),  stat = "identity", col = "black") +
      coord_flip() +
      theme_bw() +
      scale_fill_brewer(palette = 'Set2', name = "Status", drop="F") +
      xlab("Age group") +
      ylab("Count") +
      scale_x_discrete(drop = "F") +
      scale_y_continuous(breaks = seq(-30, 30, by = 10),
                         labels = as.character(c(30, 20, 10, 0, 10, 20, 30)),
                         limits = c(-max(data()$count)-10, max(data()$count)+10)) +
      annotate(geom = "text", x = 24, y = -20, label = "Males", hjust = 0, size = 5) +
      annotate(geom = "text", x = 24, y = 20, label = "Females", hjust = 1, size = 5)
    
  })
  
  output$plot2 <- renderPlot({
    data <- reactive({alldata %>%
        filter(location %in% input$countries) %>%
        select(ID, Fever, Cough, Myalgia, Arthralgia) %>%
        pivot_longer(2:5, names_to = "Condition", values_to = "Present") %>%
        mutate(Present = as.logical(Present)) %>%
        group_by(ID) %>%
        dplyr::summarise(Conditions = list(Condition), Presence = list(Present)) %>%
        mutate(conditions.present = map2(Conditions, Presence, function(c,p){
          c[which(p)]
        })) %>%
        select(-Conditions, -Presence)
    })
    
    ggplot(data(), aes(x = conditions.present)) + 
      geom_bar(fill = "deepskyblue3", col = "black") + 
      theme_bw() +
      xlab("Symptoms present at admission") +
      ylab("Count") +
      scale_x_upset() 
  })
  
  output$plot3 <- renderPlot({
    data <- reactive({alldata %>%
        filter(location %in% input$countries) %>%
        select(ID, Asthma, Smoking, Diabetes, CVD) %>%
        pivot_longer(2:5, names_to = "Condition", values_to = "Present") %>%
        mutate(Present = as.logical(Present)) %>%
        group_by(ID) %>%
        dplyr::summarise(Conditions = list(Condition), Presence = list(Present)) %>%
        mutate(conditions.present = map2(Conditions, Presence, function(c,p){
          c[which(p)]
        })) %>%
        select(-Conditions, -Presence)
    })
    
    ggplot(data(), aes(x = conditions.present)) + 
      geom_bar(fill = "indianred3", col = "black") + 
      theme_bw() +
      xlab("Comorbidities present at admission") +
      ylab("Count") +
      scale_x_upset() 
  })
  
  output$plot4 <- renderPlot({
    data <- reactive({alldata %>%
        filter(location %in% input$countries) %>%
        select(ID, Fever, Cough, Myalgia, Arthralgia, Dyspnoea, Headache, Abdominal_pain, Seizures, Nausea, Asthma, Smoking, Diabetes, CRD, HIV_AIDS, Cancer, CVD, Anaemia) %>%
        pivot_longer(2:17, names_to = "Condition", values_to = "Present") %>%
        group_by(Condition) %>%
        dplyr::summarise(Total = n(), Present = sum(Present)) %>%
        add_column(type = c(rep("Symptoms", 8), rep("Comorbidities", 8))) %>%
        mutate(prop.yes = Present/Total) %>%
        mutate(prop.no = 1-prop.yes) %>%
        arrange(type, prop.yes) %>%
        mutate(Condition = as_factor(Condition)) %>%
        pivot_longer(c(prop.yes, prop.no), names_to = "affected", values_to = "Proportion") %>%
        mutate(affected = map_lgl(affected, function(x) x == "prop.yes")) %>%
        mutate(typepresent = glue("{type}_{affected}"))
      # mutate(label.side = ifelse(prop.yes < 0.25, 0, 1)) %>%
      # mutate(textprop = glue(" {prop.yes} ")) 
    })
    
    
    ggplot(data()) + 
      geom_col(aes(x = Condition, y = Proportion, fill = affected), col = "black") +
      facet_wrap(~type, scales = "free") +
      theme_bw() + 
      coord_flip() + 
      ylim(0, 1) +
      scale_fill_brewer(palette = "Paired", name = "Condition\npresent", labels = c("No", "Yes"))
    
    
  })
  
  output$sites_value_box <- renderUI({
    box(width=6, 
        HTML('<font size="6"><p style="text-align:center"><b>Number of sites</b><br></p></font>'),
        lapply(1:nrow(site.counts), function(i) {
          valueBox(site.counts$count[i],site.counts$country[i])})
    )
  })
  
  output$cases_value_box <- renderUI({
    box(width=6,
        HTML('<font size="6"><p style="text-align:center"><b>Number of cases</b><br></p></font>'),
        lapply(1:length(countries), function(i) {
          valueBox(alldata %>% filter(location == countries[i]) %>% nrow(),countries[i], color = "maroon")})
    )
  })
  
  output$map1 <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = mapin2$cases)
    
    leaflet(mapin2) %>% addTiles() %>%
      addPolygons(weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, color = ~pal(cases)) %>%
      addLegend("bottomright", pal = pal, values = ~cases,
                title = "Number of cases",
                labFormat = labelFormat(),
                opacity = 1
      )
  })
  
  
  output$plot5 <- renderPlot({
    
    data <- reactive({
      complete.timeline %>% 
        filter(location %in% input$countries2)  %>%
        filter(day >= 0) %>%
        mutate(status = factor(status, levels = c("Censored", "Dead", "IMV", "NIMV", "ICU", "Admitted", "Discharged")))
    })
    
    ggplot(data() %>% filter(day >=0)) + geom_bar(aes(x = day, fill = status), position = "fill") +
      scale_fill_brewer(palette = "Set3", name  = "Status", drop = F) + 
      theme_bw() + 
      xlab("Days relative to admission") +
      ylab("Proportion")
  })
  
  output$plot6 <- renderPlot({
    
    #head(dat2)
    
    vd <- reactive({alldata %>% 
        filter(location %in% input$countries2) %>%
        mutate(agegp = cut(Age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 120))) %>%
        select(Sex, Admission_to_discharge, agegp) %>%
        mutate(Sex = map_chr(Sex, function(x) c("Unknown", "Male", "Female")[x+1])) %>%
        mutate(Sex = factor(Sex, levels = c("Unknown", "Male", "Female"))) %>%
        mutate(agegp = fct_relabel(agegp, function(a){
          temp <- substr(a, 2, nchar(a) -1 )
          str_replace(temp, ",", "-")
        }))
    })
    
    vd1 <- ggplot(vd(), aes(x = Sex, y = Admission_to_discharge, fill=Sex)) + 
      geom_violin(trim=FALSE)+ 
      geom_boxplot(width=0.1, fill="white")  +
      labs(x="Sex", y = "Days from admission to discharge") + 
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) + 
      scale_fill_discrete(guide = F) +
      theme_bw()
    
    vd1
    
  })
  
  output$plot7 <- renderPlot({
    
    vd <- reactive({alldata %>% 
        filter(location %in% input$countries2) %>%
        mutate(agegp = cut(Age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 120))) %>%
        select(Sex, Admission_to_discharge, agegp) %>%
        mutate(Sex = map_chr(Sex, function(x) c("Unknown", "Male", "Female")[x+1])) %>%
        mutate(Sex = factor(Sex, levels = c("Unknown", "Male", "Female"))) %>%
        mutate(agegp = fct_relabel(agegp, function(a){
          temp <- substr(a, 2, nchar(a) -1 )
          str_replace(temp, ",", "-")
          
        }))
    })
    
    vd_2 <- ggplot(vd(), aes(x = agegp, y = Admission_to_discharge, fill=agegp)) + 
      geom_violin(trim=FALSE)+
      labs(x="Age group", y = "Days from admission to discharge") +
      scale_fill_discrete(guide = F) +
      theme_bw()
    
    vd_2
    
    
  })

  output$plot8 <- renderPlot({
    #wd <- "C:/Users/Mark/Documents/st3/nCoV/reportmocks"
    #setwd(wd)
    
    dat <- reactive({alldata %>% 
        filter(location %in% input$countries2)
    })
    
    # I have a possibly cumbersome way of making the dates work (sorry!)
    change_date <- function(date) {
      d <- paste(date, sep = "", collapse = NULL)
      d[is.na(date) == TRUE] <- "01/01/1900"
      D <- str_sub(d, 1, 2)
      M <- str_sub(d, 4, 5)
      Y <- str_sub(d, 7, 10)
      YMD <- paste(Y, M, D, sep = "-", collapse = NULL)
      YMD[YMD == "1900-01-01"] <- NA
      return(as.Date(YMD))
    }
    
    # As I understand the method, we don't care about when people were admitted
    # for this plot, just the numbers that have been discharged and the number
    # who have died
    Dc_date <- change_date(dat()$Discharge_date)
    Died_date <- change_date(dat()$Death_date)
    
    # First patient in my mock data recruited 1/2/2020
    
    d.0 <- as.Date("2020-02-01")
    
    for (i in 0:60) {
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
    
    # With the example data I used the graph was dominated by a huge confidence
    # interval yet the estimated mortality ended up around 2.5% so I trimmed
    # the plot at 20%.  Obviously we might not want to do this with the real data.
    
    trim <- .2
    
    db$upper[db$upper > .2] <- trim
    
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
      limits = c(0, trim)
    )
    plot <- ggplot(data = db) +
      line +
      shade +
      yaxis + theme_bw()
    
    plot
    
  })  
  
  output$plot9 <- renderPlot({
    dat <- reactive({alldata %>% 
        filter(location %in% input$countries2)
    })
    
    ggplot(dat()) + geom_bar(aes(x = onset, fill = status), col = "black", width = 1, size = 0.5) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1", name = "Status", drop = FALSE) +
      xlab("Day of admission") +
      ylab("Cases")  +
      coord_fixed(ratio = .1)
    
    
  })
  
  output$plot10 <- renderPlot({
    timeline.subset <- complete.timeline %>%
      filter(ID %in% 1:20) %>% 
      filter(status != "Presymptomatic" & status != "Unknown" & status != "Discharged" & status != "Dead" & status != "Censored") %>%
      mutate(ID = factor(ID, levels = as.character(1:20)))
    
    events <- complete.timeline %>%
      group_by(ID, status) %>%
      dplyr::summarise(start = min(day)) %>%
      filter(status %in% c("Admitted", "Dead", "Discharged", "Censored")) %>%
      ungroup() %>%
      mutate(ID = factor(ID, levels = as.character(1:20)))
    
    ggplot(timeline.subset) + 
      geom_tile(aes(x = day, y = ID, fill = status), col = "black") + 
      geom_point(data = events %>% filter(ID %in% as.character(1:20)), aes(shape = status, x = start, y= ID), size = 3) +
      coord_fixed() +
      theme_bw() +
      scale_fill_brewer(palette = "Dark2", name = "Status") +
      scale_shape_manual(values = c(88, 68, 65, 82), name = "Events") +
      ylab("Patient ID") +
      xlab("Days relative to admission")
    
  })
  
  
  output$plot11 <- renderPlot({
    ggplot(site.counts) + geom_col(aes(x = country, y = count), col = "black", fill = "deepskyblue3") +
      theme_bw() +
      xlab("Country") +
      ylab("Sites") + 
      coord_fixed(ratio = 0.02)
  })
  
  output$plot12 <- renderPlot({
    ggplot(alldata) + geom_bar(aes(x = location, fill = status), col = "black") +
      theme_bw() +
      scale_fill_brewer(palette = "Set1", name = "Status") +
      xlab("Country") +
      ylab("Cases") + 
      coord_fixed(ratio = 0.02)
  })
  
  output$onsetAdmissionPlot <- renderPlot({
    
    X = alldata$Onset.Admission[!(is.na(alldata$Onset.Admission))]
    
    set.seed(1432)
    X = rgamma(342, shape = 3.44, rate = 0.49)
    
    t <- data.frame(x = c(X,0, 0))
    
    
    onset.adm.p <-  ggplot(data = t) + 
      geom_histogram(data = t, aes(x=x, y=..density..), binwidth = 1, color = 'white', fill = 'blue', alpha = 0.8)+    
      geom_line(aes(x=t$x, y=dgamma(t$x, 3.44, 0.49)), color="black", size = 2) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Days', title = 'Time from symptom onset to admission')+ geom_vline(aes(xintercept = mean(t$x, na.rm=T)), colour = 'red', linetype = 'longdash', size = 0.8)
    
    onset.adm.p
    
  })
  
  output$admissionICUPlot <- renderPlot({
    
    set.seed(1432)
    X  = rgamma(100, shape = 0.99, rate = 0.17)
    #X = dat2$Admission_to_ICU[!(is.na(dat2$Admission_to_ICU))]
    
    t <- data.frame(x = X)
    
    
    adm.icu <- ggplot(data = t) + 
      geom_histogram(data = t, aes(x=x, y=..density..), binwidth = 1, color = 'white', fill = 'blue', alpha = 0.8)+    
      geom_line(aes(x=t$x, y=dgamma(t$x,0.999, 0.17)), color="black", size = 2) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Days')+ geom_vline(aes(xintercept = mean(t$x, na.rm=T)), colour = 'red', linetype = 'longdash', size = 0.8)
    
    adm.icu
    
  })
  
  output$admissionDischargePlot <- renderPlot({
    X = alldata$Admission_to_discharge[!(is.na(alldata$Admission_to_discharge))]
    
    t <- data.frame(x = X)
    
    
    adm.disch.p <- ggplot(data = t) + 
      geom_histogram(data = t, aes(x=x, y=..density..), binwidth = 1, color = 'white', fill = 'blue', alpha = 0.8)+    
      geom_line(aes(x=t$x, y=dgamma(t$x,4.76, 0.62)), color="black", size = 2) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Days')+ geom_vline(aes(xintercept = mean(t$x, na.rm=T)), colour = 'red', linetype = 'longdash', size = 0.8)
    
    adm.disch.p
  })
  
  output$ICUDurationPlot <- renderPlot({
    set.seed(1432)
    X  = rgamma(100, shape = 1.9, rate = 0.22)
    #X = dat2$ICU_Duration[!(is.na(dat2$ICU_Duration))]
    
    t <- data.frame(x = c(X, 0, 0))
    
    
    dur.icu <- ggplot(data = t) + 
      geom_histogram(data = t, aes(x=x, y=..density..), binwidth = 1, color = 'white', fill = 'blue', alpha = 0.8)+    
      geom_line(aes(x=t$x, y=dgamma(t$x,1.9, 0.22)), color="black", size = 2) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Days')+ geom_vline(aes(xintercept = mean(t$x, na.rm=T)), colour = 'red', linetype = 'longdash', size = 0.8)
    
    dur.icu
    
  })
  
  output$NIMVDurationPlot <- renderPlot({
    
    set.seed(1432)
    X  = rgamma(100, shape = 0.86, rate= 0.09)
    
    #X = dat2$NIMV_Duration[!(is.na(dat2$NIMV_Duration))]
    
    t <- data.frame(x = X)
    
    dur.nimv <- ggplot(data = t) + 
      geom_histogram(data = t, aes(x=x, y=..density..), binwidth = 1, color = 'white', fill = 'blue', alpha = 0.8)+    
      geom_line(aes(x=t$x, y=dgamma(t$x, 0.86, 0.09)), color="black", size = 2) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Days')+ geom_vline(aes(xintercept = mean(t$x, na.rm=T)), colour = 'red', linetype = 'longdash', size = 0.8) + ylim(0, 0.12)
    
    dur.nimv
  })
  
  output$IMVDurationPlot <- renderPlot({
    ####### IMV Duration #######
    
    set.seed(1432)
    X  = rgamma(100, shape = 2.39, rate= 0.86)
    
    #X = dat2$NIMV_Duration[!(is.na(dat2$NIMV_Duration))]
    
    t <- data.frame(x = c(X, 0 , 0 ))
    
    
    dur.imv <- ggplot(data = t) + 
      geom_histogram(data = t, aes(x=x, y=..density..), binwidth = 1, color = 'white', fill = 'blue', alpha = 0.8)+    
      geom_line(aes(x=t$x, y=dgamma(t$x, 2.39, 0.86)), color="black", size = 2) +
      theme(
        plot.title = element_text( size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12)
      ) +
      theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) ) +
      labs(y = 'Density', x = 'Days')+ geom_vline(aes(xintercept = mean(t$x, na.rm=T)), colour = 'red', linetype = 'longdash', size = 0.8)
    
    dur.imv
    
  })
  
  output$kmplot1 <- renderPlot({
    data("lung")
    #head(lung)
    
    lung$time <- lung$time/50
    
    fit <- survfit(Surv(time, status) ~ sex, data = lung)
    #print(fit)
    
    
    d <- data.frame(time = fit$time,
                    n.risk = fit$n.risk,
                    n.event = fit$n.event,
                    n.censor = fit$n.censor,
                    surv = fit$surv,
                    upper = fit$upper,
                    lower = fit$lower
    )
    
    
    ggsurvplot(fit,
               pval = F, conf.int = T,
               risk.table = F, # Add risk table
               # risk.table.col = "strata", # Change risk table color by groups
               linetype = "strata", # Change line type by groups
               #surv.median.line = "hv", # Specify median survival
               ggtheme = theme_bw(), # Change ggplot2 theme
               palette = c('#BA55D3', '#D2691E' ),
               legend.labs = 
                 c("Female", "Male"), title = (main = ' '),  hjust = 0.5 )
  })
  
  output$kmplot2 <- renderPlot({
    ct.all.resolved <- complete.timeline %>%
      group_by(ID) %>%
      mutate(lose.this = any(status == "censored")) %>%
      ungroup() %>%
      filter(!lose.this) %>%
      select(-lose.this)
    
    outcome.lines <- ct.all.resolved %>%
      group_by(day) %>%
      dplyr::summarise(n.dead = sum(status == "Dead"), n.discharged = sum(status == "Discharged"), n=n()) %>%
      mutate(prop.dead = n.dead/n, recip.prop.disc = 1-(n.discharged/n)) %>%
      ungroup() %>%
      filter(day >= 0) %>%
      select(day, prop.dead, recip.prop.disc) %>%
      pivot_longer(2:3, names_to = "stat", values_to = "value")
    
    ggplot(outcome.lines) + 
      geom_line(aes(x= day, y=value, col = stat)) +
      theme_bw() +
      scale_colour_brewer(palette = "Set1", name = "Statistic", labels = c("Proportion dead", "1-(proportion\ndischarged)")) +
      xlab("Days after admission") +
      ylab("Cumulative probability")
  })
  

}