#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

privacy.text <- "Apologies, we cannot display graphs of data from\nless than five individuals for reasons of data privacy."
privacy.minimum <- 5

confidentiality.check <- function(data, fn, min.rows = 5, ...){
  args <- list(...)
  if(nrow(data) >= min.rows){
    exec(fn, data, !!!args)
  } else {
    ggplot() + annotate(geom = "text", x=0, y=0, label = privacy.text) + theme_void()
  } 
}

server <- function(input, output) {
  
  output$agePyramid <- {
    filtered.data.ap <- reactive({
      fd <- patient.data %>% 
        filter(!is.na(outcome)) %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(sex)) %>%
        filter(!is.na(consolidated.age))
    })
    renderPlot(confidentiality.check(filtered.data.ap(), age.pyramid), height = 300)
  }
  
  output$comorbidityPrevalence <- {
    filtered.data.cp <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(comorbidities.recorded)
    })
    renderPlot(confidentiality.check(filtered.data.cp(), comorbidity.prevalence.plot), height = 300)
  }
  
  output$symptomPrevalence <- {
    filtered.data.sp <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(symptoms.recorded)
    })
    renderPlot(confidentiality.check(filtered.data.sp(), symptom.prevalence.plot), height = 300)
  }
  
  
  output$comorbiditiesUpset <- {
    filtered.data.cu <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(comorbidities.recorded)
    })
    renderPlot(confidentiality.check(filtered.data.cu(), comorbidities.upset, max.comorbidities = 4), height = 300)
  }
  
  output$symptomsUpset <- {
    filtered.data.su <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(symptoms.recorded & comorbidities.recorded)
    })
    renderPlot(confidentiality.check(filtered.data.su(), symptoms.upset, max.symptoms = 4), height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
    filtered.data.obad <- reactive({
      fd <- patient.data %>% 
        filter(!is.na(outcome)) %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(hostdat))
    })
    renderPlot(confidentiality.check(filtered.data.obad(), outcomes.by.admission.date), height = 300)
  }
  
  output$violinAgeFunc <- 
    {
      filtered.data.vaf <- reactive({
        fd <- patient.data %>% 
          filter(Country %in% input$Country) %>%
          filter(outcome %in% input$outcome) %>%
          filter(sex %in% input$sex) %>%
          filter(!is.na(consolidated.age)) %>%
          filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
      })
      renderPlot(confidentiality.check(filtered.data.vaf(), violin.age.func), height = 300)
    }
  
  output$violinSexFunc <- 
    {
      filtered.data.vsf <- reactive({
        fd <- patient.data %>% 
          filter(Country %in% input$Country) %>%
          filter(outcome %in% input$outcome) %>%
          filter(sex %in% input$sex) %>%
          filter(!is.na(sex) & sex != 3) %>%
          filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
      })
      renderPlot(confidentiality.check(filtered.data.vsf(), violin.sex.func), height = 300)
    }
  
  output$statusByTimeAfterAdmission <- {
    filtered.data.sbtaa <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(!is.na(consolidated.age)) %>%
        filter(!is.na(hostdat))
    })
    renderPlot(confidentiality.check(filtered.data.sbtaa(), status.by.time.after.admission, height = 300))
  }
  
  output$treatmentPlot <- {
    filtered.data.tp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(treatments.recorded)
    })
    renderPlot(confidentiality.check(filtered.data.tp(), treatment.use.plot, height = 300))
  }


  output$treatmentUpset <- {
    filtered.data.tu <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(treatments.recorded)
    })
    renderPlot(confidentiality.check(filtered.data.tu(), treatment.upset, height = 300))
  }
  
  
  output$onsetAdmPlot <- {
    filtered.data.oap <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(onset.to.admission))
    })
    renderPlot(confidentiality.check(filtered.data.oap(), onset.adm.plot, height = 300))
  }
  
  output$admOutcomePlot <- {
    filtered.data.aop <- reactive({
      fd <- patient.data %>%
        filter(!is.na(outcome)) %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.to.exit) | !is.na(start.to.censored))
    })
    renderPlot(confidentiality.check(filtered.data.aop(), adm.outcome.plot, height = 300))
  }
  
  output$modifiedKMPlot <- {
    filtered.data.aop <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.to.exit) | !is.na(start.to.censored))
    })
    renderPlot(confidentiality.check(filtered.data.aop(), modified.km.plot, height = 300))
  }
  
  
  output$sitesByCountry <- {
    filtered.data.sbc <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
    })
    renderPlot(confidentiality.check(filtered.data.sbc(), sites.by.country, height = 300))
  }
  
  output$outcomesByCountry <- {
    filtered.data.obc <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(Country))
    })
    renderPlot(confidentiality.check(filtered.data.obc(), outcomes.by.country, height = 300))
  }
  
  output$recruitmentDatPlot <- {
    filtered.data.rdp <- reactive({
      fd <- patient.data %>%
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(start.date))
    })
    renderPlot(confidentiality.check(filtered.data.rdp(), recruitment.dat.plot, embargo.limit = embargo.limit, height = 300))
  }
  
  
  

# output$tree <- renderTree({ 
#   list(  'I lorem impsum'= list( 
#     'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),  
#     'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE))) 
# })
}