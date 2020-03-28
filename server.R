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
    filtered.data <- reactive({
      print(input$agegp5)
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(sex))
    })
    renderPlot(confidentiality.check(filtered.data(), age.pyramid), height = 300)
  }

  output$comorbiditySymptomPrevalence <- {
    filtered.data <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(symptoms.recorded & comorbidities.recorded)
    })
    renderPlot(confidentiality.check(filtered.data(), comorbidity.symptom.prevalence), height = 300)
  }
  
  output$comorbiditiesUpset <- {
    filtered.data <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(comorbidities.recorded)
    })
    renderPlot(confidentiality.check(filtered.data(), comorbidities.upset, max.comorbidities = 4), height = 300)
  }
  
  output$symptomsUpset <- {
    filtered.data <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(symptoms.recorded & comorbidities.recorded)
    })
    renderPlot(confidentiality.check(filtered.data(), symptoms.upset, max.symptoms = 4), height = 300)
  }
  
  output$outcomesByAdmissionDate <- {
    filtered.data <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2]) %>%
        filter(!is.na(hostdat))
    })
    renderPlot(confidentiality.check(filtered.data(), outcomes.by.admission.date), height = 300)
  }
  
  output$violinAgeFunc <- 
    {
      filtered.data <- reactive({
        fd <- patient.data %>% 
          filter(Country %in% input$Country) %>%
          filter(outcome %in% input$outcome) %>%
          filter(sex %in% input$sex) %>%
          filter(!is.na(consolidated.age)) %>%
          filter(consolidated.age >= input$agegp5[1] & consolidated.age < input$agegp5[2])
      })
      renderPlot(confidentiality.check(filtered.data(), violin.age.func), height = 300)
    }
  
  output$violinSexFunc <- 
    {
      filtered.data <- reactive({
        fd <- patient.data %>% 
          filter(Country %in% input$Country) %>%
          filter(outcome %in% input$outcome) %>%
          filter(sex %in% input$sex) %>%
          filter(!is.na(sex))
      })
      renderPlot(confidentiality.check(filtered.data(), violin.sex.func), height = 300)
    }
  
  
  
  output$statusByTimeAfterAdmission <- {
    filtered.data <- reactive({
      fd <- patient.data %>% 
        filter(Country %in% input$Country) %>%
        filter(outcome %in% input$outcome) %>%
        filter(sex %in% input$sex) %>%
        filter(!is.na(consolidated.age)) %>%
        filter(!is.na(hostdat))
    })
    renderPlot(confidentiality.check(filtered.data(), status.by.time.after.admission, height = 300))
    
  }
  
  # output$tree <- renderTree({ 
  #   list(  'I lorem impsum'= list( 
  #     'I.1 lorem impsum'   =  structure(list('I.1.1 lorem impsum'='1', 'I.1.2 lorem impsum'='2'),stselected=TRUE),  
  #     'I.2 lorem impsum'   =  structure(list('I.2.1 lorem impsum'='3'), stselected=TRUE))) 
  # })
}