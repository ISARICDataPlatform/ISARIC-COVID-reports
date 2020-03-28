#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




dbHeader <- dashboardHeader(title = "COVID-19 Analysis Report",
                            tags$li(a(href = 'http://isaric.tghn.org/',
                                      img(src = 'ISARIClogo.png',
                                          title = "Company Home", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown"
                                    # tags$style(".main-header {max-height: 40px}"),
                                    # tags$style(".main-header .logo {height: 40px;}"),
                                    # tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
                                    # tags$style(".navbar {min-height:40px !important}")
                            ))

# Define UI for application that draws a histogram
dashboardPage( skin = "black",
               # dashboardHeader(title = tags$a(href='http://mycompanyishere.com', tags$img(src='/www/ISARIClogo.png'))),   #
               dbHeader,
               dashboardSidebar(
                 # tags$style(".left-side, .main-sidebar {padding-top: 50px}"),
                 sidebarMenu(
                   menuItem("Summary", tabName = "Summary", icon = icon("align-justify")),
                   menuItem("Patient Characteristics", tabName = "patients", icon = icon("th")),
                   menuItem("Timelines and Outcomes", tabName = "outcomes", icon = icon("th")),
                   menuItem("Statistical Analysis", tabName = "stats", icon = icon("chart-bar")),
                   menuItem("Country Comparisons", tabName = "ccomp", icon = icon("globe")),
                   menuItem("Maps", tabName = "Maps", icon = icon("map")),
                   menuItem("Clusters", tabName = "Clusters", icon = icon("project-diagram")),
                   menuItem("Methods", tabName = "Methods", icon = icon("wrench")),
                   menuItem("References", tabName = "References", icon = icon("list")),
                   menuItem("Team", tabName = "Team", icon = icon("users"))
                 ),
                 
                 hr(),
                 fluidRow(column(3, verbatimTextOutput("value")))
               ),
               dashboardBody(
                 tags$head(tags$style(HTML('
                 .btn-custom {background-color: #F70656; color:  #FFFFFF;}
                 .skin-black .main-header .logo {color: #F70656; font-weight: bold;}
                 .skin-black .main-sidebar .sidebar .sidebar-menu .active a {color: #F70656; border-left-color: #F70656;}
                 .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from, .irs-grid-pol {background: #F70656; border-color: #F70656;}'
                 ))),
                 
                 dropdown(
                   inputId = "controls",
                   icon = icon("gear"),
                   size = "sm",
                   status = "custom",
                   tooltip = tooltipOptions(title = "Click for data settings"),
                   options = list(`style` = "btm-custom"),
                   tags$h3("Controls"),
                   awesomeCheckboxGroup(
                     inputId = "sex", label = "Gender", status = "custom",
                     choices = list("Male" = 1, "Female" = 2, "Unknown" = NA),
                     selected = c("Male","Female","Unknown")
                   ),
                   sliderInput(inputId = "agegp5", label = "Age group",
                               min = 0, max = 90, step = 5, value = c(0,120), dragRange = T),
                   
                   
                   pickerInput(
                     inputId = "Country",
                     label = "Country", 
                     choices = unique(countries.and.sites$Country),
                     selected = unique(countries.and.sites$Country),
                     options = list(
                       `actions-box` = TRUE), 
                     choicesOpt = list(
                       subtext = glue("{unique(countries.and.sites$n.sites)} sites")
                     ),
                     multiple = TRUE
                   ),
                   awesomeCheckboxGroup(
                     inputId = "outcome", label = "Outcome", status = 'custom',
                     choices = list("Death" = "death", "Censored" = "censored", "Discharge" = "discharge"),
                     selected = c("death","censored","discharge")
                   )
                 ),
                 hr(),
                 tabItems(
                   tabItem(tabName = "Summary",
                           h1("ISARIC COVID-19 Report Dashboard"),
                           fluidRow(
                             # @todo source this from the same origin as the text in the Rmd
                             box(width = 12,

                                  includeMarkdown("markdown/summary.md")

                                 )
                           )),
                   tabItem(tabName = "patients",
                           fluidRow(
                             box(plotOutput("agePyramid", height = "300px"), 
                                 "Bar fills are outcome (death/discharge/censored) at the time of report.", 
                                 width = 6, height = 400, solidHeader = T, title = 'Age and sex distribution of patients'),
                             box(plotOutput("comorbiditySymptomPrevalence", height = "300px"), 
                                 "Only patients for whom all these data was recorded are included.",
                                 title = "Prevalence of all recorded comorbidities and symptoms at admission",
                                 width = 6, height = 400, solidHeader = T)
                           ),
                           fluidRow(
                             box(plotOutput("comorbiditiesUpset", height = "300px"),
                                 "Only patients for whom all these data was recorded are included. Filled and empty circles below the x-axis indiciate the presence or absence of each comorbidity.",
                                 title ="Combinations of the four most common comorbidities seen at admission",
                                 width = 6, height = 400,  solidHeader = T),
                             box(plotOutput("symptomsUpset", height = "300px"), 
                                 "Only patients for whom all these data was recorded are included. Filled and empty circles below the x-axis indiciate the presence or absence of each symptom",
                                 title ="Combinations of the four most common symptoms seen at admission",
                                 width = 6,  height = 400, solidHeader = T)
                           )
                   ),
                   tabItem(tabName = "outcomes",
                           fluidRow(
                             box(plotOutput("violinAgeFunc", height = "300px"),
                                 "Data up to 19/03/2020. Hospital stay considers the time to death, recovery or censorship.",
                                 width = 6, height = 400, solidHeader = T, title = 'Distribution of length of hospital stay by patient age group'),
                             box(plotOutput("statusByTimeAfterAdmission", height = "300px"),
                                 "“Transferred” patients were moved to another institution, where their final outcome cannot be determined. 
                                 Patients with censored outcomes at the time of this report remain in the “Admitted” category.",
                                 width = 6, height = 400, solidHeader = T, title = 'Patient outcome by number of days after admission')
                           ),
                           fluidRow(
                             box(plotOutput("violinSexFunc", height = "300px"),
                                 "Data up to 19/03/2020. Hospital stay considers the time to death, recovery or censorship.",
                                 width = 4, height = 400, solidHeader = T, title = 'Distribution of length of hospital stay by patient sex'),
                             box(plotOutput("outcomesByAdmissionDate", height = "300px"),
                                 width = 4, height = 400, solidHeader = T, title = 'Patient outcomes by epidemiological week of admission'),
                             # box(plotOutput("outcomesByAdmissionDate", height = "300px"),
                             #     width = 4, height = 400, solidHeader = T, title = 'Patient outcomes by epidemiological week (of 2020) of admission')
                           )
                   )
                 )
               )
)
