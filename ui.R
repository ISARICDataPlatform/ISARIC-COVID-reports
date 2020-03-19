#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shinyWidgets)
library(leaflet)

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
                   menuItem("Hospital Outcomes", tabName = "outcomes", icon = icon("th")),
                   menuItem("Statistical Analysis", tabName = "stats", icon = icon("chart-bar")),
                   menuItem("Country Comparisons", tabName = "ccomp", icon = icon("globe")),
                   menuItem("Maps", tabName = "Maps", icon = icon("map")),
                   menuItem("Clusters", tabName = "Clusters", icon = icon("project-diagram")),
                   menuItem("Methods", tabName = "Methods", icon = icon("wrench")),
                   menuItem("References", tabName = "References", icon = icon("list")),
                   menuItem("Team", tabName = "Team", icon = icon("users"))),
                 
                 
                 hr(),
                 fluidRow(column(3, verbatimTextOutput("value")))
               ),
               dashboardBody(
                 tags$head(tags$style(HTML('
            /* logo */
                          .skin-black .main-header .logo {
                              color: #F70656;
                              font-weight: bold;
                              }
            /* active selected tab in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                  color: #F70656;
                                  border-left-color: #F70656;
                                }
    '))),
                 
                 tabItems(
                   tabItem(tabName = "Summary",
                           includeMarkdown("markdown/summary.md")
                   ),
                   
                   
                   tabItem(tabName = "patients",
                           
                           fluidRow(
                             uiOutput("sites_value_box"),
                             uiOutput("cases_value_box")
                           ),
                           
                           fluidRow(
                             box(
                               
                               checkboxGroupButtons(
                                 inputId = "countries", label = NULL,
                                 choices = list("Country A" = "Country A", "Country B" = "Country B", "Country C" = "Country C"),
                                 justified = TRUE, status = "primary",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 selected = c("Country A","Country B","Country C")
                               ), width = 12
                             )
                           ),
                           fluidRow(
                             box(plotOutput("plot1"), width = 12, title = "Age distribution of patients")),
                           fluidRow(
                             box(plotOutput("plot2"), width = 12, title = "Symptoms and combinations of symptoms present at admission")),
                           fluidRow(
                             box(plotOutput("plot3"), width = 12, title = "Comorbidities and combinations of comorbidities present at admission")),
                           fluidRow(
                             box(plotOutput("plot4"), width = 12, title = "Simple prevalence of symptoms and comorbidities"))
                   ),
                   
                   tabItem(tabName = "outcomes",
                           
                           fluidRow(
                             box(
                               checkboxGroupButtons(
                                 inputId = "countries2", label = NULL,
                                 choices = list("Country A" = "Country A", "Country B" = "Country B", "Country C" = "Country C"),
                                 justified = TRUE, status = "primary",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 selected = c("Country A","Country B","Country C")
                               ), width = 12
                             )
                           ),
                           fluidRow(
                             box(plotOutput("plot5"), width = 12, title = "Distribution of patient status by time since admission")),
                           fluidRow(
                             box(plotOutput("plot6"), width = 12, title = "Length of hospital stay by sex")),
                           fluidRow(
                             box(plotOutput("plot7"), width = 12, title = "Length of hospital stay by age group")),
                           fluidRow(
                             box(plotOutput("plot8"), width = 12, title = "Hospital fatality ratio (based only on completed outcomes)")),
                           fluidRow(
                             box(plotOutput("plot9"), width = 12, title = "Patient outcomes by date of admission"))
                   ),
                   tabItem(tabName = "Clusters",
                           fluidRow(
                             box(plotOutput("plot10"), width = 12, title = "Hospital timeline for cluster X"))
                   ),
                   tabItem(tabName = "ccomp",
                           fluidRow(
                             box(plotOutput("plot11"), width = 12, title = "Number of sites per country")),
                           fluidRow(
                             box(plotOutput("plot12"), width = 12, title = "All patients by country and outcome"))
                   ),
                   tabItem(tabName = "Methods",
                           includeMarkdown("markdown/methods.md")
                   ),
                   tabItem(tabName = "References",
                           includeMarkdown("markdown/references.md")
                   ),
                   tabItem(tabName = "Team",
                           includeMarkdown("markdown/team.md")
                   ),
                   tabItem(tabName = "stats",
                           fluidRow(box(includeMarkdown("markdown/statsintro.md"), width = 12, title = "Distributional plots")),
                           fluidRow(
                             box(plotOutput("onsetAdmissionPlot"), width = 12, title = "Time from symptom onset to admission")),
                           fluidRow(
                             box(plotOutput("admissionICUPlot"), width = 12, title = "Time from admission to ICU entry")),
                           fluidRow(
                             box(plotOutput("admissionDischargePlot"), width = 12, title = "Time from admission to discharge")),
                           fluidRow(
                             box(plotOutput("ICUDurationPlot"), width = 12, title = "Duration of ICU stay")),
                           fluidRow(
                             box(plotOutput("NIMVDurationPlot"), width = 12, title = "Duration of NIMV")),
                           fluidRow(
                             box(plotOutput("IMVDurationPlot"), width = 12, title = "Duration of IMV")),
                           fluidRow(
                             box(plotOutput("kmplot1"), width = 12, title = "'Survival' curves and 95% confidence intervals for time to discharge (by sex)")),
                           fluidRow(
                             box(plotOutput("kmplot2"), width = 12, title = "Modified Kaplan-Meier curves for outcomes (death or discharge)"))
                           
                   ),
                   tabItem(tabName = "Maps",
                           fluidPage(
                             box(
                               leafletOutput("map1"),
                               p(), width = 12, title = "Distribution of cases by local authority")
                           )
                   )
                 )
               )
)


# customHeaderPanel <- function(title,windowTitle=title){
#   tagList(
#     tags$head(
#       tags$title(windowTitle),
#       tags$link(rel="stylesheet", type="text/css",
#                 href="app.css"),
#       tags$h1(a(href="isaric.tghn.org/"))
#     )
#   )
# }