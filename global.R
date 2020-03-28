library(tidyverse)
library(glue)
library(viridis)
library(ggupset)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(magrittr)
library(binom)
library(plyr)
library(fitdistrplus)
library(lubridate)
library(grid)
library(binom)
library(boot)
library(survival)
library(survminer)
library(broom)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(knitr)

reprocess.data <- F 

# file locations

paths <- read_csv("paths.csv", col_names = F)

code.path <- paths[[which(paths$X1 == "code.path"), 2]]
data.path <- paths[[which(paths$X1 == "data.path"), 2]]
data.dict.file <- paths[[which(paths$X1 == "data.dict.file"), 2]]
site.list.file <- paths[[which(paths$X1 == "site.list.file"), 2]]
uk.data.file <- paths[[which(paths$X1 == "uk.data.file"), 2]]
row.data.file <- paths[[which(paths$X1 == "row.data.file"), 2]]
eot.data.file <- paths[[which(paths$X1 == "eot.data.file"), 2]]

# Source files

if(!(reprocess.data) & file.exists(glue("{code.path}/patient_data_{today()}.rda"))){
  load(glue("{code.path}/patient_data_{today()}.rda"))
} else {
  source(glue("{code.path}/process_data.R"))
}

source(glue("{code.path}/plot_functions.R"))

if(!(reprocess.data) & file.exists(glue("{code.path}/report_input_data_{today()}.rda"))){
  load(glue("{code.path}/report_input_data_{today()}.rda"))
} else {
  source(glue("{code.path}/generate_report_input.R"))
}

if(reprocess.data){
  setwd("markdown")
  
  rmdfiles <- list.files(pattern = ".Rmd", full.names = T)
  sapply(rmdfiles, knit, quiet = T)
  
  setwd("..")
}


