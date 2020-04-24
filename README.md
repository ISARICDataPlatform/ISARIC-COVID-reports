# ISARIC-COVID-reports



## Installation

The R code is provided as a package called "COVIDreportwriter". To install this, first download the Github repository, either by using the links on this page or using the command line (in the MacOS/Linux/Unix terminal or [git bash](https://gitforwindows.org/)):

    git clone https://github.com/ISARICDataPlatform/ISARIC-COVID-reports.git

You may also need to install [pandoc](https://pandoc.org/) and the R library "devtools".

With the respository downloaded, navigate to the directory containing it using the R command line and type:

    library(devtools)
    install(".")
    
For basic operation, you require two files from Redcap output: a data file and a data dictionry file. Supposing the file name of the former is data.csv and the latter data_dict.csv, a basic report can be generated using the R commands:

    library(COVIDreportwriter)
    imported.data <- import.and.process.data(data.csv, data_dict.csv)
    generate.report(imported.data, "report.pdf", "My site")
    
This should output a report named "report.pdf". The third argument to `generate.report` is an identifier for your data, for example the name of a hospital site.


