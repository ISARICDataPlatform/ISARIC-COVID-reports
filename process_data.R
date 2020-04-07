#### Dataset inclusion flags and embargo calculation ####

# flags for inclusion of the three data files

options(warn=1)

if(verbose) cat("Setting up datasets and embargo date...\n")

embargo.length <- 14

use.uk.data <- TRUE
use.row.data <- TRUE
use.eot.data <- TRUE
ref.date <- as.Date(substr(uk.data.file, start = 6, stop  = 15))
embargo.limit <- ref.date - embargo.length

if(!use.uk.data & !use.row.data & !use.eot.data){
  stop("No data to be imported")
}

#### List of sites and countries ####

if(verbose) cat("Getting sites and countries...\n")

# This generates a list of sites and a mapping to countries, and to URLs for country flags
# This more or less has to be done by hard-coding country codes, because they do not conform to international two- or three- letter codes
# In fact different codes can be used for the same country (e.g. Ireland)

site.list <- read_csv(glue("{data.path}/{site.list.file}")) %>% 
  dplyr::mutate(site.number = map_chr(Number, function(x) substr(x, 1, 3))) %>%
  dplyr::mutate(site.name = map_chr(Number.and.name, function(x) {
    sub("^\\s+", "", substr(x, 5, nchar(x)))
  })) %>%
  dplyr::select(site.number, site.name, Country) %>%
  filter(!is.na(site.number)) %>%
  dplyr::rename(country.code = Country) %>%
  filter(!is.na(country.code)) %>%
  mutate(Country = map_chr(country.code, function(x){
    switch(x,
           "GBR" = "UK",
           "VNM" = "Viet Nam",
           "CAN" = "Canada",
           "SAU" = "Saudi Arabia",
           "USA" = "USA",
           "MEX" = "Mexico",
           "IRL" = "Ireland",
           "KOR" = "South Korea",
           "FRA" = "France",
           "FIN" = "Finland",
           "DEN" = "Denmark",
           "HKG" = "Hong Kong",
           "KHM" = "Cambodia",
           "IND" = "India",
           "NZL" = "New Zealand",
           "AUS" = "Australia",
           "MDG" = "Madagascar",
           "NSW" = "Australia",
           "MAL" = "Malawi",
           "RWA" = "Rwanda",
           "HK" = "Hong Kong",
           "KEN" = "Kenya",
           "PAK" = "Pakistan",
           "SPA" = "Spain",
           "POL" = "Poland",
           "ALG" = "Algeria",
           "ISR" = "Israel",
           "SLV" = "Slovenia",
           "GER" = "Germany",
           "KIR" = "Kiribati",
           "LEB" = "Lebanon",
           "CHN" = "China",
           "MYA" = "Burma",
           "NL" = "Netherlands",
           "NOR" = "Norway",
           "ITA" = "Italy",
           "JPN" = "Japan",
           "NED" = "Netherlands",
           "SIN" = "Singapore",
           "BAH" = "Bahrain",
           "ROM" = "Romania",
           "IRE" = "Ireland",
           "BEL" = "Belgium",
           "SLO" = "Slovakia",
           "AUT" = "Austria",
           "CZE" = "Czechia",
           "DOM" = "Dominican Republic",
           "GRE" = "Greece",
           "IRA" = "Iran",
           "JAP" = "Japan",
           "POR" = "Portugal",
           "SK" = "South Korea",
           "SWE" = "Sweden",
           "TUR" = "Turkey",
           "UK" = "UK",
           "BRA" = "Brazil",
           "EQU" = "Ecuador",
           "CHI" = "Chile",
           NA
    ) 
  })) %>%
  mutate(flag.url = map_chr(country.code, function(x){
    switch(x,
           "GBR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
           "VNM" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/vn.svg",
           "CAN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ca.svg",
           "SAU" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sa.svg",
           "USA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
           "MEX" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mx.svg",
           "IRL" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ie.svg",
           "KOR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/kr.svg",
           "FRA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fr.svg",
           "FIN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fi.svg",
           "DEN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/dk.svg",
           "HKG" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/hk.svg",
           "KHM" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/kh.svg",
           "IND" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/in.svg",
           "NZL" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/nz.svg",
           "AUS" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/au.svg",
           "MDG" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mg.svg",
           "NSW" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/au.svg",
           "MAL" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mw.svg",
           "RWA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/rw.svg",
           "HK" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/hk.svg",
           "KEN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ke.svg",
           "PAK" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pk.svg",
           "SPA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg",
           "POL" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pl.svg",
           "ALG" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/dz.svg",
           "ISR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/il.svg",
           "SLV" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/si.svg",
           "GER" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
           "KIR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ki.svg",
           "LEB" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/lb.svg",
           "CHN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cn.svg",
           "MYA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mm.svg",
           "NL" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/nl.svg",
           "NOR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/no.svg",
           "ITA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/it.svg",
           "JPN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/jp.svg",
           "NED" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/nl.svg",
           "SIN" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sg.svg",
           "BAH" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/bh.svg",
           "ROM" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/nl.svg",
           "IRE" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ro.svg",
           "BEL" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/be.svg",
           "SLO" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sk.svg",
           "AUT" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/at.svg",
           "CZE" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cz.svg",
           "DOM" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/do.svg",
           "GRE" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gr.svg",
           "IRA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ir.svg",
           "JAP" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/jp.svg",
           "POR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pt.svg",
           "SK" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/kr.svg",
           "SWE" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/se.svg",
           "TUR" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/tr.svg",
           "UK" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
           "BRA" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/br.svg",
           "EQU" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ec.svg",
           "CHI" = "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cl.svg",
           NA)
  }))

# Check if any sites had no country mapping. If this warning is triggered, add them above,

if(any(is.na(site.list$country.code))){
  missing.sites <- site.list %>% filter(is.na(country.code)) %>% pull(site.name) %>% unique()
  error.string <- glue_collapse(missing.sites, ", ")
  warning(glue("Site codes with no country mapping: {error.string}"))
}

# Check if any country codes were not mapped. If this warning is triggered, try updating the site list from Laura's most recent version.

if(any(is.na(site.list$Country))){
  missing.countries <- site.list %>% filter(is.na(Country)) %>% pull(country.code) %>% unique()
  error.string <- glue_collapse(missing.countries, ", ")
  warning(glue("Country codes with no country name mapping: {error.string}\n"))
}

# The three file imports are subtly different.

# There is a general problem with non-numeric entries in numerical columns. This function replaces them with NA with a warning

careful.as.numeric <- function(value, subjid, colname){
  out <- suppressWarnings(as.numeric(value))
  if(!is.na(value) & is.na(out)){
    warning(glue("Non-numerical value '{value}' transformed to NA for column {colname}, subject ID {subjid}"))
  }
  out
}


pcareful.as.numeric <- function(value.col, subjid, colname){
  if(is.numeric(value.col)){
    value.col
  } else {
    map2_dbl(value.col, subjid, function(x, y){
      careful.as.numeric(x, y, colname)
    })
  }
}

# Checks for dates in the future and returns NA if they are

careful.future.date <- function(value, subjid, colname){
  if(is.na(value)) {
    return(NA)
  } else if(value > today()){
    warning(glue("Future date '{as.character(value)}' transformed to NA for column {colname}, subject ID {subjid}"))
    return(NA)
  } else {
    return(as.Date(value, origin = "1970-01-01"))
  }
}

pcareful.future.date <- function(value.col, subjid, colname){
  map2_dbl(value.col, subjid, function(x, y){
    careful.future.date(x, y, colname)
  })
}

# Checks for ages (strictly) between 0 and 1 and multiples by 100

careful.fractional.age <- function(value, subjid, colname){
  if(is.na(value)) {
    return(NA)
  } else if(0 < value & 1 > value){
    warning(glue("Fractional age {value} transformed to {value*100} for column {colname}, subject ID {subjid}"))
    return(100*value)
  }
  return(value)
}

##### UK data import #####

if(use.uk.data){
  
  if(verbose) cat("Importing UK data...\n")
  
  # the UK data has some jaw-dropping differences between column formats. 
  # For a small number of radio buttons 0 is FALSE and 2 NA; for the rest 2 FALSE and 3 NA!!!
  # This function converts the former to the latter
  
  radio.button.convert <- function(x){
    map_dbl(x, function(y) {
      if(is.na(y)){
        NA
      } else {
        switch(as.character(y),
               "1" = 1,
               "0" = 2,
               "2" = 3)
      }
    })
  }
  
  # we can use the data dictionary file to identify columns that should be numeric and those that should be text

  uk.data.dict <- read_csv(glue("{data.path}/{uk.data.dict.file}"))
   
  uk.column.types <- uk.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)

  uk.data <- read_csv(glue("{data.path}/{uk.data.file}"), guess_max = 100000)

  # Columns that should be text
  text.columns.temp <- uk.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(uk.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <- text.columns[which(text.columns %>% map_lgl(function(x) !is.Date(uk.data %>% pull(x)) & !is.character(uk.data %>% pull(x))))]
  
  # Columns that should be numerical
  nontext.columns.temp <- uk.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <- colnames(uk.data)[which(map_lgl(colnames(uk.data), function(x) any(startsWith(x, glue("{nontext.columns.temp}___")))))]
  nontext.columns <-intersect(colnames(uk.data), c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <- nontext.columns[which(nontext.columns %>% map_lgl(function(x) !is.numeric(uk.data %>% pull(x))))]
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  uk.data <- uk.data %>%
    # Columns that should be character are converted to character. 
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character) 
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA. 
  # this may actually need a for loop!
  for(ntc in nontext.columns){
    uk.data <- uk.data %>% mutate_at(vars(all_of(ntc)), .funs = ~pcareful.as.numeric(., subjid = subjid, colname = ntc))
  }
  
  uk.data <- uk.data %>%
    # Country can't be got from the site names
    dplyr::mutate(Country = "UK") %>%
    # data.source is the origin of the data (UK/ROW/EOT)
    dplyr::mutate(data.source = "UK") %>%
    dplyr::mutate(site.name = redcap_data_access_group) %>%
    # These are the three radio buttons that behave differently in the raw data
    mutate_at(c("asthma_mhyn", "modliv", "mildliver"), radio.button.convert) %>%
    # 1 is SARS-2 in ROW data but MERS in UK, and vice versa. Converting UK to ROW format.
    mutate(corna_mbcaty = map_dbl(corna_mbcaty, function(x){
      if(is.na(x)){
        NA
      } else {
        switch(as.character(x),
               "1" = 2,
               "2" = 1,
               x)
      }
    })) 
  
} else {
  uk.data <- NULL
}

##### EOT data import #####

if(use.eot.data){
  
  if(verbose) cat("Importing EOT data...\n")
  
  eot.data.dict <- read_csv(glue("{data.path}/{row.data.dict.file}"))
  
  eot.column.types <- eot.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)
  
  eot.data <- read_csv(glue("{data.path}/{eot.data.file}"), guess_max = 100000)
  
  # Columns that should be text
  text.columns.temp <- eot.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(eot.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <- text.columns[which(text.columns %>% map_lgl(function(x) !is.Date(eot.data %>% pull(x)) & !is.character(eot.data %>% pull(x))))]
  
  # Columns that should be numerical
  nontext.columns.temp <- eot.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <- colnames(eot.data)[which(map_lgl(colnames(eot.data), function(x) any(startsWith(x, glue("{nontext.columns.temp}___")))))]
  nontext.columns <-intersect(colnames(eot.data), c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <- nontext.columns[which(nontext.columns %>% map_lgl(function(x) !is.numeric(eot.data %>% pull(x))))]
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  eot.data <- eot.data %>%
    # Columns that should be character are converted to character. 
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character) 
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA. 
  # this may actually need a for loop!
  for(ntc in nontext.columns){
    eot.data <- eot.data %>% mutate_at(vars(all_of(ntc)), .funs = ~pcareful.as.numeric(., subjid = subjid, colname = ntc))
  }
  
  eot.data <- eot.data %>%
    # some variables have different names in different datasets
    dplyr::rename(chrincard = chroniccard_mhyn, 
                  modliv = modliver_mhyn, 
                  mildliver = mildliv_mhyn, 
                  chronichaemo_mhyn = chronhaemo_mhyn, 
                  diabetescom_mhyn = diabetiscomp_mhyn,
                  rheumatologic_mhyn = rheumatology_mhyr) %>%
    # join in the country table
    dplyr::mutate(site.number = map_chr(redcap_data_access_group, function(x) substr(x, 1, 3))) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    # add data source
    dplyr::mutate(data.source = "EOT")
}else{
  eot.data <- NULL
}


##### ROW (ISARIC) data import #####

if(use.row.data){
  
  if(verbose) cat("Importing ISARIC data...\n")
  
  row.data.dict <- read_csv(glue("{data.path}/{row.data.dict.file}"))
  
  row.column.types <- row.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)
  
  row.data <- read_csv(glue("{data.path}/{row.data.file}"), guess_max = 100000)
  
  # Columns that should be text
  text.columns.temp <- row.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(eot.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <- text.columns[which(text.columns %>% map_lgl(function(x) !is.Date(row.data %>% pull(x)) & !is.character(row.data %>% pull(x))))]
  
  # Columns that should be numerical
  nontext.columns.temp <- row.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <- colnames(row.data)[which(map_lgl(colnames(row.data), function(x) any(startsWith(x, glue("{nontext.columns.temp}___")))))]
  nontext.columns <-intersect(colnames(row.data), c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <- nontext.columns[which(nontext.columns %>% map_lgl(function(x) !is.numeric(row.data %>% pull(x))))]
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  row.data <- row.data %>%
    # Columns that should be character are converted to character. 
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character) 
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA. 
  # this may actually need a for loop!
  for(ntc in nontext.columns){
    row.data <- row.data %>% mutate_at(vars(all_of(ntc)), .funs = ~pcareful.as.numeric(., subjid = subjid, colname = ntc))
  }
  
  row.data <- row.data %>%
    # some variables have different names in different datasets
    dplyr::rename(chrincard = chroniccard_mhyn, 
                  modliv = modliver_mhyn, 
                  mildliver = mildliv_mhyn, 
                  chronichaemo_mhyn = chronhaemo_mhyn, 
                  diabetescom_mhyn = diabetiscomp_mhyn,
                  rheumatologic_mhyn = rheumatology_mhyr,
                  icu_hoendat = hoendat) %>%
    # join in the country table
    dplyr::mutate(site.number = map_chr(redcap_data_access_group, function(x) substr(x, 1, 3))) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    dplyr::mutate(data.source = "ROW")
} else {
  row.data <- NULL
}

raw.data <- bind_rows(uk.data, row.data, eot.data) 

##### Value adjustments #####

if(verbose) cat("Setting future dates to NA...\n")

date.columns <- c("dsstdat", "agedat", "daily_dsstdat", "daily_lbdat", "hostdat", "cestdat", "dsstdtc")

for(dc in date.columns){
  raw.data <- raw.data %>% mutate_at(vars(all_of(dc)), .funs = ~pcareful.future.date(., subjid = subjid, colname = dc))
}

raw.data <- raw.data%>%
  mutate_at(date.columns, function(x) as.Date(x, origin = "1970-01-01"))


# Now, some fields need to be numerical even if the data dictionary does not think they are.
# These are just the ones of those that we _currently use_. Others should be added as required.

if(verbose) cat("Manually adjusting some fields...\n")

raw.data <- raw.data %>%
  mutate(daily_fio2_lborres = map2_dbl(subjid, daily_fio2_lborres, function(x, y) careful.as.numeric(y, x, "daily_fio2_lborres"))) %>%
  mutate(age_estimateyears = map2_dbl(subjid, age_estimateyears, function(x, y) careful.as.numeric(y, x, "age_estimateyears"))) %>%
  mutate(hodur = map2_dbl(subjid, hodur, function(x, y) careful.as.numeric(y, x, "hodur"))) %>%
  mutate(invasive_prdur = map2_dbl(subjid, invasive_prdur, function(x, y) careful.as.numeric(y, x, "invasive_prdur"))) 

# "RD816-0001" is actually two patients. As this is currently only the example of this, recode by hand.

raw.data <- raw.data %>%
  mutate(subjid = replace(subjid, subjid == "RD816-0001", glue("RD816-0001{c(rep('a',3), rep('b',22))}")))

# Replace the fractional ages

raw.data <- raw.data %>%  mutate(age_estimateyears = map2_dbl(age_estimateyears, subjid, function(x, y){
  careful.fractional.age(x, y, "age_estimateyears")
}))

# Demographic data is in the first row


if(verbose) cat("Making patient data frame...\n")

demog.data <- raw.data %>% group_by(subjid) %>% slice(1) %>% ungroup() 

# Join in a copy of all rows as the events column for each patient

event.data <- raw.data %>% group_by(subjid) %>% nest() %>% dplyr::rename(events = data) %>% ungroup()

if(verbose) cat("Joining events tables...\n")

patient.data <- demog.data %>% left_join(event.data)  %>%
  # cut out any rows where the IDs suggest test data
  filter(!str_detect(subjid, "[tT][eE][sS][tT]"))



#### Comorbitities, symptoms, and treatments ####

# read the data dictionary to get lists of columns for symptoms at admission, comorbidities, and treatments

if(verbose) cat("Making reference tables for comorbidiities, symptoms, and treatments...\n")

d.dict <- uk.data.dict %>%
  dplyr::select(`Variable / Field Name`,`Form Name`, `Field Type`, `Field Label`) %>%
  dplyr::rename(field.name = `Variable / Field Name`, form.name = `Form Name`, field.type = `Field Type`, field.label = `Field Label`)

comorbidities.colnames <- d.dict %>% filter(form.name == "comorbidities" & field.type == "radio") %>% pull(field.name)
admission.symptoms.colnames <- d.dict %>% filter(form.name == "admission_signs_and_symptoms" & startsWith(field.label, "4") & field.type == "radio" &  field.name != "bleed_ceterm_v2") %>% pull(field.name)
treatment.colnames <- d.dict %>% filter(form.name == "treatment" & field.type == "radio" & field.label != "Would you like to add another antibiotic?") %>% pull(field.name)

#### COMORBIDITIES ####

comorbidities.labels <- d.dict %>% 
  filter(form.name == "comorbidities" & field.type == "radio") %>% 
  pull(field.label) %>%
  str_match(pattern = "4b\\.[0-9]+\\.\\s(.*)") %>%
  as_tibble() %>%
  pull(2) %>%
  map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
  map_chr(function(x) sub("\\s+$", "", x))

# At some point, farting around with regexes is more trouble than its worth

comorbidities.labels[1] <- "Chronic cardiac disease"
comorbidities.labels[18] <- "Other"

comorbidities <- tibble(field = comorbidities.colnames, label = comorbidities.labels)

# Group liver disease categories

patient.data <- patient.data %>%
  mutate(liver.disease = map2_dbl(mildliver, modliv, function(mild, moderate){
    if(is.na(mild) & is.na(moderate)){
      NA
    } else if(is.na(mild)){
      moderate
    } else if(is.na(moderate)){
      mild
    } else if(mild == 1 | moderate == 1){
      1
    } else if(mild == 2 & moderate == 2){
      2
    } else {
      3
    }
  }))

comorbidities <- comorbidities %>% bind_rows(list(field = "liver.disease", label = "Liver disease")) %>%
  filter(field != "mildliver" & field != "modliv")

# Group diabetes categories

patient.data <- patient.data %>%
  mutate(diabetes = map2_dbl(diabetes_mhyn, diabetescom_mhyn, function(simple, complex){
    if(is.na(simple) & is.na(complex)){
      NA
    } else if(is.na(simple)){
      complex
    } else if(is.na(complex)){
      simple
    } else if(simple == 1 | complex == 1){
      1
    } else if(simple == 2 & complex == 2){
      2
    } else {
      2
    }
  }))

comorbidities <- comorbidities %>% bind_rows(list(field = "diabetes", label = "Diabetes")) %>%
  filter(field != "diabetes_mhyn" & field != "diabetescom_mhyn")


#### SYMPTOMS ####

# Note that bleed_ceterm_v2 is wrongly described as a radio button; it is free text

admission.symptoms.labels <- d.dict %>% 
  filter(form.name == "admission_signs_and_symptoms" & 
           startsWith(field.label, "4") & 
           field.type == "radio" &  
           field.name != "bleed_ceterm_v2" ) %>% 
  pull(field.label) %>%
  str_match(pattern = "4a\\.[0-9]+\\.[\\.]?[0-9]?\\s(.*)") %>%
  as_tibble() %>%
  pull(2) %>%
  map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
  map_chr(function(x) sub("\\s+$", "", x)) 

admission.symptoms.labels[2] <- "Cough: no sputum"

admission.symptoms <- tibble(field = admission.symptoms.colnames, label = admission.symptoms.labels)

# Cough records have not been entered coherently, and are recoded to be mutually exclusive. Someone with a cough with sputum does not have a cough without it

patient.data <- patient.data %>% 
  mutate(cough.cols = pmap(list(cough_ceoccur_v2, coughsput_ceoccur_v2, coughhb_ceoccur_v2), function(x,y,z){
    
    if(any(c(x,y,z) == 3) | any(is.na(c(x,y,z)))){
      cough.nosputum <- NA
      cough.sputum <- NA
      cough.bloodysputum <- NA
    } else if(all(c(x,y,z) == 2)){
      cough.nosputum <- 2
      cough.sputum <- 2
      cough.bloodysputum <- 2
    } else if(y == 1){
      cough.nosputum <- 2
      if(z == 1){
        cough.sputum <- 2
        cough.bloodysputum <- 1
      } else {
        cough.sputum <- 1
        cough.bloodysputum <- 2
      } 
    } else if(z == 1) {
      cough.nosputum <- 2
      cough.sputum <- 2
      cough.bloodysputum <- 1
    } else {
      cough.nosputum <- x
      cough.sputum <- 2
      cough.bloodysputum <- 2
    }
    list(cough.sputum = cough.sputum, cough.nosputum = cough.nosputum, cough.bloodysputum = cough.bloodysputum)
  })) %>% 
  { bind_cols(., bind_rows(!!!.$cough.cols)) } %>%
  dplyr::select(-cough.cols) %>%
  mutate(cough.any = pmap_dbl(list(cough.nosputum, cough.sputum, cough.bloodysputum), function(x,y,z){
    if(is.na(x)){
      NA
    } else {
      if(all(c(x,y,z) == 2)){
        2
      } else {
        1
      }
    }
  }))

admission.symptoms <- admission.symptoms %>% bind_rows(list(field = "cough.nosputum", label = "Cough (no sputum)")) %>%
  bind_rows(list(field = "cough.sputum", label = "Cough (with sputum)")) %>%
  bind_rows(list(field = "cough.bloodysputum", label = "Cough (bloody sputum / haemoptysis)")) %>%
  filter(field != "cough_ceoccur_v2" & field != "coughsput_ceoccur_v2" & field !="coughhb_ceoccur_v2")

# Group shortness of breath categories

patient.data <- patient.data %>%
  mutate(shortness.breath = map2_dbl(shortbreath_ceoccur_v2, lowerchest_ceoccur_v2, function(adult, paed){
    if(is.na(adult) & is.na(paed)){
      NA
    } else if(is.na(adult)){
      paed
    } else if(is.na(paed)){
      adult
    } else if(adult == 1 | paed == 1){
      1
    } else if(adult == 2 & paed == 2){
      2
    } else {
      2
    }
  }))

admission.symptoms <- admission.symptoms %>% bind_rows(list(field = "shortness.breath", label = "Shortness of breath")) %>%
  filter(field != "shortbreath_ceoccur_v2" & field != "lowerchest_ceoccur_v2")



#### TREATMENTS ####

treatment.labels <- d.dict %>% 
  filter(form.name == "treatment" & field.type == "radio" & field.label != "Would you like to add another antibiotic?") %>%
  pull(field.label) %>%
  str_match(pattern = "6\\.[0-9]+[\\.]?[0-9]?[\\.]?\\s(.*)") %>%
  as_tibble() %>%
  pull(2) %>%
  map_chr(function(x) str_split_fixed(x, "\\(", Inf)[1]) %>%
  # I don't know why you can't figure this out nicely. Do it later.
  map_chr(function(x) sub("\\s+$", "", x)) %>%
  map_chr(function(x) sub("\\?+$", "", x)) %>%
  map_chr(function(x) sub("\\s+$", "", x)) 

treatment.labels[9] <- "Inhaled nitric oxide"
treatment.labels[10] <- "Tracheostomy" 
treatment.labels[14] <- "Other"

treatments <- tibble(field = treatment.colnames, label = treatment.labels)



# This function extracts a named column from the events table. If sanity.check == T it expects only one non-NA value in that column

extract.named.column.from.events <- function(events.tibble, column.name, sanity.check = F){
  out <- events.tibble %>% filter(!is.na(!!as.name(column.name))) %>% pull(column.name)
  
  if(length(out) > 1 & sanity.check){
    stop("Too many entries")
  } else if(length(out) == 0){
    NA
  } else {
    out
  }
}

# Add new columns with more self-explanatory names as needed

if(verbose) cat("Adding new columns...\n")

patient.data <- patient.data %>%
  # check if symptoms, comorbidities and treatments were actually recorded
  dplyr::mutate(symptoms.recorded = pmap_lgl(list(!!!rlang::parse_exprs(admission.symptoms$field)), ~any(!is.na(c(...))))) %>%
  dplyr::mutate(comorbidities.recorded = pmap_lgl(list(!!!rlang::parse_exprs(comorbidities$field)), ~any(!is.na(c(...))))) %>%
  dplyr::mutate(treatments.recorded = map_lgl(events, function(x){
    temp <- x %>% mutate(tr = pmap_lgl(list(!!!rlang::parse_exprs(treatments$field)), ~any(!is.na(c(...)))))
    any(temp$tr)
  })) %>%
  # exit date is whenever the patient leaves the site. @todo look at linking up patients moving between sites
  dplyr::mutate(exit.date = map2_chr(subjid, events, function(y, x){
    outcome.rows <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsstdtc)) 
    if(nrow(outcome.rows) == 0){
      return(NA)
    } else {
      if(nrow(outcome.rows) > 1) {
        warning(glue("Multiple exit dates for patient {y}"))
      }
      return(outcome.rows  %>% slice(nrow(outcome.rows)) %>% pull(dsstdtc) %>% as.character())
    }
  })) %>%
  dplyr::mutate(exit.date = ymd(exit.date)) %>%
  # exit code is the reason for leaving the site.
  dplyr::mutate(exit.code = map2_chr(subjid, events, function(y,x){
    outcome.rows <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm))
    if(nrow(outcome.rows) == 0){
      return(NA)
    } else {
      return(switch(outcome.rows %>% pull(dsterm) %>% as.character(),
                    "1" = "discharge",
                    "2" = "hospitalisation",
                    "3" = "transfer",
                    "4" = "death",
                    "5" = "transfer.palliative",
                    "6" = "unknown"))
    }
  })) %>%
  # censorship occurs if the patient is still in site
  dplyr::mutate(censored = map2_lgl(subjid, events, function(y, x){
    if(x %>% pull(redcap_event_name) %>% startsWith("discharge") %>% any() %>% not()){
      # still in site
      return(TRUE)
    } else {
      temp <- x %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) 
      if(nrow(temp) == 0){
        return(TRUE)
      } else {
        return(temp %>% pull(dsterm) %>% is.na() %>% any())
      }
    }
  })) %>%
  # outcome is death, discharge or other for transfers etc
  dplyr::mutate(outcome = map2_chr(censored, events, function(x, y){
    if(x){
      return("censored")
    } else {
      return(switch(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsterm) %>% as.character(),
                    "1" = "discharge",
                    "4" = "death",
                    NA))
    }
  })) %>%
  dplyr::mutate(outcome.date.known = map2_dbl(outcome, events, function(x, y){
    if(!(x %in% c("discharge", "death"))){
      return(2)
    } else {
      return(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtcyn))
    }
  })) %>%
  dplyr::mutate(outcome.date = map2_chr(outcome, events, function(x, y){
    if(!(x %in% c("discharge", "death"))){
      return(NA)
    } else {
      if(length(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtc) %>% as.character()) > 1){
        stop("Multiple outcome dates?")
      }
      return(y %>% filter((startsWith(redcap_event_name, "dischargeoutcome") | startsWith(redcap_event_name, "dischargedeath")) & !is.na(dsterm)) %>% pull(dsstdtc) %>% as.character())
    }
  })) %>%
  # oh for map_date!!!
  dplyr::mutate(outcome.date = ymd(outcome.date)) %>%
  # Death and discharge dates are NA if the patient is not dead/discharged
  dplyr::mutate(death.date = map2_chr(outcome, outcome.date, function(x,y){
    if(is.na(y)){
      NA
    }
    ifelse(x=="death", as.character(y), NA)
  })) %>%
  dplyr::mutate(death.date = ymd(death.date)) %>%
  dplyr::mutate(discharge.date = map2_chr(outcome, outcome.date, function(x,y){
    if(is.na(y)){
      NA
    }
    ifelse(x=="discharge", as.character(y), NA)
  }))  %>%
  dplyr::mutate(discharge.date = ymd(discharge.date)) %>%
  # Consolidated age is the exact age at enrolment if this is present. Otherwise it is taken from the estimated age column. 
  dplyr::mutate(consolidated.age = pmap_dbl(list(age_estimateyears, agedat, dsstdat), function(ageest, dob, doa){
    if(is.na(dob)){
      ageest
    } else {
      floor(decimal_date(doa) - decimal_date(dob))
    }
  })) %>%
  # Age groups in five and ten year incerements
  dplyr::mutate(agegp5 = cut(consolidated.age, c(seq(0,90,by = 5),120), right = FALSE)) %>%
  dplyr::mutate(agegp5 = fct_relabel(agegp5, function(a){
    # make nicer labels
    temp <- substr(a, 2, nchar(a) -1 )
    temp <- str_replace(temp, ",", "-")
    str_replace(temp, "90-120", "90+")
  })) %>%
  dplyr::mutate(agegp10 = cut(consolidated.age, c(seq(0,70,by = 10),120), right = FALSE)) %>%
  dplyr::mutate(agegp10 = fct_relabel(agegp10, function(a){
    # make nicer labels
    temp <- substr(a, 2, nchar(a) -1 )
    temp <- str_replace(temp, ",", "-")
    str_replace(temp, "70-120", "70+")
  })) %>%
  dplyr::mutate(ICU.admission.date = map_chr(events, function(x) as.character(extract.named.column.from.events(x, "icu_hostdat", TRUE)))) %>%
  dplyr::mutate(ICU.admission.date = ymd(ICU.admission.date)) %>%
  dplyr::mutate(ICU.discharge.date = map_chr(events, function(x) as.character(extract.named.column.from.events(x, "icu_hoendat", TRUE)))) %>%
  dplyr::mutate(ICU.discharge.date = ymd(ICU.discharge.date)) %>%
  dplyr::mutate(ICU.duration = map_dbl(events, function(x) extract.named.column.from.events(x, "hodur", TRUE))) %>%
  dplyr::mutate(IMV.duration  = map_dbl(events, function(x) extract.named.column.from.events(x, "invasive_prdur", TRUE))) %>%
  # these are just for the sake of having more self-explanatory column names
  dplyr::mutate(admission.date = hostdat) %>%
  dplyr::mutate(enrolment.date = dsstdat) %>%
  dplyr::mutate(onset.date = cestdat) %>%
  # start.date is either the admission date or the date of symptom onset, whichever is _later_ - hospital cases are counted from disease onset
  dplyr::mutate(start.date = map2_chr(admission.date, onset.date, function(x,y){
    suppressWarnings(as.character(max(x,y, na.rm = T)))
  })) %>%
  dplyr::mutate(start.date = ymd(start.date)) %>%
  dplyr::mutate(antiviral.any = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmyn", TRUE) )) %>%
  dplyr::mutate(antiviral.Ribavirin = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___1", TRUE) )) %>%
  dplyr::mutate(antiviral.Lopinavir.Ritonvir = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___2", TRUE) )) %>%
  dplyr::mutate(antiviral.Interferon.alpha = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___3", TRUE) )) %>%
  dplyr::mutate(antiviral.Interferon.beta = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___4", TRUE) )) %>%
  dplyr::mutate(antiviral.Neuraminidase.inhibitors = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___5", TRUE) )) %>%
  dplyr::mutate(antiviral.other = map_dbl(events, function(x) extract.named.column.from.events(x, "antiviral_cmtrt___6", TRUE) ))   %>%
  dplyr::mutate(antiviral.freetext = map_chr(events, function(x) extract.named.column.from.events(x, "antiviral_cmtype", TRUE) )) %>%
  dplyr::mutate(antibiotic.any = map_dbl(events, function(x) extract.named.column.from.events(x, "antibiotic_cmyn", TRUE) )) %>%
  dplyr::mutate(antifungal.any = map_dbl(events, function(x) extract.named.column.from.events(x, "antifung_cmyn", TRUE) )) %>%
  dplyr::mutate(steroid.any = map_dbl(events, function(x) extract.named.column.from.events(x, "corticost_cmyn", TRUE) ))

compareNA <- function(v1,v2) {
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

###### Date wrangling ######

# This is the function to return information about a patient's time on an intervention (e.g. IMV) from both daily and final forms

if(verbose) cat("Wrangling dates for treatment modalities...\n")

process.event.dates <- function(events.tbl, summary.status.name, daily.status.name){
  subtbl <- events.tbl %>% dplyr::select(dsstdat, daily_dsstdat, !!summary.status.name, !!daily.status.name )
  
  colnames(subtbl)[3:4] <- c("summary.col","daily.col")
  
  check.rows <- subtbl %>% filter(!is.na(summary.col) | !is.na(daily.col)) 
  if(nrow(check.rows) == 0){
    ever <- NA
  } else {
    ever <- any((check.rows$summary.col == 1) | (check.rows$daily.col == 1), na.rm = T)
  }
  
  
  rows <- subtbl %>% filter(!is.na(daily.col) & !(is.na(dsstdat) & is.na(daily_dsstdat))) %>%
    mutate(consolidated.dssdat = map2_chr(dsstdat, daily_dsstdat, function(x,y) ifelse(is.na(y), as.character(x), as.character(y)))) %>%
    mutate(consolidated.dssdat = ymd(consolidated.dssdat))
  
  if(nrow(rows) == 0){
    # no reference 
    start.date <- NA
    end.date <- NA
    first.after.date <- NA
    multiple.periods <- NA
  } else if(!any(rows$daily.col == 1)){
    # no "yes" 
    start.date <- NA
    end.date <- NA
    first.after.date <- NA
    multiple.periods <- NA
  } else {
    start.date <- rows %>% filter(daily.col == 1) %>% slice(1) %>% pull(consolidated.dssdat)
    last.date <- rows %>% filter(daily.col == 1) %>% slice(n()) %>% pull(consolidated.dssdat)
    if(is.na(start.date)){
      # sometimes happens. ever = TRUE but dates unknown
      end.date <- NA
      first.after.date <- NA
    } else if(last.date == rows %>% filter(!is.na(consolidated.dssdat)) %>% slice(n()) %>% pull(consolidated.dssdat)){
      # Patient was on at last report
      end.date <- NA
      first.after.date <- NA
    } else {
      # They were off at the next report
      end.date <- last.date
      
      next.report.row <- max(which(rows$consolidated.dssdat == last.date)) + 1
      first.after.date <- rows$consolidated.dssdat[next.report.row]
    }
    if(nrow(rows)<=2 | is.na(start.date)){
      multiple.periods <- F
    } else {
      # we are looking for the number of instances of 2 then 1. If this is more than 1, or more than 0 with the first report on NIMV, 
      # then the patient went on multiple times
      temp <- map_dbl(2:nrow(rows), function(x)  rows$daily.col[x] - rows$daily.col[x-1] )
      multiple.periods <- length(which(temp == -1)) < 1 | (length(which(temp == -1)) > 0 &  rows$daily.col[1] == 1)
    }
  }
  list(ever = ever, start.date = start.date, end.date = end.date, first.after.date = first.after.date, multiple.periods = multiple.periods)
}




patient.data <- patient.data %>% 
  # NIV
  mutate(NIMV.cols  = map(events, function(el){
    process.event.dates(el, "noninvasive_proccur", "daily_noninvasive_prtrt")
  })) %>%
  mutate(NIMV.cols = map(NIMV.cols, function(x){
    names(x) <- glue("NIMV.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$NIMV.cols)) } %>%
  dplyr::select(-NIMV.cols) %>%
  # IMV
  mutate(IMV.cols  = map2(subjid, events, function(id, el){
    process.event.dates(el, "invasive_proccur", "daily_invasive_prtrt")
  })) %>%
  mutate(IMV.cols = map(IMV.cols, function(x){
    names(x) <- glue("IMV.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$IMV.cols)) } %>%
  dplyr::select(-IMV.cols) %>%
  # ECMO
  mutate(ECMO.cols  = map2(subjid, events, function(id, el){
    process.event.dates(el, "extracorp_prtrt", "daily_ecmo_prtrt")
  })) %>%
  mutate(ECMO.cols = map(ECMO.cols, function(x){
    names(x) <- glue("ECMO.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$ECMO.cols)) } %>%
  dplyr::select(-ECMO.cols) %>%
  # ICU 
  mutate(ICU.cols  = map(events, function(el){
    process.event.dates(el, "icu_hoterm", "daily_hoterm")
  })) %>%
  mutate(ICU.cols = map(ICU.cols, function(x){
    names(x) <- glue("ICU.{names(x)}")
    x
  })) %>%
  # RRT
  { bind_cols(., bind_rows(!!!.$ICU.cols)) } %>%
  dplyr::select(-ICU.cols) %>% 
  mutate(RRT.cols  = map(events, function(el){
    process.event.dates(el, "rrt_prtrt", "daily_rrt_cmtrt")
  })) %>%
  mutate(RRT.cols = map(RRT.cols, function(x){
    names(x) <- glue("RRT.{names(x)}")
    x
  })) %>%
  # Inotrope
  { bind_cols(., bind_rows(!!!.$RRT.cols)) } %>%
  dplyr::select(-RRT.cols) %>%
  mutate(Inotrope.cols  = map(events, function(el){
    process.event.dates(el, "inotrop_cmtrt", "daily_inotrope_cmyn")
  })) %>%
  mutate(Inotrope.cols = map(Inotrope.cols, function(x){
    names(x) <- glue("Inotrope.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$Inotrope.cols)) } %>%
  dplyr::select(-Inotrope.cols)  

# O2 ever - more complex

patient.data <- patient.data %>%
  mutate(O2.ever = map_lgl(events, function(x){
    
    x2 <- x %>% filter(!is.na(daily_fio2_lborres) | !is.na(daily_nasaloxy_cmtrt) | !is.na(x$daily_nasaloxy_cmtrt))
    if(nrow(x2) == 0){
      NA 
    } else {
      x2 <- x2 %>% mutate(O2.ever = daily_fio2_lborres > .21 | daily_nasaloxy_cmtrt == 1 | oxygen_cmoccur == 1)
      any(x2$O2.ever, na.rm = T)
    }
  }))

# ICU.start.data and ICU.end.date hae values from daily sheets but omit some 
# values from the outcome sheet. Where available, the outcome sheet ispreferred.
patient.data$ICU.start.date[is.na(patient.data$ICU.admission.date) == FALSE] <- 
  patient.data$ICU.admission.date[is.na(patient.data$ICU.admission.date) == FALSE]
patient.data$ICU.end.date[is.na(patient.data$ICU.discharge.date) == FALSE] <- 
  patient.data$ICU.discharge.date[is.na(patient.data$ICU.discharge.date) == FALSE] 
# Make ICU.admission.date and ICU.discharge.date match the new fields for consistency
patient.data$ICU.admission.date <- patient.data$ICU.start.date
patient.data$ICU.discharge.date <- patient.data$ICU.end.date
 # if we can get those from the daily forms then we can get this
patient.data$ICU.duration[is.na(patient.data$ICU.duration) == TRUE] <- 
  as.numeric(difftime(
    patient.data$ICU.end.date[is.na(patient.data$ICU.duration) == TRUE], 
    patient.data$ICU.start.date[is.na(patient.data$ICU.duration) == TRUE],  
    unit="days"
  ))

###### Calculation of time periods #####

patient.data <- patient.data %>%
  dplyr::mutate(NIMV.duration = map2_dbl(NIMV.end.date, NIMV.start.date, function(x,y){
    as.numeric(difftime(x, y,  unit="days"))
  })) %>%
  dplyr::mutate(admission.to.exit = as.numeric(difftime(exit.date, admission.date,  unit="days")),
                onset.to.admission = as.numeric(difftime(admission.date, onset.date, unit="days")),
                start.to.exit = as.numeric(difftime(exit.date, start.date,  unit="days"))) %>%
  dplyr::mutate(admission.to.censored = map2_dbl(admission.to.exit, admission.date, function(x,y){
    if(is.na(x)){
      # censored until today
      as.numeric(difftime(ref.date, y,  unit="days"))
    }
    else {
      NA
    }
  })) %>%
  dplyr::mutate(start.to.censored = map2_dbl(admission.to.exit, start.date, function(x,y){
    if(is.na(x)){
      as.numeric(difftime(ref.date, y,  unit="days"))
    }
    else {
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.death = pmap_dbl(list(dsstdtcyn, dsstdtc, admission.date), function(x, y, z){
    if(compareNA(4,x )){
      as.numeric(difftime(y, z,  unit="days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(start.to.death = pmap_dbl(list(dsstdtcyn, dsstdtc, start.date), function(x, y, z){
    if(compareNA(4,x )){
      as.numeric(difftime(y, z,  unit="days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.discharge = pmap_dbl(list(dsstdtcyn, dsstdtc, admission.date), function(x, y, z){
    if(compareNA(1, x)){
      as.numeric(difftime(y, z,  unit="days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(start.to.discharge = pmap_dbl(list(dsstdtcyn, dsstdtc, start.date), function(x, y, z){
    if(compareNA(1, x)){
      as.numeric(difftime(y, z,  unit="days"))
    } else {
      NA
    }
  })) %>%
  mutate(admission.to.ICU = as.numeric(difftime(ICU.admission.date, admission.date, unit="days")),
         admission.to.IMV = as.numeric(difftime(IMV.start.date, admission.date, unit="days")),
         admission.to.NIMV = as.numeric(difftime(NIMV.start.date, admission.date, unit="days")),
         start.to.ICU = as.numeric(difftime(ICU.admission.date, start.date, unit="days")),
         start.to.IMV = as.numeric(difftime(IMV.start.date, start.date, unit="days")),
         start.to.NIMV = as.numeric(difftime(NIMV.start.date, start.date, unit="days"))) 

##### Untangling COVID test fields #####

if(verbose) cat("Untangling SARS-CoV-19 test results...\n")

patient.data <- patient.data %>% 
  mutate(cov.test.result = 
           map_dbl(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "corna_mbcat"))) %>%
  mutate(cov.test.organism = 
           map_dbl(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "corna_mbcaty"))) %>%
  mutate(cov.other.organism.freetext = 
           map_chr(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "coronaother_mborres"))) %>%
  mutate(any.test.result = 
           map(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "mborres"))) %>%
  mutate(any.test.freetext = 
           map(patient.data$events, function(x) extract.named.column.from.events(events.tibble = x, column.name = "mbtestcd")))


# Function to check for text that looks like it means SARS-CoV-2. Lots of typos in these fields. This may catch other coronaviruses.

probable.cov.freetext <- function(text){
  str_detect(text, "[sS][aA][rR][sS]") |
    str_detect(text, "[cC][oO]r?[vV]") |
    str_detect(text, "[nN][cC][oO][cC]") |
    str_detect(text, "[nN][cC][vV][oO]") |
    str_detect(text, "[cC][oO][iI][vV][dD]") |
    str_detect(text, "[cC][vV][iI][dD]") | 
    str_detect(text, "[cC][oO][vV][oO][iI][dD]") |
    str_detect(text, "[cC]or?n?o?n[ao]vir[iu]s") |
    str_detect(text, "[cC]OR?N?O?N[AO]VIR[IU]S") |
    str_detect(text, "[cC]or?n?o?n[ao]\\s[Vv]ir[iu]s") |
    str_detect(text, "[cC]OR?N?O?N[AO]\\sVIR[IU]S") |
    str_detect(text, "[Cc][Oo][Rr][Oo][Nn][Aa]")
}

# IMPORTANT: At the moment "positive.COV19.test" refers to "evidence for a positive test". FALSE is not evidence for a negative test.

patient.data <- patient.data %>%
  mutate(positive.COV19.test = pmap_lgl(list(subjid, cov.test.result, cov.test.organism, cov.other.organism.freetext, any.test.result, any.test.freetext), 
                                        function(id, ctest, cpos, cfreetext, anytest, anyfreetext){
                                          cpos.covlikely = probable.cov.freetext(cfreetext)
                                          anytest2 <- as.numeric(anytest)
                                          anyfreetext2 <- anyfreetext[which(!is.na(anytest))]
                                          if((ctest %in% c(1,2)) & ((!is.na(cpos) & cpos == 1) | (cpos == 888 & !is.na(cfreetext) & cpos.covlikely))){
                                            # You have COVID if:
                                            # 1) Your SARS-CoV-2 test result is a weak or strong positive for SARS-CoV-2
                                            # OR 2) Your SARS-CoV-2 test result is a weak or strong positive for "another virus" but the free text says that virus is actually SARS-CoV-2(!)
                                            return(TRUE)
                                          } else if(ctest == 0 | is.na(ctest) | (ctest  %in% c(1,2) & (is.na(cpos) | cpos == 2 | (!is.na(cpos.covlikely) & !cpos.covlikely)))){
                                            # Patients get here if:
                                            # 1) They have a negative SARS-CoV-2 test
                                            # OR 2) They have an NA entry for SARS-CoV-2 test result
                                            # OR 3)They have a weak or strong positive test for SARS-CoV-2 AND
                                            #   3A) The entry for virus species is NA 
                                            #   OR 3B) The entry for virus species is "MERS"
                                            #   OR 3C) The free text is missing or says that virus is not SARS-CoV-2
                                            if(length(anyfreetext) > 0){
                                              # This examines the free text from the daily form, looking for COVID-like words
                                              otherpos.covlikely = map_lgl(anyfreetext2, probable.cov.freetext)
                                              if(any(!is.na(otherpos.covlikely) & !is.na(anytest2) & otherpos.covlikely & anytest2 == 1)){
                                                # words found and the test was positive
                                                return(TRUE)
                                              } else {
                                                # words not found or the test was negative
                                                return(FALSE)
                                              }
                                            }
                                          } else {
                                            return(FALSE)
                                          }
                                        }))


# Minimal table for the unebargoed data

unembargoed.data <- patient.data %>% dplyr::select(subjid, Country, site.name, start.date, admission.date, outcome)

# Data for the by-country summary

countries.and.sites <-  unembargoed.data %>%
  group_by(Country, site.name) %>%
  dplyr::summarise(n.sites = 1) %>%
  dplyr::summarise(n.sites = sum(n.sites)) %>%
  filter(!is.na(Country))

# Impose the embargo

if(verbose) cat("Imposing the embargo...\n")

patient.data <-  patient.data %>%
  filter(dsstdat <= embargo.limit) 

# Save to disk

if(verbose) cat("Saving to disk...\n")

save(unembargoed.data, patient.data, countries.and.sites, admission.symptoms, comorbidities, embargo.limit, treatments, file = glue("{code.path}/patient_data_{ref.date)")

