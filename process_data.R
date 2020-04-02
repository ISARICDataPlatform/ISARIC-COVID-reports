

#### Dataset inclusion flags ####

# flags for inclusion of the three data files

embargo.length <- 14

use.uk.data <- TRUE
use.row.data <- TRUE
use.eot.data <- TRUE
ref.date <- as.Date(substr(uk.data.file, start = 6, stop  = 15))
embargo.limit <- ref.date - embargo.length

if(!use.uk.data & !use.row.data & !use.eot.data){
  stop("No data to be imported")
}

#### List of sites ####

site.list <- read_csv(glue("{data.path}/{site.list.file}")) %>% 
  dplyr::mutate(site.number = map_chr(`Site Number`, function(x) substr(x, 1, 3))) %>%
  dplyr::mutate(site.name = map_chr(`Site Number_1`, function(x) {
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


if(use.uk.data){
  
  # the UK data has some jaw-dropping differences between column formats. For a small number of radio buttons 0 is FALSE and 2 NA; for the rest 2 FALSE and 3 NA!!!
  
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
  
  
  uk.data <- read_csv(glue("{data.path}/{uk.data.file}"), guess_max = 30000) %>%
    dplyr::mutate(age_estimateyears = as.numeric(age_estimateyears)) %>%
    # some fields are all-numerical in some files but not others. But using col_types is a faff for this many columns. This is a hack for now. @todo
    dplyr::mutate_at(vars(ends_with("orres")), as.character) %>%
    dplyr::mutate(age_estimateyears = as.numeric(age_estimateyears)) %>%
    dplyr::mutate(apvs_weight = as.numeric(apvs_weight)) %>%
    dplyr::mutate(Country = "UK") %>%
    dplyr::mutate(data.source = "UK") %>%
    dplyr::mutate(site.name = redcap_data_access_group) %>%
    mutate_at(c("asthma_mhyn", "modliv", "mildliver"), radio.button.convert) %>%
    # 1 is SARS-2 in ROW data but MERS in UK, and vice versa.
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

if(use.eot.data){
  eot.data <- read_csv(glue("{data.path}/{eot.data.file}"), guess_max = 10000) %>%
    dplyr::mutate_at(vars(ends_with("dat")), ymd) %>%              # contains() raises flags
    dplyr::mutate(hostdat_transfer = ymd(hostdat_transfer),
                  erendat_2 = ymd(erendat_2),
                  dsstdtc = ymd(dsstdtc),
                  date = ymd(date)) %>% 
    dplyr::mutate_at(vars(ends_with("orres")), as.character) %>%
    dplyr::rename(chrincard = chroniccard_mhyn, 
                  modliv = modliver_mhyn, 
                  mildliver = mildliv_mhyn, 
                  chronichaemo_mhyn = chronhaemo_mhyn, 
                  diabetescom_mhyn = diabetiscomp_mhyn,
                  rheumatologic_mhyn = rheumatology_mhyr) %>%
    dplyr::mutate(site.number = map_chr(redcap_data_access_group, function(x) substr(x, 1, 3))) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    add_column(agedat = NA) %>%
    dplyr::mutate(data.source = "EOT")
}else{
  eot.data <- NULL
}


#### Manual date correction ####

if(use.row.data){
  row.data <- read_csv(glue("{data.path}/{row.data.file}"), guess_max = 10000) %>% 
    
    mutate(daily_lbdat = replace(daily_lbdat, 306,"01/01/2020")) %>% # NOTE MANUAL DATE CORRECTION
    
    # some fields are all-numerical in some files but not others. But using col_types is a faff for this many columns. This is a hack for now. @todo
    dplyr::mutate_at(vars(ends_with("orres")), as.character) %>%
    dplyr::mutate_at(vars(ends_with("dat")), ymd) %>%              # contains() raises flags
    dplyr::mutate(hostdat_transfer = ymd(hostdat_transfer),
                  erendat_2 = ymd(erendat_2),
                  dsstdtc = ymd(dsstdtc),
                  date = ymd(date)) %>%
    # different column names for some comorbidities
    dplyr::rename(chrincard = chroniccard_mhyn, 
                  modliv = modliver_mhyn, 
                  mildliver = mildliv_mhyn, 
                  chronichaemo_mhyn = chronhaemo_mhyn, 
                  diabetescom_mhyn = diabetiscomp_mhyn,
                  rheumatologic_mhyn = rheumatology_mhyr,
                  icu_hoendat = hoendat) %>%
    dplyr::mutate(site.number = map_chr(redcap_data_access_group, function(x) substr(x, 1, 3))) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    add_column(agedat = NA) %>%
    dplyr::mutate(data.source = "ROW")
} else {
  row.data <- NULL
}

date.sanity.check <- function(date) {
  as.Date(ifelse(date > today(), as.Date(NA), date),  origin = "1970-01-01")
}

raw.data <- bind_rows(uk.data, row.data, eot.data) %>%
  dplyr::mutate(dsstdat = date.sanity.check(ymd(dsstdat)),
                agedat = date.sanity.check(ymd(agedat)), 
                daily_dsstdat = date.sanity.check(ymd(daily_dsstdat)), 
                daily_lbdat = date.sanity.check(ymd(daily_lbdat)),
                hostdat = date.sanity.check(ymd(hostdat)),
                cestdat = date.sanity.check(ymd(cestdat)),
                dsstdtc = date.sanity.check(ymd(dsstdtc)))
  

# Demographic data is in the first row

demog.data <- raw.data %>% group_by(subjid) %>% slice(1) %>% ungroup() %>%
  # replace the fractional ages
  mutate(age_estimateyears = map_dbl(age_estimateyears, function(x){
    if(is.na(x)){
      NA
    } else if(0 < x & 1 > x){
      x*100
    } else {
      x
    }
  }))

# Clinical data is in subsequent rows but also _sometimes_ in the first row. So the events column still contains a copy of the first row.

event.data <- raw.data %>% group_by(subjid) %>% nest() %>% dplyr::rename(events = data) %>% ungroup() %>% ungroup()

patient.data <- demog.data %>% left_join(event.data)  %>%
  filter(!str_detect(subjid, "TEST"))

#### Comorbitities, symptoms, and treatments ####

# read the data dictionary to get lists of columns for symptoms at admission, comorbidities, and treatments

d.dict <- read_csv(glue("{data.path}/{data.dict.file}")) %>%
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

# these have not been entered coherently, replace them. Someone with a cough with sputum does not have a cough without it

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
  # exit code is the reason for leaving the site. Unsure what "hospitalisation" means but it's yet to appear in actual data
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
  # censorship occurs if either the patient is still in site or is moved offsite without a death or discharge code
  dplyr::mutate(censored = map2_lgl(subjid, events, function(y, x){
    if(x %>% pull(redcap_event_name) %>% startsWith("discharge") %>% any() %>% not()){
      # still in site
      return(TRUE)
    } else {
      # Anything other than discharge or death is "censored"
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
  }))  %>%
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
    as.character(max(x,y, na.rm = T))
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

# More complicated date wrangling (IMV, NIMV)

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
  mutate(NIMV.cols  = map(events, function(el){
    process.event.dates(el, "noninvasive_proccur", "daily_noninvasive_prtrt")
  })) %>%
  mutate(NIMV.cols = map(NIMV.cols, function(x){
    names(x) <- glue("NIMV.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$NIMV.cols)) } %>%
  dplyr::select(-NIMV.cols) %>%
  mutate(IMV.cols  = map2(subjid, events, function(id, el){
    process.event.dates(el, "invasive_proccur", "daily_invasive_prtrt")
  })) %>%
  mutate(IMV.cols = map(IMV.cols, function(x){
    names(x) <- glue("IMV.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$IMV.cols)) } %>%
  dplyr::select(-IMV.cols) %>%
  mutate(ECMO.cols  = map2(subjid, events, function(id, el){
    process.event.dates(el, "extracorp_prtrt", "daily_ecmo_prtrt")
  })) %>%
  mutate(ECMO.cols = map(ECMO.cols, function(x){
    names(x) <- glue("ECMO.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$ECMO.cols)) } %>%
  dplyr::select(-ECMO.cols) %>%
  mutate(ICU.cols  = map2(subjid,events, function(id, el){
    process.event.dates(el, "icu_hoterm", "daily_hoterm")$ever
  })) %>%
  mutate(ICU.ever = unlist(ICU.cols)) %>%
  dplyr::select(-ICU.cols)

patient.data <- patient.data %>%
  mutate(O2.ever = map_lgl(events, function(x){
    x$O2.ever <- 0
    x$O2.ever[x$daily_fio2_lborres > .21] <- 1
    # to add O2 flow if it becomes available
    x$O2.ever[x$daily_nasaloxy_cmtrt == 1] <- 1
    x$O2.ever[x$oxygen_cmoccur == 1] <- 1
    any(x$O2.ever)
  }))

patient.data <- patient.data %>% 
  mutate(ICU.cols  = map(events, function(el){
    process.event.dates(el, "icu_hoterm", "daily_hoterm")
  })) %>%
  mutate(ICU.cols = map(ICU.cols, function(x){
    names(x) <- glue("ICU.{names(x)}")
    x
  })) %>%
  { bind_cols(., bind_rows(!!!.$ICU.cols)) } %>%
  dplyr::select(-ICU.cols)

patient.data$ICU.start.date[is.na(patient.data$ICU.admission.date) == FALSE] <- 
  patient.data$ICU.admission.date
patient.data$ICU.end.date[is.na(patient.data$ICU.discharge.date) == FALSE] <- 
  patient.data$ICU.discharge.date
# Make ICU.admission.date and ICU.discharge.date match the new fields for consistency
patient.data$ICU.admission.date <- patient.data$ICU.start.date
patient.data$ICU.discharge.date <- patient.data$ICU.end.date
# if we can get those from the daily forms then we can get this
patient.data <- patient.data %>% 
  mutate(ICU.duration = replace(ICU.duration, 
                                is.na(ICU.duration) & !is.na(ICU.end.date) & !is.na(ICU.start.date),
                                as.numeric(difftime(ICU.end.date, ICU.start.date,  unit="days"))
                                ))

patient.data <- patient.data %>% 
  mutate(RRT.cols  = map(events, function(el){
    process.event.dates(el, "rrt_prtrt", "daily_rrt_cmtrt")
  })) %>%
  mutate(RRT.cols = map(RRT.cols, function(x){
    names(x) <- glue("RRT.{names(x)}")
    x
  })) %>%
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

# calculation of time periods @todo NIMV, IMV

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

unembargoed.data <- patient.data %>% dplyr::select(subjid, Country, site.name, start.date, outcome)

countries.and.sites <-  unembargoed.data %>%
  group_by(Country, site.name) %>%
  dplyr::summarise(n.sites = 1) %>%
  dplyr::summarise(n.sites = sum(n.sites)) %>%
  filter(!is.na(Country))


patient.data <-  patient.data %>%
  filter(dsstdat <= embargo.limit) # exclude all cases on or after embargo limit

save(unembargoed.data, patient.data, countries.and.sites, admission.symptoms, comorbidities, embargo.limit, treatments, file = glue("{code.path}/patient_data_{today()}.rda"))

