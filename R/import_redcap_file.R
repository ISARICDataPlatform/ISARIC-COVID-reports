 import.redcap.file <- function(data.file.name, data.dict.file.name, data.source.name = NA){
   if(verbose) cat(glue("Importing data from source {data.source.name}...\n"))

   data.dict <- read_csv(data.dict.file.name)

   column.types <- data.dict %>% dplyr::select(1, 4) %>%
     dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)

   raw.data.tbl <- read_csv(data.file.name, guess_max = 100000)

   # Columns that should be text
   text.columns.temp <- column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
   # some data dictionary columns are not in the data!
   text.columns <- intersect(text.columns.temp, colnames(raw.data.tbl))
   # don't waste time on ones that are already character, and avoid doing anything to date columns
   text.columns <- text.columns[which(text.columns %>% map_lgl(function(x) !is.Date(raw.data.tbl %>% pull(x)) & !is.character(raw.data.tbl %>% pull(x))))]

   # Columns that should be numerical
   nontext.columns.temp <- column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
   # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
   nontext.columns.extra <- colnames(raw.data.tbl)[which(map_lgl(colnames(raw.data.tbl), function(x) any(startsWith(x, glue("{nontext.columns.temp}___")))))]
   nontext.columns <-intersect(colnames(raw.data.tbl), c(nontext.columns.temp, nontext.columns.extra))
   # don't waste time on ones that are already character
   nontext.columns <- nontext.columns[which(nontext.columns %>% map_lgl(function(x) !is.numeric(raw.data.tbl %>% pull(x))))]

   # readr warnings about parsing failure can often be dealt with by increasing guess_max

   raw.data.tbl <- raw.data.tbl %>%
     # Columns that should be character are converted to character.
     # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
     mutate_at(text.columns, as.character)

   # Columns that should be numeric are converted to numeric. Parse failures becomes NA.
   # this may actually need a for loop!
   for(ntc in nontext.columns){
     raw.data.tbl <- raw.data.tbl %>% mutate_at(vars(tidyselect::all_of(ntc)), .funs = ~pcareful.as.numeric(., subjid = subjid, colname = ntc))
   }

   raw.data.tbl <- raw.data.tbl %>%
     dplyr::mutate(data.source = data.source.name)

   raw.data.tbl
 }
