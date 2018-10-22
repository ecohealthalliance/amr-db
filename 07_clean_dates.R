library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(googlesheets)
library(janitor)

# Structure Date Data-----------------

# read in data
segments_db <- read_csv(here("data", "segments_db.csv"))

# structure segments database into dates codes dataframe 
dates <- segments_db %>%
  filter(code_main %in% c("event month" , "event year", "event date" , "event day")) %>%
  select(-code_main_cat) %>%
  mutate(tmp = paste(study_id, code_main,  code_identifiers),
         dup = duplicated(tmp)) %>%
  select(-tmp)

# reshape 
dates %<>%
  spread(key = code_main, value = segment) %>%
  clean_names()

# create event ID - a lot of these are the same event and need to be combined (see dup_dates below)
dates %<>%
  group_by(study_id, code_identifiers) %>%
  mutate(event_id = paste(study_id, row_number(), sep="_")) %>%
  ungroup() %>%
  select(-dup)

# QA Checks-----------------
# clean (amr_db_clean_dates - not yet created)
# dups (NA for now)
# missing (amr_db_missing_dates - issue 12)

# identify duplicate dates in studies 
dup_dates <- dates %>%
  group_by(study_id, code_identifiers) %>%
  filter(n() >1 ) %>%
  ungroup()

# studies with missing codes
articles_db <- read_csv(here("data", "articles_db.csv"))
studies_missing_dates <- segments_db %>%
  filter(!study_id %in% dates$study_id) %>%
  select(study_id) %>%
  unique() %>%
  left_join(., articles_db %>% select(study_id, mex_name))
#gs_new("amr_db_missing_dates", input=studies_missing_dates)


