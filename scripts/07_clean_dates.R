library(tidyverse)
library(magrittr)
library(stringi)
library(googlesheets)
library(janitor)
library(lubridate)
library(googlesheets)
library(here) 

# Structure Date Data-----------------

# read in data
segments <- read_csv(here::here("data", "segments.csv"))

# Quick QA response - remove incorrectly coded segment
segments %<>% filter(!c(study_id == 17611 & segment =="february" & code_main == "event year"))

# structure segments database into dates codes dataframe 
dates <- segments %>%
  filter(code_main %in% c("event month" , "event year", "event date" , "event day")) %>%
  select(-code_main_cat) %>%
  mutate(tmp = paste(study_id, code_main,  code_identifiers),
         dup = duplicated(tmp)) %>%
  select(-tmp)

# QA Checks-----------------
# clean 
# dups 
# missing (amr_db_missing_dates - issue 12)
source(here::here("scripts", "helper_scripts", "functions_qa.R"))

# Identify duplicate dates in studies
studies_with_dups <- qa_duplicate(dates)
  
# Check if more than one date per study - ie multiple events
studies_with_mult_events <- qa_event(dates)

# ID studies with missing dates
studies_missing_dates <- qa_missing(dates)

# also ID studies with missing year
articles_db <- read_csv(here::here("data", "articles_db.csv"))

has_year_id <- dates %>%
  filter(code_main %in% c("event year","event date")) %>%
  pull(study_id) %>% unique()

missing_year <- dates %>%
  filter(!study_id %in% has_year_id) %>%
  select(study_id) %>%
  left_join(., articles_db %>% select(study_id, mex_name))

  
# Handling of multiple events -----------------
# some may be collapsed in the future if they are very close in time

# reshape 
dates %<>%
  spread(key = code_main, value = segment) %>%
  clean_names()

# Lookups for manual cleaning ----------------

cleaned_date_codes <- gs_read(gs_title("amr_db_clean_dates")) 
cleaned_years <- cleaned_date_codes %>% filter(field == "event_year")
cleaned_months <- cleaned_date_codes %>% filter(field == "event_month")
cleaned_month_regex <- paste(cleaned_months$old, collapse="|")

# Separate out information from dates----------------

dates %<>%
  mutate_at(vars(event_year, event_month, event_day, event_date), funs(gsub("\\.|th|mid\\-", "", .))) %>%
  mutate(event_year = ifelse(is.na(event_year) & !is.na(event_date), 
                             str_sub(event_date, start = -4L, end = -1L), 
                             event_year),
         event_month = ifelse(is.na(event_month) & !is.na(event_date),
                              str_extract(event_date, cleaned_month_regex),
                              event_month))

# for studies with multiple "events" and missing year for second entry, assume year of first entry
dates %<>%
  group_by(study_id, code_identifiers) %>%
  mutate(event_year = ifelse(is.na(event_year), paste(unique(event_year[!is.na(event_year)]), collapse=","), event_year)) %>%
  ungroup()

# Clean months, years, days----------------

# manual find and replace
dates %<>% 
  mutate(event_month = stri_replace_all_regex(event_month, cleaned_months$old, cleaned_months$new, vectorize_all = FALSE),
         event_year = stri_replace_all_regex(event_year, cleaned_years$old, cleaned_years$new, vectorize_all = FALSE)) %>%
  mutate_at(vars(event_year, event_month, event_day), funs(as.numeric)) 

# pad month and day with 0
dates %<>%
  mutate(event_month = str_pad(event_month, width = 2, side = "left", pad = "0")) %>%
  mutate(event_day = str_pad(event_day, width = 2, side = "left", pad = "0"))

#separate data according to availability of y, m, d info
#add that info to new_date column

no_day <- dates %>%
  filter(is.na(event_day)) %>%
  filter(!is.na(event_month))  %>%
  mutate(new_date = paste(event_year, event_month, sep = "-")) 

no_dayormonth <- dates %>%
  filter(is.na(event_day)) %>%
  filter(is.na(event_month)) %>%
  mutate(new_date = event_year) 

# use lubridate to put in right format, then change back to character for now
full_date <- dates %>%
  filter(!is.na(event_month)) %>%
  filter(!is.na(event_day)) %>%
  mutate(full_date = paste(event_year, event_month, event_day, sep = " ")) %>%
  mutate(new_date = ymd(full_date)) %>%
  select(-full_date)  %>%
  mutate(new_date = as.character(new_date))

clean_dates <-rbind(no_day, no_dayormonth, full_date) 
clean_dates %<>%
  mutate(event_date = new_date) %>%
  select(-new_date, -event_day, -event_year, -event_month) %>%
  arrange(study_id, event_date)

# Combine two dates into start/finish dates----------------

# create date ID - a lot of these are the same event and need to be combined (see dup_dates above)
clean_dates %<>%
  group_by(study_id, code_identifiers) %>%
  mutate(date_id = row_number()) %>%
  ungroup() %>%
  select(-dup)

clean_dates %<>%
  spread(key = date_id, value = event_date) %>%
  rename(start_date = `1`, end_date = `2`)

write_csv(clean_dates, here("data/clean_dates_db.csv"))

