library(tidyverse)
library(magrittr)
library(stringi)
#library(here) Taking this out and replacing with rprojectroot b/c here interferes with lubridate
library(googlesheets)
library(janitor)
library(rprojroot)
library(googlesheets)

# Structure Date Data-----------------

P <- rprojroot::find_rstudio_root_file #bc here doesn't work with lubridate

#Run scripts to create base files
source(P("scripts/02_index_articles.R"))
source(P("scripts/03_clean_segments.R"))

# read in data
segments_db <- read_csv(P("data", "segments.csv"))

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
  select(-dup) %>%
  select(-code_identifiers) %>%
  select(-code_identifiers_link)
#write.csv(dates, P("data/dates_raw_db.csv"))

cleaned_date_codes <- gs_read(gs_title("amr_db_clean_dates")) 


dates <- dates %<>%
  mutate_at(vars(evemt_date, event_day, event_month, event_year), 
            funs(stri_replace_all_regex(., cleaned_date_codes$old, cleaned_date_codes$new, 
                                        vectorize_all = FALSE))) %>%
  mutate_all(funs(ifelse(. == ' ', NA, trimws(., "both")))) # bring back NA's



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
articles_db <- read_csv(P("data", "articles_db.csv"))

missing_any_date <- segments_db %>%
  filter(!study_id %in% dates$study_id) %>%
  select(study_id) %>%
  unique() 

missing_year_id <- dates %>%
  filter(is.na(event_year), is.na(event_date)) %>%
  pull(study_id) 

missing_year <- dates %>%
  filter(study_id %in% missing_year_id) %>%
  group_by(study_id) %>%
  filter(n() == 1 ) %>%
  select(study_id)

missing <- bind_rows(missing_any_date, missing_year) %>%
  left_join(., articles_db %>% select(study_id, mex_name)) 
#gs_new("amr_db_missing_dates", input=missing)
 
#separate out dates with no year in them and dates with misentries (they're tagged in the google doc)
dateless <- dates %>% 
  filter(is.na(event_year))
mistakes <- dates %>%
  filter(str_detect(dates$event_year, "february|two years later"))
no_year <- rbind(dateless, mistakes)
#write.csv(no_year, P("data", "dates_noyear.csv"))

#Create a data frame with all observations that have a year
dates_year <- dates %>% 
  filter(!is.na(event_year)) %>%
  filter(event_year != "february") %>%
  filter(event_year != "two years later")
#write.csv(dates_year, P("data", "dates_year.csv"))

#Manually edit entry mistakes (need to find a clever-er way of doing this)
dates_year[82,7] <- "2003"
dates_year[128,7] <- "2007"
dates_year[115, 5]<- "28"
dates_year[139, 6]<- "04"
dates_year[146, 6]<- "08"
dates_year[149, 6]<- "07"
dates_year[160, 6]<- "11"

#Change all months to #s
unique<-unique(dates_year$event_month) #used this to create a look up table 
write.csv(unique, P("data", "unique_dates.csv"))
#(there's a better way to do this automatically but right now it works)
look_up <- read.csv(P("data", "dates_lookup.csv")) #load look up table
look_up$lookupValue <- as.character(look_up$lookupValue)
dates_year$num_month <- look_up[match(dates_year$event_month,look_up$lookupValue), "newValue"]

dates_year %<>%
  mutate(event_month = num_month) %>%
  mutate( event_month= as.character(event_month)) %>%
  mutate(event_month = str_pad(event_month, width = 2, side = "left", pad = "0")) %>%
  select(-num_month)

#Manually fix date entry
dates_year[55,4] <- "15"

#Add 0 so all single digit event-days
dates_year %<>%
  mutate(event_day = str_pad(event_day, width = 2, side = "left", pad = "0"))

#separate data according to availability of y, m, d info
#add that info to new_date column

no_day <- dates_year %>%
  filter(is.na(event_day)) %>%
  filter(!is.na(event_month))  %>%
  mutate(new_date = paste(event_year, event_month, sep = "-")) 

no_dayormonth <- dates_year %>%
  filter(is.na(event_day)) %>%
  filter(is.na(event_month)) %>%
  mutate(new_date = event_year) 

no_month <- dates_year %>%
  filter(is.na(event_month)) %>%
  filter(!is.na(event_day)) %>%
  mutate(event_month = c("06", "03", "06", "05", NA)) %>%
  mutate(new_date = paste(event_year, event_month, sep = "-"))

library(lubridate)
full_date <- dates_year %>%
  filter(!is.na(event_month)) %>%
  filter(!is.na(event_day)) %>%
  mutate(full_date = paste(event_year, event_month, event_day, sep = " ")) %>%
  mutate(new_date = ymd(full_date)) %>%
  select(-full_date)  %>%
  mutate(new_date = as.character(new_date))  #to get it in same format as the other dates while we figure this out

clean_dates <-rbind(no_day, no_dayormonth, no_month, full_date) 
clean_dates %<>%
  mutate(event_date = new_date) %>%
  select(-new_date) %>%
  arrange(study_id)

duplicated(clean_dates$event_id)

write.csv(clean_dates, P("data/clean_dates_db.csv"))

