library(tidyverse)
library(here)
library(readxl)
library(magrittr)
library(googledrive)
library(googlesheets)

articles_db <- read_csv(here("data","articles_db.csv")) 
segments <- read_csv(here("data", "segments.csv"))
segments_raw <- read_rds(here("data", "segments_raw.rds")) %>% mutate(study_id = as.numeric(study_id))

in_codes_not_art <- segments %>%
  unique() %>%
  anti_join(., articles_db)

# ^ now empty- previously 4 of these --all fixed, (See below)
# 13233 is typo on art_index, should be 13223 -- fixed
# 14751 is marked as not downloaded but appears in segments, so it was downloaded -- fixed
# 20344 - I think this might be 20334 and a typo in study id in the article index -- fixed
# 20781 marked as no downloaded but it was downloaded and marked with an exclusion tag -- fixed

in_art_not_codes <- articles_db %>%
  filter(in_codes_db == "yes") %>%
  anti_join(., unique(select(segments_raw, study_id))) 

missing_in_codes <- in_art_not_codes %>%
  mutate(in_codes_db = ifelse(!is.na(reason), "no", "check_mex_files")) %>%
  filter(!(study_id %in% c("14089", "7213", "15037", "15037", "15140", "17934",
                         "18229", "15574", "19250", "22668", "23761", "13495",
                         "23766", "7611"))) # these are na_segments to be fixed later after QA (in google drive)
# some of these have notes with reasons. add col in article index (separate from downloaded) that indicates whether it is included in the db or not
# 12 of these (exported to check_missing_in_codes.csv) -- looked into them and recoded updates in check_missing_in_code_edit.csv 

#----- # manually checked missing ones and updated notes, and in_codes_db columns----

fix_missing <- gs_title("amr_db_articles_missing_in_segs") %>% gs_read()

articles_db <- articles_db  %>%
  filter(!(study_id %in% fix_missing$study_id)) %>%
  rbind(fix_missing)

# save updated version of article db 
write_csv(articles_db, here("data", "articles_db.csv"))

# one article marked NA for downloaded column (and in_codes_db col)
articles_db  %>%
  filter(!(in_codes_db %in% c("yes", "no")))

