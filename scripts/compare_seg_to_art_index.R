library(tidyverse)
library(here)
library(readxl)
library(magrittr)

load(here("data", "index_articles.RData"))
load(file = here("data", "coded_annotations.RData"))
load(file = here("data", "segments.RData"))

in_codes_not_art <- segments %>%
  unique() %>%
  anti_join(., art)
# now empty- previously 4 of these --all fixed, (See below)
# 13233 is typo on art_index, should be 13223 I think -- fixed
# 14751 is marked as not downloaded but appears in segments, so it was downloaded -- fixed
# 20344 - I think this might be 20334 and a typo in study id in the article index -- fixed
# 20781 marked as no downloaded but it was downloaded and marked with an exclusion tag -- fixed


in_art_not_codes <- art %>%
  filter(in_codes_db == "yes") %>%
  anti_join(., unique(select(segments, study_id))) %>%
  mutate(in_codes_db = ifelse(!is.na(reason), "no", "check_mex_files")) 
# some of these have notes with reasons. add col in article index (separate from downloaded) that indicates whether it is included in the db or not
# 12 of these (exported to check_missing_in_codes.csv) -- looked into them and recoded updates in check_missing_in_code_edit.csv 

write_csv(missing_in_codes, here("data", "check_missing_in_codes.csv"))

----- # manually checked missing ones and updated notes, and in_codes_db columns----

fix_missing <- read_csv(here("data", "check_missing_in_codes_edit.csv"))

art <- art  %>%
  filter(!(study_id %in% fix_missing$study_id)) %>%
  rbind(fix_missing)

save(art, file = here("data", "index_articles.RData"))

  

missing_articles <- art  %>%
  filter(in_codes_db == "no")

write_csv(missing_articles, here("data", "missing_articles.csv"))

