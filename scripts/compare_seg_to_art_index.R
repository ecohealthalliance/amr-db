library(tidyverse)
library(here)
library(readxl)
library(magrittr)

load(here("data", "index_articles.RData"))
load(file = here("data", "coded_annotations.RData"))
load(file = here("data", "segments.RData"))


art_ids <- art %>%
  filter(in_codes_db == "yes") %>%
  pull(study_id) %>%
  unique

segments_id <-  segments %>%
  pull(study_id) %>%
  unique

in_art_not_codes <- setdiff(art_ids, segments_id)
# 26 of these (exported to check_missing_in_codes.csv) -- looked into them and recoded updates in check_missing_in_code_edit.csv 

in_codes_not_art <- setdiff(segments_id, art_ids)
# 4 of these --all fixed (See below)

# 13233 is typo on art_index, should be 13223 I think -- fixed
# 14751 is marked as not downloaded but appears in segments, so it was downloaded -- fixed
# 20344 - I think this might be 20334 and a typo in study id in the article index -- fixed
# 20781 marked as no downloaded but it was downloaded and marked with an exclusion tag -- fixed

missing_in_codes <- 
  art %>% filter(., study_id %in% in_art_not_codes)

missing_in_codes <- missing_in_codes %>%
  mutate(in_codes_db = ifelse(!is.na(reason), "no", "check_mex_files"))

# some of these have notes that they could not be highlighted in maxqda. let's add a column in article index db separate from downloaded that indicated whether it is included in the db or not
write_csv(missing_in_codes, file = here("data", "check_missing_in_codes.csv"))

# manually check missing ones
fix_missing <- read_csv(here("data", "check_missing_in_codes_edit.csv"))

art <- art  %>%
  filter(!(study_id %in% fix_missing$study_id)) %>%
  rbind(fix_missing)


  