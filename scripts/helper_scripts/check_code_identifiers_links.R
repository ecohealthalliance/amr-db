library(here)
library(tidyverse)
library(googlesheets)
source(here("scripts", "03_clean_segments.R"))
articles_db <- read_csv(here("data", "articles_db.csv")) %>% mutate_all(as.character)

# Identify code identifiers that are not "linked" - these need to be spot checked
code_links_solo <- segments %>%
  filter(!grepl("\\;", code_identifiers_link), !is.na(code_identifiers_link)) %>%
  left_join(., articles_db %>% select(study_id, mex_name) %>% mutate(study_id = as.character(study_id))) %>%
  arrange(as.numeric(study_id)) %>%
  select(study_id, segment, code_identifiers, code_identifiers_link, mex_name)

# save local and google drive copy
write_csv(code_links_solo, here("data", "data_qa", "amr_db_missing_code_identifiers_links.csv"))
#gs_new("amr_db_missing_code_identifiers_links", input=code_links_solo)
