library(here)
library(tidyverse)
library(googlesheets)
source(here("scripts", "03_clean_segments.R"))
articles_db <- read_csv(here("data", "articles_db.csv")) %>% mutate_all(as.character)

# Check Segments That Have at Least One Id Code But Zero Main Codes -----------------------------

# orphan id codes need manual checking - these have at least 1 ID code but no main code at all 
orphan_id_codes <- problem_id_codes %>% 
  filter(map_lgl(code_main, ~length(.x) == 0)) %>%
  mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
  select(-segment_all, -code_identifiers_check) %>%
  left_join(select(articles_db, study_id, mex_name), by = "study_id")

# check if anything has changed since last round of checks on google drive 
existing_orphan_ids <- gs_read(gs_title("amr_db_orphan_id_codes")) %>% 
  mutate(study_id = as.character(study_id)) %>%
  select(study_id, code_identifiers) #segments arent matching
new_orphan_ids <- anti_join(orphan_id_codes, existing_orphan_ids)

write_csv(orphan_id_codes, here("data", "data_qa", "amr_db_orphan_id_codes.csv"))
drive_upload(media = here("data", "data_qa", "amr_db_orphan_id_codes.csv"), 
             path = "~/amr-db/", #* REVIEW - this does assume you have your amr-db google drive in the top level of g-drive. 
             name = "amr_db_orphan_id_codes.csv", 
             type = "spreadsheet")