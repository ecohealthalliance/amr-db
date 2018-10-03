library(here)
library(tidyverse)

load(file = here("data", "segments_raw.RData"))
load(file = here("data", "articles_db.RData"))

# these are all excluded articles or notes on articles, usually due to annotating the title of the article in the PDF. All have strange offsets
na_segments <- segments_raw %>% 
  filter(is.na(segment)) %>%
  left_join(select(articles_db, study_id, mex_name))

# save local and google drive copy
write_csv(na_segments, here("data", "data_qa", "amr_db_na_segments_check.csv"))
drive_upload(media = here("data", "data_qa", "amr_db_na_segments_check.csv"), 
             path = "~/amr-db/", #* REVIEW - this does assume you have your amr-db google drive in the top level of g-drive. 
             name = "amr_db_na_segments_check.csv", 
             type = "spreadsheet")