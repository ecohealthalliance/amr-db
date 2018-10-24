library(tidyverse)
library(here)

segments_db <- read_csv(here("data", "segments_db.csv"))
drugs_db <- read_csv(here("data", "drugs_db.csv"))
locations_db <- read_csv(here("data", "locations_db.csv"))

events_db <- segments_db %>% 
  select(study_id) %>%
  unique() %>%
  full_join(drugs_db) %>%
  full_join(locations_db)

write_csv(events_db, here("data", "events_db.csv"))