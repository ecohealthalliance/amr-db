library(tidyverse)
library(here)

segments <- read_csv(here("data", "segments.csv"))
drugs <- read_csv(here("data", "drugs.csv"))
locations <- read_csv(here("data", "locations.csv"))

events_db <- segments %>% 
  select(study_id) %>% #event_id 
  unique() %>%
  full_join(locations) %>%
  full_join(drugs)

write_csv(events_db, here("data", "events_db.csv"))
