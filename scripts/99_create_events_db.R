library(tidyverse)
library(here)

#-----------------All data-----------------

segments <- read_csv(here("data", "segments.csv"))

locations <- read_csv(here("data", "locations.csv")) %>%
  select(
    study_id,
    code_identifiers,
    code_identifiers_link,
    study_location_basis,
    study_location,
    study_country
      ) %>%
  distinct()

bacteria <- read_csv(here("data", "bacteria_genus_species_db.csv")) %>%
  select(
    study_id,
    code_main,
    code_identifiers,
    code_identifiers_link,
    bacteria_rank,
    bacteria_preferred_label,
    bacteria_parent_rank,
    bacteria_parent_name
  )

drugs <- read_csv(here("data", "drugs.csv")) %>%  
  group_by(
    study_id,
    code_main,
    code_identifiers,
    code_identifiers_link,
    segment_drug_combo,
    drug_rank,
    drug_preferred_label
  ) %>%
  summarize(drug_parent_name = paste(unique(drug_parent_name), collapse =
                                       ", ")) %>%
  ungroup()

#-----------------Bacteria + Drugs links-----------------
paired_bac_drugs <- list(bacteria, drugs) %>%
  map(., ~ mutate(.x, join_id = ifelse(
    c(
      grepl("drug resisted", code_identifiers_link) &
        grepl("binomial", code_identifiers_link)
    ), code_identifiers, NA
  ))) %>%
  map(., ~ select(.x, -code_identifiers, -code_identifiers_link, -code_main)) %>%
  reduce(full_join) %>%
  filter(!is.na(bacteria_preferred_label), !is.na(drug_preferred_label)) %>% 
  mutate(bacteria_drug_pair = paste(bacteria_preferred_label, drug_preferred_label, sep = " - "))

#-----------------Check for unique events per country-----------------
locations2 <- locations %>%
  select(study_id, study_country)

events <- full_join(locations2, paired_bac_drugs) #assume code links from locations are universal

events_qa <- events %>%
  group_by(study_country, drug_preferred_label, bacteria_preferred_label) %>%
  summarize(study_id = paste(unique(study_id), collapse = ","))

write_csv(events_qa, here("data", "data_qa", "events_qa.csv"))
