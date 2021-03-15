library(tidyverse)
library(magrittr)
library(here)
library(janitor)

# Read raw atc db and clean
atc0 <- read_csv(here("data-raw", "atc-ontology", "ATC.csv.gz")) %>%  #downloaded directly from bioportal: https://bioportal.bioontology.org/ontologies/ATC 2020AB
  clean_names() %>%
  select(cui, class_id, preferred_label, synonyms, parents, atc_level, is_drug_class) %>%
  mutate(class_id = basename(class_id)) %>%
  mutate(parents = basename(parents)) %>%
  mutate_all(tolower) 

# Separate out synonyms, reshape, rename
atc <- atc0 %>%
  gather(key = "segment_name_class", value = "segment", preferred_label, synonyms, 
         -cui, -class_id, -parents, -atc_level, -is_drug_class) %>% unique() %>%
  filter(!is.na(segment)) %>% #cases with no synonyms
  rename_all(funs(paste0('atc_drug_', .)))  %>%
  rename(segment = atc_drug_segment, atc_drug_id = atc_drug_cui)

# filter out synonyms that are also preferred labels
pref_names <- atc %>%
  filter(atc_drug_segment_name_class == "preferred_label") %>%
  pull(segment) %>%
  unique()

atc %<>% filter(!c(segment %in% pref_names & atc_drug_segment_name_class == "synonyms"))

# save data
write_rds(atc, here("data-raw","atc-ontology", "atc.rds"))
