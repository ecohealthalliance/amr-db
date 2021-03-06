library(tidyverse)
library(magrittr)
library(here)
library(janitor)

###National Center for Biotechnology Information (NCBI) Organismal Classification###

# Read raw mesh db and cleam
ncbi0 <- read_csv(here("data-raw", "ncbi-ontology", "as_received", "NCBITAXON.csv.zip")) %>% #downloaded from bioportal: http://bioportal.bioontology.org/ontologies/NCBITAXON
  clean_names() %>%
  select(class_id, preferred_label, synonyms, definitions, parents, rank, div) %>%
  mutate_at(vars(class_id, parents), funs(gsub("http://purl.bioontology.org/ontology/NCBITAXON/", "", .))) %>%
  mutate_all(tolower) 

# Separate out synonyms, reshape, rename
ncbi <- ncbi0 %>%
  mutate(synonyms = str_split(synonyms, "\\|")) %>% unnest() %>%
  gather(key = "segment_name_class", value = "segment", preferred_label, synonyms, 
         -class_id, -definitions, -parents, -rank, -div) %>% unique() %>%
  filter(!is.na(segment)) %>% #cases with no synonyms
  rename_all(funs(paste0('bacteria_', .)))  %>%
  rename(segment = bacteria_segment, bacteria_id = bacteria_class_id)

# filter out synonyms that are also preferred labels
pref_names <- ncbi %>%
  filter(bacteria_segment_name_class == "preferred_label") %>%
  pull(segment) %>%
  unique()

ncbi %<>% filter(!c(segment %in% pref_names & bacteria_segment_name_class == "synonyms"))

# save data
write_rds(ncbi, here("data-raw","ncbi-ontology", "ncbi.rds"))
