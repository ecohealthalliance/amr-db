library(tidyverse)
library(magrittr)
library(here)
library(janitor)

###MeSH Medical Subject Headings (https://meshb.nlm.nih.gov/search)###

# Read raw mesh db and cleam
mesh0 <- read_csv(here("data-raw", "mesh-ontology", "as_received", "MESH.csv.zip")) %>% #downloaded directly from bioportal: https://bioportal.bioontology.org/ontologies/MESH?p=summary
  clean_names() %>%
  select(class_id, preferred_label, synonyms, definitions, hm, parents, pa, mn) %>%
  mutate_at(vars(class_id, parents), funs(gsub("http://purl.bioontology.org/ontology/MESH/", "", .))) %>%
  mutate_all(tolower) 

# Separate out synonyms, reshape, rename
mesh <- mesh0 %>%
  mutate(synonyms = str_split(synonyms, "\\|")) %>% unnest() %>%
  gather(key = "segment_name_class", value = "segment", preferred_label, synonyms, 
         -class_id, -definitions, -hm, -parents, -pa) %>% unique() %>%
  filter(!is.na(segment)) %>% #cases with no synonyms
  rename_all(funs(paste0('mesh_', .)))  %>%
  rename(segment = mesh_segment, mesh_id = mesh_class_id)

# filter out synonyms that are also preferred labels
pref_names <- mesh %>%
  filter(mesh_segment_name_class == "preferred_label") %>%
  pull(segment) %>%
  unique()

mesh %<>% filter(!c(segment %in% pref_names & mesh_segment_name_class == "synonyms"))

# save data
write_rds(mesh, here("data-raw","mesh-ontology", "mesh.rds"))
