library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(googlesheets)

# Structure Bacteria Data-----------------

segments_db <- read_rds(here("data", "segments_db.rds")) 

# structure segments database into bacteria codes dataframe   
bacteria <- segments_db %>%
  filter(code_main_cat == "bacteria")  %>%
  filter(code_main == "binomial (genus species)") %>% #mesh does not work for strains or resistance markers
  select(-code_main_cat, -code_identifiers_check) 

# Join segments with MESH drug ontology-----------------
mesh0 <- read_rds(here("data","mesh-ontology", "mesh_raw.rds")) #raw mesh data from clean_mesh.r
mesh <- read_rds(here("data","mesh-ontology", "mesh.rds")) #mesh data from clean_mesh.r

bacteria %<>% left_join(., mesh)  #70 did not match, 50 matched

# preferred name look ups for synonyms (MAKE FUNCTION)
bacteria %<>%
  left_join(., mesh0 %>% select(class_id, preferred_label), by = c("mesh_id"="class_id")) %>%
  rename(mesh_preferred_label = preferred_label) 

# get mesh class (c or d) and associated parent ID (MAKE FUNCTION)
# c = Class 1 Supplementary Records - Chemicals (These records are dedicated to chemicals and are primarily heading mapped to the D tree descriptors.)
# d = D tree for drugs and chemicals,
bacteria %<>%
  mutate(mesh_class = substr(mesh_id, 1, 1)) %>%
  mutate(mesh_class_desc = ifelse(mesh_class=="c", "supp", "desc")) %>%
  mutate(mesh_parent_id = ifelse(mesh_class=="c", mesh_hm, mesh_parents)) %>%
  separate(mesh_parent_id, c("mesh_parent_id", "mesh_parent_id_qualifier"), "/") %>% #warning is ok
  mutate(mesh_parent_id = str_split(mesh_parent_id, "\\|")) %>%  unnest() %>%
  mutate(mesh_pa_id = str_split(mesh_pa, "\\|")) %>%  unnest()

# get mesh parent name and pharmacological action (pa) name
bacteria %<>%
  left_join(., mesh0 %>% select(class_id, preferred_label ), by = c("mesh_parent_id"= "class_id")) %>%
  rename("mesh_parent_name" = preferred_label) %>%
  left_join(., mesh0 %>% select(class_id, preferred_label ), by = c("mesh_pa_id"= "class_id")) %>%
  rename("mesh_pa_name" = preferred_label) %>%
  select(-mesh_hm, -mesh_parents, -mesh_pa)
#TODO get qualifier name

# check matches
bacteria_unique <- bacteria %>%
  select(-study_id, -code_identifiers, -code_main) %>%
  unique()

no_match <- bacteria_unique %>%
  filter(is.na(mesh_id)) %>%
  count() #70 bacteria gen/spe no match

match <- bacteria_unique %>%
  filter(!is.na(mesh_id)) %>%
  count() #53 bacteria gen/spe match

write.csv(bacteria, "bacteria_mesh_genus_species.csv", row.names = F)
