library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(googlesheets)

# Structure Drugs Data and Manual Corrections-----------------

segments_db <- read_csv(here("data", "segments_db.csv")) 
cleaned_drug_codes <- gs_read(gs_title("amr_db_clean_drugs")) %>% as.tibble() %>% filter(new!="--") %>% #google spreadsheet with field cleanup
  mutate(segment = paste0("^", segment, "$"))

# structure segments database into drug codes dataframe   
drugs <- segments_db %>%
  filter(code_main == "drug resisted") %>%
  select(-code_main_cat) %>%
  mutate(segment = stri_replace_all_regex(segment, 
                                          c("\\(|\\)|\\:|\\;|\006|\002|\\.|\\,", "\\+|\\–"),
                                          c("", "/"), vectorize = FALSE)) %>%         
  mutate(segment = str_trim(segment)) %>%
  mutate(segment = stri_replace_all_regex(segment,  cleaned_drug_codes$segment, cleaned_drug_codes$new, vectorize_all = FALSE)) 

# separate drug combos
drugs %<>%
  mutate(segment_combo = ifelse(grepl("\\/", segment), TRUE, FALSE)) %>%
  mutate(segment = str_split(segment, "\\/")) %>%
  unnest()

# identify duplicate drugs in studies (to be checked)
dups_with_same_code <- drugs %>%
  group_by(study_id, segment_combo, code_identifiers, segment) %>%
  filter(duplicated(segment) | duplicated(segment, fromLast = TRUE)) 

dups_with_NA_code <- drugs %>%  
  group_by(study_id, segment_combo, segment) %>%
  filter(duplicated(segment) | duplicated(segment, fromLast = TRUE)) %>%
  mutate(code_identifiers = ifelse(code_identifiers=="", "NA", code_identifiers)) %>%
  summarize(code_identifiers = paste(code_identifiers, collapse="|")) %>%
  filter(grepl("NA", code_identifiers))

# Join segments with MESH drug ontology-----------------
mesh0 <- read_rds(here("data","mesh-ontology", "mesh_raw.rds")) #raw mesh data from clean_mesh.r
mesh <- read_rds(here("data","mesh-ontology", "mesh.rds")) #mesh data from clean_mesh.r

drugs %<>% left_join(., mesh) 

# preferred name look ups for synonyms
drugs %<>%
  left_join(., mesh0 %>% select(class_id, preferred_label), by = c("mesh_id"="class_id")) %>%
  rename(mesh_preferred_label = preferred_label) 

# get mesh class (c or d) and associated parent ID
# c = Class 1 Supplementary Records - Chemicals (These records are dedicated to chemicals and are primarily heading mapped to the D tree descriptors.)
# d = D tree for drugs and chemicals,
drugs %<>%
  mutate(mesh_class = substr(mesh_id, 1, 1)) %>%
  mutate(mesh_class_desc = ifelse(mesh_class=="c", "supp", "desc")) %>%
  mutate(mesh_parent_id = ifelse(mesh_class=="c", mesh_hm, mesh_parents)) %>%
  separate(mesh_parent_id, c("mesh_parent_id", "mesh_parent_id_qualifier"), "/") %>% #warning is ok
  mutate(mesh_parent_id = str_split(mesh_parent_id, "\\|")) %>%  unnest() %>%
  mutate(mesh_pa_id = str_split(mesh_pa, "\\|")) %>%  unnest()

# get mesh parent name and pharmacological action (pa) name
drugs %<>%
  left_join(., mesh0 %>% select(class_id, preferred_label ), by = c("mesh_parent_id"= "class_id")) %>%
  rename("mesh_parent_name" = preferred_label) %>%
  left_join(., mesh0 %>% select(class_id, preferred_label ), by = c("mesh_pa_id"= "class_id")) %>%
  rename("mesh_pa_name" = preferred_label) %>%
  select(-mesh_hm, -mesh_parents, -mesh_pa)
#TODO get qualifier name

# check matches
drugs_unique <- drugs %>%
  select(-study_id, -code_identifiers, -code_main) %>%
  unique()

missing <- drugs_unique %>%
  filter(is.na(mesh_id))  
not_missing <- drugs_unique %>%
  filter(!is.na(mesh_id))  

