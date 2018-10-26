library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(janitor)
library(googlesheets)

# Structure Drugs Data and Manual Corrections-----------------

# Read in data
segments <- read_csv(here("data", "segments.csv"))

# Import corrections from manual checking
cleaned_drug_codes <-
  gs_read(gs_title("amr_db_clean_drugs")) %>% filter(new != "--") %>% #google spreadsheet with field cleanup
  mutate(segment = paste0("^", segment, "$"))

# Drug codes dataframe with manual corrections
drugs <- segments %>%
  filter(code_main == "drug resisted") %>%
  select(-code_main_cat) %>%
  mutate(
    segment = stri_replace_all_regex(
      segment,
      c("\\(|\\)|\\:|\\;|\006|\002|\\.|\\,", "\\+|\\–"),
      c("", "/"),
      vectorize = FALSE),
    segment = str_trim(segment),
    segment = stri_replace_all_regex(
      segment,
      cleaned_drug_codes$segment,
      cleaned_drug_codes$new,
      vectorize_all = FALSE
    )
  )

# Separate out drug combos that are not recognized combinations
drugs %<>%
  mutate(segment_drug_combo = ifelse(grepl("\\/", segment), TRUE, FALSE),
         segment = str_split(segment, "\\/")) %>%
  unnest()

# QA Checks-----------------
# clean (amr_db_clean_drugs - issue 7)
# dups (amr_db_field_corrections - issue 2)
# missing (amr_db_missing_drugs - issue 2)
source(here("scripts", "helper_scripts", "qa.R"))

# Identify duplicate drugs in studies
studies_with_dups <- qa_duplicate(drugs, group_vars = c("study_id", "segment_drug_combo", "segment")) %>%
  filter(segment_drug_combo == FALSE) %>%
  select(-segment_drug_combo)

# Studies with missing codes
studies_missing_drugs <- qa_missing(drugs)
#gs_new("amr_db_missing_drugs_study_id", input=studies_missing_drugs)

# Compare with list of studies that were evaluated for missing drug codes (review 1)
missing_list <- gs_read(gs_title("amr_db_missing_drugs_study_id"), ws = "review_1") 
studies_missing_drugs %<>% left_join(., missing_list)

# MESH drug ontology-----------------

mesh0 <- read_csv(here("data-raw", "mesh-ontology", "as_received", "MESH.csv.zip")) %>% #downloaded directly from bioportal: https://bioportal.bioontology.org/ontologies/MESH?p=summary
  clean_names() %>%
  select(class_id, preferred_label, synonyms, definitions, hm, parents, pa, mn) %>%
  mutate_at(vars(class_id, parents), funs(gsub("http://purl.bioontology.org/ontology/MESH/", "", .))) %>%
  mutate_all(tolower) 

mesh <- read_rds(here("data-raw", "mesh-ontology", "mesh.rds")) #mesh data from clean_mesh.r

# Join with drugs data
drugs %<>% left_join(., mesh)

# Preferred name look ups for synonyms
drugs %<>%
  left_join(.,
            mesh0 %>% select(class_id, preferred_label),
            by = c("drug_id" = "class_id")) %>%
  rename(drug_preferred_label = preferred_label)

# Get mesh class (c or d) and associated parent ID
# c = Class 1 Supplementary Records - Chemicals (These records are dedicated to chemicals and are primarily heading mapped to the D tree descriptors.)
# d = D tree for drugs and chemicals,
drugs %<>%
  mutate(
    drug_class = substr(drug_id, 1, 1),
    drug_class_desc = ifelse(drug_class == "c", "supp", "desc"),
    drug_parent_id = ifelse(drug_class == "c", drug_hm, drug_parents)
  ) %>%
  separate(drug_parent_id,
           c("drug_parent_id", "drug_parent_id_qualifier"),
           "/") %>% #warning is ok
  mutate(drug_parent_id = str_split(drug_parent_id, "\\|")) %>%  unnest() %>%
  mutate(drug_pa_id = str_split(drug_pa, "\\|")) %>%  unnest()

# Get mesh parent name and pharmacological action (pa) name
drugs %<>%
  left_join(.,
            mesh0 %>% select(class_id, preferred_label),
            by = c("drug_parent_id" = "class_id")) %>%
  rename("drug_parent_name" = preferred_label) %>%
  left_join(.,
            mesh0 %>% select(class_id, preferred_label),
            by = c("drug_pa_id" = "class_id")) %>%
  rename("drug_pa_name" = preferred_label) %>%
  select(-drug_hm,-drug_parents,-drug_pa)
  #TODO get qualifier name

# Classify as group or drug based on tree (if terminal -> drug, otherwise class)
drugs_tree <- drugs %>%
  select(drug_preferred_label, drug_mn, drug_class) %>%
  filter(drug_class != "c") %>% #remove supp concepts (these are terminal)
  distinct() %>%
  mutate(drug_mn = str_split(drug_mn, "\\|")) %>%
  unnest() 

drugs_tree_terminal <- drugs_tree %>%
  mutate(drug_mn_match_count = map_int(drug_mn, function(x) length(grep(x, mesh0$mn)))) %>%
  group_by(drug_preferred_label) %>%
  summarize(drug_mn_match_count = paste(unique(drug_mn_match_count), collapse = ", ")) %>%
  filter(drug_mn_match_count == "1") %>%
  pull(drug_preferred_label)

drugs %<>% 
  mutate(drug_rank = ifelse(drug_class=="c" | drug_preferred_label %in% drugs_tree_terminal, "drug name", "drug group"))

# Check matches with MESH ontology-----------------

no_match <- qa_match(drugs, "drug_id")

# Compare with list of studies that were previously cleaned
clean_list <- gs_read(gs_title("amr_db_clean_drugs")) 
no_match %<>% left_join(., clean_list)

write_csv(drugs, here("data", "drugs.csv"))
