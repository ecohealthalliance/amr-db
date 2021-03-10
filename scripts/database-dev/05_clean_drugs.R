library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(janitor)
library(googlesheets4)
library(assertthat)

# Structure Drugs Data and Manual Corrections-----------------

# Read in data
segments <- read_csv(here("data-processed", "segments.csv")) 

# Import corrections from manual checking
amr_db_clean_drugs <- "https://docs.google.com/spreadsheets/d/1u-Ae_xmRu9JPzSW4xEY_inx-eF7QYJQVByUNO-SfNsQ/edit?usp=sharing"
cleaned_drug_codes <-
  read_sheet(amr_db_clean_drugs) %>%  #google spreadsheet with field cleanup
  mutate(segment = paste0("^", segment, "$"),
         new = ifelse(!is.na(new_correct), new_correct, new)) %>%
  filter(new != "--")

# Drug codes dataframe with manual corrections
drugs <- segments %>%
  filter(code_main == "drug resisted") %>%
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

# remove special characters
drugs %<>% 
  filter(!(study_id=="23491" & segment %in% c("1/4g/ml cipro",
                                            "1/41/41/4g/ml le",
                                            "1/41/41/41/4g/ml",
                                            "1/41/41/4g/ml",
                                            "nt to amik")))


# Separate out drug combos that are not recognized combinations 
# as id'd from no_match routine below
# to run new data through no_match routine, this code chunk will need to be commented out after segment_drug_combo col is created
drugs %<>%
  mutate(segment_drug_combo = ifelse(grepl("\\/", segment), TRUE, FALSE)) %>%
  mutate(combo_key = row_number()) %>%
  mutate(segment = str_split(segment, "\\/")) %>%
  unnest(cols = c(segment))
# Warning message:
#Expected 2 pieces. Missing pieces filled with `NA` in 2192 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
# ^ Ok

# QA Checks-----------------
# clean 
# dups 
# missing 
source(here("scripts", "helper", "functions_qa.R"))

# Identify duplicate drugs in studies
studies_with_dups <- qa_duplicate(drugs, group_vars = c("study_id", "segment_drug_combo", "segment")) %>%
  filter(segment_drug_combo == FALSE) %>%
  select(-segment_drug_combo)

# Studies with missing codes
studies_missing_drugs <- qa_missing(drugs)
#gs_new("amr_db_missing_drugs_study_id", input=studies_missing_drugs)

# Compare with list of studies that were evaluated for missing drug codes (review 2)
amr_db_missing_drugs_study_id <- "https://docs.google.com/spreadsheets/d/1IRP0xfcs2cXP8ArxfmGTNL0rCs3EELu0iXDRef9xIVc/edit?usp=sharing"
missing_list <- read_sheet(amr_db_missing_drugs_study_id, sheet = "review_2") 
studies_missing_drugs %<>% left_join(., missing_list) # 18812 is confirmed missing

# QA Response-----------------

# Remove NAs from dups
studies_with_dups2 <- studies_with_dups %>%
  filter(code_identifiers_conc!="NA|NA") %>%
  select(-mex_name)

drugs %<>%
  left_join(studies_with_dups2) %>%
  distinct() %>% #removes true duplicates
  filter(!(!is.na(code_identifiers_conc) & is.na(code_identifiers))) %>% # remove the NA versions of dups
  select(-code_identifiers_conc)

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
  unnest(cols = c(drug_mn)) 

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

articles_db <- read_csv(here("data-processed", "articles-db.csv"))  %>%
  select(study_id, mex_name) %>%
  mutate(study_id = as.character(study_id))

no_match %<>% left_join(articles_db)
assert_that(nrow(no_match) == 0)

write_csv(drugs, here("data-processed", "drugs.csv"))
