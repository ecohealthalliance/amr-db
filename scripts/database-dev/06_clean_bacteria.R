library(tidyverse)
library(magrittr)
library(readxl)
library(stringi)
library(here)
library(janitor)
library(assertthat)

# Structure Bacteria Data and Manual Corrections-----------------

# Read in data
segments <- read_csv(here("data-processed", "segments.csv"))

# Manual correction
segments$code_main[segments$study_id==23761 & segments$segment=="klebsiella pneumoniae"] <- "binomial (genus species)"

# Import corrections from manual checking
cleaned_bacteria_codes <-
  read_excel(here("manual-qa", "amr_db_clean_bacteria.xlsx")) %>% as.tibble() %>% filter(new != "--") %>% #google spreadsheet with field cleanup
  mutate(segment = paste0("^", segment, "$")) %>%
  mutate(new = ifelse(is.na(new_new), new, new_new))

# Bacteria codes dataframe with manual corrections
bacteria <- segments %>%
  filter(code_main %in% c("binomial (genus species)", "resistance marker", "strain"))  %>%
  mutate(segment = stri_replace_all_regex(segment,
                                          c("\\(|\\)|\\:|\\;|\\.|\\,|\\_"),
                                          c(""), vectorize = FALSE)) %>%
  mutate(segment = trimws(segment)) %>%
  mutate(
    segment = stri_replace_all_regex(
      segment,
      cleaned_bacteria_codes$segment,
      cleaned_bacteria_codes$new,
      vectorize_all = FALSE
    )
  ) %>%
  mutate(segment = gsub("  ", " ", segment)) %>%
  distinct()

# QA Checks-----------------
# clean 
# dups (NA for bacteria)
# missing 
source(here("scripts", "helper", "functions_qa.R"))

# Identify duplicate bacteria in studies 
studies_with_dups <- qa_duplicate(bacteria, c("study_id", "code_main", "segment"))

# ID studies with missing bacteria segments
missing_genspe <- qa_missing(bacteria %>% filter(code_main == "binomial (genus species)"))
#gs_new("amr_db_missing_bacteria", input=missing_genspe)

# NCBI Ontology-----------------
# for bacteria genus / species

ncbi0 <- read_csv(here("data-raw", "ncbi-ontology", "as_received", "NCBITAXON.csv.zip")) %>% #downloaded from bioportal: http://bioportal.bioontology.org/ontologies/NCBITAXON
  clean_names() %>%
  select(class_id, preferred_label, synonyms, definitions, parents, rank, div) %>%
  mutate_at(vars(class_id, parents), funs(gsub("http://purl.bioontology.org/ontology/NCBITAXON/", "", .))) %>%
  mutate_all(tolower) 

ncbi <- read_rds(here("data-raw", "ncbi-ontology", "ncbi.rds")) #ncbi data from clean_ncbi.r

# Get abbreviated gen/spe
bacteria_abbr <- ncbi %>%
  filter(bacteria_div == "bacteria", bacteria_rank == "species") %>%
  mutate(segment = paste(substr(segment, 1, 1), gsub(".* ", "", segment)),
         bacteria_segment_name_class = "abbreviation")

ncbi %<>% 
  bind_rows(., bacteria_abbr) %>%
  distinct()

# Join into bacteria data frame for gen/spe
bacteria_genspe <- bacteria %>% 
  filter(code_main == "binomial (genus species)") %>%
  left_join(., ncbi)

# Preferred name look ups for synonyms and abbreviations
bacteria_genspe %<>%
  left_join(.,
            ncbi0 %>% select(class_id, preferred_label),
            by = c("bacteria_id" = "class_id")) %>%
  rename(bacteria_preferred_label = preferred_label) %>%
  mutate(bacteria_preferred_label_abbr = ifelse(grepl("\\ ", bacteria_preferred_label),
    paste0(substring(bacteria_preferred_label, 1, 1), ". ", gsub("^\\S+\\s+", '', bacteria_preferred_label)),
    bacteria_preferred_label))

# Check for ambiguous abbreviation matches
bacteria_genspe_check <- bacteria_genspe %>%
  filter(bacteria_segment_name_class == "abbreviation") %>%
  group_by(study_id, code_identifiers, segment) %>%
  mutate(n_bacteria_preferred_label = n_distinct(bacteria_preferred_label)) %>%
  select(study_id, code_identifiers, segment, n_bacteria_preferred_label) %>%
  filter(n_bacteria_preferred_label >1) %>%
  arrange(study_id)
#gs_new("amr_db_bacteria_abbr", input = bacteria_genspe_check)

# Manually fix ambiguous entries
bacteria_genspe %<>%
  filter(!(study_id == 23039 & bacteria_preferred_label == "nocardia otitidiscaviarum"),
         !(study_id == 17942 & bacteria_preferred_label == "streptomyces aureus"))

# Get ncbi parent name and rank
bacteria_genspe %<>%
  left_join(.,
            ncbi0 %>% select(class_id, preferred_label, rank),
            by = c("bacteria_parents" = "class_id")) %>%
  rename(bacteria_parent_name = preferred_label, bacteria_parent_rank = rank) %>%
  select(-bacteria_parents)

# Check matches with NCBI ontology-----------------

no_match <- qa_match(bacteria_genspe, "bacteria_id")
assert_that(nrow(no_match) == 0)
# Compare with list of studies that were previously cleaned
articles_db <- read_csv(here("data-processed", "articles-db.csv"))  %>% 
  select(study_id, mex_name) %>%
  mutate(study_id = as.character(study_id))
# clean_list <- read_excel(here("manual-qa", "amr_db_clean_bacteria.xlsx")) 
# clean_list_with_mex <- clean_list %>%
#   filter(is.na(`Confirm Y?`)) %>%
#   left_join(articles_db)
# no_match %<>% left_join(., clean_list)
# new_no_match <- no_match %>%
#   filter(is.na(new))

# ID studies that do not have species (only genus or higher)
articles_db <- read_csv(here("data-processed", "articles-db.csv")) %>% select(study_id, mex_name)
missing_species <- bacteria_genspe %>% 
  filter(bacteria_rank!="species") %>%
  left_join(articles_db) %>%
  select(study_id, segment, bacteria_rank, mex_name)

# CARD Ontology-----------------
# for strains and markers

source(here("scripts", "helper", "functions_card.R"))

# Get card id and parents + ancestors
bacteria_strain_marker <- bacteria %>%
  filter(code_main != "binomial (genus species)") %>%
  mutate(card_db = ifelse(code_main == "binomial (genus species)", "card_ncbi", "card_aro")) %>%
  mutate(
    card_select_id = map_chr(map2(
      .x = segment, .y = card_db, get_select_id
    ), "select_id"),
    card_select_segment_class = map_chr(
      map2(.x = segment, .y = card_db, get_select_id),
      "select_segment_class"
    ),
    card_parent_name = map(
      map2(.x = card_select_id, .y = card_db, get_relative, "parents"),
      "relative_name"
    ),
    card_parent_id = map(
      map2(.x = card_select_id, .y = card_db, get_relative, "parents"),
      "relative_id"
    )
  ) %>%
  unnest(cols = c(card_parent_name, card_parent_id)) %>%
  mutate(
    card_ancestor_name = map(
      map2(.x = card_select_id, .y = card_db, get_relative, "ancestors"),
      "relative_name"
    ),
    card_ancestor_id = map(
      map2(.x = card_select_id, .y = card_db, get_relative, "ancestors"),
      "relative_id"
    )
  ) %>%
  unnest(cols = c(card_ancestor_name, card_ancestor_id)) %>%
  filter(card_ancestor_name != segment) %>%
  group_by(study_id, code_identifiers, card_select_id) %>%
  mutate(card_ancestor_rank = 1:n()) %>%
  ungroup()

# Check matches with CARD ontology-----------------
bacteria_strain_marker %<>%
  mutate(card_select_id = ifelse(card_select_id=="no exact match", NA, card_select_id))

no_match <- qa_match(bacteria_strain_marker, "card_select_id", group_vars = c("segment", "code_main"))

# Separate DBs export-----------------
write_csv(bacteria_genspe, here("data-processed", "bacteria_genus_species.csv"))

write_csv(bacteria_strain_marker, here("data-processed", "bacteria_strains_and_resistance_markers.csv"))

