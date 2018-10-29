library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(googlesheets)

# Structure Bacteria Data and Manual Corrections-----------------

# Read in data
segments <- read_csv(here("data", "segments.csv"))

# Import corrections from manual checking
cleaned_bacteria_codes <-
  gs_read(gs_title("amr_db_clean_bacteria")) %>% as.tibble() %>% filter(new != "--") %>% #google spreadsheet with field cleanup
  mutate(segment = paste0("^", segment, "$"))

# Bacteria codes dataframe with manual corrections
bacteria <- segments %>%
  filter(code_main_cat == "bacteria")  %>%
  select(-code_main_cat) %>%
  mutate(segment = stri_replace_all_regex(segment,
                                          c("\\(|\\)|\\:|\\;|\\.|\\,"),
                                          c(""), vectorize = FALSE)) %>%
  mutate(
    segment = stri_replace_all_regex(
      segment,
      cleaned_bacteria_codes$segment,
      cleaned_bacteria_codes$new,
      vectorize_all = FALSE
    )
  )

# QA Checks-----------------
# clean (amr_db_clean_bacteria - issue 11)
# dups (NA)
# missing (amr_db_missing_bacteria - issue 10)
source(here("scripts", "helper_scripts", "functions_qa.R"))

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

ncbi %<>% bind_rows(., bacteria_abbr)

# Join into bacteria data frame for gen/spe
bacteria_genspe <- bacteria %>% 
  filter(code_main == "binomial (genus species)") %>%
  left_join(., ncbi)

# Preferred name look ups for synonyms and abbreviations
bacteria_genspe %<>%
  left_join(.,
            ncbi0 %>% select(class_id, preferred_label),
            by = c("bacteria_id" = "class_id")) %>%
  rename(bacteria_preferred_label = preferred_label)

# Get ncbi parent name and rank
bacteria_genspe %<>%
  left_join(.,
            ncbi0 %>% select(class_id, preferred_label, rank),
            by = c("bacteria_parents" = "class_id")) %>%
  rename(bacteria_parent_name = preferred_label, bacteria_parent_rank = rank) %>%
  select(-bacteria_parents)

# Check matches with NCBI ontology-----------------

no_match <- qa_match(bacteria_genspe, "bacteria_id")

# CARD Ontology-----------------
# for strains and markers

source(here("scripts", "helper_scripts", "functions_card.R"))

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
  unnest() %>%
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
  unnest() %>%
  filter(card_ancestor_name != segment) %>%
  group_by(study_id, code_identifiers, card_select_id) %>%
  mutate(card_ancestor_rank = 1:n()) %>%
  ungroup()

# Check matches with CARD ontology-----------------
bacteria_strain_marker %<>%
  mutate(card_select_id = ifelse(card_select_id=="no exact match", NA, card_select_id))

no_match <- qa_match(bacteria_strain_marker, "card_select_id", group_vars = c("segment", "code_main"))


# Separate DBs export-----------------
write_csv(bacteria_genspe, here("data", "bacteria_genus_species_db.csv"))

write_csv(bacteria_strain_marker, here("data", "bacteria_strains_and_resistance_markers_db.csv"))
