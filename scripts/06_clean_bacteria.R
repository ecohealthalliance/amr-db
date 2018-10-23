library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(googlesheets)

# Structure Bacteria Data and Manual Corrections-----------------

# read in data
segments_db <- read_csv(here("data", "segments_db.csv"))
cleaned_bacteria_codes <-
  gs_read(gs_title("amr_db_clean_bacteria")) %>% as.tibble() %>% filter(new != "--") %>% #google spreadsheet with field cleanup
  mutate(segment = paste0("^", segment, "$"))

# bacteria codes dataframe with manual corrections
bacteria <- segments_db %>%
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
# clean (amr_db_clean_bacteria - issue 11 - includes MAXQDA edits)
# dups (NA)
# missing (amr_db_missing_bacteria - issue 10)

# identify duplicate bacteria in studies 
dups_segs_with_same_code <- bacteria %>%
  group_by(study_id, code_identifiers, code_main, segment) %>%
  filter(duplicated(segment) |
           duplicated(segment, fromLast = TRUE))

dups_segs_with_NA_code <-  bacteria %>%
  group_by(study_id, code_main, segment) %>%
  filter(duplicated(segment) |
           duplicated(segment, fromLast = TRUE)) %>%
  mutate(code_identifiers = ifelse(code_identifiers == "", "NA", code_identifiers)) %>%
  summarize(code_identifiers = paste(code_identifiers, collapse = "|")) %>%
  filter(grepl("NA", code_identifiers))

# id studies with missing bacteria segments
bacteria_genspe <-
  bacteria %>% filter(code_main == "binomial (genus species)") %>% pull(study_id)
missing_genspe <-
  segments_db %>% filter(!study_id %in% bacteria_genspe) %>% pull(study_id) %>% unique() #45 missing

bacteria_marker <-
  bacteria %>% filter(code_main == "resistance marker") %>% pull(study_id)
missing_marker <-
  segments_db %>% filter(!study_id %in% bacteria_marker) %>% pull(study_id) %>% unique()

bacteria_strain <-
  bacteria %>% filter(code_main == "strain") %>% pull(study_id)
missing_strain <-
  segments_db %>% filter(!study_id %in% bacteria_strain) %>% pull(study_id) %>% unique()

studies_missing_bacteria <- read_csv(here("data", "articles_db.csv")) %>%
  select(study_id, mex_name) %>%
  filter(study_id %in% missing_genspe) #push to google drive
#gs_new("amr_db_missing_bacteria", input=studies_missing_bacteria)

# NCBI Ontology-----------------
# for bacteria genus / species

ncbi0 <- read_rds(here("data", "ncbi-ontology", "ncbi_raw.rds")) #raw ncbi data from clean_ncbi.r
ncbi <- read_rds(here("data", "ncbi-ontology", "ncbi.rds")) #ncbi data from clean_ncbi.r

# get abbreviated gen/spe
ncbi_abbr <- ncbi %>%
  filter(ncbi_div == "bacteria", ncbi_rank == "species") %>%
  mutate(segment = paste(substr(segment, 1, 1), gsub(".* ", "", segment)),
         ncbi_segment_name_class = "abbreviation")

ncbi %<>% bind_rows(., ncbi_abbr)

# join into bacteria data frame
bacteria %<>% left_join(., ncbi)

# preferred name look ups for synonyms and abbreviations
bacteria %<>%
  left_join(.,
            ncbi0 %>% select(class_id, preferred_label),
            by = c("ncbi_id" = "class_id")) %>%
  rename(ncbi_preferred_label = preferred_label)

# get ncbi parent name and rank
bacteria %<>%
  left_join(.,
            ncbi0 %>% select(class_id, preferred_label, rank),
            by = c("ncbi_parents" = "class_id")) %>%
  rename(ncbi_parent_name = preferred_label, ncbi_parent_rank = rank) %>%
  select(-ncbi_parents)

# check matches
bacteria_unique <- bacteria %>%
  select(segment, ncbi_id, code_main) %>%
  unique()

no_match <- bacteria_unique %>%
  filter(is.na(ncbi_id))  %>%
  group_by(code_main) %>%
  count()

match <- bacteria_unique %>%
  filter(!is.na(ncbi_id))  %>%
  group_by(code_main) %>%
  count()

# CARD Ontology-----------------
# for strains and markers

source(here("scripts", "clean_card.R"))

# get card id and parents + ancestors
bacteria %<>%
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

# check matches
bacteria_unique <- bacteria %>%
  select(segment, card_db, card_select_id, code_main) %>%
  unique()

no_match <- bacteria_unique %>%
  filter(card_select_id == "no exact match")  %>%
  group_by(code_main) %>%
  count()

match <- bacteria_unique %>%
  filter(card_select_id != "no exact match")  %>%
  group_by(code_main) %>%
  count()

# Separate DBs and export-----------------
bacteria_ncbi <- bacteria %>%
  filter(code_main == "binomial (genus species)") %>%
  select("study_id", "segment", "code_main", "code_identifiers", starts_with("ncbi"))

write_csv(bacteria_ncbi, here("data", "bacteria_genus_species_db.csv"))

bacteria_card <- bacteria %>%
  filter(code_main != "binomial (genus species)") %>%
  select("study_id", "segment", "code_main", "code_identifiers", starts_with("card"))

write_csv(bacteria_card, here("data", "bacteria_strains_and_resistance_markers_db.csv"))
