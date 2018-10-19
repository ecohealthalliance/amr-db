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
  select(-code_main_cat, -code_identifiers_check) %>%
  mutate(segment = stri_replace_all_regex(segment, 
                                          c("\\(|\\)|\\:|\\;|\\.|\\,"),
                                          c(""), vectorize = FALSE))

# id studies with missing bacteria segments
bacteria_genspe <- bacteria %>% filter(code_main == "binomial (genus species)") %>% pull(study_id)
missing_genspe <- segments_db %>% filter(!study_id %in% bacteria_genspe) %>% pull(study_id) %>% unique

bacteria_marker <- bacteria %>% filter(code_main == "resistance marker") %>% pull(study_id)
missing_marker <- segments_db %>% filter(!study_id %in% bacteria_marker) %>% pull(study_id) %>% unique

bacteria_strain <- bacteria %>% filter(code_main == "strain") %>% pull(study_id)
missing_strain <- segments_db %>% filter(!study_id %in% bacteria_strain) %>% pull(study_id) %>% unique

# load card + functions
source(here("scripts", "clean_card.R"))

bacteria %<>% 
  mutate(card_db = ifelse(code_main=="binomial (genus species)", "card_ncbi", "card_aro")) %>%
  mutate(
    card_select_id = map_chr(map2(.x = segment, .y = card_db, get_select_id), "select_id"),
    card_select_segment_class = map_chr(map2(.x = segment, .y = card_db, get_select_id), "select_segment_class"),
    card_parent_name = map(map2(.x = card_select_id, .y = card_db, get_relative, "parents"), "relative_name"), 
    card_parent_id = map(map2(.x = card_select_id, .y = card_db, get_relative, "parents"), "relative_id")) %>%
  unnest() %>%
  mutate(
    card_ancestor_name = map(map2(.x = card_select_id, .y = card_db, get_relative, "ancestors"), "relative_name"), 
    card_ancestor_id = map(map2(.x = card_select_id, .y = card_db, get_relative, "ancestors"), "relative_id"))%>% 
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
  filter(card_select_id=="no exact match")  %>%
  group_by(code_main) %>%
  count() 
#68 bacteria gen/spe no match
#66 resistance no match
#111 strain no match

match<- bacteria_unique %>%
  filter(card_select_id!="no exact match")  %>%
  group_by(code_main) %>%
  count() 
#52 bacteria gen/spe  match
#48 resistance match
#7 strain match

write.csv(bacteria, "bacteria_card_all.csv", row.names = F)
