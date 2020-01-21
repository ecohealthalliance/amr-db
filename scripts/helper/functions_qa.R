library(tidyverse)
library(here)

# Duplicates - exact-----------------

qa_duplicate <- function(df, group_vars = c("study_id", "segment")){
  
  articles_db <- read_csv(here("data-processed", "articles_db.csv"))
  
  # identify duplicates with a letter code identifier and NA, or two of the same code identifiers
  dups <- df %>%
    group_by(!!! syms(group_vars)) %>%
    filter(duplicated(segment) |
             duplicated(segment, fromLast = TRUE)) %>%
    mutate(code_identifiers_conc = paste(code_identifiers, collapse = "|")) %>%
    mutate(is_dup = grepl("NA", code_identifiers_conc) || 
             duplicated(code_identifiers) |
             duplicated(code_identifiers, fromLast = TRUE)) %>%
    filter(is_dup) %>%
    select(!!! syms(group_vars), code_identifiers_conc) %>%
    distinct() %>%
    left_join(., articles_db %>% select(study_id, mex_name)) %>%
    arrange(study_id) %>%
    ungroup()
  
  return(dups)
  
}

# Duplicates - potential events-----------------
qa_event <- function(df){
  
  mult_events <- df %>%
    group_by(study_id, code_identifiers, code_main) %>%
    filter(n() >1 ) %>%
    ungroup()
  
  return(mult_events)
  
}

# Find articles with missing codes-----------------

qa_missing <- function(df) {
  
  articles_db <- read_csv(here("data-processed", "articles_db.csv"))
  segments <- read_csv(here("data-processed", "segments.csv"))
  
  not_missing <- df %>% pull(study_id) %>% unique()
  
  missing <- segments %>% 
    filter(!study_id %in% not_missing) %>% 
    select(study_id) %>% 
    unique() %>%
    left_join(., articles_db %>% select(study_id, mex_name)) %>%
    arrange(study_id)
  
  return(missing)
  
}

# Evaluate match against ontology-----------------

qa_match <- function(df, id_col, group_vars = c("segment")){
  
  no_match <- df %>%
    filter(is.na(!! sym(id_col))) %>%
    group_by(!!! syms(group_vars)) %>%
    summarize(study_id = paste(unique(study_id), collapse = ", "))
  
  return(no_match)
}



