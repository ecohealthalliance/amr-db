library(tidyverse)
library(magrittr)
library(here)
library(stringdist)
library(googlesheets)

#-----------------All data-----------------
segments <- read_csv(here("data", "segments.csv"))

articles_db <- read_csv(here("data", "articles_db.csv")) %>%
  mutate(title = tolower(title)) %>%
  filter(study_id %in% unique(segments$study_id)) 

locations <- read_csv(here("data", "locations.csv")) %>%
  group_by(
    study_id,
    code_identifiers,
    code_identifiers_link,
    study_location_basis,
    study_location,
    study_country,
    residence_location
  ) %>%
  summarize(travel_location = paste(unique(travel_location), collapse =
                                      "; ")) %>%
  ungroup()

bacteria <- read_csv(here("data", "bacteria_genus_species.csv")) %>%
  select(
    study_id,
    code_identifiers,
    code_identifiers_link,
    bacteria_rank,
    bacteria_preferred_label,
    bacteria_preferred_label_abbr,
    bacteria_parent_rank,
    bacteria_parent_name
  )

drugs <- read_csv(here("data", "drugs.csv")) %>%  
  group_by(
    study_id,
    code_identifiers,
    code_identifiers_link,
    segment_drug_combo,
    drug_rank,
    drug_preferred_label
  ) %>%
  # collapse multiple parent names
  summarize(drug_parent_name = paste(unique(drug_parent_name), collapse = "; ")) %>%
  ungroup() %>%
  filter(!is.na(drug_preferred_label))

# collapse drug combos
drugs_combos <- drugs %>%
  filter(segment_drug_combo == TRUE) %>%
  group_by(study_id,
           code_identifiers,
           code_identifiers_link,
           segment_drug_combo
  ) %>%
  summarize(drug_rank = paste(drug_rank, collapse = " + "),
            drug_preferred_label = paste(drug_preferred_label, collapse = " + "),
            drug_parent_name = paste(drug_parent_name, collapse = " + "),
  ) %>%
  ungroup() 

drugs %<>%
  filter(segment_drug_combo == FALSE) %>%
  bind_rows(drugs_combos)

dates <- read_csv(here("data", "dates.csv")) 

#-----------------Make full DB (in progress)-----------------
event_list <- list(locations, drugs, bacteria, dates)

event_list_no_codeid <- map(event_list, function(x){
  filter(x, is.na(code_identifiers)) %>%
    select(-code_identifiers, -code_identifiers_link)
})
event_no_codeid <- reduce(event_list_no_codeid, full_join)


event_list_codeid <- map(event_list, function(x){
  x %>%
  replace_na(list(code_identifiers = "none", code_identifiers_link = "none"))
})
event_codeid <- reduce(event_list_codeid, full_join)

event <- left_join(event_codeid, event_no_codeid, by = "study_id")

cnames <- colnames(event) %>%
  keep(~grepl(".x", .x)) %>%
  modify(~gsub(".x", "", .x)) %>% 
  unlist()

for(x in cnames){
  event %<>%
    mutate(!!sym(x) := ifelse(is.na(!!sym(paste0(x, ".x"))), 
                              !!sym(paste0(x, ".y")), 
                              !!sym(paste0(x, ".x")))) %>%
    select(-!!sym(paste0(x, ".x")), -!!sym(paste0(x, ".y")))
}

######
test = event %>% filter(study_id==137)
View(test)
test2 = segments %>% filter(study_id==137)
View(test2)
#-----------------Bacteria + Drugs links-----------------
paired_bac_drugs <- list(bacteria, drugs) %>%
  map(., ~ mutate(.x, join_id = ifelse(
    c(
      grepl("drug resisted", code_identifiers_link) &
        grepl("binomial", code_identifiers_link)
    ), code_identifiers, NA
  ))) %>%
  #map(., ~ select(.x, -code_identifiers, -code_identifiers_link, -code_main)) %>%
  reduce(full_join, by= c("study_id", "join_id")) %>%
  mutate(bacteria_drug_pair = ifelse(!is.na(bacteria_preferred_label) & !is.na(drug_preferred_label), 
                                     paste(bacteria_preferred_label_abbr, drug_preferred_label, sep = " - "),
                                     NA))

#-----------------Bring in locations & dates to create base events db-----------------
# ignoring loctions and dates identifiers for now
locations2 <- locations %>%
  select(-code_identifiers, -code_identifiers_link)

dates2 <- dates %>%
  select(study_id, start_date, end_date)

events <- full_join(locations2, paired_bac_drugs) %>% #assume code links from locations are universal for now
  full_join(dates2) %>%
  left_join(articles_db)

#-----------------Dup checks-----------------
# Any duplicated titles?
articles_dups <- articles_db %>%
  mutate(dup_title = duplicated(title) | 
           duplicated(title, fromLast = TRUE)) %>%
  filter(dup_title==TRUE) %>%
  select(study_id, title, author, year, url, volume, doi, edition, language, mex_name) %>%
  arrange(title)

# Any fuzzy duplicated titles?
articles_dups_fuzz <- expand.grid(articles_db$title, articles_db$title) %>%
  filter(Var1 != Var2) %>%
  left_join(articles_db %>% select(title, study_id, author, year, url, volume, doi, edition, language, mex_name), by = c("Var1" = "title")) %>%
  left_join(articles_db %>% select(title, study_id, author, year, url, volume, doi, edition, language, mex_name), by = c("Var2" = "title")) %>%
  mutate(comp = stringdist(tolower(Var1),tolower(Var2), method = "osa")) %>%
  filter(comp <= 20) %>%
  arrange(comp) %>%
  mutate(tmp = apply(cbind(Var1, Var2), 1, function(x) paste(sort(x), collapse=" "))) %>%
  filter(!duplicated(tmp)) %>%
  select(-tmp, -comp) %>%
  setNames(gsub("\\.x", "1", colnames(.) )) %>%
  setNames(gsub("\\.y", "2", colnames(.) ))

#-----------------Check for unique events per country-----------------
# first remove dups based on above review
dups_remove <-
  gs_read(gs_title("amr_db_dups_titles")) %>% as.tibble() %>% filter(NOTES=="delete") %>% pull(study_id) #google spreadsheet with field cleanup

events_qa <- events %>%
  filter(!study_id %in% c(dups_remove)) %>%
  group_by(study_country, drug_preferred_label, bacteria_preferred_label) %>%
  summarize(study_id = paste(unique(study_id), collapse = "; "),
            mex_name = paste(unique(mex_name), collapse = "; "),
            title = paste(unique(title), collapse = ";\n")
  )

non_unique_events <- events_qa %>%
  filter(grepl(";", study_id))

write_csv(events, here("data", "events_db.csv"))
