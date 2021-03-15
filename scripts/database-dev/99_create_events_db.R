library(tidyverse)
library(magrittr)
library(here)
library(stringdist)
library(googlesheets4)

#-----------------All data-----------------
segments <- read_csv(here("data-processed", "segments.csv"))

articles_db <- read_csv(here("data-processed", "articles-db.csv")) %>%
  mutate(title = tolower(title)) %>%
  filter(study_id %in% unique(segments$study_id)) 

locations <- read_csv(here("data-processed", "locations.csv")) %>%
  group_by(
    study_id,
    code_identifiers,
    code_identifiers_link,
    study_location_basis,
    study_location,
    study_country,
    study_iso3c,
    residence_location
  ) %>%
  summarize(travel_location = paste(unique(travel_location), collapse =
                                      "; ")) %>%
  ungroup() 

bacteria <- read_csv(here("data-processed", "bacteria_genus_species.csv")) %>%
  select(
    study_id,
    code_identifiers,
    code_identifiers_link,
    bacteria_rank,
    bacteria = bacteria_preferred_label,
    bacteria_abbreviation = bacteria_preferred_label_abbr,
    bacteria_parent_rank,
    bacteria_parent_name
  )

drugs <- read_csv(here("data-processed", "drugs.csv")) %>%  
  select(study_id,
         code_identifiers,
         code_identifiers_link,
         drug_mesh = mesh_drug_preferred_label,
         mesh_drug_rank,
         mesh_drug_parent_id,
         mesh_drug_parent_name,
         drug_atc = atc_drug_preferred_label,
         atc_drug_rank,
         atc_drug_parents_class_ids,
         atc_drug_parent_name)

dates <- read_csv(here("data-processed", "dates.csv")) 

#-----------------Make full DB-----------------
event_list <- list(locations, drugs, bacteria, dates)

# Replace NA code IDs with "none"
event_list <- map(event_list, function(x){
  x %>% replace_na(list(code_identifiers = "none", code_identifiers_link = "none"))
})

# Make event dataframe of only cases with code identifiers
event_codeid <- reduce(event_list, full_join) %>%
  filter(code_identifiers != "none")

# Make event dataframe of only cases without code identifiers  These apply to all cases within a study.  
event_list_no_codeid <- map(event_list, function(x){
  filter(x, code_identifiers == "none")
})

event_no_codeid <- reduce(event_list_no_codeid, full_join)

# Join dataframe of code identifier cases with dataframe of non-code identifier cases
events <- full_join(event_codeid, event_no_codeid, by = "study_id") 

# Get clean column names
cnames <- colnames(events) %>%
  keep(~grepl(".x", .x)) %>%
  modify(~gsub(".x", "", .x)) %>% 
  unlist()

# For each column, if the x version (code identifier case) is na, replace it with the y version (no code identifier -- applies to all cases if fields do not already have values from the code identifier case)
for(x in cnames){
  events %<>%
    mutate(!!sym(x) := ifelse(is.na(!!sym(paste0(x, ".x"))), 
                              !!sym(paste0(x, ".y")), 
                              !!sym(paste0(x, ".x")))) %>%
    select(-!!sym(paste0(x, ".x")), -!!sym(paste0(x, ".y")))
}

events %<>% 
  distinct() %>% 
  arrange(study_id, code_identifiers)

# Remove NAs in country, drugs (atc or mesh), bacteria 
events %<>%
  filter(!is.na(study_country), !is.na(drug_atc), !is.na(bacteria))
#events <-  left_join(events, articles_db)

# Check study IDs that did not make it into DB
segments_id <- unique(segments$study_id)
events_id <- unique(events$study_id)
segments_id[!segments_id %in% events_id]

#-----------------Dupe checks-----------------
# Any duplicated titles?
articles_dups <- articles_db %>%
  mutate(dup_title = duplicated(title) | 
           duplicated(title, fromLast = TRUE)) %>%
  filter(dup_title==TRUE) %>%
  select(study_id, title, author, year, url, volume, doi, edition, language, mex_name) %>%
  arrange(title)

# Any fuzzy duplicated titles?
# note - this does not capture all dupes.  For example study_ids 18947 & 18946 are the same.  Assuming coded identically, these dupes are removed when selecting for emergence below.
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

#-----------------Get unique events per country-----------------
# first remove dups based on above review
amr_db_dups_titles <- "https://docs.google.com/spreadsheets/d/1dftRNfP4wLWxn-LHYA_LkLo44DrfDFIgjM3nGNsHsI8/edit?usp=sharing"
dups_remove <- read_sheet(amr_db_dups_titles, sheet = "exact_match") %>% # google spreadsheet with field cleanup
  as_tibble() %>% 
  filter(NOTES=="delete") %>%
  pull(study_id) 

events %<>% filter(!study_id %in% dups_remove)

# this isn't all the fuzzy dups, but they should be removed by the filtering for first events below
fuzzy_dups_remove <-read_sheet(amr_db_dups_titles, sheet = "fuzzy_match") %>% # google spreadsheet with field cleanup
  as_tibble() %>% 
  filter(!is.na(delete)) %>%
  pull(delete) 

events %<>% 
  filter(!study_id %in% fuzzy_dups_remove)

# remove code identifiers and combo keys
events %<>% 
  select(-code_identifiers, -code_identifiers_link, -bacteria_abbreviation) %>%
  distinct()

# for multiple events, select the most recent
# first need to assume publication date for start_date NAs

events_dates_na <- events %>%
  filter(is.na(start_date)) %>%
  left_join(articles_db %>% 
              select(study_id, year)) %>%
  select(-start_date, start_date = year) %>%
  mutate(start_date = as.character(start_date))

# still a few missing start dates
events_dates_na %>% filter(is.na(start_date))
events_dates_na$start_date[events_dates_na$study_id == 22574] <- "2007"
events_dates_na$start_date[events_dates_na$study_id == 1005663] <- "2012"
events_dates_na$start_date[events_dates_na$study_id == 1224333] <- "2012"
events_dates_na$start_date[events_dates_na$study_id == 1249316] <- "2012"
events_dates_na$start_date[events_dates_na$study_id == 2203256] <- "2003"
assertthat::assert_that(nrow(events_dates_na %>% filter(is.na(start_date)))==0)
events_dates_na$start_date_rank <- "year"

events %<>%
  drop_na(start_date) %>%
  bind_rows(events_dates_na) %>%
  distinct() # some of the assigned pub dates are same as start_date, so these get filtered out

# select first report.  if two are identical, select first study.
# note that there may be differences in strain or marker, which would mean some of these are in fact separate emergence events.  to be revisited.  
events_atc <- events %>%
  group_by(study_country, drug_atc, bacteria) %>%
  mutate(is_first = start_date == min(start_date, na.rm=T)) %>%
  filter(is_first) %>% # get first event for each unique combo (if there is only 1 event, it will be selected) 
  slice(1) %>% # if there is a tie (ie more than one event reported at same time and same place) select first instance
  select(-is_first) %>%
  ungroup()

write_csv(events_atc, here("events-db-atc.csv"))

events_mesh <- events %>%
  group_by(study_country, drug_mesh, bacteria) %>%
  mutate(is_first = start_date == min(start_date, na.rm=T)) %>%
  filter(is_first) %>% # get first event for each unique combo (if there is only 1 event, it will be selected) 
  slice(1) %>% # if there is a tie (ie more than one event reported at same time and same place) select first instance
  select(-is_first) %>%
  ungroup()

write_csv(events_mesh, here("events-db-mesh.csv"))

# get overall study combo first events
events_atc_study_combos <- events %>%
  select(-drug_mesh, -starts_with("mesh"), -starts_with("atc")) %>% 
  group_by_at(vars(-c("drug_atc"))) %>% 
  summarize(drug_combo = paste(sort(drug_atc), collapse = " + ")) %>% 
  ungroup() %>% 
  group_by(study_country, drug_combo, bacteria) %>%
  mutate(is_first = start_date == min(start_date, na.rm=T)) %>%
  filter(is_first) %>% # get first event for each unique combo (if there is only 1 event, it will be selected) 
  slice(1) %>% # if there is a tie (ie more than one event reported at same time and same place) select first instance
  select(-is_first) %>%
  ungroup()

events_mesh_study_combos <- events %>%
  select(-drug_atc, -starts_with("atc"), -starts_with("mesh")) %>% 
  group_by_at(vars(-c("drug_mesh"))) %>% 
  summarize(drug_combo = paste(sort(drug_mesh), collapse = " + ")) %>% 
  ungroup() %>% 
  group_by(study_country, drug_combo, bacteria) %>%
  mutate(is_first = start_date == min(start_date, na.rm=T)) %>%
  filter(is_first) %>% # get first event for each unique combo (if there is only 1 event, it will be selected) 
  slice(1) %>% # if there is a tie (ie more than one event reported at same time and same place) select first instance
  select(-is_first) %>%
  ungroup()

