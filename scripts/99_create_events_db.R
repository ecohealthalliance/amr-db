library(tidyverse)
library(here)

#-----------------All data-----------------
articles_db <- read_csv(here("data", "articles_db.csv")) %>%
  mutate(title = tolower(title))

segments <- read_csv(here("data", "segments.csv"))

locations <- read_csv(here("data", "locations.csv")) %>%
  select(
    study_id,
    code_identifiers,
    code_identifiers_link,
    study_location_basis,
    study_location,
    study_country
  ) %>%
  distinct()

bacteria <- read_csv(here("data", "bacteria_genus_species.csv")) %>%
  select(
    study_id,
    code_main,
    code_identifiers,
    code_identifiers_link,
    bacteria_rank,
    bacteria_preferred_label,
    bacteria_parent_rank,
    bacteria_parent_name
  )

drugs <- read_csv(here("data", "drugs.csv")) %>%  
  group_by(
    study_id,
    code_main,
    code_identifiers,
    code_identifiers_link,
    segment_drug_combo,
    drug_rank,
    drug_preferred_label
  ) %>%
  summarize(drug_parent_name = paste(unique(drug_parent_name), collapse =
                                       ", ")) %>%
  ungroup()

dates <- read_csv(here("data", "dates.csv")) 

#-----------------Bacteria + Drugs links-----------------
paired_bac_drugs <- list(bacteria, drugs) %>%
  map(., ~ mutate(.x, join_id = ifelse(
    c(
      grepl("drug resisted", code_identifiers_link) &
        grepl("binomial", code_identifiers_link)
    ), code_identifiers, NA
  ))) %>%
  map(., ~ select(.x, -code_identifiers, -code_identifiers_link, -code_main)) %>%
  reduce(full_join) %>%
  filter(!is.na(bacteria_preferred_label), !is.na(drug_preferred_label)) %>% 
  mutate(bacteria_drug_pair = paste(bacteria_preferred_label, drug_preferred_label, sep = " - "))

#-----------------Bring in locations & dates to create base events db-----------------
# ignoring loctions and dates identifiers for now
locations2 <- locations %>%
  select(study_id, study_country)

dates2 <- dates %>%
  select(study_id, start_date, end_date)

events <- full_join(locations2, paired_bac_drugs) %>% #assume code links from locations are universal for now
  full_join(dates2)

#-----------------Check for unique events per country-----------------
events_qa <- events %>%
  group_by(study_country, drug_preferred_label, bacteria_preferred_label) %>%
  summarize(study_id = paste(unique(study_id), collapse = ","))

non_unique_events <- events_qa %>%
  filter(grepl(",", study_id))

# Any duplicated titles?
articles_dups <- articles_db %>%
  mutate(dup_title = duplicated(title) | 
           duplicated(title, fromLast = TRUE)) %>%
  filter(dup_title==TRUE) %>%
  group_by(title) %>%
  summarize(study_id = paste(study_id, collapse = ", "),
            mex_name = paste(mex_name, collapse = ", "))

# Any fuzzy duplicated titles?
library(stringdist)

articles_dups_fuzz <-articles_db %>%
  filter(study_id %in% unique(segments$study_id)) 

articles_dups_fuzz <- expand.grid(articles_dups_fuzz$title, articles_dups_fuzz$title) %>%
  filter(Var1 != Var2) %>%
  left_join(articles_db %>% select(title, study_id), by = c("Var1" = "title")) %>%
  left_join(articles_db %>% select(title, study_id), by = c("Var2" = "title")) %>%
  left_join(articles_db %>% select(title, mex_name), by = c("Var1" = "title")) %>%
  left_join(articles_db %>% select(title, mex_name), by = c("Var2" = "title")) %>% 
  mutate(comp = stringdist(tolower(Var1),tolower(Var2), method = "osa")) %>%
  filter(comp <= 20) %>%
  arrange(comp) %>%
  mutate(tmp = apply(cbind(Var1, Var2), 1, function(x) paste(sort(x), collapse=" "))) %>%
  filter(!duplicated(tmp)) %>%
  select(-tmp, -comp) %>%
  rename(title1 = Var1, title2 = Var2, study_id1 = study_id.x, study_id2 = study_id.y, mex_name1 = mex_name.x, mex_name2 = mex_name.y)

write_csv(events, here("data", "events_db.csv"))
write_csv(events_qa, here("data", "data_qa", "events_qa.csv"))
