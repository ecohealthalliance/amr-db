library(magrittr)
library(stringi)
library(ggmap)
library(maps)
library(googlesheets)
library(tidyverse)
library(here)
library(assertthat)
library(countrycode)

# Structure Location Data -----------------
segments <- read_csv(here("data-processed", "segments.csv"), col_types = cols(
  study_id = col_double()))
articles_db <- read_csv(here("data-processed","articles-db.csv"), col_types = cols(
  study_id = col_double()))

# Structure segments database into location codes dataframe   
locations <- segments %>%
  filter(code_main %in% c("hospital name", "city", "country","state/province/district", "country of residence", "place traveled to" )) %>%
  mutate(segment = stri_replace_all_regex(segment,
                                          c("\\(|\\)"),
                                          c(""), vectorize = FALSE))

# QA checks -----------------
# clean 
# dups 
# missing
source(here("scripts", "helper", "functions_qa.R"))

# Identify duplicate location segments in studies
assert_that(qa_duplicate(locations, c("study_id", "code_main", "segment")) %>% nrow() == 0)

# Check if more than one location per study - ie multiple events
studies_with_mult_events <- qa_event(locations)

# ID studies with missing locations
studies_missing_locs <- qa_missing(locations)

# Compare with list of studies that were evaluated for missing location codes (review 1)
missing_list <- gs_read(gs_title("amr_db_missing_locations_study_id"), ws = "review_2") 
studies_missing_locs %<>% left_join(., missing_list) # 18812 is ok - it's going to be excluded
# ^ these are missing any location.  Eval for missing country below.  

# Handling of multiple events -----------------

# We assume that most studies correspond to one single event of amr resistance. If there are > 1 base locations (city, country columns) associated with a study, check to see if this assumption is true.
# Did not consider multiple travel, or residence location datapoints as multiple events, only mulitple primary city/country locations
# Later we will refine this to defining events by unique country only, but for now retaining all location info 

# Create events_id
locations %<>% 
  group_by(study_id, code_identifiers, code_main) %>%
  mutate(events_id = paste(study_id, row_number(), sep = "_")) %>%
  ungroup()

# Reshape Data
locations %<>%
  spread(key = code_main, value = segment) %>%
  rename("hospital" = `hospital name`, 
         "travel_location" = `place traveled to`, 
         "residence_location" = `country of residence`, 
         "state_province_district" = `state/province/district`) %>%
  mutate_all(as.character())

# Correction of Location Codes -----------------

# * Clean Main Locations ----

# Import corrections from manual checking of strange fields and replace in events
cleaned_location_codes <- gs_read(gs_title("amr_db_clean_locs")) %>% 
  replace(is.na(.), "")

locations %<>%
  mutate_at(vars(country, city, hospital, state_province_district, 
                 travel_location, residence_location), 
            funs(stri_replace_all_regex(., cleaned_location_codes$old, cleaned_location_codes$new, 
                                        vectorize_all = FALSE))) %>%
  mutate_if(is_character, ~ifelse(. == ' ', NA, trimws(., "both"))) %>% # bring back NA's
  mutate_if(is_character, ~gsub(",$", "",.)) %>%
  mutate_if(is_character, ~gsub("  ", " ",.)) %>%
  mutate_if(is_character, ~gsub("`|\\'|\\~", "", iconv(., to="ASCII//TRANSLIT"))) %>% # remove accents
  mutate(city = replace(city, city=="quebec", "quebec city"))

# * Clean Travel Locations ------

# Clean travel city, travel country data points and split into two separate columns 
locations %<>%
  mutate(travel_location = str_split(travel_location, "; ")) %>%
  unnest(travel_location) %>%
  group_by(study_id) %>%
  mutate(travel_location_id = paste0("travel_loc_", row_number())) %>%
  ungroup() 

# Check what information is available for each study location field. Collpase locations fields in order to geocode
locations %<>%
  mutate(ls = pmap(list(.$hospital, .$city, .$state_province_district, .$country), 
                   function(hospital, city, state_province_district, country) {
                     location_basis_tb <- tribble(
                       ~location,              ~location_fields,
                       hospital,                "hospital",
                       city,                    "city",
                       state_province_district, "state_province_district", 
                       country, "country"
                     )
                     location_basis_tb_n <- na.omit(location_basis_tb)
                     return(location_basis_tb_n)
                   }), 
         study_location_basis = map(ls, ~.x$location_fields), 
         study_location = map(ls, ~.x$location), 
         study_location_basis = map_chr(study_location_basis, ~paste(., collapse = ", ")), 
         study_location = map_chr(study_location, ~paste(., collapse = ", "))) %>%
  select(-ls)

# Manually assign study country if missing
add_country <- gs_read(gs_title("amr_db_add_country")) %>% 
  rename(study_country_add = study_country) 

locations %<>%
  left_join(., add_country) %>%
  mutate(country = ifelse(is.na(study_country_add), country, study_country_add))

# Countries still missing?
missing_countries <- locations %>%
  filter(is.na(country)) %>%
  left_join(articles_db %>% select(study_id, mex_name)) %>%
  arrange(mex_name)
# articles with non-blank study_location need country lookup, others need to be evaluated in mex files
# 8266, 23314 is ok because only a travel location; 9340 to be excluded for no drug

locations %<>%
  mutate(study_location = ifelse(is.na(study_country_add), study_location, paste(study_location, study_country_add, sep = ", ")),
         study_location_basis = ifelse(is.na(study_country_add), study_location_basis, paste(study_location_basis, "country", sep = ", "))) %>%
  select(-study_country_add)

# Geocode ---
register_google(key = Sys.getenv("GOOGLE_MAPS_KEY"))

# prepare a list of locations to be geocoded - 

geocode_locations <- tibble(geocode_loc = c(locations$study_location, locations$travel_location, locations$residence_location)) %>%
  unique() %>%
  mutate(geocode_loc = ifelse(geocode_loc == "", NA, geocode_loc)) %>%
  na.omit()

# If data has already been geocoded, load geocodes but identify if any new codes exist that need to be geocoded

if (file.exists(here("data-processed", "geocode-locations-complete.csv"))) {
  former_geocode_locations <- read_csv(here("data-processed", "geocode-locations-complete.csv"))
  new_to_geocode <- anti_join(geocode_locations, former_geocode_locations) %>%
    unique()
  
  if (nrow(new_to_geocode) == 0) {
    #if no new locations, do not geocode
    geocode_locations_to_use <- former_geocode_locations
    
  } else {
    #geocode new locations
    new_to_geocode %<>%
      mutate_geocode(geocode_loc)
    
    geocode_locations_to_use <- former_geocode_locations %>%
      rbind(new_to_geocode) }
  
} else {
  #if no recorded geolocations, geocode all
  geocode_locations_to_use <- geocode_locations %>%
    mutate_geocode(geocode_loc) 
}

write_csv(geocode_locations_to_use, here("data-processed", "geocode-locations-complete.csv")) 

locations %<>%
  left_join(geocode_locations_to_use, by = c("travel_location" = "geocode_loc")) %>%
  left_join(geocode_locations_to_use, by = c("residence_location" = "geocode_loc"), suffix = c("_travel", "_residence")) %>%
  left_join(geocode_locations_to_use, by = c("study_location" = "geocode_loc")) %>%
  rename(study_city = city,
         study_country = country,
         study_state_province_district = state_province_district,
         study_hospital = hospital,
         lon_study = lon,
         lat_study = lat)


# Add ISO3C ---------------------------------------------------------------

locations %<>%
  mutate(study_iso3c = countrycode(sourcevar = study_country,
                             origin = "country.name",
                             destination = "iso3c")) %>%
  mutate(study_country = countrycode(sourcevar = study_iso3c,
                                origin = "iso3c",
                                destination = "country.name"))

# Final QA check on data cleaning-----------------

# all study locs
study_locs <- locations %>% 
  filter(study_location!="") %>%
  select(study_id, code_identifiers, study_location_basis, study_location, lat_study, lon_study) %>% 
  distinct() %>% 
  mutate(study_id = as.integer(study_id)) %>% 
  arrange(study_id)

filter(study_locs, is.na(study_locs$lat_study)) #these are study locations that were not geocoded

# Find studies that were not checked in review_1 or review_2
clean_list1 <- gs_read(gs_title("amr_db_locations_qa"), ws = "review_1")  %>%
  select(study_id, study_location) %>% slice(-1:-5)
clean_list2 <- gs_read(gs_title("amr_db_locations_qa"), ws = "review_2_study", skip=1) %>%
  select(study_id, study_location)
clean_list <- bind_rows(clean_list1, clean_list2)

updated_studies <- anti_join(study_locs, clean_list)

library(leaflet)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = updated_studies, lng = ~lon_study, lat = ~lat_study, radius = 2,
                   label = ~study_location) 

# all residence locs
res_locs <- locations %>% 
  filter(residence_location!="") %>%
  select(study_id, code_identifiers, residence_location, lat_residence, lon_residence) %>% 
  distinct() %>% 
  mutate(study_id = as.integer(study_id)) %>% 
  arrange(study_id)

filter(res_locs, is.na(res_locs$lat_residence)) #these are study locations that were not geocoded

# Find studies that were not checked in review_2
clean_list <- gs_read(gs_title("amr_db_locations_qa"), ws = "review_2_residence", skip=1) %>%
  select(residence_location)

updated_studies <- anti_join(res_locs, clean_list)

# all travel locs
trav_locs <- locations %>% 
  filter(travel_location!="") %>%
  select(study_id, code_identifiers, travel_location, lat_travel, lon_travel) %>% 
  distinct() %>% 
  mutate(study_id = as.integer(study_id)) %>% 
  arrange(study_id)

filter(trav_locs, is.na(trav_locs$lat_travel)) #these are study locations that were not geocoded

# Find studies that were not checked in review_2
clean_list <- gs_read(gs_title("amr_db_locations_qa"), ws = "review_2_travel", skip=1) %>%
  select(travel_location)

updated_studies <- anti_join(trav_locs, clean_list)

write_csv(locations, here("data-processed", "locations.csv"))
