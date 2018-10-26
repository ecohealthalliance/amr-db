library(magrittr)
library(stringi)
library(ggmap)
library(maps)
library(googlesheets)
library(tidyverse)
library(here)

# Structure Location Data -----------------

segments <- read_csv(here("data", "segments.csv"))

# Structure segments database into location codes dataframe   
locations <- segments %>%
  filter(code_main_cat == "location" | code_main == "country of residence") %>%
  select(-code_main_cat) 

# QA checks -----------------
# clean 
# dups 
# missing
source(here("scripts", "helper_scripts", "qa.R"))

# Identify duplicate location segments in studies
studies_with_dups <- qa_duplicate(locations, c("study_id", "code_main", "segment"))

# Check if more than one location per study - ie multiple events
studies_with_mult_events <- qa_event(locations)

# ID studies with missing locations
studies_missing_locs <- qa_missing(locations)

# Compare with list of studies that were evaluated for missing location codes (review 1)
missing_list <- gs_read(gs_title("amr_db_missing_locations_study_id"), ws = "review_1") 
studies_missing_locs %<>% left_join(., missing_list)

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
  bind_rows(tribble(
    ~old,      ~new,   ~field,
    "2",        "",      NA, 
    "maringa´", "",      NA))

locations %<>%
  mutate_at(vars(country, city, hospital, state_province_district, 
                 travel_location, residence_location), 
            funs(stri_replace_all_regex(., cleaned_location_codes$old, cleaned_location_codes$new, 
                                        vectorize_all = FALSE))) %>%
  mutate_all(funs(ifelse(. == ' ', NA, trimws(., "both")))) # bring back NA's

# Clean States/Provinces/Districts field
state_names  <- tribble( #add countries to cities missing country or state
  ~"state_province_district",  ~country,
  "california",         "united states",
  "kanto",              "japan",
  "georgia",            "united states",
  "new york",           "united states"
)

# Assign countries to cases when state is known but country is not to help fill in this info
locations %<>%
  left_join(state_names, by="state_province_district", suffix = c("", "_province")) %>%
  mutate(country = ifelse(is.na(country), country_province, country)) %>%
  select(-country_province) %>%
  mutate_all(funs(gsub(",$", "",.))) # get rid of any trailing commas 

# * Clean Travel Locations ------

# Clean travel city, travel country data points and split into two separate columns 
locations %<>%
  mutate(travel_location = str_split(travel_location, "; ")) %>%
  unnest(travel_location) %>%
  group_by(study_id) %>%
  mutate(travel_location_id = paste0("travel_loc_", row_number())) %>%
  ungroup() 

# Geocode ---

register_google(key = Sys.getenv("GOOGLE_MAPS_KEY"))

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

# Geocode ---------

# prepare a list of locations to be geocoded - 

geocode_locations <- tibble(geocode_loc = c(locations$study_location, locations$travel_location, locations$residence_location)) %>%
  unique() %>%
  mutate(geocode_loc = ifelse(geocode_loc == "", NA, geocode_loc)) %>%
  na.omit()

# If data has already been geocoded, load geocodes but identify if any new codes exist that need to be geocoded

if (file.exists(here("data-raw", "geocode_locations_complete.csv"))) {
  former_geocode_locations <- read_csv(here("data-raw", "geocode_locations_complete.csv"))
  new_to_geocode <- anti_join(geocode_locations, former_geocode_locations) %>%
    unique()
  
  if (nrow(new_to_geocode) == 0) {
    geocode_locations_to_use <- former_geocode_locations
    
  } else {
    new_to_geocode %<>%
      mutate_geocode(geocode_loc)
    
    geocode_locations_to_use <- former_geocode_locations %>%
      rbind(new_to_geocode) }
  
  } else {
    geocode_locations_to_use <- geocode_locations %>%
      mutate_geocode(geocode_loc) 
    
    write_csv(geocode_locations_to_use, here("data-raw", "geocode_locations_complete.csv")) }


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

write_csv(locations, here("data", "locations.csv"))


