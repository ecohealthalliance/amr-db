library(magrittr)
library(stringi)
library(ggmap)
library(maps)
library(googlesheets)
library(tidyverse)
library(here)


# Structure Location Data -----------------

segments <- read_csv(here("data", "segments.csv"))

# structure segments database into location codes dataframe   
locations_raw <- segments %>%
  mutate(segment_id = 1:nrow(.)) %>%
  filter(code_main_cat == "location" | code_main == "country of residence") %>%
  select(-code_main_cat) %>%
  spread(key = code_main, value = segment) %>%
  unique() %>%
  rename_all(tolower) %>%
  rename("hospital" = `hospital name`, 
         "travel_location" = `place traveled to`, 
         "residence_location" = `country of residence`, 
         "state_province_district" = `state/province/district`) %>%
  mutate_all(funs(gsub("\\|", ", ", .)), 
             funs(trimws(., "both")), 
             funs(tolower))

# Create Events Database -----------------

# We assume that most studies correspond to one single event of amr resistance. If there are > 1 base locations (city, country columns) associated with a study, check to see if this assumption is true.
# Did not consider multiple travel, or residence location datapoints as multiple events, only mulitple primary city/country locations
# Later we will refine this to defining events by unique country only, but for now retaining all location info 

# Group codes by "event" (study + code identifiers) and create list columns. Create an event_id column, and omit NA's
raw_event_locations <- locations_raw %>%
  group_by(study_id, code_identifiers, code_identifiers_link) %>%
  summarise_all(funs(list)) %>%
  mutate(event_id = paste0(study_id, "_", row_number())) %>%
  mutate_at(vars(city, country, residence_location, hospital, travel_location, state_province_district),
            funs(purrr::map(., ~na.omit(.x))))
  

# Find studies that have two cities or more (but no identifier codes). Re-id them as two-event studies (may want to double check these manually as well in mex files)
two_cities_fix <- raw_event_locations %>%
  filter(study_id %in% (filter(raw_event_locations, map(city, ~length(.x)) > 1) %>% pull(study_id))) %>% # get all events for that study id (not just double locations ones) to properly re-index generated event id's
  unnest(city, .drop = FALSE) %>%
  group_by(study_id) %>%
  mutate(event_id = paste0(study_id, "_", row_number()), # re-index them
         country = replace(country, city == "alberta", "canada"),  # on-the-spot cleaning of these 
         country = replace(country, city == "pristina", "kosovo"))

# Find studies that have two countries or more - there is only one observation that has this b/c "rome, italy" is in the country column - fixed
two_countries_fix <- raw_event_locations %>%
  filter(study_id %in% (filter(raw_event_locations, purrr::map(country, ~length(.x)) > 1) %>% pull(study_id))) %>%
  mutate(city = "rome", country = list("italy")) %>%
  unnest(country)
  
# Unnest city and country columns in main location dataframe and bind with fixed city and countries
unnest_cities <- raw_event_locations %>%
  filter(!(study_id %in% c(two_cities_fix$study_id, two_countries_fix$study_id))) %>% # filter out problem studies with more than one city or country
  unnest(city) %>%   
  right_join(select(raw_event_locations, -city)) %>%  # This doesn't seem ideal, but unnest drops data points with empty location lists so this joins them back
  bind_rows(two_cities_fix)

unnest_cities_countries <- unnest_cities %>%
  unnest(country) %>%
  right_join(select(unnest_cities, -country)) %>%
  bind_rows(two_countries_fix) 

# Replace NA's with blank (in order to retain all rows when unnesting) and unnest
locations <- unnest_cities_countries %>%
  mutate_at(vars(residence_location, travel_location, hospital, state_province_district), 
            funs(purrr::map(., ~ifelse(length(.x) < 1, ' ', .x)) %>% 
                   unlist(.))) %>%
  select(-segment_id)
  
# Correction of Location Codes -----------------

# * Clean Main Locations ----

# Import corrections from manual checking of strange fields and replace in events
cleaned_location_codes <- gs_read(gs_title("amr_db_clean_locs")) %>% 
  bind_rows(tribble(
    ~old,      ~new,   ~field,
    "2",        "",      NA, 
    "maringa´", "",      NA))

locations <- locations %<>%
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
locations <- locations %>%
  left_join(state_names, by="state_province_district", suffix = c("", "_province")) %>%
  mutate(country = ifelse(is.na(country), country_province, country)) %>%
  select(-country_province) %>%
  mutate_all(funs(gsub(",$", "",.))) # get rid of any trailing commas 

# * Clean Travel Locations ------

# Create world cities key- some travel locations contain city information. Identify these so they can be separated out. 
data(world.cities)

# These are not in the world cities dataframe 
travel_location_fix <- tribble(
  ~city, ~country,
  "new delhi", "india", 
  "san francisco", "california", 
  "new york city", "new york",
  "mumbai",  "india", 
  "kolkata", "india", 
  "medina", "saudi arabia" # because top population medina is in columbia but study 14475 is probably referring to saudi arabia where the other travel case was
)

# Take top pop city when cities share names - could potentially include some sort of logic that decides what city based on proximity to other locations in the future? But it's difficut to make generalizable assumptions, especially with travel locations
world_cities <- world.cities %>%
  mutate_all(tolower) %>%
  rename("city" = name, "country" = country.etc) %>%
  group_by(city) %>%
  top_n(1, pop) %>% 
  filter(city != "medina") %>% # Medina, Columbia is top pop city but Medina, Saudi Arabia is in our dataset 
  bind_rows(travel_location_fix) %>%
  mutate(city_country = paste(city, country, sep = ", "))


# Clean travel city, travel country data points and split into two separate columns 
locations <- locations %>%
  mutate(travel_location = ifelse(travel_location %in% world_cities$city_country, travel_location,
                                  gsub("\\,", "\\;", travel_location)) %>%
           str_split(., ";") %>%
           purrr::map(., ~trimws(.x, "both")) %>%
           purrr::map(., ~unique(.x)) %>%
           ifelse(is.na(.), "", .)) %>%
  unnest(travel_location) %>%
  group_by(study_id) %>%
  mutate(travel_location_id = paste0("travel_loc_", row_number())) %>%
  ungroup() %>%
  separate(travel_location, into = c("travel_location_city", "travel_location_country"), sep = ",", fill = "left") %>%
  mutate(travel_location_city = ifelse((travel_location_country %in% world_cities$city & !(travel_location_country %in% world_cities$country)), travel_location_country, travel_location_city), 
         travel_location_country = map_chr(travel_location_country, function(potentially_a_city) {
           ifelse((potentially_a_city %in% world_cities$city & !(potentially_a_city %in% world_cities$country)), filter(world_cities, city == potentially_a_city) %>% pull(country), potentially_a_city) }))

# Geocode ---

register_google(key = Sys.getenv("GOOGLE_MAPS_KEY"))

# Check what information is available for each study location field. Collpase locations fields in order to geocode
locations <- locations %>%
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

locations <- locations %>%
  mutate(ls = map2(.$travel_location_city, .$travel_location_country, 
                   function(city, country) {
                     location_basis_tb <- tribble(
                      ~location,   ~location_fields,
                       city,        "city",
                       country,     "country"
                     )
                     location_basis_tb_n <- na.omit(location_basis_tb)
                     return(location_basis_tb_n)
                   }), 
         travel_location_basis = map(ls, ~.x$location_fields), 
         travel_location = map(ls, ~.x$location), 
         travel_location_basis = map_chr(travel_location_basis, ~paste(., collapse = ", ")), 
         travel_location = map_chr(travel_location, ~paste(., collapse = ", "))) %>%
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


locations <- locations %>%
  left_join(geocode_locations_to_use, by = c("travel_location" = "geocode_loc")) %>%
  left_join(geocode_locations_to_use, by = c("residence_location" = "geocode_loc"), suffix = c("_travel", "_residence")) %>%
  left_join(geocode_locations_to_use, by = c("study_location" = "geocode_loc"))

write_csv(locations, here("data", "locations.csv"))


