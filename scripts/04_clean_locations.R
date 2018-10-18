library(magrittr)
library(stringi)
library(sf)
library(ggmap)
library(maps)
library(googlesheets)
library(tidyverse)
library(here)


# Structure Location Data -----------------

segments_db <- read_rds(here("data", "segments_db.rds"))

# structure segments database into location codes dataframe   
locations <- segments_db %>%
  ungroup() %>%
  mutate(segment_id = 1:nrow(.)) %>%
  filter(code_main_cat=="location"|code_main=="country of residence") %>%
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
raw_event_locations <- locations %>%
  group_by(study_id, code_identifiers) %>%
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
event_locations <- unnest_cities_countries %>%
  mutate_at(vars(residence_location, travel_location, hospital, state_province_district), 
            funs(purrr::map(., ~ifelse(length(.x) < 1, ' ', .x)) %>% 
                   unlist(.))) %>%
  select(-segment_id)
  
# Manual Correction of Location Codes -----------------

# Clean Main Locations ---

# Import corrections from manual checking of strange fields and replace in events
cleaned_location_codes <- gs_read(gs_title("amr_db_clean_locs")) %>% 
  bind_rows(tribble(
    ~old,      ~new,   ~field,
    "2",        "",      NA, 
    "maringa´", "",      NA))

event_locations <- event_locations %<>%
  mutate_at(vars(country, city, hospital, state_province_district, 
                 travel_location, residence_location), 
            funs(stri_replace_all_regex(., cleaned_location_codes$old, cleaned_location_codes$new, 
                                        vectorize_all = FALSE))) %>%
  mutate_all(funs(ifelse(. == ' ', NA, trimws(., "both")))) # bring back NA's

# Clean Travel Locations ---

# Create world cities key- some travel locations contain city information. Identify these so they can be separated out. 
data(world.cities)

world_cities <- world.cities %>%
  mutate_all(tolower) %>%
  transmute(city_country = paste(name, country.etc, sep = ", ")) %>%
  bind_rows(tribble(
    ~city_country,
    "new delhi, india", 
    "san francisco, california", 
    "new york city", 
    "mumbai, india"
  ))

# Clean travel city, country data points and split into two columns - travel_city, and travel_country
event_locations <- event_locations %>%
  mutate(travel_location = ifelse(travel_location %in% world_cities$city_country, travel_location,
                                  gsub("\\,", "\\;", travel_location)) %>%
           str_split(., ";") %>%
           purrr::map(., ~trimws(.x, "both")) %>%
           purrr::map(., ~unique(.x)) %>%
           ifelse(is.na(.), "", .)) %>%
  unnest(travel_location) %>%
  mutate(travel_location = str_trim(travel_location)) %>% # might not need this
  group_by(study_id) %>%
  mutate(travel_location_id = paste0("travel_loc_", row_number())) %>%
  ungroup() %>%
  separate(travel_location, into = c("travel_city", "travel_country"), sep = ",", fill = "left") 


# Clean travel states
state_names  <- tribble( #add countries to cities missing country or state
  ~"state_province_district",  ~country,
  "california",         "united states",
  "kanto",              "japan",
  "georgia",            "united states",
  "new york",           "united states"
)

# assign countries to cases when state is known but country is not to help fill in this info
event_locations <- event_locations %>%
  left_join(state_names, by="state_province_district", suffix = c("", "_province")) %>%
  mutate(country = ifelse(is.na(country), country_province, country)) %>%
  select(-country_province) %>%
  mutate_all(funs(gsub(",$", "",.))) # get rid of any trailing commas 

# Geocode ---

register_google(key = Sys.getenv("GOOGLE_MAPS_KEY"))
# geocodeQueryCheck()

pref <- c( #Order from most preferable to least
  "hospital:city:country",
  "hospital:city:state_province_district",
  "hospital:state_province_district:country",
  "hospital:city",
  "hospital:state_province_district",
  "hospital:country",
  "hospital",
  "city:country", 
  "city:state_province_district", 
  "state_province_district:country", 
  "city", 
  "state_province_district", 
  "country")  

event_locations <- as.data.frame(event_locations) %>%
  mutate_all(funs(replace(., is.na(.), "")))

event_locations$study_location_basis <- ""
event_locations$study_location <- ""

# based on above preference order, assign basis for geocode (study_location_basis) and extract location name (study_location)
for (i in rev(seq_along(pref))) {
  cnames <- strsplit(pref[i], ":") %>% unlist
  event_locations_avail <- apply(event_locations[,c("study_id", cnames)]!="", 1, all) #if all relevant columns have values
  event_locations$study_location_basis <- ifelse(event_locations_avail, 
                                     pref[i], 
                                     event_locations$study_location_basis)
  if(length(cnames) > 1){
    event_locations$study_location <- ifelse(event_locations_avail, 
                                 apply(event_locations[, c(cnames)], 1 , paste, collapse = ", "),
                                 event_locations$study_location)
  }else{
    event_locations$study_location <- ifelse(event_locations_avail, 
                                 event_locations[, c(cnames)],
                                 event_locations$study_location)
  }
}

events_db <- event_locations  %>%
 mutate_geocode(location = study_location) %>%
 rename(study_location_lat = lat, study_location_lon=lon)

write_rds(events_db, path = here("data", "events_db.rds"))


# To Do
 #  mutate_geocode(location = travel_location) %>%
 #  rename(travel_location_lat = lat, travel_location_lon=lon) %>%
 #  mutate_geocode(location = residence_location) %>%
 #  rename(residence_location_lat = lat, residence_location_lon=lon) 

# final events db
#saveRDS(events_db, file = here("data", "events_db.rds"))


# To Do ----

# We may be able to just unite and geocode (see below), instead of the for loop, but we would not explicitely retain the hierarchy info.
# Not sure that we need the hierarchical info for the database itself (just for vis) so that may be fine
# I will explore this more before next meeting

# test <- event_locations %>%
  #replace_na(list(hospital = "", city = "", state_province_district = "", country = "")) %>%
  #unite(col = geocode_compare, hospital, city, state_province_district, country, remove = FALSE, sep = " ") 

# Stil need to to incorporate the few lines that clean travel locations and geocode those
