library(tidyverse)
library(here)
library(RColorBrewer)
library(sf)
library(leaflet)
library(leaflet.extras)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapview)
library(countrycode)

events <- read_csv(here("data", "events_db.csv"))

locs <- read_csv(here("data", "locations.csv")) %>%
  mutate(study_resolution = gsub(",.*$", "", study_location_basis)) %>%
  filter(study_id %in% events$study_id)

study_locs <- locs %>% 
  filter(!is.na(study_location)) 
  
admin <- ne_countries(type='countries', scale = 'large') %>%
  st_as_sf() %>%
  mutate(iso3c = countrycode(sourcevar = name,
                             origin = "country.name",
                             destination = "iso3c"),
         iso3c = ifelse(is.na(iso3c), iso_a3_eh, iso3c)) %>%
  select(name, iso3c)

events_sum <- events %>% 
  group_by(study_iso3c) %>%
  count(name = "AMR Events") %>%
  ungroup() 
  
admin <- left_join(admin, events_sum, by = c("iso3c" = "study_iso3c")) %>%
  mutate(`AMR Events` = replace_na(`AMR Events`, 0))
  
pal <- colorNumeric("OrRd", domain = admin$`AMR Events`, na.color = "#e9e9f0")

caption <- glue::glue(nrow(events), " AMR emergence events<br/>",
                      n_distinct(events$study_country), " countries<br/>",
                      n_distinct(events$drug), " antimicrobial drugs<br/>",
                      n_distinct(events$bacteria), " resistant bacteria<br/>",
                      str_sub(min(events$start_date), 1, 4), " - ",  str_sub(max(events$start_date), 1, 4))

lf <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addFullscreenControl(position = "topright") %>%
  addPolygons(data = admin, 
              stroke = TRUE, color = "#46464a", weight = 1,
              fill = TRUE, fillColor = ~pal(`AMR Events`), fillOpacity = 0.9,
              label = ~paste0(name, ": ", `AMR Events`)) %>%
  addLegend(data = admin, pal = pal, values = ~`AMR Events`, position = "bottomright") %>%
  addCircleMarkers(data = study_locs,  radius = 3,
                   lng = ~jitter(lon_study), lat = ~jitter(lat_study), 
                   stroke = TRUE, color = "#210106", opacity = 1, weight = 1,
                   fill = TRUE, fillColor = "#210106", fillOpacity = 0.5,
                   label = ~study_location) %>%
  addControl(caption)
            
lf
htmlwidgets::saveWidget(lf, here("figures/map.html"))
