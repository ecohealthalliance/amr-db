#'---
#' title: "exploratory_data"
#' output: github_document
#' always_allow_html: yes
#' ---
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(tidyverse)
library(magrittr)
library(here)
library(knitr)
library(kableExtra)
library(ggparallel)
library(RColorBrewer)
library(sf)
library(mapview)
library(igraph)

events <- read_csv(here("data", "events_db.csv"))
segments <- read_csv(here("data", "segments.csv"))
segments_excluded <- read_csv(here("data", "segments_excluded.csv"))
articles_db <- read_csv(here("data", "articles_db.csv")) %>%
  mutate(title = tolower(title)) %>%
  filter(study_id %in% unique(segments$study_id)) 
articles_db17 <- read_rds(here("data", "2017-dat", "complete.rds")) #articles from 2017 review

#' -----------------Locations-----------------
#+ r locations
locs <- read_csv(here("data", "locations.csv")) %>%
  mutate(study_resolution = gsub(",.*$", "", study_location_basis))
study_locs <- st_as_sf(locs %>% filter(!is.na(study_location)) , coords = c("lon_study", "lat_study"), crs = 4326)
travel_locs <- st_as_sf(locs %>% filter(!is.na(travel_location)) , coords = c("lon_travel", "lat_travel"), crs = 4326)
residence_locs <- st_as_sf(locs %>% filter(!is.na(residence_location)) , coords = c("lon_residence", "lat_residence"), crs = 4326)

#' Study Locations
mapview(study_locs, zcol="study_resolution", col.regions = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1")), alpha.regions=1, layer.name="Study Location Spatial Resolution")

#' Travel Locations
mapview(travel_locs)

#' Residence Locations
mapview(residence_locs)

#' -----------------Bacteria-----------------
#+ r bacteria
bacteria <- events %>%
  select(
    study_id,
    bacteria_rank,
    bacteria_preferred_label,
    bacteria_preferred_label_abbr,
    bacteria_parent_rank,
    bacteria_parent_name
  ) %>% distinct()

bacteria_by_rank <- bacteria %>%
  select(-bacteria_parent_rank, -bacteria_parent_name) %>%
  group_by(bacteria_rank) %>%
  count() %>%
  spread(bacteria_rank, n)

bacteria_sum <- bacteria %>%
  group_by(bacteria_rank, bacteria_preferred_label, bacteria_preferred_label_abbr, bacteria_parent_rank, bacteria_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(bacteria), 1)) %>%
  ungroup() %>%
  filter(!is.na(bacteria_preferred_label))

#' Count by rank
kable(bacteria_by_rank)

#' Count by name and parent
kable(bacteria_sum %>% slice(1:5), format = "markdown")

ggplot(bacteria_sum[bacteria_sum$n > 4,], aes(x = reorder(bacteria_preferred_label_abbr, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(title = "Most common bacteria", x = "", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12,face = "italic"))

#' -----------------Drugs-----------------
#+ r drugs
drugs <- events %>%
  mutate(drug_preferred_label_abbr = gsub("drug|combination|drug combinations, ", "", drug_preferred_label),
         drug_preferred_label_abbr = trimws(drug_preferred_label_abbr),
         drug_parent_name_abbr = gsub("drug|combination|drug combinations, ", "", drug_parent_name),
         drug_parent_name_abbr = trimws(drug_parent_name_abbr)) %>%
  select(
    study_id,
    segment_drug_combo,
    drug_rank,
    drug_preferred_label_abbr,
    drug_parent_name_abbr
  )

drugs_by_rank <- drugs %>%
  group_by(drug_rank) %>%
  count() %>%
  spread(drug_rank, n)

drugs_sum <- drugs %>%
  group_by(drug_rank, drug_preferred_label_abbr, drug_parent_name_abbr) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(drugs), 1)) %>%
  ungroup() %>%
  filter(!is.na(drug_preferred_label_abbr))

drugs_sum2 <- drugs %>%
  mutate(drug_group = ifelse(drug_rank=="drug name", drug_parent_name_abbr, drug_preferred_label_abbr)) %>%
  group_by(drug_group) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(drugs), 1)) %>%
  ungroup() %>%
  filter(!is.na(drug_group)) %>%
  mutate(drug_group_lab = gsub(", ", "&\n", drug_group))

#' Count by rank
kable(drugs_by_rank)

#' Count by name and parent
kable(drugs_sum %>% slice(1:5))

ggplot(drugs_sum[drugs_sum$percent > 1,], aes(x = reorder(drug_preferred_label_abbr, -n), y = n, fill = drug_rank)) +
  geom_bar(stat = "identity") +
  
  labs(title = "Most common drugs", x = "", y = "Number of studies", fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

#' Count by group only
kable(drugs_sum2 %>% slice(1:5) %>% select(-drug_group_lab))

ggplot(drugs_sum2[drugs_sum2$percent > 1,], aes(x = reorder(drug_group, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(title = "Most common drug groups", x = "", y = "Number of studies") +
  #coord_fixed(ratio = 0.01) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        title = element_text(size = 12))

#' -----------------Bacteria + Drugs Paired-----------------
#+ r bacteria_drugs
paired_segments_count <- events %>%
  select(drug_preferred_label, bacteria_preferred_label_abbr) %>%
  group_by(drug_preferred_label, bacteria_preferred_label_abbr) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  na.omit() %>%
  rename(bacteria = bacteria_preferred_label_abbr, drug = drug_preferred_label) %>%
  arrange(-n)

idat <- paired_segments_count %>% filter(n>15) %>% select( -n)
g <- graph.data.frame(idat, directed = FALSE)
#bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

idat2 <- paired_segments_count %>% distinct() %>% slice(1:25)
g2 <- graph.data.frame(idat2, directed = FALSE)
#bipartite.mapping(g2)
V(g2)$type <- bipartite_mapping(g2)$type
V(g2)$color <- ifelse(V(g2)$type, "lightblue", "salmon")
V(g2)$shape <- ifelse(V(g2)$type, "circle", "square")
E(g2)$color <- "lightgray"

#' Count by linkages (12 most common linkages)
paired_segments_count %>%
  distinct() %>%
  slice(1:10) %>%
  kable(format = "markdown")

ggparallel(list("drug", "bacteria"),
           data=as.data.frame(paired_segments_count %>% filter(n>15)),
           text.angle=0) +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired") +
  theme_bw()+
  theme(legend.position = "none")

plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")

#' Count by pub date
pub_date_count <- articles_db %>%
  group_by(year) %>%
  count() %>% ungroup()

pub_date_count17 <- articles_db17 %>%
  group_by(year) %>%
  count() %>%
  filter(!is.na(year)) %>% ungroup()

ggplot(pub_date_count, aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(title = "Publications by year", x = "", y = "") +
  scale_x_continuous(breaks = unique(pub_date_count$year)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

ggplot(pub_date_count17, aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(title = "Publications by year", x = "", y = "") +
  scale_x_continuous(breaks = unique(pub_date_count17$year)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

pub_date_count_all <- full_join(pub_date_count, pub_date_count17, by="year") %>%
  rename(current = n.x, prev = n.y) %>%
  mutate(total = prev - current) %>%
  select(-prev) %>%
  gather(key = case, value = n, -year ) %>%
  mutate(case = factor(case, levels = c("total", "current")))

ggplot(pub_date_count_all, aes(x = year, y = n, fill = case)) +
  geom_bar(stat = "identity") +
  labs(title = "Publications by year", x = "", y = "", fill = "") +
  scale_x_continuous(breaks = unique(pub_date_count17$year)) +
  scale_fill_manual(values = c(total = "gray", current = "green3")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
#broken axis?

#' Count by exclusion criteria
segments_excluded_count <- segments_excluded %>%
  filter(code_main_cat=="exclusion") %>%
  group_by(code_main) %>%
  count()

# check for studies with more than one exclusion criteria
segments_excluded_check <- segments_excluded %>%
  filter(code_main_cat=="exclusion") %>%
  mutate(dup = duplicated(study_id) |
           duplicated(study_id, fromLast = TRUE)) %>%
  filter(dup == TRUE) %>%
  group_by(study_id) %>%
  count() #27 studies with 2 exclusion criteria

ggplot(segments_excluded_count, aes(x = reorder(code_main, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(title = "Count of Studies by Exclusion Criteria", x = "", y = "", caption = "27 studies had 2 exclusion criteria") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))



