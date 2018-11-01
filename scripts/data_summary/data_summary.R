#'---
#' title: "exploratory_data"
#' date: "2018-11-02"
#' output: github_document
#' always_allow_html: yes
#' ---

```{r setup, include = FALSE}
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


#+ r setup, include = FALSE

#' -----------------Locations-----------------
```{r locations, include = FALSE}
locs <- read_csv(here("data", "locations.csv")) %>%
  mutate(study_resolution = gsub(",.*$", "", study_location_basis))
study_locs <- st_as_sf(locs %>% filter(!is.na(study_location)) , coords = c("lon_study", "lat_study"), crs = 4326)
travel_locs <- st_as_sf(locs %>% filter(!is.na(travel_location)) , coords = c("lon_travel", "lat_travel"), crs = 4326)
residence_locs <- st_as_sf(locs %>% filter(!is.na(residence_location)) , coords = c("lon_residence", "lat_residence"), crs = 4326)

#+ r locations, include = FALSE

#' Study Locations
```{r locations2, echo = FALSE}
mapview(study_locs, zcol="study_resolution", col.regions = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1")), alpha.regions=1, layer.name="Study Location Spatial Resolution")
#+ r locations2, echo = FALSE

#' Travel Locations
```{r locations3, echo = FALSE}
mapview(travel_locs)
#+ r locations3, echo = FALSE

#' Residence Locations
```{r locations4, echo = FALSE}
mapview(residence_locs)
#+ r locations4, echo = FALSE

#' -----------------Bacteria-----------------
```{r bacteria, include = FALSE}
bacteria <- read_csv(here("data", "bacteria_genus_species_db.csv")) %>%
  select(
    study_id,
    segment,
    code_main,
    code_identifiers,
    code_identifiers_link,
    bacteria_rank,
    bacteria_preferred_label,
    bacteria_parent_rank,
    bacteria_parent_name
  )

bacteria_by_rank <- bacteria %>%
  group_by(bacteria_rank) %>%
  count() %>% 
  spread(bacteria_rank, n) 

bacteria_sum <- bacteria %>%
  group_by(bacteria_rank, bacteria_preferred_label, bacteria_parent_rank, bacteria_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(bacteria), 1)) %>%
  ungroup()
#+ r bacteria, include = FALSE

#' Count by rank
```{r bacteria2, echo = FALSE}
kable(bacteria_by_rank) 
#+ r bacteria2, echo = FALSE

#' Count by name and parent
```{r bacteria3, echo = FALSE}
kable(bacteria_sum %>% slice(1:5))

ggplot(bacteria_sum[bacteria_sum$n > 4,], aes(x = reorder(bacteria_preferred_label, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(x = "Most common species", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
#+ r bacteria3, echo = FALSE

#' -----------------Drugs-----------------
```{r drugs, include = FALSE}
drugs <- read_csv(here("data", "drugs.csv")) %>%  
  group_by(
    study_id,
    segment,
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

drugs_by_rank <- drugs %>%
  group_by(drug_rank) %>%
  count() %>% 
  spread(drug_rank, n)

drugs_sum <- drugs %>%
  group_by(drug_rank, drug_preferred_label, drug_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(drugs), 1)) %>%
  ungroup()

drugs_sum2 <- drugs %>%
  mutate(drug_group = ifelse(drug_rank=="drug name", drug_parent_name, drug_preferred_label)) %>%
  group_by(drug_group) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(drugs), 1)) %>%
  ungroup()
#+ r drugs, include = FALSE

#' Count by rank
```{r drugs1, echo = FALSE}
kable(drugs_by_rank) 
#+ r drugs1, echo = FALSE

#' Count by name and parent
```{r drugs2, echo = FALSE}
kable(drugs_sum %>% slice(1:5)) 

ggplot(drugs_sum[drugs_sum$percent > 1,], aes(x = reorder(drug_preferred_label, -n), y = n, fill = drug_rank)) +
  geom_bar(stat = "identity") +
  labs(x = "Most common drugs", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
#+ r drugs2, echo = FALSE

#' Count by group only
```{r drugs3, echo = FALSE}
kable(drugs_sum2 %>% slice(1:5)) 

ggplot(drugs_sum2[drugs_sum2$percent > 1,], aes(x = reorder(drug_group, -n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Most common drug groups", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
#+ r drugs3, echo = FALSE

#' -----------------Bacteria + Drugs Paired-----------------
```{r bacteria_drugs, include = FALSE}
paired_segments <- list(bacteria, drugs) %>%
  map(., ~ mutate(.x, join_id = ifelse(
    c(
      grepl("drug resisted", code_identifiers_link) &
        grepl("binomial", code_identifiers_link)
    ), code_identifiers, NA
  ))) %>%
  map(., ~ select(.x, -code_identifiers, -code_identifiers_link, -segment, -code_main)) %>%
  reduce(full_join) %>%
  filter(!is.na(bacteria_preferred_label), !is.na(drug_preferred_label)) %>%
  mutate(bacteria_drug_pair = paste(bacteria_preferred_label, drug_preferred_label, sep = " - "))

paired_segments_count <- paired_segments %>%
  group_by(bacteria_drug_pair) %>%
  count(sort = TRUE) %>%
  ungroup() 

sub_paired_segments <- paired_segments %>%
  left_join(paired_segments_count) %>%
  select(bacteria_preferred_label, drug_preferred_label, bacteria_drug_pair, n) %>%
  rename(bacteria = bacteria_preferred_label, drug = drug_preferred_label) %>%
  arrange(-n) 

idat <- sub_paired_segments %>% filter(n>15)  %>% select(-bacteria_drug_pair, -n) 
g <- graph.data.frame(idat, directed = FALSE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type 
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

idat2 <- sub_paired_segments  %>% select(-bacteria_drug_pair, -n) %>% distinct() %>%slice(1:25)
g2 <- graph.data.frame(idat2, directed = FALSE)
bipartite.mapping(g2)
V(g2)$type <- bipartite_mapping(g2)$type 
V(g2)$color <- ifelse(V(g2)$type, "lightblue", "salmon")
V(g2)$shape <- ifelse(V(g2)$type, "circle", "square")
E(g2)$color <- "lightgray"
#+ r bacteria_drugs, include = FALSE

#' Count by linkages (12 most common linkages)
```{r bacteria_drugs2, echo = FALSE}
# paired_segments_count %>%
#   slice(1:5) %>%
#   kable() 

ggparallel(list("drug", "bacteria"), 
           data=as.data.frame(sub_paired_segments %>% filter(n>15)), 
           text.angle=0) +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired") +
  theme_bw()+
  theme(legend.position = "none")

plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")
#+ r bacteria_drugs2, echo = FALSE
