#'---
#' title: "exploratory_data"
#' date: "2018-10-23"
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

#+ r setup, include = FALSE

#' -----------------Bacteria-----------------
```{r bacteria_read, include = FALSE}
bacteria <- read_csv(here("data", "bacteria_genus_species_db.csv")) %>%
  select(
    study_id,
    segment,
    code_main,
    code_identifiers,
    code_identifiers_link,
    ncbi_rank,
    ncbi_preferred_label,
    ncbi_parent_rank,
    ncbi_parent_name
  )
#+ r bacteria_read, include = FALSE

```{r bacteria}
#' Count by rank
bacteria %>%
  group_by(ncbi_rank) %>%
  count() %>% 
  spread(ncbi_rank, n) %>%
  kable() 

#' Count by name and parent
bacteria_sum <- bacteria %>%
  group_by(ncbi_rank, ncbi_preferred_label, ncbi_parent_rank, ncbi_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(bacteria), 1)) %>%
  ungroup()

kable(bacteria_sum %>% slice(1:10))

ggplot(bacteria_sum[bacteria_sum$n > 4,], aes(x = reorder(ncbi_preferred_label, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(x = "Most common species", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#+ r bacteria

#' -----------------Drugs-----------------
```{r drugs_read, include = FALSE}
drugs <- read_csv(here("data", "drugs_db.csv")) %>%  
  group_by(
    study_id,
    segment,
    code_main,
    code_identifiers,
    code_identifiers_link,
    segment_combo,
    mesh_rank,
    mesh_preferred_label
  ) %>%
  summarize(mesh_parent_name = paste(unique(mesh_parent_name), collapse =
                                       ", ")) %>%
  ungroup()
#+ r drugs_read, include = FALSE

```{r drugs}
#' Count by rank
drugs %>%
  group_by(mesh_rank) %>%
  count() %>% 
  spread(mesh_rank, n) %>%
  kable() 

#' Count by name and parent (note that dups have not yet been handled 10/23/18)
drugs_sum <- drugs %>%
  group_by(mesh_rank, mesh_preferred_label, mesh_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(drugs), 1)) %>%
  ungroup()

kable(drugs_sum %>% slice(1:10)) 

ggplot(drugs_sum[drugs_sum$percent > 1,], aes(x = reorder(mesh_preferred_label, -n), y = n, fill = mesh_rank)) +
  geom_bar(stat = "identity") +
  labs(x = "Most common drugs", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#+ r drugs

#' -----------------Bacteria + Drugs links-----------------
```{r bacteria_drugs, include = TRUE}
paired_segments <- list(bacteria, drugs) %>%
  map(., ~ mutate(.x, join_id = ifelse(
    c(
      grepl("drug resisted", code_identifiers_link) &
        grepl("binomial", code_identifiers_link)
    ), code_identifiers, NA
  ))) %>%
  map(., ~ select(.x, -code_identifiers, -code_identifiers_link, -segment, -code_main)) %>%
  reduce(full_join) %>%
  filter(!is.na(ncbi_preferred_label), !is.na(mesh_preferred_label)) %>%
  mutate(bacteria_drug_pair = paste(ncbi_preferred_label, mesh_preferred_label, sep = " - "))

#' Count by combo
paired_segments_count <- paired_segments %>%
  group_by(bacteria_drug_pair) %>%
  count(sort = TRUE) %>%
  ungroup() %>%
  filter(n > 15)

paired_segments_count %>%
  slice(1:10) %>%
  kable() 

sub_paired_segments <- paired_segments %>%
  select(ncbi_preferred_label, mesh_preferred_label, bacteria_drug_pair) %>%
  rename(bacteria = ncbi_preferred_label, drug = mesh_preferred_label) %>% 
  filter(bacteria_drug_pair %in% paired_segments_count$bacteria_drug_pair)

ggparallel(list("drug", "bacteria"), 
           data=as.data.frame(sub_paired_segments), 
           text.angle=0) +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired") +
  theme_bw()+
  theme(legend.position = "none")
#+ r bacteria_drugs, include = TRUE
