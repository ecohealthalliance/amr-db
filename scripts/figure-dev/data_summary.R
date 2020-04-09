library(tidyverse)
library(here)
library(cowplot)


# Get data ----------------------------------------------------------------
events <- read_csv(here("events-db.csv")) %>%
  mutate(drug = str_remove_all(drug, "drug|combination")) %>%
  mutate(drug = str_replace_all(drug, ",", " + ")) %>%
  mutate(drug = str_squish(drug)) %>%
  mutate(drug = fct_infreq(drug)) %>%
  mutate(bacteria = str_to_sentence(bacteria)) %>%
  mutate(bacteria = fct_infreq(bacteria))

# summary counts
# see amr-db.csv in AMR repo for all article abstracts reviewed - 23770
# see link_tracking_current_07122018.xls for all promed abstracts reviewed - 1196
23770 + 1196
read_csv(here("data-processed","articles-db.csv")) %>% filter(downloaded == "yes") %>% nrow() # number abstracts reviewed
# promed_article_index.csv has number of promeds reviewed - 208
1583 + 208
  
n_distinct(events$study_id) # count studies in database
n_distinct(events$study_iso3c) # count countries
nrow(events) # count events

events %>%
  group_by(study_iso3c) %>%
  count(sort=T) 

events %>%
  select(study_id, study_country) %>%
  distinct() %>%
  group_by(study_country) %>%
  count(sort=T) 

top_drugs <- events %>% 
  select(bacteria, drug) %>%
  group_by(drug) %>%
  mutate(n_events = n()) %>%
  ungroup() %>%
  distinct() %>%
  group_by(drug, n_events) %>%
  count(name = "n_bacteria") %>%
  ungroup() %>%
  arrange(-n_events) %>%
  mutate(lab = ifelse(n_events == max(n_events), paste(n_bacteria, "distinct resistant bacteria"), n_bacteria)) %>%
  slice(1:12)

colistin <- events %>% 
  filter(drug == "colistin")
n_distinct(colistin$study_country)
n_distinct(colistin$bacteria)

top_bact <- events %>% 
  select(bacteria, drug) %>%
  group_by(bacteria) %>%
  mutate(n_events = n()) %>%
  ungroup() %>%
  distinct() %>%
  group_by(bacteria, n_events) %>%
  count(name = "n_drugs") %>%
  ungroup() %>%
  arrange(-n_events) %>%
  mutate(lab = ifelse(n_events == max(n_events), paste(n_drugs, "distinct drugs resisted"), n_drugs)) %>%
  slice(1:12)


# Heatmaps -----------------------------------------------------------------

heatmap_mat <- events %>%
  group_by(drug, bacteria) %>%
  count() %>%
  ungroup() %>%
  full_join(., expand(., drug, bacteria)) %>%
  filter(bacteria %in% top_bact$bacteria) %>%
  filter(drug %in% top_drugs$drug) 

heatmap <- ggplot(heatmap_mat %>% 
                    mutate(bacteria = fct_relabel(bacteria, ~str_replace(., " ", "\n"))) %>%
                    mutate(drug = fct_rev(drug)),
                  aes(bacteria, drug)) + 
  geom_tile(color = "black", aes(fill = n), alpha = 0.5) +
  geom_text(aes(label = n), color = "black", size = 7) +
  scale_fill_viridis_c(guide = FALSE, alpha = 0.5, na.value="transparent") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, face = "italic", color = "black"),
        axis.ticks = element_blank())
ggsave(here("figures/drug-bacteria-heatmap.png"), width = 19.5, height = 9)

drugs_total <- top_drugs %>% 
  mutate(drug = fct_rev(drug)) %>%
  ggplot(., aes(x = drug, y = n_events)) +
  geom_bar(stat = "identity", fill = "gray70") +
  geom_text(aes(label=lab), y = 2, hjust = "left", size = 5) +
  #scale_fill_viridis_c(guide = FALSE, alpha = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), position = "right") +
  labs(x = "", y = "Global AMR Emergence Events") +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 14, hjust = 0, color = "black"),
        axis.ticks = element_blank())
ggsave(here("figures/drug-barchart.png"), width = 12, height = 9)


bact_total <- top_bact %>% 
  mutate(bacteria = fct_rev(bacteria)) %>%
  ggplot(., aes(x = bacteria, y = n_events)) +
  geom_bar(stat = "identity", fill = "gray70") +
  geom_text(aes(label=lab), y = 2, hjust = "left", size = 5) +
  #scale_fill_viridis_c(guide = FALSE, alpha = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), position = "right") +
  labs(x = "", y = "Global AMR Emergence Events") +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, face = "italic", color = "black"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 14, hjust = 0, color = "black"),
        axis.ticks = element_blank())
ggsave(here("figures/bacteria-barchart.png"), width = 12, height = 9)

plot_grid(drugs_total, NULL, bact_total, nrow = 1,
          rel_widths = c(1, 0.05, 1), labels = c('A', '', 'B'), label_size = 12)
ggsave(here("figures/drug-bacteria-barchart.png"), width = 14, height = 7)

# Publications over time --------------------------------------------------
events %>%
  mutate(start_year = as.numeric(str_sub(start_date, 1, 4))) %>%
  group_by(start_year) %>%
  count() %>%
  ungroup() %>%
  bind_rows(tibble(start_year = setdiff(seq(min(.$start_year), max(.$start_year)), .$start_year), n=0)) %>%
  arrange(start_year) %>%
  mutate(start_year = as.character(start_year)) %>%
  ggplot(., aes(x=start_year, y=n)) +
  geom_bar(stat = "identity", fill = "gray60" ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 305)) +
  labs(y = "Global AMR Emergence Events", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6, color = "black"),
        axis.text.y = element_text(color = "black"))
ggsave(here("figures/pubs-over-time.png"))

# Field specificity -------------------------------------------------------

# drug (group/spec) 
drug <- events %>%
  select(drug, rank = drug_rank) %>%
  distinct() %>%
  mutate(rank = recode(rank, 
                            "drug group + drug group" = "drug group",
                            "drug name + drug name" = "drug name",
                            "drug group + drug name" = "drug group/name combo")) %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(rank = factor(rank, levels = c("drug name", "drug group", "drug group/name combo")),
         cat = "Drug") %>%
  arrange(rank)

# bact (family, genus, species) 
bact <- events %>%
  select(bacteria, rank = bacteria_rank) %>%
  distinct() %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(rank = factor(rank, levels = c("species", "genus", "family")),
         cat = "Bacteria") %>%
  arrange(rank)

# location (county, state, city , hosp, impute)
loc <- events %>%
  select(study_id, study_location_basis) %>%
  distinct() %>%
  mutate(rank = ifelse(grepl("hospital", study_location_basis), 
                                      "hospital", ifelse(grepl("city", study_location_basis),
                                                         "city", ifelse(grepl("state_province_district", study_location_basis),
                                                                        "state/province/district", "country")))) %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(rank = factor(rank, levels = c("hospital", "city", "state/province/district", "country")),
         cat = "Location") %>%
  arrange(rank)


# date (year, month, day, impute)
date <- events %>%
  select(study_id, start_date, rank = start_date_rank) %>%
  distinct() %>%
  group_by(rank) %>%
  count() %>%
  ungroup() %>%
  mutate(rank = factor(rank, levels = c("day", "month", "year")),
         cat = "Date") %>%
  arrange(rank)


ranks <- reduce(list(drug, bact, loc, date), rbind) %>%
  select(" "=cat, "Classification" = rank, "Count" = n)

write_csv(ranks, here::here("figures", "field_summary.csv"))


