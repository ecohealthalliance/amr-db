library(tidyverse)
library(here)
library(igraph)
library(ggthemes)

events <- read_csv(here("data", "events_db.csv")) %>%
  rename(drug = drug_preferred_label, bacteria = bacteria_preferred_label) %>%
  mutate(drug = str_remove_all(drug, "drug|combination")) %>%
  mutate(drug = str_replace_all(drug, ",", " + ")) %>%
  mutate(drug = str_squish(drug))

n_distinct(events$study_id)
n_distinct(events$study_country)
nrow(events)

top_bact <- events %>%
  group_by(bacteria) %>%
  count(sort = TRUE) %>%
  ungroup()

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
  mutate(lab = ifelse(n_events == max(n_events), paste(n_bacteria, "resistant bacteria"), n_bacteria))

top_drugs %>% 
  slice(1:20) %>%
  mutate(drug = fct_rev(fct_inorder(drug))) %>%
  ggplot(., aes(x=drug, y=n_events)) +
  geom_bar(stat = "identity", fill = "gray60" ) +
  geom_text(aes(label=lab), y = 2, hjust = "left",  fontface = "bold") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Global AMR Resistance Events", x = "") +
  coord_flip() +
  theme_minimal() 
ggsave(filename = here("scripts/data_summary/most-common-drugs.png"), width = 8, height = 6)


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
  mutate(lab = ifelse(n_events == max(n_events), paste(n_drugs, "drugs resisted"), n_drugs))

top_bact %>% 
  slice(1:20) %>%
  mutate(bacteria = fct_rev(fct_inorder(bacteria))) %>%
  ggplot(., aes(x=bacteria, y=n_events)) +
  geom_bar(stat = "identity", fill = "gray60" ) +
  geom_text(aes(label=lab), y = 2, hjust = "left",  fontface = "bold") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Global AMR Resistance Events", x = "") +
  coord_flip() +
  theme_minimal() 
ggsave(filename = here("scripts/data_summary/most-common-bact.png"), width = 8, height = 6)


# Heatmap -----------------------------------------------------------------
events %>%
  group_by(drug, bacteria) %>%
  count() %>%
  ungroup() %>%
  filter(bacteria %in% top_bact$bacteria[top_bact$n_events>30]) %>%
  filter(drug %in% top_drugs$drug[top_drugs$n_events>30]) %>%
  full_join(., expand(., drug, bacteria)) %>%
  mutate(bacteria = factor(bacteria, levels = top_bact$bacteria, labels =  gsub(" ", "\n", top_bact$bacteria))) %>%
  mutate(drug = factor(drug, levels = rev(top_drugs$drug))) %>%
  ggplot(., aes(bacteria, drug)) + 
  scale_x_discrete(position = "top") +
  geom_tile(color = 'gray', aes(fill = n), na.rm = FALSE, alpha = 0.5) +
  geom_label(aes(label = n)) +
  scale_fill_viridis_c(guide = FALSE, alpha = 0.5) +
  labs(x = "", y="", size = "AMR Resistance Events") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(color = "black", size = 11))
ggsave(filename = here("scripts/data_summary/drug-bacteria-heatmap.png"), width = 14, height = 7)

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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  labs(y = "Global AMR Resistance Events", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

ggsave(filename = here("scripts/data_summary/pubs-by-year.png"), width = 8, height = 6)

