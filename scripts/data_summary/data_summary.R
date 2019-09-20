library(tidyverse)
library(here)
library(patchwork)

# ggplot function ---------------------------------------------------------
plot_heatmap <- function(dat){
  
  ggplot(dat,aes(bacteria, drug)) + 
    scale_x_discrete(position = "top") +
    geom_tile(color = "black", aes(fill = n), na.rm = FALSE, alpha = 0.5) +
    geom_label(aes(label = n)) +
    scale_fill_viridis_c(guide = FALSE, alpha = 0.5) +
    labs(x = "", y="") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          axis.text = element_text(color = "black", size = 11))
  
}


# Get data ----------------------------------------------------------------

events <- read_csv(here("data", "events_db.csv")) %>%
  rename(drug = drug_preferred_label, bacteria = bacteria_preferred_label) %>%
  mutate(drug = str_remove_all(drug, "drug|combination")) %>%
  mutate(drug = str_replace_all(drug, ",", " + ")) %>%
  mutate(drug = str_squish(drug)) %>%
  mutate(drug = fct_infreq(drug)) %>%
  mutate(bacteria = fct_infreq(bacteria))

n_distinct(events$study_id)
n_distinct(events$study_country)
nrow(events)

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
  mutate(lab = ifelse(n_events == max(n_events), paste(n_bacteria, "unique resistant bacteria"), n_bacteria)) %>%
  slice(1:12)

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
  mutate(lab = ifelse(n_events == max(n_events), paste(n_drugs, "unique drugs resisted"), n_drugs)) %>%
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
  geom_tile(color = "black", aes(fill = n), alpha = 0.5 ) +
  geom_text(aes(label = n)) +
  scale_fill_viridis_c(guide = FALSE, alpha = 0.5, na.value="transparent") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12, angle = -25, vjust = 1),
        axis.text.x = element_text(size = 10.5, face = "italic"),
        axis.ticks = element_blank())

drugs_total <- top_drugs %>% 
  mutate(drug = fct_rev(drug)) %>%
  ggplot(., aes(x = drug, y = n_events, fill = n_events)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=lab), y = 2, hjust = "left") +
  scale_fill_viridis_c(guide = FALSE, alpha = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), position = "right") +
  labs(y = "", x = "") +
  coord_flip() + 
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14, hjust = 1),
        axis.ticks = element_blank())

bact_total <- top_bact %>% 
  ggplot(., aes(x = bacteria, y = n_events, fill = n_events)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=lab), y = -5, angle = 90, hjust = 1) +
  scale_fill_viridis_c(guide = FALSE, alpha = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_reverse(expand = c(0, 0)) +
  labs(y = "", x = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 11, hjust = 0),
        axis.ticks = element_blank())

heatmap + drugs_total + bact_total + 
  plot_layout(ncol = 2, widths = c(3, 1), heights = c(2, 1))


ggsave(here("scripts/data_summary/drug-bacteria-combos.png"), width = 17, height = 9)


