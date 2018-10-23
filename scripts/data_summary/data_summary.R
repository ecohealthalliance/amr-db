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
#+ r setup, include = FALSE

#' -----------------Bacteria-----------------
#' 
```{r bacteria_read, include = FALSE}
bacteria <- read_csv(here("data", "bacteria_genus_species_db.csv"))
#+ r bacteria_read, include = FALSE

```{r bacteria}
#' Count by rank
bacteria %>%
  group_by(ncbi_rank) %>%
  count() %>% 
  spread(ncbi_rank, n) %>%
  kable() 

#' count by name and parent
bacteria_sum <- bacteria %>%
  group_by(ncbi_rank, ncbi_preferred_label, ncbi_parent_rank, ncbi_parent_name) %>%
  count(sort = TRUE) %>%
  mutate(percent = round(100*n/nrow(bacteria), 0)) 

kable(bacteria_sum %>% filter(n > 9))

ggplot(bacteria_sum[bacteria_sum$n > 4,], aes(x = reorder(ncbi_preferred_label, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green3") +
  labs(x = "Most common species", y = "Number of studies") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#+ r bacteria



