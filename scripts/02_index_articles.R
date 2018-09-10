library(tidyverse)
library(here)
library(readxl)
library(magrittr)
library(compare)

# Clean Article Index Data -------------------

# read in all article index files from individual reviewers

files <- dir(path = here('data-raw', 'art_index_csvs'), pattern = "*.csv", full.names = TRUE)
art <- map(files, ~read_csv(.x, col_types = cols("STUDY_ID" = col_integer(),
                                                 "year" = col_integer(), 
                                                 "pmid" = col_integer(),
                                                 .default = "c"), local = locale(encoding = "latin1"))) %>%
  map_df(function(x) {
    names(x) %<>% tolower
    x }) %>%
  rename("author_year" = x1)

# check if there are any missing study ID's from original RData assignent dataframe

original_art <- readRDS(here("data", "original_full_text.rds"))
compare(art, original_art, allowAll = TRUE) # longer character columns are not identical but this is likely due to going back and forth in excel and character encodings

# clean up article index dataframe

art <- art %>%
  mutate_at(vars(-abstract, - author, -journal, -title, -keywords), funs(str_to_lower))

# check status of downloaded column (spoiler alert...it needs cleaning)
art %>% group_by(downloaded) %>%
  summarise(n = n()) 

#clean downloaded column, and write to master index csv
art <- art %>%
  mutate(reason = ifelse(art$downloaded %in% c("not full text", "could not access"),
                          art$downloaded, art$reason), 
         downloaded = fct_collapse(downloaded, 
                                     yes = c("yes", "downloaded"), 
                                     no = c("no", "not full text", "could not access")))

write.csv(art, file = here("data", "index_articles.csv"))
save(art, file = here("data", "index_articles.RData"))

# count number of articles that were able to be downloaded
fct_count(art$downloaded)
fct_count(art$include)

