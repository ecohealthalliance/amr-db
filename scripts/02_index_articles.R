library(tidyverse)
library(here)
library(readxl)
library(magrittr)
library(compare)

# Clean Article Index Data -------------------

# read in all article index files from individual reviewers
read_article_csvs <- function(fileloc, filename){
  each_article_csv <- read_csv(paste(fileloc, filename, sep = "/"), 
                               col_types = cols("STUDY_ID" = col_integer(),
                                                .default = "c"), 
                               local = locale(encoding = "latin1")) %>%
    filter(!is.na(STUDY_ID))  %>% #remove rows that are all NA
    mutate(csv_name = filename)
  return(each_article_csv)
}


fileloc <- here("data-raw", "art_index_csvs")
files <- list.files(fileloc)

articles_db <- map_df(files, ~read_article_csvs(fileloc, .x)) %>%
  rename_all(tolower) %>%
  rename("author_year" = x1)

# read in mex files to link document names
src_sqlite_mex <- function(fileloc, filename){
  path <- src_sqlite(paste(fileloc, filename, sep = "/"))
  table <- tbl(path, "Texts") %>% 
    collect() %>%
    rename_all(tolower) %>%
    mutate(name  = as.numeric(name), 
           mex_name = filename) %>%
    select(name, mex_name) %>%
    rename("study_id" = name)
  return(table)
}

fileloc <- here("data-raw", "coded_text_mex")
files <- list.files("data-raw/coded_text_mex/", pattern = "*.mex")
mex <- map_df(files, ~src_sqlite_mex(fileloc, .x))

articles_db <- left_join(articles_db, mex)

# clean up article index dataframe
articles_db <- articles_db %>%
  mutate_at(vars(-abstract, - author, -journal, -title, -keywords), funs(str_to_lower))

# check status of downloaded column (spoiler alert...it needs cleaning)
articles_db %>% group_by(downloaded) %>%
  summarise(n = n()) ##Note article 23579 - NA for downloaded - not in mex but not clear why

#clean downloaded column, and write to master index csv
articles_db <- articles_db %>%
  mutate(reason = ifelse(.$downloaded %in% c("not full text", "could not access"),
                          .$downloaded, .$reason), 
         downloaded = fct_collapse(downloaded, 
                                     yes = c("yes", "downloaded"), 
                                     no = c("no", "not full text", "could not access"))) %>%
  mutate(in_codes_db = ifelse(.$downloaded == "yes", "yes", "no"), 
         article_type = case_when(grepl("promed", .$csv_name) ~ "promed", 
                                  TRUE ~ "journal"))

write_csv(articles_db, path = here("data", "articles_db.csv"))

# count number of articles that were able to be downloaded
fct_count(articles_db$downloaded)
fct_count(articles_db$in_codes_db)
fct_count(articles_db$include)


