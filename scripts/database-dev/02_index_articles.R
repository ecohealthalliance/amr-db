library(tidyverse)
library(here)
library(readxl)
library(magrittr)
library(compare)
library(assertthat)

# Clean Article Index Data -------------------
# and make sure all articles are accounted for in csvs, mex files, and segment exports

# Read in all article index files from individual reviewers
read_article_csvs <- function(fileloc, filename){
  each_article_csv <- read_csv(paste(fileloc, filename, sep = "/"), 
                               col_types = cols("STUDY_ID" = col_integer(),
                                                .default = "c"), 
                               local = locale(encoding = "latin1")) %>%
    filter(!is.na(STUDY_ID))  %>% #remove rows that are all NA
    mutate(csv_name = filename) %>%
    mutate(DOWNLOADED = tolower(DOWNLOADED))
  return(each_article_csv)
}

fileloc <- here("screening", "selected")
files <- list.files(fileloc)

articles_db <- map_df(files, ~read_article_csvs(fileloc, .x)) %>%
  rename_all(tolower) %>%
  rename("author_year" = x1)

# Cleaning csv index files - the dupes are studies that were originally marked as downloaded but were not (identified in QA below) - they were coded by SD or EM
dupes_csv <- janitor::get_dupes(articles_db, study_id) %>%
  group_by(study_id) %>%
  summarize(new_reviewer = any(str_detect(unique(csv_name), "sd|em"))) %>%
  ungroup()

assert_that(all(dupes_csv$new_reviewer) )

dupes_csv_study_id <- dupes_csv %>% pull(study_id)

articles_db %<>%
  filter(!(study_id %in% dupes_csv_study_id & !str_detect(csv_name, "sd|em")))

# Read in mex files to link document names
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

fileloc <- here("data-raw", "coded-text-mex")
files <- list.files("data-raw/coded-text-mex/", pattern = "*.mex")
mex <- map_df(files, ~src_sqlite_mex(fileloc, .x)) 

# Check for mex file dupes 
dupes_mex <- janitor::get_dupes(mex, study_id)
# The dupes with sd_1_1 are not actually coded
# The zm_2_2 and md_7_1 dupes are odd, but they are exact dupes.  They are only in the csv for md_7, so leave there.  
c1 <- nrow(mex)
mex <- mex %>%
  filter(!(study_id %in% unique(dupes_mex$study_id) & !mex_name %in% c("sd_1_1.mex", "md_7_1.mex")))
c2 <- nrow(mex)
assert_that(c1-c2 == n_distinct(dupes_mex$study_id))

# Any study ids in csvs but not mex files?
assert_that(
  articles_db %>%
    filter(!study_id %in% mex$study_id) %>%
    filter(downloaded %in% c("yes", "downloaded", NA_character_)) %>%
    nrow() == 0 )
# Those that could NOT be downloaded are addressed in issue #4

# Any study ids in mex but not csv?
assert_that(
  mex %>%
    filter(!study_id %in% articles_db$study_id) %>%
    nrow() == 0 ) 

# Join mex file names into index
articles_db <- left_join(articles_db, mex)

# Check NAs were all not downloaded
nd <- articles_db %>% 
  filter(is.na(mex_name)) %>%
  pull(downloaded) %>% unique() 
assert_that(all(nd %in% c("no", "could not access", "not full text")))

# Now, make sure exports (segments) match index
files <- dir(path = here("data-raw", "coded-segments"), pattern = "*.xlsx", full.names = TRUE)
segments_raw <- map_dfr(files, ~read_xlsx(.x, col_types = "text"))

assert_that(
  articles_db %>%
    filter(downloaded %in% c("yes", "downloaded")) %>%
    filter(!study_id %in% unique(segments_raw$`Document name`)) %>% # articles in mex but not in segments
    nrow() == 0 )

assert_that(
  segments_raw %>%
    filter(!`Document name` %in% unique(articles_db$study_id)) %>% # articles in segments but not in mex/csvs
    nrow() == 0 )

# check for dupes in segments (rough check- making sure any article with more than one author has a new reviewer- there shouldn't be an article coded by two old reviewers)
assert_that(
  segments_raw %>%
    select(`Document name`, Author) %>%
    distinct() %>%
    janitor::get_dupes(., `Document name`) %>%
    group_by(`Document name`) %>%
    mutate(new_reviewer = any(str_detect(unique(Author), "Sonia|chen|dattaray|emma"))) %>%
    ungroup() %>%
    filter(new_reviewer == FALSE) %>%
    nrow() == 0 )

# clean up article index dataframe
articles_db <- articles_db %>%
  mutate_at(vars(-abstract, - author, -journal, -title, -keywords), funs(str_to_lower))

# check status of downloaded column (spoiler alert...it needs cleaning)
articles_db %>% group_by(downloaded) %>%
  summarise(n = n())

# clean downloaded column, and write to master index csv
articles_db <- articles_db %>%
  mutate(reason = ifelse(.$downloaded %in% c("not full text", "could not access"),
                         .$downloaded, .$reason), 
         downloaded = fct_collapse(downloaded, 
                                   yes = c("yes", "downloaded"), 
                                   no = c("no", "not full text", "could not access"))) %>%
  mutate(in_codes_db = ifelse(.$downloaded == "yes", "yes", "no"), 
         article_type = case_when(grepl("promed", .$csv_name) ~ "promed", 
                                  TRUE ~ "journal"))

write_csv(articles_db, path = here("data-processed", "articles-db.csv"))

# count number of articles that were able to be downloaded
fct_count(articles_db$downloaded)
fct_count(articles_db$in_codes_db)
fct_count(articles_db$include)

