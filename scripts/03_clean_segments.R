library(here)
library(readxl)
library(magrittr)
library(googlesheets)
library(stringi)
library(tidyverse)

# Import Data and Clean Segments -----------------------------
articles_db <- read_csv(here("data","articles_db.csv")) %>% mutate_all(as.character)
files <- dir(path = here('data', 'coded_segments'), pattern = "*.xlsx", full.names = TRUE)

# individual segment data (from MaxQDA exports) - each observation is an instance of an annotation (ungrouped)
segments_raw <- map_dfr(files, ~read_xlsx(.x, col_types = "text")) %>%
  rename_all(tolower) %>%
  rename_all(funs(str_replace(., " ", "_")))  %>%
  rename("study_id" = document_name) %>%
  separate(begin, into = c("begin_page", "begin_off"), sep = ":", fill = "left") %>%
  separate(end, into = c("end_page", "end_off"), sep = ":", fill = "left") %>%
  select(study_id, 
         segment, 
         code,
         begin_page, 
         begin_off, 
         end_page, 
         end_off)

save(segments_raw, file = here("data", "segments_raw.RData"))

# create offset range col
segments_raw %<>%
  na.omit(.) %>% # see check_na_segments.R script
  mutate(range = map2(begin_off, end_off, function(x,y) x:y))

# <insert back NA segments once they have been checked >

# only one segment spans two pages. NOTE - this segment matching method is not robust to cases like this :(
segments_raw %>%
  filter(begin_page != end_page)

# Find Offsets That Match (with tolerance = 80% on both sides of match) and Group  -----------------------------

# function that returns which other segments in the group have an offset that is nearly (80% both ways) identical
return_matches <- function(element1, list, ids) {
  prop_matches1 <- map_dbl(list, function(x) length(intersect(element1, x))/ length(element1))
  prop_matches2 <- map_dbl(list, function(x) length(intersect(element1, x))/ length(x))
  ids[which((prop_matches1 > .8 & prop_matches2 > .8))]
}

# function to flatten list columns for exporting to csv/google 
from_ls_to_flat <- function(col) {
  flat_col <-  map(col, ~paste(.x, collapse = ", ")) %>%
    unlist()
  return(flat_col)
}

# group_by study_id and page numbers and check for range matches using above func, then group by matches, and study_id, and paste corresponding codes together
segments_grp <- segments_raw %>%
  group_by(study_id) %>%
  mutate(id = row_number()) %>%
  group_by(study_id,begin_page, end_page) %>%
  mutate(matches = map(range, ~return_matches(.x, range, id)), 
         matches_str = from_ls_to_flat(matches)) %>% # I don't think dplyr can group_by list columns? so created a string to group_by for now.
  group_by(study_id, matches_str) %>%
  summarise(codes = list(code), 
            length_codes = n(), 
            segment_all = list(segment), 
            segment = segment[1], # assumes segments have been checked and that they in fact match, so you can just grab the first one of the vector of segments 
            range = list(range)) %>%
  ungroup()


# data sanity check export - do the segments grouped by offsets match? - yes
segments_grp %>%
  mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
  write_csv(here("data", "data_qa",  "int_segments_check.csv"))

# Split Codes from Code ID's -----------------------------

# extract code identifiers from main codes and code categories (A-A and AA-ZZ)
code_id_letters <- map2(LETTERS, LETTERS, ~paste0(.x, .y)) %>%
  c(., LETTERS) %>% unlist

# clean up segments database. keep segment_ls just in case something looks funky
segments <- segments_grp %>%
  mutate(code_identifiers = map(codes, function(x) x[x %in% code_id_letters]), 
         code_main =  map(codes, function(x) x[!x %in% code_id_letters])) %>%
  select(-matches_str, -length_codes, -range, -codes)

# Check Id Codes Missing Corresponding Main Code -----------------------------

# identify problematic id codes for a given segment (there are more id codes than there are main codes)
check_orphan_ids <- function(code_id_vec, code_main_vec) {
  if (length(code_id_vec) == 0 || (length(code_id_vec) == length(code_main_vec))) {
    check_vec = "fine" 
  } else {
    check_vec = "check"
  }
  return(check_vec)
}

# parent data frame
review_codes <- segments %>% 
  mutate(code_identifiers_check = map2(.$code_identifiers, .$code_main, ~check_orphan_ids(.x, .y)))

# codes that need review because the number of identifier codes does not match the number of main codes
problem_id_codes <- review_codes %>%
  filter(code_identifiers_check == "check")

# for all codes that have several identifier codes, but only 1 main code, it seems appropriate to repeat the main code for each identifier
codes_mult <- problem_id_codes %>% 
  filter(map_lgl(code_main, ~length(.x) != 0)) %>%
  mutate(code_main = map2(code_main, code_identifiers, ~rep(.x, length(.y))))

fixed_codes_mult <- codes_mult %>%
  filter(map2_lgl(code_main, code_identifiers, ~length(.x) <= length(.y))) 

# there are cases where there are more main codes than id codes. These should be reviewed.  
check_codes_mult <- codes_mult %>%
  filter(map2_lgl(code_main, code_identifiers, ~length(.x) > length(.y))) %>%
  mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
  left_join(select(articles_db, study_id, mex_name), by = "study_id")


# id orphan id codes (no main code) - these are checked manually (amr_db_orphan_id_codes on google drive)
orphan_id_codes <- problem_id_codes %>% 
  filter(map_lgl(code_main, ~length(.x) == 0)) %>%
  mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
  select(-segment_all, -code_identifiers_check) %>%
  left_join(select(articles_db, study_id, mex_name), by = "study_id")

# bring in orphan ids that have been fixed manually on google drive
orphan_id_codes_review <- gs_read(gs_title("amr_db_orphan_id_codes"), ws = "review_1") %>%
  mutate(study_id = as.character(study_id)) 

fixed_codes_ophans <- orphan_id_codes_review %>% 
  filter(!is.na(code_main)) %>%
  select(-Notes, -mex_name, -code_main_cat)

# remove from fixed_codes_ophans anything that is no longer current (b/c files have been updated post QA check)
remove_orphan_ids <- left_join(
  # data frame 1
  fixed_codes_ophans %>%
    select(study_id, code_identifiers),
  # data frame 2
  problem_id_codes %>%
    mutate_if(is.list, funs(from_ls_to_flat(.)))
) %>%
  filter(is.na(segment)) %>%
  select(study_id, code_identifiers)

fixed_codes_ophans %<>%
  anti_join(remove_orphan_ids) %>%
  # formatting to match review_codes 
  mutate(code_identifiers = stri_split_fixed(code_identifiers, ", "),
         segment_all = as.list(segment),
         code_main = map2(code_main, code_identifiers, ~rep(.x, length(.y))), # code_main to apply to all segments with same code identifier
         code_identifiers_check = as.list("check")) 

# identify remaining orphans - including those that were added in more recent rounds.  
# compare fixed_codes_orphans, which is from google drive, to orphan_id_codes which is generated on fly with most recent dat
remaining_orphan_ids <- left_join(
  # data frame 1
  orphan_id_codes %>% 
    select(study_id, code_identifiers, segment) %>%
    group_by(study_id, code_identifiers) %>%
    mutate(tmp_id = 1:n()) %>% ungroup(), # tmp id to handle dups
  # data frame 2
  fixed_codes_ophans %>%
    mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
    group_by(study_id, code_identifiers) %>%
    mutate(tmp_id = 1:n()) %>% ungroup(), # tmp id to handle dups
  by = c("study_id", "code_identifiers", "tmp_id")
) %>%
  filter(is.na(code_main)) %>%
  select(-segment.y, -segment_all, -code_identifiers_check, -tmp_id) %>%
  rename(segment = segment.x) %>%
  left_join(select(articles_db, study_id, mex_name), by = "study_id")

# attach back fixed codes
segments <- review_codes %>%
  filter(code_identifiers_check != "check") %>%
  rbind(., fixed_codes_mult) %>%
  rbind(., fixed_codes_ophans)

# sanity check - no codes lost in orphan id process!
nrow(review_codes) == nrow(segments) + nrow(remaining_orphan_ids)  + nrow(review_codes_mult)

# Unnest Codes for Final Segments DB  -----------------------------

# function to assign same number of NA's as main code terms so we can unnest later
rep_na_for_unnest <- function(code_id_vec, code_main_len) {
  if (length(code_id_vec) == 0) {
    new_code_id_vec = rep(NA, times = code_main_len) 
  } else {
    new_code_id_vec = code_id_vec
  }
  return(new_code_id_vec)
}

# attach back fixed codes from above, and prepare for unnesting, 
# unnest so each code and id have their own observation (gets rid of list column)
# separate code category from main code 
segments %<>%
  mutate(code_identifiers = map2(.$code_identifiers, .$code_main, ~rep_na_for_unnest(.x, length(.y)))) %>%
  select(-segment_all, -code_identifiers_check) %>% 
  unnest(code_main, code_identifiers) %>%
  separate(code_main, into = c("code_main_cat", "code_main"), sep = ":", fill = "left") %>%
  mutate_all(tolower) %>%
  mutate(segment = stri_replace_all_regex(segment, c("\r\n",  "- "), c("", ""), vectorize_all = FALSE)) 

# get vector of excluded study IDs
excluded_studies <- segments %>%
  filter(code_main_cat == "exclusion") %>% 
  pull(study_id) %>% 
  unique()

# excluded segments export
write_csv(segments %>%
            filter(study_id %in% excluded_studies), path = here("data", "segments_excluded.csv"))

# omit excluded study Ds from final database
segments %<>%
  filter(!study_id %in% excluded_studies)

# collapse travel locations
segments %<>%
  group_by(study_id, code_main_cat, code_identifiers, code_main) %>%
  mutate(segment = ifelse(code_main=="place traveled to", paste(segment, collapse = "; "), segment)) %>% #summarize not working b/c dups in data
  unique() %>%
  ungroup()

# Add code_identifiers_link to provide linked fields for code identifiers
segments %<>%
  group_by(study_id, code_identifiers) %>%
  mutate(code_identifiers_link = ifelse(is.na(code_identifiers), NA, paste(sort(unique(code_main)), collapse = "; "))) %>%
  ungroup()

# Remove solo code links
segments_out <- segments %>% 
  mutate(code_identifiers_link = ifelse(grepl("\\;", code_identifiers_link), code_identifiers_link, NA),
         code_identifiers = ifelse(is.na(code_identifiers_link), NA, code_identifiers)) 

# final segments db
write_csv(segments_out, path = here("data", "segments.csv"))
