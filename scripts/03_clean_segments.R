library(tidyverse)
library(here)
library(readxl)
library(magrittr)
library(googledrive)


# Import Data and Clean Segments -----------------------------

files <- dir(path = here('data', 'coded_segments'), pattern = "*.xlsx", full.names = TRUE)

# individual segment data (from MaxQDA exports) - each observation is an instance of an annotation (ungrouped)
segments_raw <- map_dfr(files, ~read_xlsx(.x)) %>%
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

# these are all excluded articles or notes on articles, usually due to annotating the title of the article in the PDF. All have strange offsets
na_segments <- segments_raw %>% filter(is.na(segment))

# save local and google drive copy
write_csv(na_segments, here("data", "data_qa", "amr_db_na_segments_check.csv"))
drive_upload(media = here("data", "data_qa", "amr_db_na_segments_check.csv"), 
             path = "~/amr-db/", #* REVIEW - this does assume you have your amr-db google drive in the top level of g-drive. 
             name = "amr_db_na_segments_check.csv", 
             type = "spreadsheet")

# create offset range col
segments_raw %<>%
  na.omit(.) %>% # see above na.seg
  mutate(range = map2(begin_off, end_off, function(x,y) x:y))

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
segments_db <- segments_grp %>%
  mutate(code_identifiers = map(codes, function(x) x[x %in% code_id_letters]), 
         code_main =  map(codes, function(x) x[!x %in% code_id_letters])) %>%
  select(-matches_str, -length_codes, -range, -codes)

# Check Id Codes Missing Corresponding Main Code -----------------------------

# identify problematic id codes- for a given segment (there are more id codes than there are main codes)
check_orphan_ids <- function(code_id_vec, code_main_vec) {
  if (length(code_id_vec) == 0 || (length(code_id_vec) == length(code_main_vec))) {
    check_vec = "fine" 
    } else {
    check_vec = "check"
  }
  return(check_vec)
}

review_codes <- segments_db %>% 
  mutate(code_identifiers_check = map2(.$code_identifiers, .$code_main, ~check_orphan_ids(.x, .y)))

# codes that need review because the number of identifier codes does not match the number of main codes
problem_id_codes <- review_codes %>%
  filter(code_identifiers_check == "check")

# for all codes that have several identifier codes, but only 1 main code, it seems appropriate to repeat the main code for each identifier
fixed_codes <- problem_id_codes %>% 
  filter(map_lgl(code_main, ~length(.x) != 0)) %>%
  mutate(code_main = map2(code_main, code_identifiers, ~rep(.x, length(.y)))) %>%
  filter(map2_lgl(code_main, code_identifiers, ~length(.x) <= length(.y))) 
# ^ there are 2 cases where there are more main codes than id codes. These are just notes on the doc. ommiting for now

# function to assign same number of NA's as main code terms so we can unnest later
rep_na_for_unnest <- function(code_id_vec, code_main_len) {
  if (length(code_id_vec) == 0) {
    new_code_id_vec = rep(NA, times = code_main_len) 
  } else {
    new_code_id_vec = code_id_vec
  }
  return(new_code_id_vec)
}

# attach back fixed codes from above, and prepare for unnesting
segments_db <- review_codes %>%
  filter(code_identifiers_check != "check") %>%
  rbind(., fixed_codes) %>%
  mutate(code_identifiers = map2(.$code_identifiers, .$code_main, ~rep_na_for_unnest(.x, length(.y)))) %>%
  select(-segment_all, -code_identifiers_check)

load(here("data", "index_articles.RData"))

# orphan id codes need manual checking - these have at least 1 ID code but no main code at all 
orphan_id_codes <- problem_id_codes %>% 
  filter(map_lgl(code_main, ~length(.x) == 0)) %>%
  mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
  select(-segment_all, -code_identifiers_check) %>%
  left_join(select(art, study_id, mex_name), by = "study_id")

write_csv(orphan_id_codes, here("data", "data_qa", "amr_db_orphan_id_codes.csv"))
drive_upload(media = here("data", "data_qa", "amr_db_orphan_id_codes.csv"), 
             path = "~/amr-db/", #* REVIEW - this does assume you have your amr-db google drive in the top level of g-drive. 
              name = "amr_db_orphan_id_codes.csv", 
              type = "spreadsheet")

# < once orphan_ids are checked, rejoin to segments_db > 


# Unnest Codes for Final Segments DB  -----------------------------

# unnest so each code and id have their own observation (gets rid of list column), and separate code cateogry identifier from main code
segments_db_long <- segments_db %>% 
  unnest(code_main, code_identifiers) %>%
  separate(code_main, into = c("code_main_cat", "code_main"), sep = ":", fill = "left")

# final segments db
save(segments_db_long, file = here("data", "segments_db.RData"))

# spread version of segments db
segments_db_wide <- segments_db_long %>%
  tibble::rowid_to_column() %>% 
  spread(., key = code_main_cat, value = code_main) %>%
  spread(., key = Location, value = segment)

