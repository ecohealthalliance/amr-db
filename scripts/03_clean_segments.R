library(here)
library(readxl)
library(magrittr)
library(googlesheets)
library(stringi)
library(tidyverse)
library(textclean)
library(assertthat)

# Import Data and Clean Segments -----------------------------
articles_db <- read_csv(here("data","articles_db.csv")) %>% mutate_all(as.character)
files <- dir(path = here('data', 'coded_segments'), pattern = "*.xlsx", full.names = TRUE)

# individual segment data (from MaxQDA exports) - each observation is an instance of an annotation (ungrouped)
segments_raw <- map_dfr(files, ~read_xlsx(.x, col_types = "text")) %>%
  janitor::clean_names() %>%
  rename("study_id" = document_name) %>%
  mutate(begin = coalesce(begin, beginning)) %>%
  separate(begin, into = c("begin_page", "begin_off"), sep = ":", fill = "left") %>%
  separate(end, into = c("end_page", "end_off"), sep = ":", fill = "left") %>%
  select(study_id, 
         segment, 
         code,
         begin_page, 
         begin_off, 
         end_page, 
         end_off) %>%
  distinct()

write_rds(segments_raw,  here("data", "segments_raw.rds"))

# QA CHECK - these are all excluded articles or notes on articles, usually due to annotating the title of the article in the PDF. All have strange offsets
# github issue 6
na_segments <- segments_raw %>% 
  filter(is.na(segment)) %>%
  left_join(select(articles_db, study_id, mex_name))
# study ID 22668 to be manually added at end of script
# everything else should be Exclusion or Notes

apply(segments_raw, 2, function(x) sum(is.na(x))) 

# set aside promed articles b/c they have different offsets
promed_raw <- segments_raw %>%
  left_join(articles_db) %>%
  filter(grepl("promed", mex_name)) %>%
  select(colnames(segments_raw)) %>%
  mutate(segment = str_replace(segment, "\r\n", " ")) # manual cleaning to help with merging codes at bottom of script

# create offset range col
segments_raw %<>%
  drop_na(segment) %>% # NAs id'd above
  left_join(articles_db) %>%
  filter(!grepl("promed", mex_name)) %>% # remove promeds
  select(colnames(segments_raw)) %>%
  mutate(range = map2(begin_off, end_off, function(x,y) x:y))

# only one segment spans two pages. NOTE - this segment matching method is not robust to cases like this :(
segments_raw %>%
  filter(begin_page != end_page)

# misc qa fixes
segments_raw %<>%
  filter(!(study_id=="12359" & segment=="Levofloxacin" & code=="MIC"),
         !(study_id=="13376" & segment=="\u000316" & code=="Drug Resisted"),
         !(study_id=="23107" & segment=="Meropenem" & code=="MIC"),
         !(study_id=="22703" & segment=="Connecticut" & code == "Patient:Country of Residence"),
         !(study_id=="17222" & segment=="n"), 
         !(study_id=="3799" & segment=="MIC"),
         !(study_id=="8619" & segment=="EM")) %>%
  mutate(segment = textclean::replace_non_ascii(segment),
         code = replace(code, code == "Drug Resisted" & segment == "16", "MIC"))

# Find Offsets That Match (with tolerance = 50% on both sides of match) and Group  -----------------------------

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
            segment = segment[1], # assumes segments have been checked and that they in fact match, so you can just grab the first one of the vector of segments (this is confirmed below) 
            range = list(range)) %>%
  ungroup()


# data sanity check export - do the segments grouped by offsets match? 
# there are some non-excact matches but they represent the same segments so it's ok
segments_grp %>%
  mutate(segment_all_check = map_lgl(segment_all, ~length(unique(.x))==1)) %>%
  filter(segment_all_check==FALSE)
#mutate_if(is.list, funs(from_ls_to_flat(.))) #%>%
#write_csv(here("data", "data_qa",  "int_segments_check.csv"))

# Split Codes from Code ID's -----------------------------

# extract code identifiers from main codes and code categories (A-A and AA-ZZ)
code_id_letters <- map2(LETTERS, LETTERS, ~paste0(.x, .y)) %>%
  c(., LETTERS) %>% unlist

# clean up segments database. keep segment_ls just in case something looks funky
segments <- segments_grp %>%
  mutate(code_identifiers = map(codes, function(x) x[x %in% code_id_letters]), 
         code_main =  map(codes, function(x) x[!x %in% code_id_letters])) %>%
  select(-matches_str, -length_codes, -range, -codes) 

# QA CHECK - ID Codes Missing Corresponding Main Code -----------------------------

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
  mutate(code_main = map2(code_main, code_identifiers, ~rep(.x, length(.y)))) # this will only apply when length(main code) < length(code identifier)

fixed_codes_mult <- codes_mult %>%
  filter(map2_lgl(code_main, code_identifiers, ~length(.x) == length(.y))) 

# there are cases where there are more main codes than id codes. 
review_codes_mult <- codes_mult %>%
  filter(map2_lgl(code_main, code_identifiers, ~length(.x) > length(.y))) #%>%
# mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
# left_join(select(articles_db, study_id, mex_name), by = "study_id")

# ^ for these cases (20373, 23314, 8982), assume code identifier applies to all main codes, then add back to fixed_codes_mult.  5362 is to be excluded
review_codes_mult %<>% 
  mutate(code_identifiers = map2(code_identifiers, code_main, ~rep(.x, length(.y))))

fixed_codes_mult %<>%
  bind_rows(review_codes_mult)

# id orphan id codes (no main code) - these are checked manually 
# amr_db_orphan_id_codes on google drive - github issue 5 
orphan_id_codes <- problem_id_codes %>% 
  filter(map_lgl(code_main, ~length(.x) == 0)) %>%
  mutate_if(is.list, funs(from_ls_to_flat(.))) %>%
  select(-segment_all, -code_identifiers_check) %>%
  left_join(select(articles_db, study_id, mex_name), by = "study_id") %>%
  distinct()

# bring in orphan ids that have been fixed manually on google drive
orphan_id_codes_review <- gs_read(gs_title("amr_db_orphan_id_codes"), ws = "review_1") %>%
  bind_rows(gs_read(gs_title("amr_db_orphan_id_codes"), ws = "review_2")) %>%
  bind_rows(gs_read(gs_title("amr_db_orphan_id_codes"), ws = "review_3")) %>%
  mutate(study_id = as.character(study_id)) 

fixed_codes_orphans <- orphan_id_codes_review %>% 
  filter(!is.na(code_main)) %>%
  select(-Notes, -mex_name, -code_main_cat, -new_segment, -comments) %>%
  distinct()

# prep fixed codes (from google drive) and orphan id codes (generated on fly with latest data) for merging
fixed_codes_orphans %<>%
  group_by(study_id, code_identifiers) %>%
  arrange(study_id, code_identifiers, segment) %>%
  mutate(tmp_id = 1:n()) %>% ungroup() # tmp id to handle dups

orphan_id_codes %<>%
  group_by(study_id, code_identifiers) %>%
  arrange(study_id, code_identifiers, segment) %>%
  mutate(tmp_id = 1:n()) %>% ungroup() # tmp id to handle dups

# remove from fixed_codes_orphans anything that is no longer current (b/c files have been updated post QA check)
remove_orphan_ids <- anti_join(
  # data frame 1 (all codes that have been manually fixed on google drive)
  fixed_codes_orphans, 
  # data frame 2 (all orphan id codes)
  orphan_id_codes,
  by = c("study_id", "code_identifiers", "tmp_id")) # these are no longer orphans 

fixed_codes_orphans %<>%
  anti_join(remove_orphan_ids, by = c("study_id", "code_identifiers", "tmp_id")) # these are fixed orphans 

# get remaining orphan ids to be checked
remaining_orphan_ids <- anti_join(orphan_id_codes, fixed_codes_orphans, by = c("study_id", "code_identifiers", "tmp_id")) # these are unfixed orphans 

# formatting to match segments (to be added back in) 
fixed_codes_orphans %<>%
  select(-tmp_id) %>%
  mutate(code_identifiers = stri_split_fixed(code_identifiers, ", "),
         segment_all = as.list(segment),
         code_main = map2(code_main, code_identifiers, ~rep(.x, length(.y))), # code_main to apply to all segments with same code identifier
         code_identifiers_check = as.list("check"))  

# attach back fixed codes
segments <- review_codes %>%
  filter(code_identifiers_check != "check") %>%
  rbind(., fixed_codes_mult) %>%
  rbind(., fixed_codes_orphans)

# sanity check - no codes lost in orphan id process- should be TRUE
assert_that(nrow(review_codes) == nrow(segments) + nrow(remaining_orphan_ids)  + sum(duplicated(problem_id_codes)))

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

# Add promed back in -
promed_code_ids <- promed_raw %>%
  filter(code %in% LETTERS) %>%
  select(study_id, segment, code_identifiers = code) 

promed <- promed_raw %>%
  filter(!code %in% LETTERS) %>%
  select(study_id, segment, code_main = code) %>%
  left_join(promed_code_ids) %>%
  mutate_at(.vars = c("code_main", "code_identifiers"), ~as.list(.))

# unnest so each code and id have their own observation (gets rid of list column)
# separate code category from main code 
segments %<>%
  bind_rows(promed) %>%
  mutate(code_identifiers = map2(.$code_identifiers, .$code_main, ~rep_na_for_unnest(.x, length(.y)))) %>%
  select(-segment_all, -code_identifiers_check) %>% 
  unnest(code_main, code_identifiers) %>%
  separate(code_main, into = c("code_main_cat", "code_main"), sep = ":", fill = "left") %>%
  mutate_all(tolower) %>%
  mutate(segment = stri_replace_all_regex(segment, c("\r\n",  "- "), c("", ""), vectorize_all = FALSE)) 

# get vector of excluded study IDs
excluded_studies <- segments %>%
  filter(code_main_cat == "exclusion"|str_detect(code_main, "surveillance")) %>% 
  pull(study_id) %>% 
  unique()

# excluded segments export
write_csv(segments %>%
            filter(study_id %in% excluded_studies), path = here("data", "segments_excluded.csv"))

# omit excluded study IDs from final database
segments %<>%
  filter(!study_id %in% excluded_studies)

# remove code_main_cat because inconsistent (was not included in manual code_main additions)
segments %<>%
  select(-code_main_cat)

# collapse travel locations
segments %<>%
  group_by(study_id, code_identifiers, code_main) %>%
  mutate(segment = ifelse(code_main=="place traveled to", paste(segment, collapse = "; "), segment)) %>% #summarize not working b/c dups in data
  unique() %>%
  ungroup()

# Add code_identifiers_link to provide linked fields for code identifiers
segments %<>%
  group_by(study_id, code_identifiers) %>%
  mutate(code_identifiers_link = ifelse(is.na(code_identifiers), NA, paste(sort(unique(code_main)), collapse = "; "))) %>%
  ungroup()

# QA CHECK - Identify code identifiers that are not "linked" - these need to be spot checked
# Github issue 13
code_links_solo <- segments %>%
  filter(!grepl("\\;", code_identifiers_link), !is.na(code_identifiers_link)) %>%
  left_join(., articles_db %>% select(study_id, mex_name) %>% mutate(study_id = as.character(study_id))) %>%
  arrange(as.numeric(study_id)) %>%
  select(study_id, segment, code_identifiers, code_identifiers_link, mex_name)
# ^ references for followup are okay

# Remove solo code links
segments %<>% 
  mutate(code_identifiers_link = ifelse(grepl("\\;", code_identifiers_link), code_identifiers_link, NA),
         code_identifiers = ifelse(is.na(code_identifiers_link), NA, code_identifiers)) 

# Manually add study dat for study ID 22668 (this study is actually excluded)
# segments %<>%
#   bind_rows(tibble(study_id = "22668", segment = "2007", code_main = "event year", code_identifiers = NA, code_identifiers_link = NA))

# Remove dups
segments %<>%
  distinct()
# final segments db
write_csv(segments, path = here("data", "segments.csv"))
