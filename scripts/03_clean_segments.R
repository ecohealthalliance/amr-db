library(tidyverse)
library(here)
library(readxl)
library(magrittr)

# Import Data and Clean Segments -----------------------------

files <- dir(path = here('data', 'coded_segments'), pattern = "*.xlsx", full.names = TRUE)

# individual segment data - each observation is an annotation instance (ungrouped)
segments <- map_dfr(files, ~read_xlsx(.x)) %>%
  rename_all(tolower) %>%
  rename_all(funs(str_replace(., " ", "_")))  %>%
  rename("study_id" = document_name) %>%
  transmute(study_id, 
            segment, 
            code,
            begin_off = stringr::str_split(begin, ":", 2, simplify = TRUE)[, 2], 
            end_off =  stringr::str_split(end, ":", 2, simplify = TRUE)[, 2])

# these are all excluded articles or notes on articles. usually due to annotating the title of the article in the PDF. All have strange offsets
na.seg <- segments %>% filter(is.na(segment))
map(segments, ~sum(is.na(.x)))

# create offset range col
segments %<>%
  na.omit(.) %>% # see above na.seg
  mutate(range = map2(begin_off, end_off, function(x,y) x:y))


# Find Offsets That Match (with tolerance = 80% on both sides of match) and Group  -----------------------------

# function that returns which other segments in the group have an offset that is nearly (80% both ways) identical
return_matches <- function(element1, list) {
  prop_matches1 <- map_dbl(list, function(x) length(intersect(element1, x))/ length(element1))
  prop_matches2 <- map_dbl(list, function(x) length(intersect(element1, x))/ length(x))
  which((prop_matches1 > .8 & prop_matches2 > .8))
}

# group_by study_id, and check for range matches using above func
# then group by matches, and study_id, and paste corresponding codes together
# also create column that is a vector of string codes
segments_grp <- segments %>%
  group_by(study_id) %>%
  mutate(id = row_number(), 
         matches = map(range, ~return_matches(.x, range)), 
         matches_str = as.character(matches)) %>% # I don't think dplyr can group_by list columns, so created a string to group by for now 
  group_by(study_id, matches_str) %>%
  summarise(codes = paste(code, collapse = ","), 
            length_codes = n(), 
            seg_ls = list(segment), 
            segment = segment[1], 
            range_ls = list(range))  %>%
  ungroup() 

# save an intermediate version to do some data checking (in segments_data_checking.R)
save(segments_grp, file = here("data", "int_segments_grp.RData"))

# Split Codes from Code ID's -----------------------------

# extract code identifiers from main codes and code categories (A-A and AA-ZZ)
code_id_letters <- map2(LETTERS, LETTERS, ~paste0(.x, .y)) %>%
  c(., LETTERS)

codes <- segments_grp %>%
  mutate(code_identifiers = map(codes, function(x) x[x %in% code_id_letters]), 
         code_main =  map(codes, function(x) x[!x %in% code_id_letters])) %>%
  select(-length_codes, -codes)

# identify which code identifiers don't have corresponding main codes
# *REVIEW - better way to do this?
match_len_id_to_main <- function(vec1, vec2) {
  if (length(vec1) == 0) {
    vec1 = rep(NA, length(vec2)) # make sure there is the same number of NA's as main codes so you can properly unnest
  } else if (length(vec1) == length(vec2)) {
    vec1 }
  else {
    vec1 = "problem"
  }
  return(vec1)
}

codes %<>% 
  mutate(code_identifiers = map2(.$code_identifiers, .$code_main, ~match_len_id_to_main(.x, .y)))

# which codes need to be reviewed
# *REVIEW - these need to be reviewed manually ASAP
codes_to_review <- codes %>%
  filter(code_identifiers == "problem")

# unnest so each code and id have their own observation (gets rid of list column)
# separate code category from main code
codes %<>% 
  filter(code_identifiers != "problem") %>%
  unnest(code_main, code_identifiers) %>%
  separate(code_main, into = c("code_main_cat", "code_main"), sep = ":", fill = "left")

save(codes, file = here("data", "coded_annotations.RData"))

# spread version of data (not sure what version to put final data in yet)
all_codes_sp <- codes %>%
  tibble::rowid_to_column() %>% 
  spread(., key = code_main_cat, value = code_main) %>%
  spread(., key = Location, value = segment)

