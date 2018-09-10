library(tidyverse)
library(here)
library(readxl)
library(magrittr)

load(here("data", "index_articles.RData"))
load(file = here("data", "coded_annotations.RData"))


art_ids <- art %>%
  filter(downloaded == "yes") %>%
  pull(study_id) %>%
  unique

segments_id <-  codes %>%
  pull(study_id) %>%
  unique

diff1 <- setdiff(art_ids, segments_id)
# 68 - need to check if these are zeros

diff2 <- setdiff(segments_id, art_ids)
# 13233 is typo on art_index, should be 13223 I think
# 14751 is marked as not downloaded but appears in segments, so it was downloaded
# 20344 - I think this might be 20334 and a typo in study id in the article index
# 20781 marked as no downloaded but it was downloaded and marked with an exclusion tag

missing_in_codes <- 
  art %>% filter(., study_id %in% diff1)
#some of these have notes that they could not be highlighted in maxqda. let's add a column in article index db separate from downloaded that indicated whether it is included in the db or not
write.csv(missing_in_codes, file = here("data", "check_missing_in_codes.csv"))
     