---
title: "Drugs AMR"
author: "Emma Mendelsohn"
date: "9/26/2018"
output:
      html_document:
        keep_md: true
---

```r
knitr::opts_chunk$set(include=TRUE, echo=TRUE, message=FALSE, warning=FALSE)
```


```r
library(tidyverse)
library(magrittr)
library(stringi)
library(here)
library(ontologyIndex)

load(here("data", "segments_db.RData")) #data compiled in 03_clean_segments.r
dat <- segments_db %>%
  filter(code_main == "drug resisted") %>%
  mutate(segment = gsub("\\(|\\)", "", segment))
```

MeSH Medical Subject Headings (https://meshb.nlm.nih.gov/search)

```r
mesh0 <- read_csv(here("scripts", "drugs", "mesh-ontology", "MESH.csv")) %>% #downloaded from bioportal: https://bioportal.bioontology.org/ontologies/MESH?p=summary
  mutate_at(vars(`Class ID`, Parents), funs(gsub("http://purl.bioontology.org/ontology/MESH/", "", .))) %>%
  mutate_at(vars(`Preferred Label`, Synonyms), tolower) 

#mesh clean and reshape
mesh <- mesh0 %>%
  select("Class ID" ,"Preferred Label","Synonyms", "Definitions", "HM", "Parents", "PA") %>%
  mutate(Synonyms = str_split(Synonyms, "\\|")) %>%
  unnest() %>%
  gather(key="segment_class", value="segment", `Preferred Label`, Synonyms, 
         -`Class ID`, -Definitions, -HM, -Parents, -PA) %>%
  unique() %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  rename_all(tolower) %>%
  setNames(paste0('mesh_', names(.)))  %>%
  rename(mesh_id = `mesh_class id`, segment = mesh_segment)

pref_names <- unique(mesh$segment[mesh$mesh_segment_class=="Preferred Label"])
syn_names <- unique(mesh$segment[mesh$mesh_segment_class=="Synonyms"])
mesh <- mesh[!c(mesh$segment %in% pref_names & mesh$mesh_segment_class=="Synonyms"),] #filter out synonymns that are also primary names

#join mesh with dat based on segment name
dat <- dat %>% left_join(., mesh) 

#name look ups
dat %<>%
  left_join(., mesh0 %>% select("Class ID", "Preferred Label" ), by=c("mesh_id"="Class ID")) %>%
  rename("mesh_preferred_label" = `Preferred Label`) %>%
  mutate(mesh_class = substr(`mesh_id`, 1, 1)) %>%
  mutate(mesh_class_desc = ifelse(mesh_class=="C", "supp", "desc")) %>%
  mutate(mesh_parent_id = ifelse(mesh_class=="C", mesh_hm, mesh_parents)) %>%
  separate(mesh_parent_id, c("mesh_parent_id", "mesh_parent_id_qualifier"), "/") %>% #warning is ok
  mutate(mesh_parent_id = str_split(mesh_parent_id, "\\|")) %>%  unnest() %>%
  mutate(mesh_pa_id = str_split(mesh_pa, "\\|")) %>%  unnest() %>%
  left_join(., mesh0 %>% select("Class ID", "Preferred Label" ), by=c("mesh_parent_id"="Class ID")) %>%
  rename("mesh_parent_name" = `Preferred Label`) %>%
  left_join(., mesh0 %>% select("Class ID", "Preferred Label" ), by=c("mesh_pa_id"="Class ID")) %>%
  rename("mesh_pa_name" = `Preferred Label`) %>%
  select(-mesh_hm, -mesh_parents, -mesh_pa)

dat_unique <- dat %>%
  select(-study_id, -code_identifiers, -code_main) %>%
  unique()
#141/261 drugs with no exact match
#length(unique(dat_unique$segment[is.na(dat_unique$mesh_id)]))

#120/261 drugs with exact match
#length(unique(dat_unique$segment[!is.na(dat_unique$mesh_id)]))

dat_unique %<>%
  filter(!is.na(mesh_id))  
```
Save data

```r
#all data
save(dat, file=here("scripts", "drugs", "drug_mesh_ontology.RData"))

#missing names
outm <- dat %>%
  filter(is.na(mesh_id))%>%
  select(study_id, segment, code_identifiers) 
write.csv(outm, "missing_drug_ontology.csv", row.names = F)
```
