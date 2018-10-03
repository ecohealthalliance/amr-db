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
excluded_studies <- unique(segments_db$study_id[segments_db$code_main_cat=="Exclusion"])
segments_db %<>%
  filter(!study_id %in% excluded_studies) 
```
Initial clean-up

```r
sub_names <- tribble( 
  ~old,                    ~new,
  "\r\n",                    "",
  "- ",                      ""
)

dat <- segments_db %>%
  filter(code_main == "Drug Resisted") %>%
  mutate(segment = stri_replace_all_regex(tolower(segment),  sub_names$old, sub_names$new, vectorize_all = FALSE),
         code_identifiers = ifelse(is.na(code_identifiers), "", code_identifiers)) %>%
  select(-code_main_cat)
dat$segment <- gsub("\\(|\\)", "", dat$segment)

#unique(dat$segment)
```

Ontologies at the Comprehensive Antibiotic Resistance Database are freely available under
the Creative Commons CC-BY license version 4.0, https://creativecommons.org/licenses/by/4.0/
license may be needed: https://card.mcmaster.ca/about

```r
# ARO - The Antibiotic Resistance Ontology that serves as the primary organizing principle 
# of the CARD.

#advantage of CARD - clean ranking of classes
#requires permission to use for publication

file <- here("scripts", "drugs", "card-ontology", "aro.obo")
card <- get_ontology(file, extract_tags="everything")

#get synonyms
syn <- as.tibble(unlist(card$synonym)) %>%
  rownames_to_column() %>%
  rename(id = rowname, synonym = value) %>%
  mutate(synonym = gsub("EXACT \\[\\]|", "", synonym),
         synonym = gsub("\"", "", synonym),
         synonym= tolower(str_trim(synonym))) %>%
  mutate(synonym = str_split(synonym, "\\,")) %>%
  unnest() 

get_select_id <- function(seg){
  #seg = "gentamicin" #example not finding match
  #seh = "fluoroquinolones"
  id <-   card$id[grep(x=card$name, pattern=seg, ignore.case = T)] #get all ids associated with segment term
  all_prop <- map(id, function(x) get_term_property(ontology=card, property="id", term=x, as_names=TRUE))  #get all names
  select_name <- all_prop[match(seg, unlist(all_prop))] #select name with exact match to term
  select_id <- names(select_name) #select id with exact match to term
  select_name <- unlist(select_name)
  select_segment_class <- "primary"
  # if(is.null(select_name)){
  #   #check for synonyms - many not in DB
  #   select_id <- syn$id[syn$synonym==seg]
  #   select_segment_class <- "synonym"
  if(length(select_name)==0){  select_name <- select_id <- select_segment_class <-"no exact match"}
  # }
  return(list(select_name=select_name, select_id=select_id, select_segment_class=select_segment_class))
}

get_relative <- function(id, relative) {
  if(!startsWith(id, "ARO")){
    return(list(relative_name=paste("NA", relative), relative_id=paste("NA", relative)))
  }else{
    relative_name <- get_term_property(ontology=card, property=relative, term=id, as_names=TRUE)
    relative_id <- names(relative_name)
    if(is.null(relative_name)){relative_name <- relative_id <- paste("no", relative, "found")}
    return(list(relative_name=relative_name, relative_id=relative_id))
  }
}

#eeeek double maps...work around??
card_dat <- dat %>%
  mutate(
    #card_select_name = map_chr(map(segment, get_select_id), "select_name"), 
    card_select_id = map_chr(map(segment, get_select_id), "select_id"),
    card_select_segment_class = map_chr(map(segment, get_select_id), "select_segment_class"),
    card_parent_name = map_chr(map(card_select_id, get_relative, "parents"), "relative_name"), 
    card_parent_id = map_chr(map(card_select_id, get_relative, "parents"), "relative_id"),
    #card_isa_name = map_chr(map(card_select_id, get_relative, "is_a"), "relative_name"), #confirmed same as parent
    #card_isa_id = map_chr(map(card_select_id, get_relative, "is_a"), "relative_id"), #confirmed same as parent
    card_ancestor_name = map(map(card_select_id, get_relative, "ancestors"), "relative_name"), 
    card_ancestor_id = map(map(card_select_id, get_relative, "ancestors"), "relative_id"))%>% 
  unnest() %>%
  filter(card_ancestor_name != segment)

card_dat_unique <- card_dat %>%
  select(-study_id, -code_identifiers, -code_main) %>%
  unique()
#177/263 drugs with no exact match
#length(unique(card_dat_unique$segment[card_dat_unique$card_select_id=="no exact match"]))

#86/263 drugs with no exact match
#length(unique(card_dat_unique$segment[card_dat_unique$card_select_id!="no exact match"]))

card_dat_unique %<>%
  filter(card_select_id!="no exact match") %>%
  group_by(card_select_id) %>%
  mutate(card_ancestor_rank = 1:n()) 

#TODO: can still be in db as no exact match (eg above)
#TODO: figure out synonyms? eg:
# syn$id[syn$synonym=="gentamicin"]
# card$id[card$id=="ARO:30040152"]
# get_term_property(ontology=card, property="parents", term="ARO:30040152", as_names=TRUE)
```
MeSH Medical Subject Headings (https://meshb.nlm.nih.gov/search)

```r
#advantage of Mesh - synonyms
#no order of parent name

mesh0 <- read_csv(here("scripts", "drugs", "mesh-ontology", "MESH.csv")) %>% #downloaded from bioportal 
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
mesh_dat <- dat %>% left_join(., mesh) 

#name look ups
mesh_dat %<>%
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

mesh_dat_unique <- mesh_dat %>%
  select(-study_id, -code_identifiers, -code_main) %>%
  unique()
#143/263 drugs with no exact match
#length(unique(mesh_dat_unique$segment[is.na(mesh_dat_unique$mesh_id)]))

#120/263 drugs with no exact match
#length(unique(mesh_dat_unique$segment[!is.na(mesh_dat_unique$mesh_id)]))

mesh_dat_unique %<>%
  filter(!is.na(mesh_id))  
```
Combine

```r
#all data
out <- full_join(card_dat, mesh_dat)
save(out, file=here("scripts", "drugs", "drug_card_mesh_ontology.RData"))

#compare parents
out2 <- out %>%  
  filter(!(card_select_id=="no exact match" & is.na(mesh_id)))%>%
  select(segment, card_parent_name, mesh_parent_name)  %>%
  unique()
save(out, file=here("scripts", "drugs", "drug_card_mesh_comparison.RData"))

#
out3 <- out %>%
  filter((card_select_id=="no exact match" & is.na(mesh_id)))%>%
  select(study_id, segment, code_identifiers) 
write.csv(out3, "missing_drug_ontology.csv", row.names = F)
unique(out3$segment)
```

```
##   [1] "amoxicillin+clavulanic acid"            
##   [2] "amoxicillin-clavulanate"                
##   [3] "ampicillin/clav"                        
##   [4] "ceftazidime/clav"                       
##   [5] "pipc"                                   
##   [6] "cfpm"                                   
##   [7] "azt"                                    
##   [8] "gm"                                     
##   [9] "tob"                                    
##  [10] "amk"                                    
##  [11] "amoxicillin/clavulanic acid"            
##  [12] "all ß-lactam antibiotic"                
##  [13] "trimethoprim/sulfamethoxazole"          
##  [14] "piperacillin/tazobactam"                
##  [15] "cefoperazone/sulbactam"                 
##  [16] ": ceftazidime"                          
##  [17] "ampicillin/sulbactam"                   
##  [18] "\002-lactam antibiotics"                
##  [19] "trimethoprim-sulphamethoxazole"         
##  [20] "trimethoprim/ sulfamethoxazole"         
##  [21] "piperacillin/ tazobactam"               
##  [22] "ampicillin/ sulbactam"                  
##  [23] "tetracyclin"                            
##  [24] "moxicillin-clavulanic acid"             
##  [25] "evofloxacin"                            
##  [26] "ampicilin"                              
##  [27] "sulbactam-ampicillin"                   
##  [28] "trimethoprim–sulfamethoxazole"          
##  [29] "piperacillin–tazobactam"                
##  [30] "cefoxitin,"                             
##  [31] "penicillin,"                            
##  [32] "b-lactams"                              
##  [33] "sulphonamides"                          
##  [34] "carbapenemic antibiotics"               
##  [35] "ticarcillin/clavulanic acid"            
##  [36] "trimethoprim/sulphamethoxazole"         
##  [37] "tobramicin"                             
##  [38] "amoxicillin plus clavulanic  acid"      
##  [39] "trimethoprim/sulfamethoxazole tmp/smx"  
##  [40] "ampicillin/clavulanic acid"             
##  [41] "amoxacillin/clavulanate"                
##  [42] "doxicycline"                            
##  [43] "ciprofl oxacin"                         
##  [44] "piperacillin – tazobactam"              
##  [45] "amoxicillin +cla"                       
##  [46] "ticarcillin + cla"                      
##  [47] "piperacillin + tzb"                     
##  [48] "ciprofloxacin;"                         
##  [49] "trimethoprim­sulphamethoxazole"         
##  [50] "cefazoline"                             
##  [51] "cefotaxime/clavulanate"                 
##  [52] "fluoroquinolone"                        
##  [53] "piperacillintazobactam"                 
##  [54] "moxifl oxacin"                          
##  [55] "ofl oxacin"                             
##  [56] "cefotetan/cloxacillin"                  
##  [57] "cefotaxime/clavulanic acid"             
##  [58] "ceftazidime/clavulanic acid"            
##  [59] "cefepime/clavulanic acid"               
##  [60] "moxicillin-clavulanate"                 
##  [61] "enterococcus raffinosus"                
##  [62] "amoxicillin/clavulanate"                
##  [63] "acinetobacter  baumannii  osteomyelitis"
##  [64] "sulfamethoxazole/trimethoprim"          
##  [65] "nitrofurantoi"                          
##  [66] "levofl oxacin"                          
##  [67] "imipinem"                               
##  [68] "piperacillin/tazobactam,"               
##  [69] "o rifampic"                             
##  [70] "ampicillin and sulbactam"               
##  [71] "piperacillin and tazobactam"            
##  [72] "rif"                                    
##  [73] "amoxicillin \006 clavulanate"           
##  [74] "piperacillin \006 tazobactam"           
##  [75] "cefalothin"                             
##  [76] "amoxicillin + cla"                      
##  [77] "ticarcillin +cla"                       
##  [78] "piperacillin + taz"                     
##  [79] "fosfomycin;"                            
##  [80] "β-lactams"                              
##  [81] "amoxicillin þ cla"                      
##  [82] "piperacillin þ tzb"                     
##  [83] "daptomycin nonsusceptible"              
##  [84] "amoxicillin clavulanate"                
##  [85] "icarcillin"                             
##  [86] "icarcillin/clavulanic acid"             
##  [87] "iperacillin/tazobactam"                 
##  [88] "tmp/sxt"                                
##  [89] "azithromycin-atovaquone"                
##  [90] "ticarcillin-clavulanate"                
##  [91] "sodium  cefoperazone"                   
##  [92] "sodium imipenem/cliastatin"             
##  [93] "-tazobactam 256"                        
##  [94] ", piperacillin-tazobact"                
##  [95] "/ml, trimethoprim-sulfametho"           
##  [96] "�g/ml, cipro"                           
##  [97] "���g/ml, le"                            
##  [98] "����g/ml"                               
##  [99] "���g/ml"                                
## [100] "nt to amik"                             
## [101] "vancomycin-intermediate"                
## [102] "methicillin-"                           
## [103] "\002-lactams"                           
## [104] "cloramphenicol"                         
## [105] "amoxicillin/clav"                       
## [106] "etapenem"                               
## [107] "ticarcillin/clavulanate"                
## [108] "tobramycin "                            
## [109] "cefurox"                                
## [110] "rimethoprimsulfamethoxazole"            
## [111] "ceftazidime-clavulanic acid"            
## [112] "trimethoprimsulfamethoxazole"           
## [113] "broad-spectrum cephalosporins"          
## [114] "rimethoprim/sufamethoxazole"            
## [115] "third generation cephalosporins"        
## [116] "rimethoprim/sulfamethoxazole"           
## [117] "fosfomycin."                            
## [118] "sulfamethoxazole-trimetoprim"           
## [119] "imipeneme"                              
## [120] "meropeneme"                             
## [121] "all drugs"                              
## [122] "k. pneumoniae"                          
## [123] "entamicin"                              
## [124] "proxeti"                                
## [125] "flomoxef sodium"                        
## [126] "fosfomycin i.v."                        
## [127] "ampicillin ."                           
## [128] "amoxicillin/ clavulanate"               
## [129] "cefoperazone-sulbactam"                 
## [130] "ticarcilin-clavulanate"                 
## [131] "phenicol"                               
## [132] "sulphonamide"                           
## [133] "trimethopri"                            
## [134] "amoxicillin/clavulate"                  
## [135] "streptomycinb"                          
## [136] "ceftiofurb"                             
## [137] "piperacillin– tazobactam"               
## [138] "pipercillin/tazobactam"                 
## [139] "ticarcillin–clavulanic acid"            
## [140] "tazobactam–piperacillin"                
## [141] "amoxicillin/clavulinic acid"
```


MeSH test archive

```r
# mesh_desc <- xmlToDataFrame(here("scripts", "drugs", "mesh-ontology", "xml", "desc2018.xml"))
# save(mesh_desc, file=here("scripts", "drugs", "mesh-ontology", "xml", "desc2018.RData"))
# mesh_qual <- xmlToDataFrame(here("scripts", "drugs", "mesh-ontology", "xml", "qual2018.xml"))
# save(mesh_qual, file=here("scripts", "drugs", "mesh-ontology", "xml", "qual2018.RData"))
# mesh_pa <- xmlToDataFrame(here("scripts", "drugs", "mesh-ontology", "xml", "pa2018.xml"))
# save(mesh_pa, file=here("scripts", "drugs", "mesh-ontology", "xml", "pa2018.RData"))
# mesh_supp <- xmlToDataFrame(here("scripts", "drugs", "mesh-ontology", "xml", "supp2018.xml"))
#save(mesh_supp, file=here("scripts", "drugs", "mesh-ontology", "xml", "supp2018.RData"))

# endpoint <- "http://id.nlm.nih.gov/mesh/sparql"
# q <-
#  'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
# PREFIX owl: <http://www.w3.org/2002/07/owl#>
# PREFIX meshv: <http://id.nlm.nih.gov/mesh/vocab#>
# PREFIX mesh: <http://id.nlm.nih.gov/mesh/>
# PREFIX mesh2015: <http://id.nlm.nih.gov/mesh/2015/>
# PREFIX mesh2016: <http://id.nlm.nih.gov/mesh/2016/>
# PREFIX mesh2017: <http://id.nlm.nih.gov/mesh/2017/>
# PREFIX mesh2018: <http://id.nlm.nih.gov/mesh/2018/>
# 
# SELECT * 
# FROM <http://id.nlm.nih.gov/mesh>
# WHERE {
#   mesh:D015242 meshv:pharmacologicalAction ?pa .
#   ?pa rdfs:label ?paLabel .
# }
# '
# 
# page <- httr::GET(paste0(endpoint, "?query=", URLencode(q, reserved = TRUE), "&format=CSV"))
# test <- httr::content(page)

# file<-here("scripts", "drugs", "mesh-ontology", "archive", "desc2018.xml")
# library(XML)
# library(methods)
# mesh_desc_xml <- xmlToDataFrame(file)
# test <- mesh[mesh$DescriptorName=="ertapenem",]
```
