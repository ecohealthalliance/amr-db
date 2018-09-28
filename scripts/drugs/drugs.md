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

unique(dat$segment)
```

```
##   [1] "ertapenem"                              
##   [2] "imipenem"                               
##   [3] "fluoroquinolones"                       
##   [4] "co-trimoxazole"                         
##   [5] "tobramycin"                             
##   [6] "gentamicin"                             
##   [7] "amoxicillin"                            
##   [8] "amoxicillin+clavulanic acid"            
##   [9] "cefuroxime"                             
##  [10] "ceftriaxone"                            
##  [11] "ceftazidime"                            
##  [12] "meropenem"                              
##  [13] "cephalosporins"                         
##  [14] "isoniazid"                              
##  [15] "rifampin"                               
##  [16] "pyrazinamide"                           
##  [17] "ethambutol"                             
##  [18] "amoxicillin-clavulanate"                
##  [19] "erythromycin"                           
##  [20] "clarithromycin"                         
##  [21] "tetracycline"                           
##  [22] "levofloxacin"                           
##  [23] "moxifloxacin"                           
##  [24] "gatifloxacin"                           
##  [25] "ampicillin"                             
##  [26] "ampicillin/clav"                        
##  [27] "ceftazidime/clav"                       
##  [28] "aztreonam"                              
##  [29] "clavulanic acid"                        
##  [30] "tazobactam"                             
##  [31] "rifampicin"                             
##  [32] "ciprofloxacin"                          
##  [33] "amikacin"                               
##  [34] "azithromycin"                           
##  [35] "pipc"                                   
##  [36] "cfpm"                                   
##  [37] "czop"                                   
##  [38] "azt"                                    
##  [39] "gm"                                     
##  [40] "tob"                                    
##  [41] "amk"                                    
##  [42] "amoxicillin/clavulanic acid"            
##  [43] "all ß-lactam antibiotic"                
##  [44] "trimethoprim/sulfamethoxazole"          
##  [45] "piperacillin/tazobactam"                
##  [46] "cefepime"                               
##  [47] "cefoperazone/sulbactam"                 
##  [48] ": ceftazidime"                          
##  [49] "ticarcillin"                            
##  [50] "tigecycline"                            
##  [51] "ampicillin/sulbactam"                   
##  [52] "\002-lactam antibiotics"                
##  [53] "nalidixic acid"                         
##  [54] "norfloxacin"                            
##  [55] "cefotaxime"                             
##  [56] "cefoxitin"                              
##  [57] "piperacillin"                           
##  [58] "cephalothin"                            
##  [59] "amoxicillin-clavulanic acid"            
##  [60] "minocycline"                            
##  [61] "doxycycline"                            
##  [62] "gentamycin"                             
##  [63] "trimethoprim-sulphamethoxazole"         
##  [64] "sulperazon"                             
##  [65] "colistin"                               
##  [66] "trimethoprim/ sulfamethoxazole"         
##  [67] "cefoperazone"                           
##  [68] "piperacillin/ tazobactam"               
##  [69] "ampicillin/ sulbactam"                  
##  [70] "trimethoprim-sulfamethoxazole"          
##  [71] "chloramphenicol"                        
##  [72] "tetracyclin"                            
##  [73] "streptomycin"                           
##  [74] "cefpirome"                              
##  [75] "carbapenems"                            
##  [76] "doripenem"                              
##  [77] "aminoglycosides"                        
##  [78] "fosfomycin"                             
##  [79] "moxicillin-clavulanic acid"             
##  [80] "cefixime"                               
##  [81] "penicillin"                             
##  [82] "cefotetan"                              
##  [83] "nitrofurantoin"                         
##  [84] "methicillin"                            
##  [85] "evofloxacin"                            
##  [86] "piperacillin-tazobactam"                
##  [87] "ampicillin-sulbactam"                   
##  [88] "cefazolin"                              
##  [89] "ampicilin"                              
##  [90] "trimethoprim"                           
##  [91] "sulfamethoxazole"                       
##  [92] "linezolid"                              
##  [93] "ceftaroline"                            
##  [94] "clindamycin"                            
##  [95] "sulbactam-ampicillin"                   
##  [96] "ofloxacin"                              
##  [97] "trimethoprim–sulfamethoxazole"          
##  [98] "piperacillin–tazobactam"                
##  [99] "cotrimoxazole"                          
## [100] "furoxone"                               
## [101] "cefpodoxime"                            
## [102] "cefoxitin,"                             
## [103] "penicillin,"                            
## [104] "penicillin g"                           
## [105] "mezlocillin"                            
## [106] "b-lactams"                              
## [107] "sulphonamides"                          
## [108] "kanamycin"                              
## [109] "daptomycin"                             
## [110] "ethionamide"                            
## [111] "vancomycin"                             
## [112] "carbapenemic antibiotics"               
## [113] "ticarcillin/clavulanic acid"            
## [114] "trimethoprim/sulphamethoxazole"         
## [115] "tobramicin"                             
## [116] "teicoplanin"                            
## [117] "telithromycin"                          
## [118] "amoxicillin plus clavulanic  acid"      
## [119] "carbapenem"                             
## [120] "trimethoprim/sulfamethoxazole (tmp/smx)"
## [121] "netilmicin"                             
## [122] "ampicillin/clavulanic acid"             
## [123] "quinolones"                             
## [124] "oxacillin"                              
## [125] "amoxacillin/clavulanate"                
## [126] "doxicycline"                            
## [127] "metronidazole"                          
## [128] "penicillins"                            
## [129] "ciprofl oxacin"                         
## [130] "piperacillin – tazobactam"              
## [131] "amoxicillin +cla"                       
## [132] "ticarcillin + cla"                      
## [133] "piperacillin + tzb"                     
## [134] "sulfonamides"                           
## [135] "ciprofloxacin;"                         
## [136] "trimethoprim­sulphamethoxazole"         
## [137] "amoxycillin"                            
## [138] "cefazoline"                             
## [139] "cefotaxime/clavulanate"                 
## [140] "sulbenicillin"                          
## [141] "fluoroquinolone"                        
## [142] "piperacillintazobactam"                 
## [143] "moxifl oxacin"                          
## [144] "ofl oxacin"                             
## [145] "rifabutin"                              
## [146] "cefotetan/cloxacillin"                  
## [147] "cefotaxime/clavulanic acid"             
## [148] "ceftazidime/clavulanic acid"            
## [149] "cefepime/clavulanic acid"               
## [150] "moxicillin-clavulanate"                 
## [151] "enterococcus raffinosus"                
## [152] "amoxicillin/clavulanate"                
## [153] "prulifloxacin"                          
## [154] "acinetobacter  baumannii  osteomyelitis"
## [155] "sulfamethoxazole/trimethoprim"          
## [156] "nitrofurantoi"                          
## [157] "levofl oxacin"                          
## [158] "cefadroxil"                             
## [159] "imipinem"                               
## [160] "piperacillin/tazobactam,"               
## [161] "o rifampic"                             
## [162] "ampicillin and sulbactam"               
## [163] "piperacillin and tazobactam"            
## [164] "cefaclor"                               
## [165] "rif"                                    
## [166] "amoxicillin \006 clavulanate"           
## [167] "piperacillin \006 tazobactam"           
## [168] "cefalothin"                             
## [169] "amoxicillin + cla"                      
## [170] "ticarcillin +cla"                       
## [171] "piperacillin + taz"                     
## [172] "cephradine"                             
## [173] "ticarcillin-clavulanic acid"            
## [174] "arbekacin"                              
## [175] "fosfomycin;"                            
## [176] "ceftizoxime"                            
## [177] "cephalexin"                             
## [178] "carbenicillin"                          
## [179] "β-lactams"                              
## [180] "ceftriaxon"                             
## [181] "amoxicillin þ cla"                      
## [182] "piperacillin þ tzb"                     
## [183] "daptomycin nonsusceptible"              
## [184] "amoxicillin clavulanate"                
## [185] "icarcillin"                             
## [186] "icarcillin/clavulanic acid"             
## [187] "iperacillin/tazobactam"                 
## [188] "tmp/sxt"                                
## [189] "cefalotin"                              
## [190] "azithromycin-atovaquone"                
## [191] "ticarcillin-clavulanate"                
## [192] "temocillin"                             
## [193] "sodium  cefoperazone"                   
## [194] "sulbactam"                              
## [195] "ornidazole"                             
## [196] "sodium imipenem/cliastatin"             
## [197] "-tazobactam (256"                       
## [198] ", piperacillin-tazobact"                
## [199] "/ml), trimethoprim-sulfametho"          
## [200] "�g/ml), cipro"                          
## [201] "���g/ml), le"                           
## [202] "����g/ml"                               
## [203] "���g/ml)"                               
## [204] "nt to amik"                             
## [205] "vancomycin-intermediate"                
## [206] "methicillin-"                           
## [207] "\002-lactams"                           
## [208] "cloramphenicol"                         
## [209] "amoxicillin/clav"                       
## [210] "etapenem"                               
## [211] "aztreonam("                             
## [212] "tetracyclines"                          
## [213] "ticarcillin/clavulanate"                
## [214] "tobramycin "                            
## [215] "polymyxin b"                            
## [216] "clavulanate"                            
## [217] "cefurox"                                
## [218] "benzylpenicillin"                       
## [219] "cephaloridine"                          
## [220] "rimethoprimsulfamethoxazole"            
## [221] "mupirocin"                              
## [222] "fusidic acid"                           
## [223] "ceftazidime-clavulanic acid"            
## [224] "co-amoxiclav"                           
## [225] "trimethoprimsulfamethoxazole"           
## [226] "broad-spectrum cephalosporins"          
## [227] "rimethoprim/sufamethoxazole"            
## [228] "cefuroxime axetil"                      
## [229] "third generation cephalosporins"        
## [230] "macrolides"                             
## [231] "rimethoprim/sulfamethoxazole"           
## [232] "fosfomycin."                            
## [233] "gemifloxacin"                           
## [234] "sulfamethoxazole-trimetoprim"           
## [235] "flucloxacillin"                         
## [236] "imipeneme"                              
## [237] "meropeneme"                             
## [238] "all drugs"                              
## [239] "k. pneumoniae"                          
## [240] "entamicin"                              
## [241] "proxeti"                                
## [242] "cefozopran"                             
## [243] "flomoxef sodium"                        
## [244] "capreomycin"                            
## [245] "sulphamethoxazole"                      
## [246] "fosfomycin i.v."                        
## [247] "ampicillin ."                           
## [248] "amoxicillin/ clavulanate"               
## [249] "cefoperazone-sulbactam"                 
## [250] "ticarcilin-clavulanate"                 
## [251] "phenicol"                               
## [252] "sulphonamide"                           
## [253] "trimethopri"                            
## [254] "amoxicillin/clavulate"                  
## [255] "streptomycinb"                          
## [256] "ceftiofurb"                             
## [257] "neomycin"                               
## [258] "meticillin"                             
## [259] "furazolidone"                           
## [260] "piperacillin– tazobactam"               
## [261] "pipercillin/tazobactam"                 
## [262] "ticarcillin–clavulanic acid"            
## [263] "tazobactam–piperacillin"                
## [264] "amoxicillin/clavulinic acid"
```

Ontologies at the Comprehensive Antibiotic Resistance Database are freely available under
the Creative Commons CC-BY license version 4.0, https://creativecommons.org/licenses/by/4.0/

```r
# ARO - The Antibiotic Resistance Ontology that serves as the primary organizing principle 
# of the CARD.
aro <- jsonlite::fromJSON(here("scripts", "drugs", "card-ontology", "aro.json")) %>%
  rename(segment = name, aro_accession = accession, aro_description = description)
dat<- left_join(dat, aro)
dat
```

```
## # A tibble: 2,006 x 6
##    study_id segment code_identifiers code_main aro_accession
##    <chr>    <chr>   <chr>            <chr>     <chr>        
##  1 10020    ertape… ""               Drug Res… ARO:0000070  
##  2 10020    imipen… ""               Drug Res… ARO:3000170  
##  3 10020    fluoro… ""               Drug Res… <NA>         
##  4 10020    co-tri… ""               Drug Res… <NA>         
##  5 10020    tobram… ""               Drug Res… ARO:0000052  
##  6 10020    gentam… ""               Drug Res… <NA>         
##  7 10020    amoxic… A                Drug Res… ARO:0000064  
##  8 10020    amoxic… B                Drug Res… <NA>         
##  9 10020    cefuro… C                Drug Res… ARO:0000063  
## 10 10020    ceftri… D                Drug Res… ARO:0000062  
## # ... with 1,996 more rows, and 1 more variable: aro_description <chr>
```

```r
#fluoroquinolones is a class
#co-trimoxazole is Trimethoprim or sulfamethoxazole, both of which are in db
```
MeSH Medical Subject Headings (https://meshb.nlm.nih.gov/search)

```r
mesh0 <- read_csv(here("scripts", "drugs", "mesh-ontology", "MESH.csv")) 
mesh <- mesh0 %>%
  select(`Class ID`, `Preferred Label`, Synonyms, Definitions) %>%
  mutate(cat_split = str_split(Synonyms, "\\|")) %>%
  unnest() %>%
  mutate(cat_split = str_trim(cat_split)) %>%
  select(-Synonyms) %>%
  rename(Synonyms = cat_split) %>%
  gather(key="mesh_segment_class", value="segment", `Preferred Label`, Synonyms, -`Class ID`, -Definitions) %>%
  rename(mesh_class_id = `Class ID`, mesh_definitions=Definitions) %>%
  mutate(segment = tolower(segment)) %>%
  unique() %>%
  filter(rowSums(is.na(.)) != ncol(.))

pref_names <- unique(mesh$segment[mesh$mesh_segment_class=="Preferred Label"])
syn_names <- unique(mesh$segment[mesh$mesh_segment_class=="Synonyms"])
mesh <- mesh[!c(mesh$mesh_segment_class=="Synonyms" & mesh$segment %in% pref_names),] #filter out synonymns that are also primary names
  
out <- left_join(dat, mesh)
write.csv(out, "drug_card_mesh_ontology.csv", row.names = F)
out2 <- out[c(is.na(out$aro_accession) & is.na(out$mesh_class_id)),]
write.csv(out2, "missing_drug_ontology.csv", row.names = F)
```
MeSH test archive

```r
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
```
