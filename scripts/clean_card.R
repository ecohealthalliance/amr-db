library(tidyverse)
library(magrittr)
library(ontologyIndex)
library(here)

#Ontologies at the Comprehensive Antibiotic Resistance Database are freely available under
#the Creative Commons CC-BY license version 4.0, https://creativecommons.org/licenses/by/4.0/
#  license may be needed: https://card.mcmaster.ca/about

# ARO - The Antibiotic Resistance Ontology that serves as the primary organizing principle 
# of the CARD.

card_aro <- get_ontology(here("data-raw", "card-ontology", "as_received", "aro.obo"), extract_tags="everything")
card_ncbi <- get_ontology(here("data-raw", "card-ontology", "as_received", "ncbi_taxonomy.obo"), extract_tags="everything")

card_aro$name <- tolower(card_aro$name)
card_ncbi$name <- tolower(card_ncbi$name)


# functions for getting data from ontology
get_select_id <- function(seg, card){
  
  id <- get(card)$id[grep(x = get(card)$name, pattern = seg, ignore.case = T)] #get all ids associated with segment term
  all_prop <- map(id, function(x) get_term_property(ontology = get(card), property = "id", term = x, as_names = TRUE))  #get all names
  select_name <- all_prop[match(seg, unlist(all_prop))] #select name with exact match to term
  select_id <- names(select_name) #select id with exact match to term
  select_name <- unlist(select_name)
  select_segment_class <- "primary"
  if(length(select_name) == 0){  select_name <- select_id <- select_segment_class <-"no exact match"}
  return(list(select_name = select_name, select_id = select_id, select_segment_class = select_segment_class))
}

get_relative <- function(id, card, relative) {
  if(id == "no exact match"){
    return(list(relative_name=paste("NA", relative), relative_id=paste("NA", relative)))
  }else{
    relative_name <- get_term_property(ontology = get(card), property = relative, term = id, as_names = TRUE)
    relative_id <- names(relative_name)
    if(is.null(relative_name)){relative_name <- relative_id <- paste("no", relative, "found")}
    return(list(relative_name = relative_name, relative_id = relative_id))
  }
}
