library(stringi)
library(ggmap)
load("data/coded_annotations.RData")

#'Temp fix to mislabeled city
codes %<>%
  mutate(code_main=ifelse(study_id==3095 & segment=="Rome", "City", code_main)) 

#'Function for data manipulation
paste_collapse <- function(x){
  x <- x[!is.na(x) & x!=""]
  paste(x, collapse = ", ")
}

#'Manual substitution - countries
country_sub <- tribble(
  ~old,             ~new,
  "\r\n",            "",
  "the",             "",
  "2",               "",
  "republic of korea", "south korea", 
  "greece, greece",  "greece",
  "uk",              "united kingdom",
  "ndia",            "india", 
  "colombian",       "colombia",
  "usa",             "united states", 
  "méxico",          "mexico",
  "nerlands",        "netherlands",
  "austria, austria","austria",
  "united statesb",  "united states",
  "norwegian",       "norway",
  "japanese",        "japan"
)

#'Clean dat (incomplete)
dat <- codes %>%
  filter(code_main_cat=="Location") %>%
  spread(key = code_main, value = segment) %>%
  group_by(code_main_cat, study_id, code_identifiers) %>%
  summarise_all(funs(paste_collapse)) %>%
  mutate_all(funs(tolower)) %>%
  mutate(Country = stri_replace_all_fixed(Country,
                                          country_sub$old,
                                          country_sub$new,
                                          vectorize_all = FALSE)) %>%
  mutate(Country = trimws(Country, "both")) %>%
  mutate(Country = gsub("iindia", "india", Country)) %>%
  mutate(City_Country = ifelse(Country!= "" & City!="", paste(City, Country, sep=", "), ""),
         `State/Province/District_Country` = ifelse(Country!= "" & `State/Province/District`!="", paste(`State/Province/District`, Country, sep=", "), "")) %>%
  ungroup()

#'Evaluate data available
pref <- c("City_Country",  "State/Province/District_Country", "City", "State/Province/District", "Country") #TODO"Hospital name", "Place traveled to" 

dat = as.data.frame(dat)
dat$geoloc_basis <- ""
dat$geoloc_value <- ""
for (i in seq_along(pref)) {
  dat$geoloc_basis <- ifelse(dat[,pref[i]]!="" & dat$geoloc_basis=="", pref[i], dat$geoloc_basis)
}
for(gl in unique(dat$geoloc_basis[!dat$geoloc_basis==""])){
  dat$geoloc_value[dat$geoloc_basis==gl] <-dat[dat$geoloc_basis==gl, gl]
}

#'Geocoding
test <- filter(dat, geoloc_basis=="Country")
test <- test %>% 
  mutate_geocode(geoloc_value, source = "dsk") 

library(maptools)
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% tools::toTitleCase(test$Country)
tools::toTitleCase(test$Country)[!tools::toTitleCase(test$Country) %in% wrld_simpl@data$NAME]
pdf("countries.pdf", paper="USr", width=10, height=7.25)
plot <- plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
dev.off()

#TODO handle locations with A, B etc identifiers

#TODO follow up on following
# 10248, 23314 cty/country listed twice 
# 3095 Rome was coded as country
# Should "Country of Residence"   be included with location?  it's with patient now
