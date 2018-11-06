
# Definitions  ----

.PHONY: segments_excels all_data articles_db segments_db events_db clean_mx12 clean_mx18 clean_all

SEGMENTS_EXCELS := $(wildcard data/coded_segments/*.xlsx)
ARTICLE_INDEX_CSV := $(wildcard data-raw/art_index_csvs/*.csv)
CODED_MEX := $(wildcard data-raw/coded_text_mex/*.mex)
ARTICLES_DB := data/articles_db.csv
SEGMENTS := data/segments.csv
EVENTS_DB := data/events_db.csv
LOCATIONS:= data/locations.csv
DRUGS := data/drugs.csv
BACTERIA := data/bacteria_genus_species.csv
DATES := data/dates.csv
MESH_ONTOL := data-raw/mesh-ontology/mesh.rds
MESH_ONTOL_RAW := data-raw/mesh-ontology/as_received/MESH.csv.zip
NCBI_ONTOL := data-raw/ncbi-ontology/ncbi.rds
NCBI_ONTOL_RAW := data-raw/ncbi-ontology/as_received/NCBITAXON.csv.zip


# Cleaning annotated segments
segments: $(SEGMENTS)
drugs: $(DRUGS)
locations: $(LOCATIONS)
bacteria: $(BACTERIA)
dates: $(DATES)

# Data Base Outputs 
articles_db: $(ARTICLES_DB)
events_db: $(EVENTS_DB)

# Other 
segments_excels : $(SEGMENTS_EXCELS)
all_data: $(ARTICLES_DB) $(EVENTS_DB)


# Rules  ----

data/coded_segments/%.xlsx: data-raw/coded_text_mex/%.mex
	osascript scripts/01_export_segs_single_mex.scpt $^

# Locations rules

$(LOCATIONS): scripts/04_clean_locations.R $(SEGMENTS)
	Rscript $<
	
# Drug rules

$(MESH_ONTOL): scripts/helper_scripts/clean_mesh.R $(MESH_ONTOL_RAW)
	Rscript $<

$(DRUGS): scripts/05_clean_drugs.R $(SEGMENTS) $(ARTICLES_DB) $(MESH_ONTOL)
	Rscript $<
	
# Bacteria rules

$(NCBI_ONTOL): scripts/helper_scripts/clean_ncbi.R $(NCBI_ONTOL_RAW)
	Rscript $<

$(BACTERIA): scripts/06_clean_bacteria.R $(SEGMENTS) $(ARTICLES_DB) $(NCBI_ONTOL)
	Rscript $<

# Dates rules

$(DATES): scripts/07_clean_dates.R $(SEGMENTS)
	Rscript $<

# General rules

$(ARTICLES_DB): scripts/02_index_articles.R $(ARTICLE_INDEX_CSV) $(CODED_MEX)
	Rscript $<

$(SEGMENTS): scripts/03_clean_segments.R $(SEGMENTS_EXCELS) $(ARTICLES_DB)
	Rscript $<

$(EVENTS_DB): scripts/99_create_events_db.R $(SEGMENTS_DB) $(DRUGS) $(LOCATIONS)
	Rscript $<

# Clean up

clean_mx12: 
	rm -r data-raw/coded_text_mex/*.mx12

clean_all:
	rm -r data/articles_db.csv data/events_db.csv

clean_mx18: 
	rm -r data-raw/coded_text_mex/*.mx18

# data-raw/coded_text_mex/%.mex: data/coded_segments/%.xlsx
