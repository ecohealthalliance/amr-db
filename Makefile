
.PHONY: segments_excels all_data articles_db segments_db events_db clean_mx12 clean_all

SEGMENTS_EXCELS := $(wildcard data/coded_segments/*.xlsx)
ARTICLE_INDEX_CSV := $(wildcard data-raw/art_index_csvs/*.csv)
CODED_MEX := $(wildcard data-raw/coded_text_mex/*.mex)
ARTICLES_DB := data/articles_db.csv
SEGMENTS_DB := data/segments_db.csv
EVENTS_DB := data/events_db.csv
LOCATIONS:= data/locations_db.csv
DRUGS := data/drugs_db.csv
MESH_ONTOL := data-raw/mesh-ontology/mesh.rds
MESH_ONTOL_RAW := data-raw/mesh-ontology/mesh_raw.zip



# Cleaning annotated segments
segments_db: $(SEGMENTS_DB)
drugs: $(DRUGS_DB)
locations: $(LOCATIONS)

# Data Base Outputs 
articles_db: $(ARTICLES_DB)
events_db: $(EVENTS_DB)

# Other 
segments_excels : $(SEGMENTS_EXCELS)
all_data: $(ARTICLES_DB) $(SEGMENTS_DB)


data/coded_segments/%.xlsx: data-raw/coded_text_mex/%.mex
	osascript scripts/01_export_segs_single_mex.scpt $^



# Drug rules

$(MESH_ONTOL): scripts/clean_mesh.R $(MESH_ONTOL_RAW)
	Rscript $<

$(DRUGS): scripts/05_clean_drugs.R $(SEGMENTS_DB) $(ARTICLES_DB) $(MESH_ONTOL)
	Rscript $<


# Locations rules

$(LOCATIONS): scripts/04_clean_locations.R $(SEGMENTS_DB)


# General rules


$(ARTICLES_DB): scripts/02_index_articles.R $(ARTICLE_INDEX_CSV) $(CODED_MEX)
	Rscript $<

$(SEGMENTS_DB): scripts/03_clean_segments.R $(SEGMENTS_EXCELS) $(ARTICLES_DB)
	Rscript $<


$(EVENTS_DB): scripts/99_create_events_db.R $(SEGMENTS_DB) $(DRUGS) $(LOCATIONS)
	Rscript $<

# Clean up

clean_mx12: 
	rm -r data-raw/coded_text_mex/*.mx12

clean_all:
	rm -r data/articles_db.csv data/segments_db.csv data/events_db.csv


# data-raw/coded_text_mex/%.mex: data/coded_segments/%.xlsx
