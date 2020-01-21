# Antimicrobial Resistance Database

This repository contains code, data, and documentation for the AMR events database. 

---

### Repo Structure

-  `screening/` contains three folders related to the process of finding articles and medical reports:
	- 	`literature/search-results.csv` contains the results of PubMed and Embase literature search (n = 23,770). This file contains full article abstracts, which were individually reviewed to determine whether the article should be downloaded for full text review. 
	-	`promed-mail/search-results.csv` contains the results of the ProMED-mail search (n = 1,196). This file contains links to the ProMED-mail reports. The first lines of these reports were reviewed to determine whether they should be included in the full text review.
	-	`selected/` contains `.csv` files listing the articles that were selected for full text review. The `downloaded` column indicates whether articles were successfully downloaded. 1,791 articles were downloaded for full-text review.
	
-  `data-raw/` contains raw data used in the creation of database files including:
	-	`coded_text_mex/` directory that contains all coded article pdfs as `.mex` files. Note: `.mx12` or `mx.18` files are created as a byproduct of opening a `.mex` file. All `.mx12` and `.mx18` files can be cleared using `make clean_mx12` and `make clean_mx18`. 
	-	`art_index_csvs/` directory contains all csvs used to index articles that were reviewed in the full text review phase of the project. There was one csv per review batch.
	-	`card-ontology/` directory contains the Comprehensive Antibiotic Resistance Database ontology.
	-	`mesh-ontology/` directory contains the Medical Subject Headings ontology.
	-	`ncbi-ontology/` directory contains the National Center for Biotechnology Information ontology.
	- `maxqda_code_index.csv` contains the schema used to code studies in MAXQDA.
	- `geocode_locations_complete.csv` is a tracking list of all locations that have been geolocated.
- `data/` contains all derived data including:
	-	`coded_segments/` directory containing all exported segments from `.mex` files (exported using MAXQDA). These files can be created via the applscript `scripts/01_export_segs_single_mex.scpt` or manually from MAXQDA.
	-	`segments_raw.rds` is the raw database.
	-	`events_db_full.csv` is the cleaned and standardized database and `events_db.csv` is the database containing only primary fields: study country, drug name, bacteria name, and date.
	-	Remaining csv files are intermediate steps created in the data cleaning process.
- `figures/` contains data summary figures and map.
- `scripts/` contains all scripts used to derive outputs. These are formulated in a pipeline and should be run sequentially (or run using the `Makefile`).
	-	`01_export_segs_single_mex.scpt` - an applescript that uses the raw `.mex` files to create `coded_segments/` `.xlsx` files.
	-	`02_index_articles.R` - builds the `articles_db.csv` database using the `art_index_csvs/` files and `coded_text_mex/` files.
	-	`03_clean_segments.R` - builds the `segments.csv` database using the `articles_db.csv` database and derived `coded_segments/` `.xlsx` files.
	-	`04_clean_locations.R` -  builds the `locations.csv` file from `segments.csv` using Google geocoding. 
	-	`05_clean_drugs.R` -  builds the `drugs.csv` file from `segments.csv` based on MeSH ontology. 
	-	`06_clean_bacteria.R` -  builds the `bacteria_genus_species.csv` and  `bacteria_strains_and_resistance_markers.csv` file from `segments.csv` based on NCBI and CARD ontologies, respectively. 
	-	`07_clean_dates.R` -  builds the `dates.csv` file from `segments.csv`. 
	- `99_create_events_db.R` - combines outputs of locations, drugs, bacteria, and dates scripts to create the final database as `events_db_full.csv` and `events_db.csv`.
	- `data_summary.R` - creates figures to summarize contents of events database.  Exports to `figures/`.
	- `data_map.R` - creates leaflet map showing location of AMR events.  Exports to `figures/`.
	-	`helper_scripts` contains functions to QA the data and to curate and clean the data outside the data generation pipeline. 

---

### Reproducing the Database

First, if you have the package `devtools` installed, you can ensure you have the correct packages installed to run this repo's code by running the following R code: 

```
library(devtools)
install_deps()
```

The `Makefile` will re-build the project. Before running the `Makefile` you can run `sh gittime.sh` in terminal (from within the repo) to ensure file time stamps represent the time the file was last committed to the repo, not the time the file was last checked out/ cloned from the repo. To run the applescripts, be sure your `Accessibility` settings allow the Terminal app to control your computer. You can set this in `System Preferences > Security & Privacy > Accessibility`.

Main `Makefile` commands are:

- `make aricles_db` 
- `make segments_db`
- `make events_db`
- `clean_mx12` - cleans only `.mx12` files
- `clean_all` - cleans all files including `.mx12` and `.csv` files 
