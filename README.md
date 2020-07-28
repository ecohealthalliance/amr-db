[![DOI](https://zenodo.org/badge/146802522.svg)](https://zenodo.org/badge/latestdoi/146802522)

# Antimicrobial Resistance Emergence Events Database

This repository contains code, data, and documentation used to genreate a database of antimicrobial
resistance emergence events. 

___Authors___:  _Emma Mendelsohn, Noam Ross, Allison M. White, Karissa Whiting, 
Cale Basaraba, Brooke Watson Madubuonwu, Mushtaq Dualeh, Zach Matson, Erica Johnson,
Sonia Dattaray, Samantha Maher, Melanie Kirshenbaum, Jacob Kotcher, 
Nchedo Ezekoli, Carlos Zambrana-Torrelio, Peter Daszak_

---

### Repo Structure

The repo includes raw data and intermediate steps used in the process of identifying
articles from search, screening abstracts by multiple reviewers, coding full-text
articles and processing coded test.  Please see the manuscript for more detail.

-	`events-db.csv` is the cleaned and standardized database. It contains the following fields: 
	-	`study_id` - unique study identification number that can be joined with `articles-db.csv` for study metadata.
	-	`study_country` - name of country where event occurred. Note that there are some studies that report on events in multiple countries.
	-	`study_iso3c` - three letter International Organization for Standardization (ISO) code
	-	`study_location` - full study location (including hospital, city, and state if available)
	-	`study_location_basis` - spatial basis of study location (e.g., "hospital, city, state_province_district, country") 
	-	`residence_location` - location of patient residence
	-	`travel_location` - patient travel locations, if any reported. Multiple locations are separated by `;`.
	-	`drug` - antimicrobial drug, standardized to the Medical Subject Headings (MeSH) ontology14. Drug combinations are concatenated by `+`.
	-	`drug_rank` - taxonomic classification of drug (i.e., drug name or group)
	-	` segment_drug_combo` - TRUE/FALSE resistance is to a combination of drugs
	-	`drug_parent_name` - name of the taxonomic parent of antimicrobial drug, standardized to the Medical Subject Headings (MeSH) ontology.
	-	`bacteria` - name of resistant bacteria, standardized to NCBI Organismal Classification ontology. 
	-	`bacteria_rank` - taxonomic classification of bacteria name (e.g., “species”, “genus”)
	-	`bacteria_parent_name` - name of the taxonomic parent of bacteria, standardized to NCBI Organismal Classification ontology
	-	`bacteria_parent_rank` - - taxonomic classification of bacteria parent name (e.g., “species”, “genus”)
	-	`start-date` - date that emergence was reported in format of yyyy-mm-dd
	-	`end_date` - date that emergence was resolved, if reported, in format of yyyy-mm-dd
	-	`start_date_rank` - specificity of the start date (i.e., year, month, day)
	
Sub-folders contain components of the workflow that generated the database.

-  `screening/` contains three folders related to the process of finding articles and medical reports, and 
	- 	`literature/search-results.csv.zip` contains the (compressed) results of PubMed and Embase literature search (n = 23,770). This file contains full article abstracts, which were individually reviewed to determine whether the article should be downloaded for full text review. 
	-	`promed-mail/search-results.csv` contains the results of the ProMED-mail search (n = 1,196). This file contains links to the ProMED-mail reports. The first lines of these reports were reviewed to determine whether they should be included in the full text review.
	-	`selected/` contains `.csv` files listing the articles that were selected for full text review. The `downloaded` column indicates whether full-text articles were able to be successfully downloaded. There is one csv per review batch. 1,791 articles were downloaded for full-text review.
	
-  `data-raw/` contains raw data used in the creation of database files including:
	-	`coded-text-mex/` is empty here. In the workflow this folder contains `.mex` (MAXQDA) files.  These files include both the full text and annotations, organized by batch and primary coder.
***We are unable to include full text PDFs and coded files here due to publisher copyrights.  Please contact [amrdb@ecohealthalliance.org](mailto:amrdb@ecohealthalliance.org) if you require them***
	
	-	`coded-segments/` directory contains all exported coded text segments from `.mex` files (exported using MAXQDA). These files can be created via the applscript `scripts/01_export_segs_single_mex.scpt` or manually from MAXQDA.
	-	`card-ontology/` directory contains the Comprehensive Antibiotic Resistance Database ontology.
	-	`mesh-ontology/` directory contains the Medical Subject Headings ontology.
	-	`ncbi-ontology/` directory contains the National Center for Biotechnology Information ontology.
	- 	`maxqda-code-index.csv` contains the schema used to code studies in MAXQDA.
	
- `data-processed/` contains all derived data including:
	-	`articles-db.csv` is a master list of all the articles that were selected for full-text review. It is the compilation of all csvs in `screening/selected/`.
	-	`segments.csv` is the raw database, before any data munging. 
	-	Remaining csv files are intermediate steps created in the data cleaning process.
	
	
- `figures/` contains data summary figures and map.

- `scripts/` contains all scripts used to derive outputs. 

	- `database-dev/` contains scripts to process the data. These are formulated in a pipeline and should be run sequentially.
	
		-	`01_export_segs_single_mex.scpt` an applescript that uses the raw `.mex` files to create `data-raw/coded-segments/` `.xlsx` files.
		-	`02_index_articles.R` builds the `articles-db.csv` database using the `screening/selected/` files and `data-raw/coded-text-mex/` files.
		-	`03_clean_segments.R` builds the `segments.csv` database using `articles-db.csv` and `data-raw/coded-segments/` `.xlsx` files.
		-	`04_clean_locations.R` builds the `locations.csv` file from `segments.csv` using Google geocoding. 
		-	`05_clean_drugs.R` builds the `drugs.csv` file from `segments.csv` based on MeSH ontology. 
		-	`06_clean_bacteria.R` builds the `bacteria_genus_species.csv` and  `bacteria_strains_and_resistance_markers.csv` file from `segments.csv` based on NCBI and CARD ontologies, respectively. 
		-	`07_clean_dates.R`  builds the `dates.csv` file from `segments.csv`. 
		- 	`99_create_events_db.R` combines outputs of locations, drugs, bacteria, and dates scripts to create the final database `events_db.csv`.
		
	- `figure-dev/` contains scripts to make figures.	
		- 	`data_summary.R` creates figures to summarize contents of events database.  Exports to `figures/`.
		-  	`data_map.R` creates leaflet map showing location of AMR events.  Exports to `figures/`.
		-	`flowchart.R` makes a flowchart of the data pipeline for this project. 
		
	- `helper/` contains functions to QA the data and to curate and clean the data outside the data generation pipeline. 
```
