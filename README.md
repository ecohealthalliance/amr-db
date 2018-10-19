# Antimicrobial Resistance Database

This repository contains code, data, and documentation for the AMR events database. 

---

### Repo Structure


-  `data-raw/` contains raw data used in the creation of database files including:
	-	`coded_text_mex/` directory that contains all coded article pdfs as `.mex` files. Note: `.mx12` files are created as a byproduct of opening a `.mex` file. All `.mx12` files can be cleared using `make clean_mx12`. 
	-	`art_index_csvs/` directory contains all csvs used to index articles that were reviewed in the full text review phase of the project. There was one csv per review batch.
- `data/` contains all derived data including:
	-	`coded_segments/` directory containing all exported segments from `.mex` files (exported using MAXQDA). These files are created via the applscript `scripts/01_export_segs_single_mex.scpt`.
	-	Resulting data files `articles_db.rds`, `segments_db.rds`, and `events_db.rds` are the three primary components of the amr database and are placed in this directory when the appropriate `make` commands are run.
- `scripts/` contains all scripts used to derive outputs. These are formulated in a pipeline and should be run sequentially (or run using the `Makefile`).
	-	`01_export_segs_single_mex.scpt` - an applescript that uses the raw `.mex` files to create `coded_segments/` `.xlsx` files.
	-	`02_index_articles.R` - builds the `articles_db.rds` database using the `art_index_csvs/` files and `coded_text_mex/` files.
	-	`03_clean_segments.R` - builds the `segments_db.rds` database using the `articles_db.rds` database and derived `coded_segments/` `.xlsx` files.
	-	`04_clean_locations.R` -  builds the `events_db.rds` database using the `segments_db.rds` and `articles_db.rds` database. 
	-	other scripts beginning with `check_` are used to curate and clean databases outside the data generation pipeline. 
	-	`locations/` directory contains files to generate reports on study, residence, and travel locations of events.
	-	`drugs/` directory contains files to generate reports on drugs resisted.

---

### Reproducing the Database

The `Makefile` will re-build the project. Main commands are:

- `make aricles_db` 
- `make segments_db`
- `make events_db`
- `clean_mx12` - cleans only `.mx12` files
- `clean_all` - cleans all files including `.mx12` and `.rds` files 