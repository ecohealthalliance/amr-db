
.PHONY: segments_excels articles_db segments_db spec_coded_excel clean all_data

SEGMENTS_EXCELS := $(wildcard data/coded_segments/*.xlsx)
ARTICLE_INDEX_CSV := $(wildcard data-raw/art_index_csvs/*.csv)
CODED_MEX := $(wildcard data-raw/coded_text_mex/*.mex)
ARTICLES_DB := data/articles_db.RData
SEGMENTS_DB := data/segments_db.RData

segments_excels : $(SEGMENTS_EXCELS)
all_data: $(ARTICLES_DB) $(SEGMENTS_DB)
articles_db: $(ARTICLES_DB)
segments_db: $(SEGMENTS_DB)


data/coded_segments/%.xlsx: data-raw/coded_text_mex/%.mex
	osascript scripts/01_export_segs_single_mex.scpt $^


data/articles_db.RData: scripts/02_index_articles.R $(ARTICLE_INDEX_CSV) $(CODED_MEX)
	Rscript $<

$(SEGMENTS_DB): scripts/03_clean_segments.R $(SEGMENTS_EXCELS) $(ARTICLES_DB)
	Rscript $<



clean: 
	rm -r data-raw/coded_text_mex/*.mx12
	rm -r data/*.RData
	