
.PHONY: segments_excels articles_db segments_db spec_coded_excel clean all_data

SEGMENTS_EXCELS := $(wildcard data/coded_segments/*.xlsx)
ARTICLE_INDEX_CSV := $(wildcard data-raw/art_index_csvs/*.csv)
CODED_MEX := $(wildcard data-raw/coded_text_mex/*.mex)
ARTICLES_DB := data/articles_db.RData
SEGMENTS_DB := data/segments_db.RData

segments_excels : $(wildcard data/coded_segments/*.xlsx)
all_data: $(ARTICLES_DB) $(SEGMENTS_DB)
articles_db: $(ARTICLES_DB)
segments_db: $(SEGMENTS_DB)


$(ARTICLES_DB): scripts/02_index_articles.R $(ARTICLE_INDEX_CSV) $(CODED_MEX)
	Rscript $<

$(SEGMENTS_DB): scripts/03_clean_segments.R $(SEGMENTS_EXCELS) $(ARTICLES_DB)
	Rscript $<

data/coded_segments/%.xlsx: data-raw/coded_text_mex/%.mex
	osascript 01_export_segs_single_mex.scpt $^


clean: 
	rm -r data/*.RData
	rm -r data-raw/coded_text_mex/*.mx12