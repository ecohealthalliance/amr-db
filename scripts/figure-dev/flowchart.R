library(DiagrammeR)
library(here)


png(filename = here("figures/flowchart.png"))
flowchart <- grViz("digraph flowchart {

      # node definitions with substituted label text
      node [fontname = Ariel, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4 -> tab5;
      }

      [1]: 'Search relevant keywords in PubMed, Embase, and ProMED-mail (24,966 results) '
      [2]: 'Review abstracts or first lines of ProMED-mail reports (1,791 selected)'
      [3]: 'Full article review and coding in MAXQDA (294 articles in database)'
      [4]: 'Data cleaning and standardization in R (1,757 total events)'
      [5]: 'Database published and publically available'
      ")
flowchart
dev.off() #why not working?
