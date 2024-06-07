
# Packages ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(igraph)

devtools::load_all()

# Making fixtures ---------------------------------------------------------

## klass_131_graph.R ------------------------------------------------------

klass_131_graph <- build_klass_graph(classification = 131)

saveRDS(klass_131_graph, 
        "tests/testthat/fixtures/klass_131_graph.rds")

## klass_131_2020_graph.R -------------------------------------------------

klass_131_2020_graph <- build_klass_graph(classification = 131, 
                                          date = "2020-01-01")

saveRDS(klass_131_2020_graph, 
        "tests/testthat/fixtures/klass_131_2020_graph.rds")

## klass_131_1964_graph.R -------------------------------------------------

## Endringen i kommunestruktur i 1964 er det enkeltåret som både har flest
## splittelser og sammenslåinger av kommuner.

klass_131_1964_graph <- build_klass_graph(classification = 131,
                                          date = "1964-01-01")

saveRDS(klass_131_1964_graph, 
        "tests/testthat/fixtures/klass_131_1964_graph.rds")
