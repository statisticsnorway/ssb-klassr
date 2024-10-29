# Packages ----------------------------------------------------------------

# Making fixtures ---------------------------------------------------------

## klass_131_graph.R ------------------------------------------------------
library(klassR)
klass_131_graph <- KlassGraph(classification = 131, date = "2024-10-29")

save(klass_131_graph,
  file ="./data/klass_131_graph.RData",
  compress = TRUE
)

## klass_131_2020_graph.R -------------------------------------------------

klass_131_2020_graph <- KlassGraph(
  classification = 131,
  date = "2020-01-01"
)

save(
  klass_131_2020_graph,
  file = "data/klass_131_2020_graph.RData",
  compress = TRUE
)

## klass_131_1964_graph.R -------------------------------------------------

## Endringen i kommunestruktur i 1964 er det enkeltåret som både har flest
## splittelser og sammenslåinger av kommuner.

klass_131_1964_graph <- KlassGraph(
  classification = 131,
  date = "1964-01-01"
)

save(
  klass_131_1964_graph,
  file ="data/klass_131_1964_graph.RData",
  compress = TRUE
)

# Compress data
tools::resaveRdaFiles("./data", compress = "xz")
