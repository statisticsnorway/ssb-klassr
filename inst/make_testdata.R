# Packages ----------------------------------------------------------------

# Making fixtures ---------------------------------------------------------

## klass_131_graph.RData --------------------------------------------------
library(klassR)
klass_131_graph <- klass_graph(classification = 131, date = "2024-12-19")

save(klass_131_graph, file = "./data/klass_131_graph.RData", compress = TRUE)

## klass_131_2020_graph.RData ---------------------------------------------

klass_131_2020_graph <- klass_graph(
  classification = 131,
  date = "2020-01-01"
)

save(
  klass_131_2020_graph,
  file = "data/klass_131_2020_graph.RData",
  compress = TRUE
)

## klass_131_1964_graph.RData ---------------------------------------------

## Endringen i kommunestruktur i 1964 er det enkeltåret som både har flest
## splittelser og sammenslåinger av kommuner.

klass_131_1964_graph <- klass_graph(
  classification = 131,
  date = "1964-01-01"
)

save(
  klass_131_1964_graph,
  file = "data/klass_131_1964_graph.RData",
  compress = TRUE
)

## api_endringer_2019.RData -----------------------------------------------

api_endringer_2019 <- jsonlite::fromJSON(
  klassR:::GetUrl2(paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    131,
    "/changes?from=2019-01-01"
  )),
  flatten = TRUE
)[["codeChanges"]]

save(
  api_endringer_2019,
  file = "data/api_endringer_2019.RData",
  compress = TRUE
)

## api_endringer_1963.RData -----------------------------------------------

api_endringer_1963 <- jsonlite::fromJSON(
  klassR:::GetUrl2(paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    131,
    "/changes?from=1963-01-01"
  )),
  flatten = TRUE
)[["codeChanges"]]

save(
  api_endringer_1963,
  file = "data/api_endringer_1963.RData",
  compress = TRUE
)

# Compress data -----------------------------------------------------------
tools::resaveRdaFiles("./data", compress = "xz")
