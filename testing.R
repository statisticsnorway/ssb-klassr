devtools::load_all()

## Funksjon for testing ---------------------------------------------------

download_some_changes <- function(classification, from, to) {
  changes_url <- paste0(
    "https://data.ssb.no/api/klass/v1/classifications/",
    classification,
    "/changes?from=",
    from,
    "&to=",
    to
  )

  api_endringer <- jsonlite::fromJSON(
    klassR:::GetUrl2(changes_url),
    flatten = TRUE
  )[[
    "codeChanges"
  ]]

  return(api_endringer)
}

download_all_changes <- function(classification, by = 100) {
  dates <- c(
    seq(from = as.Date("0000-01-01"), to = Sys.Date(), by = "100 year"),
    Sys.Date()
  )

  res <- mapply(
    FUN = download_some_changes,
    from = dates[-length(dates)],
    to = dates[-1],
    classification = 131,
    USE.NAMES = FALSE
  )

  return(do.call(rbind, res))
}

## Test av bitvis nedlastning ---------------------------------------------

results <- map(
  .x = c(10, 25, 50, 100, 200, 500),
  .f = function(by) {
    t1 <- Sys.time()

    res <- download_all_changes(classification = 131, by = by)

    print(Sys.time() - t1)

    return(bind_rows(res))
  }
)

## Test av nedlastning av hele tabellen -----------------------------------

request <- "https://data.ssb.no/api/klass/v1/classifications/131/changes?from=0-01-01&to=100-01-01"

t1 <- Sys.time()

result <- jsonlite::fromJSON(
  klassR:::GetUrl2(request),
  flatten = TRUE
)[["codeChanges"]]

print(Sys.time() - t1)

## Funksjon uten tidyverse ------------------------------------------------

res <- download_all_changes(131)
