# Run tests as if the API cannot be reached -------------------------------

# helpers for symmetry with unmocked section below, default in load_all

devtools::load_all(helpers = TRUE)

fail_network <- function(...) {
  curl:::raise_libcurl_error(6, "Simulated network failure.")
  stop(
    structure(
      list(message = "Simulated network failure"),
      class = c("curl_error", "error", "condition")
    )
  )
}

with_mocked_bindings(
  devtools::test(),
  GET = fail_network,
  .package = "httr"
)

# Run tests normally ------------------------------------------------------

devtools::load_all(helpers = FALSE)

devtools::test()

# Run code interactively as if the API cannot be reached ------------------

with_mocked_bindings(
  browser(),
  GET = fail_network,
  .package = "httr"
)

# Test single lines of code as if the API cannot be reached ---------------

with_mocked_bindings(
  get_klass(131),
  GET = fail_network,
  .package = "httr"
)
