# tests/testthat/helper-api.R

# This helper is intended to ensure that tests do not fail or throw errors in
# the case that the Klass API is unavailable. We wrap the low level function
# check_connect, which is responsible for making GET calls, such that errors in
# curl leads to a test skip.

skip_if_curl_error <- function(f) {
  force(f)

  function(...) {
    tryCatch(
      f(...),
      error = function(e) {
        if (
          rlang::cnd_inherits(e, "curl_error") &&
            isTRUE(
              getOption("klassR.skip_api_failures", TRUE)
            )
        ) {
          testthat::skip(conditionMessage(e))
        }

        stop(e)
      }
    )
  }
}

assignInNamespace(
  "check_connect",
  skip_if_curl_error(getFromNamespace("check_connect", "klassR")),
  ns = "klassR"
)
