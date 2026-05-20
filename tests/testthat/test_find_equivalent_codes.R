test_that("find_equivalent_codes returnerer en velformet data.frame", {
  data("klass_131_graph")

  dates <- c("2019-01-01", "2025-01-01")

  resultat <- find_equivalent_codes(
    classification = 131,
    dates = dates,
    graph = klass_131_graph
  )

  expect_s3_class(resultat, "data.frame")
  expect_shape(resultat, ncol = 6)
  expect_contains(
    colnames(resultat),
    c("date", "code", "name", "validFrom", "validTo", "label")
  )

  # slår ut dersom noen av variablene spesifisert under har en NA-verdi
  expect_true(
    all(vapply(
      resultat[c("date", "code", "name", "validFrom", "label")],
      function(col) !all(is.na(col)),
      FUN.VALUE = logical(1)
    ))
  )

  # slår ut dersom noen av datoene mangler ...
  expect_contains(resultat$date, format(as.Date(dates), "%Y"))

  # ... eller resultatet har datoer vi ikke har angitt
  expect_contains(format(as.Date(dates), "%Y"), resultat$date)
})
