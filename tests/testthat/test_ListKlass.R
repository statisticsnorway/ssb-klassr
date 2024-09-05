# Test searches for classifications

# library(klassR)

head(ListKlass(codelists = TRUE))

test_that("ListKlass returns a list", {
  klass_table <- ListKlass()
  expect_gt(nrow(klass_table), 1)

  codelist_table <- ListKlass(codelists = TRUE)
  expect_gt(nrow(codelist_table), nrow(klass_table))
})


test_that("SearchKlass returns searches", {
  komm_search <- SearchKlass("kommune")
  expect_in("Standard for kommuneinndeling", komm_search$klass_name)
})
