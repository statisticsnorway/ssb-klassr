# Code for tests for the function GetKlass

test_that("GetKlass returns a classification", {
  class_data <- GetKlass(klass = 131, date = "2015-01-01")
  expect_equal(class_data$code[1], "0101")
})


test_that("GetKlass returns a classification with Norwegian letters", {
  class_data <- GetKlass(klass = 131, date = "2015-01-01")
  expect_equal(class_data$name[8], "Rømskog")
})


test_that("GetKlass returns a correspondence table", {
  class_data <- GetKlass(klass = 104, correspond = 131,
                         date = "2015-01-01")
  expect_equal(class_data$sourceCode[1], "01")
})


test_that("GetKlass returns a correspondence table in both directions", {
  class_data1 <- GetKlass(klass = 104, correspond = 131,
                         date = "2020-02-01")
  class_data2 <- GetKlass(klass = 131, correspond = 104,
                          date = "2020-02-01")  
  expect_equal(class_data1$sourceCode[1], "03")
  expect_equal(class_data2$sourceCode[1], "0301")
})


test_that("GetKlass returns a valid variant", {
  variant_data <- GetKlass(klass = 6, variant = 1616, date = "2021-01-02")
  expect_equal(variant_data$name[2], '01-03 Jordbruk, skogbruk og fiske')
  
})

