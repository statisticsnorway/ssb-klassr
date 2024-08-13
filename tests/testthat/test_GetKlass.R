# Code for tests for the function GetKlass

test_that("GetKlass returns a classification", {
  class_data <- GetKlass(klass = 131, date = "2015-01-01")
  expect_equal(class_data$code[1], "0101")
})


test_that("GetKlass returns a classification with Norwegian letters", {
  class_data <- GetKlass(klass = 131, date = "2015-01-01")
  expect_equal(class_data$name[8], "Rømskog")
})

test_that("GetKlass returns no error when no date given", {
  class_data <- GetKlass(klass = 131)
})

test_that("GetKlass returns changes when two dates given", {
  class_data1 <- GetKlass(klass = 131, date = c("2022-01-01", "2023-01-01"))
  class_data2 <- GetKlass(klass = 131, date = c("2023-01-01", "2022-01-01"))
  expect_equal(nrow(class_data1), nrow(class_data2))
})

test_that("GetKlass returns correct output level", {
  class_data <- GetKlass(klass = 7, date = "2024-01-01",
                         output_level = 1)
  expect_equal(nrow(class_data), 10)
  expect_equal(class_data$name[2], "Ledere")
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
  expect_equal(variant_data$name[2], 'Jordbruk, skogbruk og fiske')
})


test_that("GetKlass returns notes when requested", {
  class_data <- GetKlass(277, notes = TRUE, date = "2023-01-12")
  note <- class_data$notes[class_data$code == "FGK8"]
  expect_equal(note, 'Grupperingen omfatter funksjonene 202, 215, 222 og 223 innen grunnskole.')
})


test_that("GetKlass returns a future classification", {
  expect_message(classdata <- GetKlass(104, date = "2050-01-01"))
  expect_equal(classdata$name[6], "Akershus")
})


test_that("GetKlass returns a correspondence table using ID", {
  expect_message(classdata <- GetKlass(correspondID = 1111, date = "2023-01-12"))
  expect_equal(classdata$targetName[1], "Oslo")
})

test_that("GetKlass returns valid dates for a date range", {
  class_data <- GetKlass(131, date = c("2020-01-01","2024-01-02"))
  expect_equal(class_data$validFromInRequestedRange[1], "2020-01-01")
  
  variant_data <- GetKlass(klass = 6, variant = 1616, date = c("2020-01-01","2024-01-02"))
  expect_equal(variant_data$validFromInRequestedRange[1], "2020-01-01")
  
})
