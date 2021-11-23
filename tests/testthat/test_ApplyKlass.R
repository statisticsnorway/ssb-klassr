
test_that("ApplyKlass returns correct names for numeric codes", {
  data(klassdata)
  kommune_names <- ApplyKlass(klassdata$kommune, 
                              klass = 131, date = "2015-01-01")
  expect_equal(kommune_names[1], "Sandefjord")
})


test_that("ApplyKlass returns correct names for character codes", {
  data(klassdata)
  sektor_names <- ApplyKlass(c("INNL", "UTL", "B_FIN"), 
                              klass = 39, date = "2019-01-01")
  expect_equal(sektor_names[2], "Utlandet")
})


test_that("ApplyKlass returns correct language", {
  data(klassdata)
  occ <- ApplyKlass(klassdata$occupation, 
                              klass = 7, 
                              language = "en",
                              date = "2015-01-01", format = F)
  expect_equal(occ[1], "Bartenders")
  occ <- ApplyKlass(klassdata$occupation, 
                    klass = 7, 
                    language = "nb",
                    date = "2015-01-01", format = F)
  expect_equal(occ[4], "Elektronikkingeniører")
})


test_that("ApplyKlass returns correctly for character codes",{
         sektor <- c("INNL", "UTL")
         sektor_apply <- ApplyKlass(sektor, 39, date = "2020-01-01")
         expect_equal(sektor_apply[2], "Utlandet")
})


test_that("Check levelCheck for numerical codes", {
  occ <- GetKlass(7, date = "2020-01-01")
  expect_equal(levelCheck("031", occ), 3)
  expect_equal(levelCheck(c("0210", "021"), occ), 3)
  sn <- GetKlass(6, date = "2020-01-01")
  expect_equal(levelCheck("03.1", sn), 3)
  expect_equal(levelCheck("A", sn), 1) #Character code but added in here
})


test_that("Check levelCheck for character codes", {
  sek <- GetKlass(39, date = "2020-01-01")
  expect_equal(levelCheck("INNL", sek), 1)
  expect_equal(levelCheck(c("INNL", "B_FIN", "B_FIN", NA), sek), 3)
})


test_that("ApplyKlass can return a variant classification",{
  dat <- c('000','101','102','103')
  new <- ApplyKlass(dat,
             klass = 91,
             variant = 847,
             output_level = 1,
             output = "code",
             date = "2020-01-01")
  expect_equal(new[1], "0")
  new <- ApplyKlass(dat,
                    klass = 91,
                    variant = 847,
                    output_level = 1,
                    date = "2020-01-01")
  expect_equal(new[2], "EU/EØS, USA, Canada, Australia og New Zealand")
})

test_that("ApplyKlass works for variant classification with Norwegian characters in the variant name",{
  dat <- c("05", "01")
  new <- ApplyKlass(dat,
                    klass = 6,
                    variant = 1616,
                    output_level = 1,
                    output = "name",
                    date = "2020-01-01")
  expect_equal(new[1], "05-09 Bergverksdrift og utvinning")
})
