
test_that("UpdateKlass gir riktig resultat ved enkle endringer", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_2020_graph.rds"))
  
  endringer_kommunestruktur_enkle <- 
    get_klass_changes(131) %>% 
    dplyr::filter(changeOccurred == "2020-01-01") %>% # 2020 hadde mange enkle endringer
    dplyr::group_by(oldCode) %>% 
    dplyr::filter(dplyr::n() == 1) %>% # vi tester ikke delinger av koder
    dplyr::group_by(newCode) %>% 
    dplyr::filter(dplyr::n() == 1) %>% # vi tester ikke sammenslåtte koder
    dplyr::ungroup() %>% 
    dplyr::select(oldCode, newCode, changeOccurred)
  
  omkodet <- 
    endringer_kommunestruktur_enkle %>% 
    dplyr::mutate(oppdatert = UpdateKlass(codes = oldCode,
                                          classification = 131,
                                          output = "code",
                                          report = FALSE,
                                          graph = graph))
  
  feil <- omkodet %>% dplyr::filter(newCode != oppdatert)
  
  expect_equal(nrow(feil), 0)
  
})

test_that("UpdateKlass gir riktig resultat ved sammenslåtte koder", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_1964_graph.rds"))
  
  endringer_kommunestruktur_sammenslåinger <-
    get_klass_changes(131) %>% 
    dplyr::filter(changeOccurred == "1964-01-01") %>% 
    dplyr::group_by(oldCode, changeOccurred) %>% 
    dplyr::filter(dplyr::n() == 1) %>% # vi tester ikke delinger av koder
    dplyr::group_by(newCode) %>% 
    dplyr::filter(dplyr::n() > 1)
  
  omkodet <-
    endringer_kommunestruktur_sammenslåinger %>% 
    dplyr::mutate(oppdatert = UpdateKlass(codes = oldCode,
                                          dates = as.Date(changeOccurred) - 1,
                                          classification = 131,
                                          graph = graph),
                  oppdatert_ikkecomb = UpdateKlass(codes = oldCode,
                                                   dates = as.Date(changeOccurred) - 1,
                                                   classification = 131,
                                                   graph = graph,
                                                   combine = FALSE))
  
  omkodet %>% 
    dplyr::filter(newCode != oppdatert) %>% 
    nrow() %>% 
    expect_equal(expected = 0)
  
  omkodet %>% 
    dplyr::filter(!is.na(oppdatert_ikkecomb)) %>% 
    nrow() %>% 
    expect_equal(expected = 0)

})

test_that("UpdateKlass gir riktig resultat ved delte koder", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_1964_graph.rds"))
  
  endringer_kommunestruktur_delinger <-
    get_klass_changes(131) %>%
    dplyr::filter(changeOccurred == "1964-01-01") %>% # 1964 hadde klart flest delinger av koder
    dplyr::group_by(oldCode) %>%
    dplyr::filter(dplyr::n() > 1) %>% 
    dplyr::ungroup() %>%
    dplyr::select(oldCode, newCode, changeOccurred)
  
  omkodet <-
    endringer_kommunestruktur_delinger %>% 
    dplyr::mutate(oppdatert = UpdateKlass(codes = oldCode,
                                          dates = as.Date(changeOccurred) - 1,
                                          classification = 131,
                                          graph = graph))
  
  expect_true(all(is.na(omkodet$oppdatert)))
  
})


test_that("UpdateKlass gir forventet format på output", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_graph.rds"))
  
  update_helper <- function(output, report) {
    
    UpdateKlass(codes = "0301",
                 dates = "1838-01-01",
                 output = output,
                 report = report,
                 graph = graph)
    
  }
  
  expect_type(update_helper(output = "code", report = FALSE), "character")
  expect_type(update_helper(output = "code", report = TRUE), "list")
  expect_type(update_helper(output = TRUE,   report = FALSE), "list")
  expect_type(update_helper(output = "code", report = TRUE)[[1]], "character")
  expect_s3_class(update_helper(output = c("code", "name"), report = TRUE)[[1]], "data.frame")
  expect_type(update_helper(output = c("code", "name"), report = TRUE), "list")
  expect_type(update_helper(output = TRUE, report = FALSE), "list")
  expect_s3_class(update_helper(output = TRUE, report = FALSE)[[1]], "data.frame")
  expect_equal(nrow(update_helper(output = TRUE, report = FALSE)[[1]]), 1)
  expect_equal(nrow(update_helper(output = c("code", "name"), report = FALSE)[[1]]), 1)
  expect_equal(length(update_helper(output = "code", report = FALSE)), 1)
  
})
