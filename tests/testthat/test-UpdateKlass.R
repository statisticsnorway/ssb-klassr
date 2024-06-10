

test_that("UpdateKlass gir riktig resultat ved enkle endringer", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_2020_graph.rds"))
  
  endringer_kommunestruktur_enkle <- 
    get_klass_changes(131) %>% 
    filter(changeOccurred == "2020-01-01") %>% # 2020 hadde mange enkle endringer
    group_by(oldCode) %>% 
    filter(n() == 1) %>% # vi tester ikke delinger av koder
    group_by(newCode) %>% 
    filter(n() == 1) %>% # vi tester ikke sammenslåtte koder
    ungroup() %>% 
    select(oldCode, newCode, changeOccurred)
  
  omkodet <- 
    endringer_kommunestruktur_enkle %>% 
    mutate(oppdatert = UpdateKlass(codes = oldCode,
                                    classification = 131,
                                    output = "code",
                                    report = FALSE,
                                    graph = graph))
  
  feil <- omkodet %>% filter(newCode != oppdatert)
  
  expect_equal(nrow(feil), 0)
  
})

test_that("UpdateKlass gir riktig resultat ved sammenslåtte koder", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_1964_graph.rds"))
  
  endringer_kommunestruktur_sammenslåinger <-
    get_klass_changes(131) %>% 
    filter(changeOccurred == "1964-01-01") %>% 
    group_by(oldCode, changeOccurred) %>% 
    filter(n() == 1) %>% # vi tester ikke delinger av koder
    group_by(newCode) %>% 
    filter(n() > 1)
  
  omkodet <-
    endringer_kommunestruktur_sammenslåinger %>% 
    mutate(oppdatert = UpdateKlass(codes = oldCode,
                                    dates = as.Date(changeOccurred) - 1,
                                    classification = 131,
                                    graph = graph),
           oppdatert_ikkecomb = UpdateKlass(codes = oldCode,
                                             dates = as.Date(changeOccurred) - 1,
                                             classification = 131,
                                             graph = graph,
                                             combine = FALSE))
  
  omkodet %>% 
    filter(newCode != oppdatert) %>% 
    nrow() %>% 
    expect_equal(expected = 0)
  
  omkodet %>% 
    filter(!is.na(oppdatert_ikkecomb)) %>% 
    nrow() %>% 
    expect_equal(expected = 0)

})

test_that("UpdateKlass gir riktig resultat ved delte koder", {
  
  graph <- readRDS(test_path("fixtures", "klass_131_1964_graph.rds"))
  
  endringer_kommunestruktur_delinger <-
    get_klass_changes(131) %>%
    filter(changeOccurred == "1964-01-01") %>% # 1964 hadde klart flest delinger av koder
    group_by(oldCode) %>%
    filter(n() > 1) %>% 
    ungroup() %>%
    select(oldCode, newCode, changeOccurred)
  
  omkodet <-
    endringer_kommunestruktur_delinger %>% 
    mutate(oppdatert = UpdateKlass(codes = oldCode,
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
                 classification = 131,
                 date = NULL,
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

# 
# test_that("tilbakedatering av koder virker", {
#   
#   # 1214 Ølen byttet fylke og fikk nytt kommunenummer 1159 i 2002. Siden vi ber
#   # om å tilbakedatere 1159 til gjeldende kode i 2001 forventer vi å få 1214.
#   expect_equal(
#     object = UpdateKlass(x = "1159", 
#                          date_selected = "2001-01-01", 
#                          classification = 131,
#                          output = "result"),
#     expected = "1214"
#   )
#   
# })
# 
# test_that("koder gir riktig resultat ved sammenslåinger", {
#   
#   # Sammenslåing av koder der den nye koden er en av de gamle:
#   #   0218 Aker slo seg sammen med 0301 Oslo (allerede eksisterende) i 1948.
#   #   Siden vi ber om å oppdatere 0218 Aker forventer i å få 0301.
#   expect_equal(
#     object = UpdateKlass(x = "0218", 
#                          date_selected = "1949-01-01", 
#                          classification = 131,
#                          output = "result"),
#     expected = "0301"
#   )
#   
#   # Sammenslåing av koder der den nye koden ikke er en av de gamle:
#   #   0102 Sarpsborg og tre andre kommuner (0114, 0115 og 0130) slo seg sammen
#   #   til 0105 Sarpsborg i 1992. Siden vi ber om å få den oppdaterte koden for
#   #   0102 forventer vi å få 0105.
#   expect_equal(
#     object = UpdateKlass(x = "0102", 
#                          date_selected = "1993-01-01", 
#                          classification = 131,
#                          output = "result"),
#     expected = "0105"
#   )
#   
# })
# 
# test_that("sammenslåinger av koder blir rapportert i output", {
#   
#   # Sammenslåing av koder der den nye koden er en av de gamle:
#   #   0218 Aker slo seg sammen med 0301 Oslo (allerede eksisterende) i 1948.
#   #   Siden vi ber om å oppdatere 0218 Aker forventer i å få "combined".
#   expect_equal(
#     object = UpdateKlass(x = "0218", 
#                          date_selected = "1949-01-01", 
#                          classification = 131,
#                          output = "report"),
#     expected = "combined"
#   )
#   
#   # Sammenslåing av koder der den nye koden ikke er en av de gamle:
#   #   0102 Sarpsborg og tre andre kommuner (0114, 0115 og 0130) slo seg sammen
#   #   til 0105 Sarpsborg i 1992. Siden vi ber om å få den oppdaterte koden for
#   #   0102 forventer vi å få 0105.
#   expect_equal(
#     object = UpdateKlass(x = "0102", 
#                          date_selected = "1993-01-01", 
#                          classification = 131,
#                          output = "report"),
#     expected = "combined"
#   )
#   
# })
# 
# test_that("splittelser av koder blir rapportert i output", {
#   
#   # Deling av koder der en av de nye kodene er lik den gamle koden:
#   #   0218 Aker slo seg sammen med 0301 Oslo (allerede eksisterende) i 1948.
#   #   Siden vi ber om å oppdatere 0301 til tidspunktet før 1948 bør vi få
#   #   "split" i report. Per 31.05.2024 får vi "valid", siden 0301 faktisk fantes
#   #   i 1930.
#   
#   expect_equal(
#     object = UpdateKlass("0301", 
#                          date_selected = "1947-01-01", 
#                          classification = 131,
#                          output = "report"),
#     expected = "split"
#   )
#   
#   # Deling av koder der ingen av de nye kodene er den gamle koden: 
#   #   0102 Sarpsborg og tre andre kommuner (0114, 0115 og 0130) slo seg sammen
#   #   til 0105 Sar2psborg i 1992. Siden vi ber om å oppdatere 0105 til
#   #   tidspunktet før sammenslåing bør vi få "split" i report.
#   
#   expect_equal(
#     object = UpdateKlass("0105", 
#                          date_selected = "1991-01-01", 
#                          classification = 131,
#                          output = "report"),
#     expected = "split"
#   )
#   
# })
# 
# test_that("ugyldige koder blir rapportert i output", {
#   
#   
#   
# })