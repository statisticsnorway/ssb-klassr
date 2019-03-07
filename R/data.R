#' Testdata for KLASS package
#'
#' A dataset containing variables for testing of Statistics Norways classification API with the KLASS package. Some observations are missing or incorrect for testing and demonstrations.
#'
#' @format A data frame containing 100 rows and 7 variables:
#' \describe{
#' \item{ID}{Identification number}
#' \item{sex}{1/2 variable for sex}
#' \item{education}{3- number for education standard ISCED97 (level and subject area) NUS (klass = 66) 20165.01.01}
#' \item{kommune}{4-digit code for Norwegian municipality (klass = 131). Based on 2015.01.01}
#' \item{kommune2}{Numeric variable for Norwegian municipality with dropped leading zero's for testing (klass = 131). Based on 2015.01.01}
#' \item{nace5}{5-digit code for industry (NACE). Based on 01.01.2015 standard industry codes (klass = 7)}
#' \item{occupation}{4-digit occupation codes using standard for ISCED97 (level and subject area) NUS (klass = 66) 20165.01.01}
#' }
"klassdata"
