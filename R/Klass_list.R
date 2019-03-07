
#' Get target ID numbers from Url
#'
#' @param x Url address
#' @return Number
GetNums <- function(x){
  x <- as.character(x)
  gsub(".*/", "", x)
}

#' Get json file from Url
#'
#' @param url String url address
#'
#' @return text in json format
GetUrl <- function(url){
  hent_klass <- httr::GET(url) ## henter innholdet fra klass med acceptheader json
  klass_text <- httr::content(hent_klass, "text") ## deserialisering med httr funksjonen content
  klass_data <- jsonlite::fromJSON(klass_text)
  return(klass_data)
}



#' Classification list
#' Get a full list of all classifications and codelists
#'
#' @param codelists True/False for whether to include codelists. Default = FALSE
#'
#' @return A data frame containing a full list of classifications. The data frame includes the classification name, number, family and type.
#' @export
#'
#' @examples
#' K <- KlassList(codelists = TRUE)
#' View(K)
KlassList <- function(codelists = FALSE){
  fams <- FamilyList()$family_nr
  Klist <- data.frame(klass_name = NA, klass_nr = NA, klass_family = NA, klass_type = NA)
  code <- ifelse(codelists, "?includeCodelists=true", "")
  for (i in fams){
    url <- paste('http://data.ssb.no/api/klass/v1/classificationfamilies/', i, code, sep ="")
    dt <- data.frame(GetUrl(url)$classifications)
    nums <- as.vector(sapply(dt$X_links[, 1], GetNums))
    dt2 <- data.frame(klass_name = dt$name, klass_nr = nums, klass_family = i, klass_type = dt$classificationType)
    Klist <- rbind(Klist, dt2)
    }
  return(Klist[-1, ])
}




#' Classification family list
#' Print a list of all families and the number of classifications in each
#'
#' @param family Input family ID number to get a list of classifications in that family
#' @param codelists True/False for whether to include codelists. Default = FALSE
#'
#' @return dataset containing a list of families
#' @export
#'
#' @examples
#' FamilyList()
#' FamilyList(codelists=TRUE)
#' FamilyList(family = 1)
#' FamilyList(family = 1, codelists = TRUE)
FamilyList <- function(family=NULL, codelists = FALSE){
  code <- ifelse(codelists, "?includeCodelists=true", "")
  if (is.null(family)){
    url <- paste('http://data.ssb.no/api/klass/v1/classificationfamilies', code, sep="")
    dt <- data.frame(GetUrl(url)$'_embedded'$classificationFamilies)
    nums <- as.vector(sapply(dt$X_links$self$href, FUN = GetNums))
    dt2 <- data.frame(family_name = dt$name, family_nr = nums,
                      number_of_classifications = dt$numberOfClassifications)
  }
  if (!is.null(family)){
    family <- MakeChar(family)
    url <- paste('http://data.ssb.no/api/klass/v1/classificationfamilies/', family, code, sep ="")
    dt <- data.frame(GetUrl(url)$classifications)
    nums <- as.vector(sapply(dt$X_links[, 1], GetNums))
    dt2 <- data.frame(klass_name = dt$name, klass_nr = nums)
    row.names(dt2) <- NULL
  }
  return(dt2)
}



#' Search Klass
#'
#' @param query String with key word to search for
#' @param codelists True/False for whether to include codelists. Default = FALSE
#' @param size The number of results to show. Default = 20.
#'
#' @return Data frame of possible classifications that match the query
#' @export
#'
#' @examples
#' SearchKlass("yrke")
#' SearchKlass("yrke", codelists = TRUE)
#' SearchKlass("yrke", codelists = TRUE, size = 50)
#' SearchKlass("*fold")
SearchKlass <- function(query, codelists = FALSE, size = 20){
  query <- as.character(query)
  code <- ifelse(codelists, "&includeCodelists=true", "")
  url <- paste('http://data.ssb.no/api/klass/v1/classifications/search?query=', query, code, "&size=", size, sep ="")
  dt <- data.frame(GetUrl(url)$'_embedded'$searchResults)
  nums <- as.vector(sapply(dt$X_links$self$href, GetNums))
  dt2 <- data.frame(klass_name = dt$name, klass_nr = nums)
  row.names(dt2) <- NULL
  return(dt2)
}


#' Get version number of a class given a date
#'
#' @param klass Classification number
#' @param date Date for version to be valid
#' @param family Family ID number if a list of version number for all classes is desired
#' @param klassNr True/False for whether to output classification numbers. Default = FALSE
#'
#' @return Number, vector or data frame with version numbers and calssification numbers if specified.
#' @export
#'
#' @examples
#' GetVersion(7)
#' GetVersion(7, "2010-01-01")
#' GetVersion(family = 1)
#' GetVersion(family = 1, klassNr = TRUE)
GetVersion <- function(klass=NULL,  date=NULL, family = NULL, klassNr=FALSE){
  if(is.null(date)) date <- Sys.Date()
  if(is.null(family)){
    if (klassNr == TRUE) stop("To output Klass number from this function you need to input a family number")
    klass <- MakeChar(klass)
    url <- paste("http://data.ssb.no/api/klass/v1/classifications/", klass, sep="")
    df <- as.data.frame(GetUrl(url)$versions)
    df$validTo[is.na(df$validTo)] <- as.character(Sys.Date() + 1)
    for (i in 1:nrow(df)){
      cond <- as.Date(date) >= as.Date(df$validFrom[i]) & as.Date(date) < as.Date(df$validTo[i])
      if (cond) {
        vers <- GetNums(df$`_links`$self$href[i])
      }
    }
  } else {
    family = MakeChar(family)
    fam <- FamilyList(family, codelists = TRUE)
    vers <- NULL
    klass_nr <- NULL
    for (i in fam$klass_nr){
      url <- paste("http://data.ssb.no/api/klass/v1/classifications/", i, sep="")
      df <- as.data.frame(GetUrl(url)$versions)
      if(is.null(df$validTo)) df$validTo <- as.character(Sys.Date() + 1)
      df$validTo[is.na(df$validTo)] <- as.character(Sys.Date() + 1)
      for (j in 1:nrow(df)){
        cond <- as.Date(date) >= as.Date(df$validFrom[j]) & as.Date(date) < as.Date(df$validTo[j])
        if (cond) {
          vers <- c(vers, GetNums(df$`_links`$self$href[j]))
          klass_nr <- c(klass_nr, i)
        }
      }
    }
    if(klassNr == TRUE){
      vers <- data.frame(vers, klass_nr)
    }
  }
return(vers)
}




#' Get the name of a classification version
#'
#' @param version Version number
#'
#' @return string or vector of strings with name of version
#' @export
#'
#' @examples
#' GetName("33")
#' GetName(GetVersion(family = 1))
GetName <- function(version){
  version <- MakeChar(version)
  vernames = NULL
  for (i in version){
    url <- paste('http://data.ssb.no/api/klass/v1/versions/', i, sep ="")
    vernames <- c(vernames, GetUrl(url)$name)
  }
  return(vernames)
}


#' Identify corresponding family from a classification number
#'
#' @param klass Classification number
#'
#' @return Family number
#' @export
#'
#' @examples
#' GetFamily(klass = 7)
GetFamily <- function(klass){
  klass <- MakeChar(klass)
  K <- KlassList(codelists = TRUE)
  m <- match(klass, K$klass_nr)
  return(K$klass_family[m])
 }





#' Correspondence list
#' Print a list of correspondence tables for a given klass with source and target IDs
#'
#' @param klass Classification number
#' @param date Date for classification (format = "YYYY-mm-dd"). Default is current date
#'
#' @return Data frame with list of corrsepondence tables, source ID and target ID.
#' @export
#'
#' @examples
#' CorrespondList("7")
#' CorrespondList("131")
#' CorrespondList("131", date="2016-01-01") #ta litt tid for å finne og koble alle

CorrespondList <- function(klass, date = NULL){
  cat("Finding correspondence tables ...")
  klass <- MakeChar(klass)
  if (is.null(date)) {
    date <- Sys.Date()
  }
  vers <- GetVersion(klass = klass, date = date)
  url <- paste('http://data.ssb.no/api/klass/v1/versions/', vers, sep ="")
  df <- GetUrl(url)
  versName <- df$name
  dt <- data.frame(df$correspondenceTables)
  fam <- GetFamily(klass = klass)
  versValid <- GetVersion(family=fam, date = date, klassNr = TRUE)
  vers_names <- GetName(versValid$vers)
  source_klass <- NULL
  target_klass <- NULL

  for (i in 1:nrow(dt)){
    m <- match(versName, c(dt$source[i], dt$target[i]))
    findName <- ifelse(m == 2, dt$source[i], dt$target[i])
    m2 <- match(findName, vers_names)
    newdate <- date
    counter = 0
    while(is.na(m2) & counter < 10){ # hvis versjonen ikke ble funnet på date søkes det tilbake i tid
      newdate <- as.character(as.Date(newdate) - 60)
      versValidold <- GetVersion(family=fam, date = newdate, klassNr = TRUE)
      vers_names <- GetName(versValidold$vers)
      m2 <- match(findName, vers_names)
      counter = counter + 1
      cat('.')
    }
    sourceTarget <- ifelse(is.na(m2), NA, as.character(versValid[m2, "klass_nr"]))
    source_klass[i] <- ifelse(m == 1, klass, sourceTarget)
    target_klass[i] <- ifelse(m == 1, sourceTarget, klass)
  }
  correspondence_table <- sapply(dt$X_links$self$href, GetNums)
  dt2 <- data.frame(correspondence_name=dt$name,
                    source_klass = source_klass,
                    target_klass = target_klass,
                    correspondence_table, stringsAsFactors=FALSE)
  row.names(dt2) <- NULL
  dt2$target_klass[dt2$source_klass == dt2$target_klass] <- NA #dropping target for tables within version

  if (any(is.na(dt2$target_klass))) warning("\n\n There are correspondence tables within classification ",klass," (between different time points). Use the changes = TRUE option in the KLASS and GetKLASS functions to get these\n ")
  return(dt2)
}

