
#' Internal function to check date
#' @param d Date
#' @return NULL
CheckDate <- function(date){
  dcheck <- try(as.Date(date, format= "%Y-%m-%d"))
  if(class(date) == "try-error" | is.na(dcheck)) {
    stop("An incorrect date format was given. Please use format 'YYYY-mm-dd'.")
  }
}

#' Internal function to create URL address
#'
#' @param klass Classification number
#' @param correspond Target number for correspondence table
#' @param type String describing type. "vanlig" for normal classification and "kor" for correspondence. Default = "vanlig"
#' @param fratil True/False for whether a date interval is to be used. Default = False
#' @param date Date(s) for classification
#' @param output_level_coding Coding for output level
#' @param language_coding Coding for language
#'
#' @return String url adress

MakeUrl <- function(klass, correspond = NULL, type="vanlig", fratil=FALSE, date=NULL, output_level_coding=NULL, language_coding=NULL){
  if (type == "vanlig" & fratil == FALSE){
    coding <- paste("/codesAt?date=", date, sep="")
  }
  if (type == "vanlig" & fratil == TRUE){
    coding <- paste("/codes?from=", date[1], "&to=", date[2], sep = "")
  }
  if (type == "kor" & fratil == FALSE){
    coding <- paste("/correspondsAt?targetClassificationId=", MakeChar(correspond), "&date=", date, sep="")
  }
  if (type == "kor" & fratil == TRUE){
    coding <- paste("/corresponds?targetClassificationId=", MakeChar(correspond), "&from=", date[1],"&to=", date[2], sep="")
  }
  if (type == "change"){
    coding <- paste0("/changes?from=", date[1],"&to=", date[2])
  }

  # Sett sammen til URL
  url <- paste("http://data.ssb.no/api/klass/v1/classifications/",
               klass,
               coding,
               output_level_coding,
               language_coding,
               sep=""
  )
  return(url)
}

#' Get json file from Url - alternative version
#'
#' @param url String url address
#'
#' @return text in json format
GetUrl2 <- function(url){
  hent_klass <- httr::GET (url) ## henter innholdet fra klass med acceptheader json
  klass_text <- httr::content(hent_klass, "text") ## deserialisering med httr funksjonen content
  return(klass_text)
}

#' Fetch classification data
#' Fetch Statistics Norway classification data using API
#'
#' @param klass Number/string of the classification ID/number. (use Klass_list() to find this)
#' @param date String for the required date of the classification. Format must be "yyyy-mm-dd". For an inverval, provide two dates as a vector. If blank, will default to today's date.
#' @param correspond Number/string of the target correspondence (if a correspondence table is requested).
#' @param output_level Number/string specifying the requested heirachy level (optional).
#' @param language Two letter string for the requested language output. Default is bokmål ("nb"). Nynorsk ("nn") and English ("en") also available for some classificatio.)
#' @param output String varibale for the output type. Default is "normal" and only option currently prorammed
#'
#' @return The function returns a data frame of the specified classification/correspondence table. Output variables include:
#' code, parentCode, level, and name for standard lists. For correspondence tables variables include:
#' sourceCode, sourceName, targetCode and targetName. For time correspondence tables variables include:
#' oldCode, oldName, newCode and newName.
#' @export
#'
#' @examples
#' # Get classification for occupation classifications
#' GetKLASS(klass = "7")
#' # Get classification for occupation classifications in English
#' GetKLASS(klass = "7", language = "en")
#' # Get classifications for level 2 only
#' GetKLASS(klass = "7", output_level = 2)
#' # Get classifications for level 2 only valid on a specified date of between two dates
#' GetKLASS(klass = "7", output_level = 2, date = "2007-01-01")
#' GetKLASS(klass = "7", date = c("2007-01-01", "2018-01-01"))
#' # Get correspondence table between two occupation classifications
#' GetKLASS(klass = "145", correspond = "7", date = "2018-01-01")
#' #Get correspondence table between two dates for municipality
#' GetKLASS(klass = "131", correspond = TRUE, date = c("2015-01-01", "2019-01-01")
GetKLASS <- function(klass,
                      date = NULL,
                      correspond = NULL,
                      output_level = NULL,
                      language = "nb",
                      output_style = "normal"){

  type <- ifelse(is.null(correspond), "vanlig", "kor")
  type <- ifelse(isTRUE(correspond), "change", type)

  # sjekk klass er char
  klass <- MakeChar(klass)

  # dato sjekking
  if(is.null(date[1])) date <- Sys.Date()

  if (length(date) == 1 & !is.numeric(date)){
    fratil <- FALSE# om det er dato fra og til
    ver <- FALSE# om det skal finne en versjon (not implemented yet)
    CheckDate(date)
  }

  if (length(date) == 1 & is.numeric(date)){
    fratil <- FALSE
    ver <- TRUE
    # legg inn her hvordan å finne versjon
  }

  if (length(date) == 2){
    fratil <- TRUE
    ver <- FALSE
    CheckDate(date[1])
    CheckDate(date[2])
    if (difftime(as.Date(date[2]), as.Date(date[1])) < 0){
      date <- date[c(2,1)]
      dateswap <- TRUE
    } else {
      dateswap <- FALSE
    }
    date[2] <- as.character(as.Date(date[2], format= "%Y-%m-%d") + 1) # endre dette til å inkludere end dato punkt
  }
  if (length(date) > 2) stop("You have provided too many dates.")

# Check levels
  if (is.null(output_level)) {
    output_level_coding <- ""
  } else {
    output_level_coding = paste("&selectLevel=", output_level, sep="")
  }

  # Set spraak
  language_coding = paste("&language=", language, sep="")

  # kjor url og data ut
  url <- MakeUrl(klass, correspond, type, fratil, date, output_level_coding, language_coding)
  klass_text <- GetUrl2(url)

  # sjekk at det finnes
  targetswap <- FALSE
  if (type == "kor" & grepl("no correspondence table", klass_text)){
    targetswap <- TRUE
    url <- MakeUrl(klass=correspond, correspond = klass, type, fratil, date, output_level_coding, language_coding)
    klass_text <- GetUrl2(url)
    if (grepl("no correspondence table", klass_text)){
      stop("No correspondence table found between classes ", klass, " and ", correspond, " for the date ", date,
           "For a list of valid correspondence tables use the function CorrespondList()")
    }
  }

  if (grepl("not found", klass_text)){
    stop("No KLASS table was found for KLASS number ", klass,".
    Please try again with a diferent KLASS number.
    For a list of possible KLASS's use the function KlassList() or FamilyList()")
  }
  if (grepl("not published in language", klass_text)){
    stop("The classification requested was not found for language = ", gsub(".*=", "", language_coding))
    }

  if (type == "vanlig"){
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codes
    klass_data <- klass_data[, c("code", "parentCode", "level", "name")]
  }
  if (type == "kor"){
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$correspondenceItems
    if (targetswap){
      klass_data <- klass_data[, c("targetCode", "targetName", "sourceCode", "sourceName")]
    } else {
      klass_data <- klass_data[, c("sourceCode", "sourceName","targetCode", "targetName")]
    }
    names(klass_data) <- c("sourceCode", "sourceName", "targetCode", "targetName")
  }
  if (type == "change"){
    klass_data <- jsonlite::fromJSON(klass_text, flatten = TRUE)$codeChanges
    if (!is.data.frame(klass_data)) stop("No changes found for this classification.")
    if (dateswap){
      klass_data <- klass_data[, c("newCode", "newName", "oldCode", "oldName")]
    } else {
      klass_data <- klass_data[, c("oldCode", "oldName","newCode", "newName")]
    }
    names(klass_data) <- c("sourceCode", "sourceName", "targetCode", "targetName")
  }
  return (as.data.frame(klass_data))
}
