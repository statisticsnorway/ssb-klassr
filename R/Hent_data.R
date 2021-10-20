
#' Internal function to check date
#' @param date Date
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
#' @param variant_name The name of the variant of the classification
#' @param type String describing type. "vanlig" for normal classification and "kor" for correspondence. Default = "vanlig"
#' @param fratil True/False for whether a date interval is to be used. Default = False
#' @param date Date(s) for classification
#' @param output_level_coding Coding for output level
#' @param language_coding Coding for language
#'
#' @return String url adress

MakeUrl <- function(klass, correspond = NULL, variant_name = NULL,
                    type="vanlig", 
                    fratil=FALSE, date=NULL, 
                    output_level_coding=NULL, 
                    language_coding=NULL){
  
  # Standard classification/codelist
  if (type == "vanlig" & fratil == TRUE){
      coding <- paste0("/codes?from=", date[1], "&to=", date[2])
  }
  if (type == "vanlig" & fratil == FALSE) {
      coding <- paste0("/codesAt?date=", date)
  }
  
    # For correspondence tables
  if (type == "kor" & fratil == TRUE){
      coding <- paste("/corresponds?targetClassificationId=", MakeChar(correspond), "&from=", date[1],"&to=", date[2], sep="")
    }
  if (type == "kor" & fratil == FALSE){
      coding <- paste("/correspondsAt?targetClassificationId=", MakeChar(correspond), "&date=", date, sep="")
  }
  
    # For time changes
  if (type == "change"){
    coding <- paste0("/changes?from=", date[1],"&to=", date[2])
  }
  
    # For fetching a variant
  if (type == "variant" & fratil == TRUE){
      coding <- paste0("/variant?variantName=", variant_name, "&from=", date[1],"&to=", date[2])
  }
  if (type == "variant" & fratil == FALSE){
      coding <- paste0("/variantAt?variantName=", variant_name, "&date=", date)
  }
  
  # Paste together to an URL
  url <- paste("http://data.ssb.no/api/klass/v1/classifications/",
               klass,
               coding,
               output_level_coding,
               language_coding,
               sep=""
  )
  return(url)
}
#' Check connection
#' Function to check that a connection to data.ssb.no is able to be established
#' @param url String url address for connection to check
#' @return Nothing is returned but a error or warning message is return if no connection is available
check_connect <- function(url){
  tryget <- tryCatch(
    httr::GET(url = url),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  if (class(tryget) != "response"){
    message(tryget)
    return(invisible(NULL))
  } else if (httr::http_error(tryget$status_code)){
    message(paste("Connection failed with error code", tryget$status_code))
    return(invisible(NULL))
  }
  tryget
}

#' Stop quietly function
#' Stop from a function without an error. Used for stopping when no internet
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


#' Get variant name
#' Internal function for fetching the variant name based on the number
#' @param variant The variant number
get_variant_name <- function(variant){
  url <- paste0("http://data.ssb.no/api/klass/v1/variants/", variant)
  #variant_url <- httr::GET(url)
  variant_url <- check_connect(url)
  if (is.null(variant_url)) stop_quietly()
  variant_text <- httr::content(variant_url, "text")
  if (grepl("variant not found", variant_text)){
    stop("The variant ", variant, " was not found.")
  }
  variant_name_full <- jsonlite::fromJSON(variant_text, flatten = TRUE)$name
  name_sm <- strsplit(variant_name_full, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = T)[[1]][1]
  gsub(" ", "%20", name_sm)
}



#' Get json file from Url - alternative version
#'
#' @param url String url address
#' @param check Logical parameter on whether to check if the url exists
#'
#' @return text in json format
GetUrl2 <- function(url, check = TRUE){
  # henter innholdet fra klass med acceptheader json
  if (check){
    hent_klass <- check_connect(url)
  } else {
    hent_klass <- httr::GET(url = url)
  }
  if (is.null(hent_klass)){
    return(invisible(NULL))
  } 
  klass_text <- httr::content(hent_klass, "text") ## deserialisering med httr funksjonen content
  return(klass_text)
}


#' Fetch classification data
#' Fetch Statistics Norway classification data using API
#'
#' @param klass Number/string of the classification ID/number. (use Klass_list() to find this)
#' @param date String for the required date of the classification. Format must be "yyyy-mm-dd". For an inverval, provide two dates as a vector. If blank, will default to today's date.
#' @param correspond Number/string of the target correspondence (if a correspondence table is requested).
#' @param variant The classification variant to fetch (if a variant is wanted).
#' @param output_level Number/string specifying the requested hierarchy level (optional).
#' @param language Two letter string for the requested language output. Default is bokmål ("nb"). Nynorsk ("nn") and English ("en") also available for some classification.)
#' @param output_style String varibale for the output type. Default is "normal". Specify "wide" for a wide formatted table output.
#'
#' @return The function returns a data frame of the specified classification/correspondence table. Output variables include:
#' code, parentCode, level, and name for standard lists. For correspondence tables variables include:
#' sourceCode, sourceName, targetCode and targetName. For time correspondence tables variables include:
#' oldCode, oldName, newCode and newName. For "wide" output, code and name with level suffixes is specified.
#' @export
#'
#' @examples
#' # Get classification for occupation classifications
#' head(GetKlass(klass = "7"))
#' # Get classification for occupation classifications in English
#' head(GetKlass(klass = "7", language = "en"))
GetKlass <- function(klass,
                      date = NULL,
                      correspond = NULL,
                      variant = NULL,
                      output_level = NULL,
                      language = "nb",
                      output_style = "normal"){
  
  # create type of klassification for using later
  type <- ifelse(is.null(correspond), "vanlig", "kor")
  type <- ifelse(isTRUE(correspond), "change", type)
  type <- ifelse(is.null(variant), type, "variant")

  # sjekk klass er char
  klass <- MakeChar(klass)

  # dato sjekking
  if(is.null(date[1])) date <- Sys.Date()
  
  # Create variables fratil (whether to and from dates should be used) and ver
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

  # if two dates are provided, check both and check order. Swap if neccessary
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

  # Set language coding
  language_coding = paste("&language=", language, sep="")

  # Create url and collect data
  if (type == "variant"){
    if(is.numeric(variant) | grepl("^[0-9]", variant)){
      variant_name <- get_variant_name(variant)
    } else {
      variant_name <- gsub(" ", "%20", variant)
    }
  }
  url <- MakeUrl(klass=klass, correspond=correspond, variant_name = variant_name,
                 type=type, 
                 fratil=fratil, date=date, output_level_coding=output_level_coding, 
                 language_coding=language_coding)
  
  if (type == "kor"){
    klass_text <- GetUrl2(url, check = FALSE)
    # sjekk at det finnes
    targetswap <- FALSE
    if (grepl("no correspondence table", klass_text)){
      targetswap <- TRUE
      url <- MakeUrl(klass=correspond, correspond = klass, type=type, fratil=fratil, date=date, 
                     output_level_coding=output_level_coding, language_coding=language_coding)
      klass_text <- GetUrl2(url)
      if (grepl("no correspondence table", klass_text)){
        stop("No correspondence table found between classes ", klass, " and ", correspond, " for the date ", date,
             "For a list of valid correspondence tables use the function CorrespondList()")
      }
      if (is.null(klass_text)) stop_quietly()
    }
  } else {
    klass_text <- GetUrl2(url)
  }
  if (is.null(klass_text)) stop_quietly()  

  if (grepl("not found", klass_text)){
    stop("No KLASS table was found for KLASS number ", klass,".
    Please try again with a different KLASS number.
    For a list of possible KLASS's use the function ListKlass() or ListFamily()")
  }
  if (grepl("not published in language", klass_text)){
    stop("The classification requested was not found for language = ", gsub(".*=", "", language_coding))
  }
  if (grepl("does not have a variant named", klass_text)){
    stop("The variant ", variant, " was not found for KLASS number ", klass)
  }

  if (type %in% c("vanlig", "variant")){
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
  
  if (output_style == "wide" & is.null(output_level) & is.null(correspond)){
      # get maximum level
      maxlength <- max(klass_data$level)
      minlength <- min(klass_data$level)
      
      # check several levels exist
      if (maxlength == minlength){
        warning("Only one level was detected. Klassification returned with output_style normal. ")
        return(as.data.frame(klass_data))
      }
      
      # create most detailed level dataframe
      wide_data <- klass_data[klass_data$level == maxlength, c("code", "name")]
      names(wide_data) <- c(paste0("code",maxlength), paste0("name", maxlength))
      
      # loop through and match the other levels
      for (i in (as.numeric(maxlength) - 1):minlength){
        m1 <- match(wide_data[, paste0("code", (i+1))], klass_data$code)
        m2 <- match(klass_data$parentCode[m1], klass_data$code)
        tmp <- data.frame(klass_data$parentCode[m1], klass_data$name[m2])
        names(tmp) <- c(paste0("code",i), paste0("name", i))
        
        # paste in wide format beside previous
        wide_data <- cbind(wide_data, tmp)
      }
      
      # rename as klass_data for return
      klass_data <- wide_data
  }
  
  as.data.frame(klass_data)
}
