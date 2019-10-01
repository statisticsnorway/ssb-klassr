

#' insert missing dots to the right place in a string
#' @param x - character with missing dots
#' @param dot - the place of missing dots
#' @return a string is returned with the insertion of a formatted period (.) in the specifed location.
splitChar <- function(x, dot){
  code_mod <- unlist(strsplit(x, split=""))
  for( j in 1:length(dot)){
    code_mod <- append(code_mod, ".", after=dot[j]-1)
  }
  verdi <- paste(code_mod, sep="", collapse="")
  return(verdi)
}

#' Convert vector to the right format
#' @param x - vector of character
#' @param klass - classification number
#' @param input_level - which classification level
#' @param klass_data - the right formatting to the classification levels
#' @return vector of character
#' @examples
#' \donttest{
#' klass_data <- GetKlass(klass = "6", date = "2007-01-01")
#' input_level <- levelCheck(x = klassdata$nace5, klass_data = klass_data)
#' formattering(x = klassdata$nace5, input_level = input_level, klass = 6, klass_data=klass_data)
#'
#' klass_data <- GetKlass(klass = "7", date = "2007-01-01")
#' input_level <- levelCheck(x = klassdata$occupation, klass_data = klass_data)
#' formattering(x = klassdata$occupation, input_level = input_level, klass = 7, klass_data=klass_data)
#'
#' klass_data <- GetKlass(klass = "131", date = "2007-01-01")
#' input_level <- levelCheck(x = klassdata$occupation, klass_data = klass_data)
#' formattering(x = klassdata$kommune2, input_level = input_level, klass = 131, klass_data=klass_data)
#' }
#'
#'
#' @export
formattering <- function(x, input_level, klass, klass_data){

  code <- klass_data[klass_data$level==input_level,]$code
  # hente funksjonen til å lese inn: Fetch_data
  dot <- unlist(lapply(strsplit(code[1], ''), function(x) which(x == '.')))
  len <- length(tm::removePunctuation(unlist(strsplit(code[1], ''))))

  verdi <- c()
  riktig <- 0
  mangler0 <- 0
  miss <- 0
  manglerDot <- 0
  for ( i in 1:length(x)){
    test <- length(tm::removePunctuation(unlist(strsplit(x[i], ''))))
    if (is.na(x[i]) | x[i]==""){
      verdi[i] <- NA
      miss <- miss +1
    }
    else{
      if(is.logical(dot)){
        if (unlist(strsplit(x[i], ''))[dot] =="."){
          verdi[i] <- x[i]
          riktig <- riktig + 1
        }
      }
      else{
        if (any(unlist(lapply(strsplit(x[i], ''), function(x) which(x == '.')))) == any(dot)){
          if(nchar(x[i]) != len){
            verdi[i] <- paste0("0", x[i])
            mangler0 <- mangler0 +1
          }
          else{
            verdi[i] <- x[i]
          }
        }
        else{
          if( (test) == (len)-length(dot)){
            verdi[i] <- splitChar(x[i], dot)
            manglerDot <- manglerDot + 1
          }
          else{
            verdi[i] <- NA
            miss <- miss +  0
          }
        }
      }

    }
  }

  if (riktig != 0 | mangler0 != 0 | miss != 0 | manglerDot != 0){
   # warning(c("Number correct: ", riktig)) # Fungere ikke for andre variabel enn nace
    warning(c("Number missing leading 0:", mangler0))
    warning(c("Number missing .:" , manglerDot))
    warning(c("Number of NA:", miss))
  }

  return(verdi)

}
