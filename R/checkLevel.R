
#' checking which level input data is
#' @param x - vector of character
#' @param klass_data - - the right formatting to the classification levels
#' @examples
#' sn <- GetKlass(klass = "6", date = "2007-01-01")
#' levelCheck(x = klassdata$nace5, klass_data = sn)
#'
#' sn <- GetKlass(klass = "7", date = "2007-01-01")
#' levelCheck(x = klassdata$occupation, klass_data = sn)
#' @return input_level

levelCheck <- function(x, klass_data){

  tab <- data.frame(table(nchar(tm::removePunctuation(unlist(x)))))
  t <- which.max(tab$Freq)

  nVar <- tab[t,]$Var1
  nVar <- nVar[!is.na(nVar)]

  input_level <- NA
  for (i in 1:length(unique(klass_data$level))){
    if(nVar==nchar(tm::removePunctuation(unlist(klass_data[klass_data$level==i,]$code[1])))){
      input_level <- i
    }
  }
  if(is.na(input_level))
    stop("Cannot find a level")
  return (input_level)
}
