#' Check which level input data/vector is
#' 
#' The input level is calculated as the mode (most common) hierarchical 
#' level in the input vector.
#' 
#' @param x - vector of character
#' @param klass_data - - the right formatting to the classification levels
#' @return The hierarchical level of the input data is returned. 
#' @keyword internal
#' @examples
#' data(klassdata)
#' 
#' sn <- GetKlass(klass = "6", date = "2007-01-01")
#' levelCheck(x = klassdata$nace5, klass_data = sn)

levelCheck <- function(x, klass_data){
  # Check if only numbers are included. If yes, testing without formatting requirements
  if(!all(grepl("^[A-Za-z_-]+$", x))){
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
    # if letters included, then vector is matched on codes
  } else {
    m <- match(x, klass_data$code)
    tab <- table(klass_data[m, ]$level)
    input_level <- as.numeric(names(tab)[which.max(tab)])
  }
  if(is.na(input_level))
    stop("Cannot find an input level, please check the input vector")
  return (input_level)
}

