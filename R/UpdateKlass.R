#' Update a code found in a directed graph based on a Klass-classification
#'
#' @inheritParams find_dates
#' @inheritParams KlassNode
#'
#' @param output Either a character vector, containing one or more of the items
#'   in the list below, or \code{TRUE} to include all columns.
#'
#'   \describe{
#'    \item{\code{"code"}}{The Klass code.}
#'    \item{\code{"name"}}{The Klass name.}
#'    \item{\code{"validFrom"}}{The date that the code is valid from.}
#'    \item{\code{"validTo"}}{The date that the code is valid to.}
#'    \item{\code{"split"}}{Logical: Does the code split into two or more codes?}
#'    \item{\code{"combined"}}{Logical: Does two or more codes become this code?}
#'    \item{\code{"nextCode"}}{If \code{split == FALSE}, gives the code this code changed into. \code{NA} otherwise.}
#'   }
#'
#' @param combine \code{TRUE} or \code{FALSE}. See the value section.
#'
#' @param report \code{TRUE} or \code{FALSE}. See the value section.
#'
#' @return If \code{report == TRUE} and \code{length(output) > 1 | TRUE}, the
#'   result will be a \code{data.frame}. The number of rows in the data.frame
#'   will be the the number of codes in the sequence of changes between the
#'   input code and output code. The columns in the \code{data.frame} are
#'   specified with \code{output}.
#'
#'   If \code{report == TRUE} and \code{length(output) == 1}, the result will be
#'   a character vector. The length of the vector will be the number of codes in
#'   the sequence of changes between the input code and output code. The
#'   contents of the character vector is specified with \code{output}.
#'
#'   If \code{report == FALSE} and \code{length(output) > 1 | TRUE} the result
#'   will be a \code{data.frame} with one row representing the updated code. The
#'   data.frame will have columns specified by \code{output}. If a code has been
#'   split, the result will be \code{NA}. If \code{combine == FALSE} and a code
#'   is the result of a combination of codes, the result will be code{NA}.
#'
#'   If \code{report == FALSE} and \code{length(output) == 1}, the result will
#'   be a character vector of length one, containing information about the
#'   updated code. The information provided is specified by \code{output}. If a
#'   code has been split, the result will be \code{NA}. If \code{combine ==
#'   FALSE} and a code is the result of a combination of codes, the result will
#'   be \code{NA}.
#'
#' @seealso See [UpdateKlass] for updating multiple codes in one function call.
#' 
update_code <- function(graph, 
                        code, 
                        date = NA, 
                        output = "code",
                        combine = TRUE,
                        report = FALSE) {
  
  if (is.na(code)) {
    
    return(NA)
    
  }
  
  result <- UpdateKlassNode(graph = graph, 
                            node = KlassNode(graph, code, date = date))
  
  nextCodes <- mapply(function(node, split) {
        
                        if (split | length(node) == 0) {
                          
                          return(NA_character_)
                          
                        } else if (length(node) > 1) {
                          
                          return(paste0(node$code, collapse = "/"))
                          
                        } else {
                          
                          return(node$code)
                        
                        }
                      },
                      node = result$nextNodes,
                      split = result$split,
                      SIMPLIFY = TRUE)
  
  report_df <- data.frame(code      = result$code,
                          name      = result$label,
                          validFrom = result$validFrom,
                          validTo   = result$validTo,
                          split     = result$split,
                          combined  = result$combined,
                          nextCode  = unname(nextCodes))
  
  if (report) {
    
    return(report_df[, output])
    
  } else  if (any(report_df$split) | 
              (!combine & any(report_df$combined)) | 
              length(result) == 0) {
    
    return(NA)
      
  } else {
      
    return(report_df[length(result), output])
      
  }
  
}

#' Update multiple Klass codes to a desired date.
#'
#' @param codes Codes to be updated.
#'
#' @param dates Optional. Can be used to specify what date each of the codes was
#'   valid in. Supply a character vector of length 1 to specify the same valid
#'   date for all codes, or a character vector of the same length as
#'   \code{codes} to specify valid dates for each code. The character vector(s)
#'   should have a format coercible by \code{\link[base]{as.Date}}, e.g.
#'   \code{YYYY-MM-DD}. The function will return an error if a code was not
#'   valid at the specified date.
#'
#' @param date Optional. Can be used to specify the date the codes should be
#'   updated to, e.g. if you have codes that are valid in year \code{T}, but
#'   want to change the codes to the corresponding version in year \code{T-1}.
#'   If unspecified (the default), the function will update codes to the most
#'   recent version.
#'
#' @param graph Optional. A graph object generated by \code{\link{KlassGraph}}.
#'   If you're making multiple calls to \code{\link[klassR]{UpdateKlass}}, you
#'   can save some time by generating the graph beforehand and reusing it for
#'   each call to \code{\link[klassR]{UpdateKlass}} with this parameter. If
#'   providing the graph directly, you do not need to provide the
#'   \code{classification} and \code{date} parameters.
#'
#' @param output Either a character vector, containing one or more of the items
#'   in the list below, or \code{TRUE} to include all columns.
#'
#'   \describe{
#'    \item{\code{"code"}}{The Klass code.}
#'    \item{\code{"name"}}{The Klass name.}
#'    \item{\code{"validFrom"}}{The date that the code is valid from.}
#'    \item{\code{"validTo"}}{The date that the code is valid to.}
#'    \item{\code{"split"}}{Logical: Does the code split into two or more codes?}
#'    \item{\code{"combined"}}{Logical: Does two or more codes become this code?}
#'    \item{\code{"nextCode"}}{If \code{split == FALSE}, gives the code this code changed into. \code{NA} otherwise.}
#'   }
#'
#'   If only one value is specified (e.g. \code{"code"}), the function will
#'   return either a character vector or a list of character vectors, depending
#'   on \code{report}, containing the information specified. If more than one
#'   value is specified (e.g. \code{c("code", "name")}), the function will
#'   return a list of \code{data.frames} containing the specified columns.
#'
#' @param report \code{TRUE} or \code{FALSE}. If \code{TRUE}, the output
#'   character vectors/\code{data.frames} will contain information about the
#'   input code, output code, and any "intermediate" codes between the input and
#'   output. If \code{FALSE}, the output will only contain information about the
#'   updated code.
#'
#' @param combine \code{TRUE} or \code{FALSE}. If \code{FALSE}, any code that
#'   has been combined with another code will return \code{NA}. If \code{TRUE},
#'   combined codes will not return \code{NA}.
#'
#' @inheritParams KlassGraph
#'
#' @inheritParams update_code
#'
#' @return Either a character vector or list of the same length as \code{codes},
#'   depending on \code{output} and \code{report}. The function returns
#'   information about the updated code, and optionally information about
#'   intermediate codes, as specified by \code{output} and \code{report}.
#'
#'   Any input codes that have been split will return \code{NA}.
#'
#' @export
#'
#' @examples
#'
#' codes <- GetKlass(131, date = "2020-01-01")[["code"]]
#'
#' updated_codes <- UpdateKlass(codes,
#'                              dates = "2020-01-01",
#'                              classification = 131)
#' 
UpdateKlass <- function(codes,
                        dates = NA,
                        classification = NULL,
                        date = NULL,
                        graph = KlassGraph(classification, date),
                        output = "code",
                        report = FALSE,
                        combine = TRUE) {
  
  if (!methods::hasArg(graph) & !methods::hasArg(classification)) {
    
    stop("\nPlease provide either:\n",
         "- A graph with the `graph` argument\n",
         "- A classification ID with the `classification` argument")
    
  }
  
  # TODO Optimize by making a table of input codes, updating those, and
  # returning an expanded vector based on input
  
  simplify <- length(output) == 1 & !isTRUE(output) & !report
  
  result <- mapply(FUN      = update_code,
                   code     = codes,
                   date     = dates,
                   SIMPLIFY = simplify,
                   MoreArgs = list(graph   = graph,
                                   output  = output,
                                   combine = combine,
                                   report  = report))
  
  return(result)
  
}
