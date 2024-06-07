
#' Get all changes that have occurred in a Klass classification
#'
#' @param classification The ID of the desired classification.
#'
#' @return A \code{data.frame} containing the code changes.
#'
#' @examples
get_klass_changes <- function(classification) {
  
  bind_rows(
    content(
      GET(
        paste0("https://data.ssb.no/api/klass/v1/classifications/",
               classification, "/changes?from=0001-01-01")
      )
    )[["codeChanges"]]
  )
  
}

#' Build a directed graph of code changes based on a Klass classification
#'
#' @inheritParams get_klass_changes
#'
#' @param date The date which the edges of the graph should be directed
#'   towards.
#'
#'   Defaults to the current year plus one, which ensures the graph is directed
#'   to the most recent codes.
#'
#' @return An igraph object with the vertexes representing codes, and edges
#'   representing changes between codes. The direction of the edges represent
#'   changes towards the date specified in \code{date}
#' @export
#'
#' @examples
#'
#' # Build a graph directed towards the most recent codes
#'
#' klass_131 <- build_klass_graph(131) 
#' 
#' # Build a graph directed towards valid codes in 2020.
#' 
#' klass_131_2020 <- build_klass_graph(131, "2020-01-01") 
#'
#' 
build_klass_graph <- function(classification, 
                              date = NULL) {
  ## Downloading api stuff 
  
  api_endringer <- get_klass_changes(classification)

  api_alle <-
    bind_rows(
      content(
        GET(
          paste0("https://data.ssb.no/api/klass/v1/classifications/",
                 classification, "/codes?from=0001-01-01")
        )
      )[["codes"]]
    )

  ## Calculating variants and changes 
  
  variants <-
    api_alle %>%
    distinct(code) %>%
    group_by(code) %>%
    summarise(df = map(code, find_dates, api_alle, api_endringer)) %>%
    unnest(df) %>%
    mutate(across(c(validFrom, validTo), as.Date)) %>%
    select(code, name, variant, validFrom, validTo)
  
  changes <-
    api_endringer %>%
    select(oldCode, changeOccurred, newCode) %>% 
    mutate(
      variantFrom = map2_int(oldCode, changeOccurred, find_variant_from, variants),
      variantTo = map2_int(newCode, changeOccurred, find_variant_to, variants),
      changeOccurred = as.Date(changeOccurred)
    )
  
  # Certain changes are not recorded explicitly in Klass. In order to preserve
  # the link between versions of the same code, we can add edges between
  # successive variants that cannot be directly observed in the Klass API.
  
  # Not clear if this is a good approach. Commented out solution below causes
  # incorrect linkages. Might be better to add changes in Klass.
  
  # variant_changes <-
  #   variants %>% 
  #   group_by(code) %>% 
  #   mutate(oldCode = code, 
  #          newCode = code,
  #          variantFrom = variant, variantTo = variant + 1,
  #          changeOccurred = validTo) %>% 
  #   filter(variant != max(variant)) %>% 
  #   ungroup() %>% 
  #   select(oldCode, changeOccurred, newCode, variantFrom, variantTo)
  # 
  # changes <-
  #   bind_rows(changes,
  #             anti_join(x = variant_changes, 
  #                       y = changes,
  #                       by = join_by(oldCode, changeOccurred, newCode, 
  #                                    variantFrom, variantTo)))
  
  ## Calculating vertices and edges 
  
  klass_vertices <-
    variants %>%
    mutate(vertex = as.character(row_number()))
  
  klass_edges <-
    changes %>%
    left_join(select(klass_vertices, code, variant, vertexFrom = vertex),
              join_by(oldCode == code, variantFrom == variant)) %>%
    left_join(select(klass_vertices, code, variant, vertexTo = vertex),
              join_by(newCode == code, variantTo == variant))
  
  ## Building graph 
  
  graph <-
    klass_edges %>%
    select(vertexFrom, vertexTo) %>%
    as.matrix() %>%
    graph_from_edgelist() %>%
    set_edge_attr("changeOccurred", value = klass_edges$changeOccurred)
  
  no_edges <-
    klass_vertices %>%
    filter(!vertex %in% V(graph)$name)
  
  graph <-
    graph %>%
    add_vertices(nrow(klass_vertices %>% filter(!vertex %in% V(graph)$name)),
                 attr = list(name = no_edges$vertex))
  
  graph <- reverse_edges(graph, E(graph)[changeOccurred > as.Date(date)])
  
  klass_vertices <- arrange(klass_vertices, factor(vertex, V(graph)$name))
  
  attributes <- 
    klass_vertices %>% 
    select(-vertex) %>% 
    rename(label = name)

  graph <- reduce2(
    .x    = names(attributes),
    .y    = attributes,
    .init = graph,
    .f    = function(graph, attribute, values) {

      set_vertex_attr(graph, attribute, value = values)

  })
  
  return(graph)
  
}

#' Find the variant of a code corresponding to a change *from* a specific code.
#'
#' @param x The code that is being changed
#' @param changeOccurred The date the change occurred
#' @param variants The variants lookup-table.
#'
#' @return The variant corresponding to the code \code{x} at date \code{changeOccurred}.
#'
#' @seealso [find_variant_to]
#' 
find_variant_from <- function(x, changeOccurred, variants) {
  
  variants[variants$code == x &
             changeOccurred > variants$validFrom & 
             (changeOccurred <= variants$validTo | is.na(variants$validTo)),]$variant
  
}

#' Find the variant of a code corresponding to a change *to* a specific code.
#'
#' @inheritParams find_variant_from
#' 
#' @inherit find_variant_from return
#'
#' @seealso [find_variant_from()]
find_variant_to <- function(x, changeOccurred, variants) {
  
  variants[variants$code == x &
             changeOccurred >= variants$validFrom & 
             (changeOccurred < variants$validTo | is.na(variants$validTo)),]$variant
  
}


#' For a given Klass code and corresponding name, produce a table of dates
#' describing the valid-from and valid-to dates of all versions of the code
#'
#' @param code A Klass code
#' @param name The name corresponding to the Klass code
#' @param api_alle A table of all codes in the classification. See example.
#' @param api_endringer A table of all changes in the classification See
#'   example.
#'
#' @return A \code{data.frame} with number of rows equal to the number of variants of
#'   the combination of code and name, determined by the changes the code has
#'   been involved in. The \code{data.frame} has two columns:
#'   
#'   \itemize{
#'    \item{"validFrom"}
#'    \item{"validTo"}
#'   }
#'   
#' @examples
#'
#'
#' api_alle <-
#'   glue("https://data.ssb.no/api/klass/v1/classifications/",
#'        "{classification}/codes?from=0001-01-01") %>%
#'   GET() %>%
#'   content() %>%
#'   pluck("codes") %>%
#'   bind_rows()
#'
#' api_endringer <-
#'   glue("https://data.ssb.no/api/klass/v1/classifications/",
#'        "{classification}/changes?from=0001-01-01") %>%
#'   GET() %>%
#'   content() %>%
#'   pluck("codeChanges") %>%
#'   bind_rows()
#' 
find_dates <- function(code, api_alle, api_endringer) {
  
  dates_df <- api_alle[api_alle$code == code,]
  
  dates_df <- dates_df[c("name",
                         "validFromInRequestedRange",
                         "validToInRequestedRange")]
  
  names(dates_df)[2:3] <- c("validFrom", "validTo")
  
  dates_changed <- api_endringer[
    (api_endringer$oldCode == code) | (api_endringer$newCode == code),
  ][["changeOccurred"]]
  
  if (all(dates_changed %in% unlist(dates_df[c("validFrom", "validTo")]))) {
    
    dates_df <- dates_df[order(dates_df$validFrom),
                         c("name", "validFrom", "validTo")]
    
    dates_df$variant <- 1:nrow(dates_df)
    
    return(dates_df)
    
  } else {
    
    # `api_alle` does not give information on codes combining with already
    # existing codes. We use `api_endringer` to expand the dates table to
    # include periods based on when a code has been either combined with
    # another code or a code has split into multiple codes.
    
    for (date in c(dates_df$validFrom, dates_df$validTo, dates_changed)) {
      
      if (date %in% c(dates_df$validFrom, dates_df$validTo)) {
        
        next()
        
      } else {
        
        dates_df$rn <- 1:nrow(dates_df)
        
        row_to_edit <- 
          dates_df[date >= dates_df$validFrom & 
                     (date < dates_df$validTo | is.na(dates_df$validTo)),]
        
        if (nrow(row_to_edit) > 1) stop("Multiple changes matched!")
        
        old_row <- row_to_edit
        old_row$validTo <- date
        
        new_row <- row_to_edit
        new_row$validFrom <- date
        
        dates_df <-
          rbind(dates_df[!dates_df$rn %in% row_to_edit$rn,],
                old_row,
                new_row)
        
      }
      
    }
    
    dates_df <- dates_df[order(dates_df$validFrom),
                         c("validFrom", "validTo")]
    
    dates_df$variant <- 1:nrow(dates_df)
    
    dates_df$name <- map2_chr(dates_df$validFrom,
                              dates_df$validTo,
                              find_name,
                              code = code,
                              api_alle = api_alle)
    
    return(dates_df[c("name", "variant", "validFrom", "validTo")])
    
  }
  
}

find_name <- function(code, validFrom, validTo, api_alle) {
  
  koder <- api_alle[api_alle$code == code, ]
  
  if (is.na(validTo)) {
    
    return(koder[is.na(koder$validToInRequestedRange),]$name)
    
  }  else {
    
    return(
      koder[validFrom >= koder$validFromInRequestedRange &
              (validTo <= koder$validToInRequestedRange | 
                 is.na(koder$validToInRequestedRange)),]$name
    )
  }
  
}

#' Given a Klass graph, find the node corresponding to a code and (optionally)
#' date
#'
#' @param graph A graph generated by \code{build_klass_graph}.
#' @param x The code to search for.
#' @param date Optional. The specific date the supplied code is valid in.
#'
#' @return The node in the graph corresponding to the supplied code. If date is
#'   not provided, the node with the most recent code is returned. If date is
#'   provided, the code with date between \code{validFrom} and \code{validTo} is returned.
#' @export
#' 
#' @inherit build_klass_graph examples
#' 
#' @examples
#'
#' Find the most recent node in the graph representing the code "0101" (Halden,
#' valid to 2020.)
#'
#' halden_node <- find_node(klass_131, "0101")
#'
#' 
find_node <- function(graph, x, date = NULL) {
  
  if (!is.null(date)) {
    
    date <- as.Date(date)
    
    node <- V(graph)[code == x & date >= validFrom & (date < validTo | is.na(validTo))]
    
  } else {
    
    node <- V(graph)[code == x][variant == max(variant)]
    
  }
  
  if (length(node) == 1) {
    
    return(node)
    
  } else if (length(node) > 1) {
    
    stop("More than one node found.")
    
  } else {
    
    stop("Found no nodes.")
  }
  
}

#' Count the neighbors of a node
#'
#' @inheritParams update_node
#' @inheritParams igraph::neighbors
#'
#' @return A numeric vector of length one giving the number of neighbors.
#' @export
#'
count_neighbors <- function(graph, node, mode) {
  
  length(igraph::neighbors(graph, node, mode))
  
}

#' Given a graph and a node, determine if the node is a split code
#'
#' @param graph
#' @param node
#'
#' @return \code{TRUE} if the node is split, otherwise \code{FALSE}.
#'
#' @details The function will attempt to reconcile nodes that have split and
#'   then later merged again. A node is considered to be split if there is more
#'   than one node that does not itself have children (i.e. nodes at the end of
#'   a sequence of changes) that can be reached from \code{node}
#'
#' @export
#'
#' @examples
is_split <- function(graph, node) {
  
  bfs_result <- igraph::bfs(graph = graph, 
                            root = node, 
                            mode = "out", 
                            unreachable = FALSE)
  
  end_nodes <- bfs_result$order[map_dbl(.x = bfs_result$order, 
                                        .f = count_neighbors,
                                        graph = graph,
                                        mode = "out") == 0]
  
  length(unique(end_nodes)) > 1
  
}

#' Given a graph and a node, determine if the node is a result of combinations
#' of multiple codes
#'
#' @inheritParams update_node
#'
#' @return \code{TRUE} if the node is a combination of two or more nodes,
#'   otherwise \code{FALSE}.
#'
#' @details The function will attempt to reconcile nodes that have split and
#'   then later merged again. A node is considered to be combined if more than
#'   one node that does not itself have a parent (i.e. codes at the start of a
#'   sequence of changes) contribute to \code{node}.
#'
#' @export
#' 
is_combined <- function(graph, node) {
  
  bfs_result <- igraph::bfs(graph = graph, 
                            root = node, 
                            mode = "in", 
                            unreachable = FALSE)
  
  start_nodes <- bfs_result$order[map_dbl(.x = bfs_result$order, 
                                          .f = count_neighbors,
                                          graph = graph,
                                          mode = "in") == 0]
  
  length(unique(start_nodes)) > 1
  
}



#' Given a node and a graph, find the node at the end of a sequence of changes.
#'
#' @inheritParams find_node
#' @param node A node in the supplied graph, as returned by [find_node].
#'
#' @param combine Allow nodes to be combined? If \code{FALSE}, the node search will
#'   halt if the visited node is a combination of multiple nodes.
#'
#' @return A sequence of vertices, starting with \code{node} and ending with the last
#'   visited node.
#'   
#' @export
#'
#' @inherit build_klass_graph examples
#' @inherit find_node examples
#' 
#' @examples
#' 
#' halden_node <- find_node(klass_131, "0101")
#' 
#' halden_node_updated <- update_node(klass_131, halden_node)
#' 
#' 
update_node <- function(graph, node, combine = TRUE) {
  
  bfs_result <- igraph::bfs(graph = graph, 
                            root = node, 
                            mode = "out", 
                            unreachable = FALSE)
  
  
  end_nodes <- bfs_result$order[map_dbl(.x = bfs_result$order, 
                                        .f = count_neighbors,
                                        graph = graph,
                                        mode = "out") == 0]

  visited <- c(bfs_result$order[!name %in% unique(end_nodes)$name],
               unique(end_nodes))
  
  vertex_attr(graph, "split", visited$name) <- 
    unname(map_lgl(visited, is_split, graph = graph))
  
  vertex_attr(graph, "combined", visited$name) <- 
    unname(map_lgl(visited, is_combined, graph = graph))
  
  vertex_attr(graph, "nextNodes", visited$name) <-
    map(visited, neighbors, graph = graph, mode = "out")
  
  visited <- V(graph)[visited$name]
  
  return(visited)
  
}

#' Update a code found in a directed graph based on a Klass-classification
#'
#' @inheritParams find_dates
#' @inheritParams find_node
#'
#' @param output A character vector, containing one or more of the following
#'   desired outputs:
#'
#'   \describe{
#'    \item{\code{"code"}}{The Klass code}
#'    \item{\code{"name"}}{The Klass name}
#'    \item{\code{"validFrom"}}{The date that the code is valid from}
#'    \item{\code{"validTo"}}{The date that the code is valid to}
#'    \item{\code{"split"}}{Logical: Does the code split into two or more codes?}
#'    \item{\code{"combined"}}{Logical: Does two or more codes become this code?}
#'    \item{\code{"nextCode"}}{If \code{split == FALSE}, gives the code this code changed into. \code{NA} otherwise.}
#'   }
#'
#' @return If \code{report == TRUE} and \code{length(output) > 1}, the result
#'   will be a \code{data.frame} with number of rows equal to the number of
#'   codes in the sequence of changes between the input code and output code.
#'   The columns in the \code{data.frame} are specified with \code{output}.
#'
#'   If \code{report == TRUE} and \code{length(output) == 1}, the result will be
#'   a character vector with length equal to the number of codes in the sequence
#'   of changes between the input code and output code. The contents of the
#'   character vector is specified with \code{output}.
#'
#'   If \code{report == FALSE} and \code{length(output) > 1} the result will
#'   either be a \code{data.frame} with one row representing the last code in
#'   the change sequence and columns specified by \code{output}. If a code has
#'   been split, the result will be \code{NA}. If \code{combine == FALSE} and a
#'   code is the result of a combination of codes, the result will be code{NA}.
#'
#'   If \code{report == FALSE} and \code{length(output) == 1}, the result will
#'   be a character vector of length one, containing information about the
#'   updated code specified by \code{output}. If a code has been split, the
#'   result will be \code{NA}. If \code{combine == FALSE} and a code is the
#'   result of a combination of codes, the result will be \code{NA}.
#'
#' @export
#'
#' @inherit build_klass_graph examples
#'
#' @seealso See [update_codes()] for updating multiple codes in one function
#'   call.
#'
#' @examples
#'
#' # Update an outdated code for the Halden municipality.
#'
#' update_code(klass_131, "0101")
#' 
update_code <- function(graph, 
                        code, 
                        date = NULL, 
                        output = "code",
                        combine = TRUE,
                        report = FALSE) {
  
  result <- update_node(graph = graph, 
                        node = find_node(graph, code, date = date), 
                        combine = combine)
  
  nextCodes <- 
    map2_chr(
      result$nextNodes,
      result$split,
      function(node, split) {
        if (split | length(node) == 0) {
          
          return(NA)
          
        } else if (length(node) > 1) {
          
          return(paste0(node$code, collapse = "/"))
          
        } else {
          
          return(node$code)
          
        }
      }
    )
  
  report_df <- 
    data.frame(code = result$code,
               name = result$label,
               validFrom = result$validFrom,
               validTo = result$validTo,
               split = result$split,
               combined = result$combined,
               nextCode = unname(nextCodes))
  
  if (report) {
    
    return(report_df[, output])
    
  } else {
    
    if (any(report_df$split) | (!combine & any(report_df$combined))) {
      
      return(NA)
  
    } else {
      
      return(report_df[length(result), output])
      
    }
    
  }
  
}

#' Update multiple Klass codes to a specific date.
#'
#' @param codes Codes to be updated.
#' @param dates Optional. Character vector of length 1 or \code{length(codes)}, used
#'   to specify the dates at which the supplied codes are valid.
#' @param classification The Klass classification ID the supplied codes are
#'   from.
#' @param date The date to which the codes should be updated. Setting an earlier
#'   date than the current date will "backdate" codes if possible.
#'
#' @param output 
#' 
#' @param report
#' 
#' @param graph
#'
#' @return If \code{output = "simple"}, a vector of length \code{length(codes)}
#'   containing either a code if the update is successful or \code{NA} if the code
#'   has been split. If \code{combine = FALSE}, a code being combined with another
#'   code will also return \code{NA}.
#'
#'   If \code{output = "report"}, a list of length \code{length(codes)} containing
#'   \code{data.frame}s detailing the codes visited through the node search. The
#'   tables have the following columns:
#'
#'   \describe{
#'    \item{code}{The Klass code}
#'    \item{name}{The Klass name}
#'    \item{validFrom}{The date that the code is valid from}
#'    \item{validTo}{The date that the code is valid to}
#'    \item{split}{Logical: Does the code split into two or more codes?}
#'    \item{combined}{Logical: Does two or more codes become this code?}
#'    \item{nextCode}{If \code{split == FALSE}, gives the code this code changed into. \code{NA} otherwise.}
#'   }
#' @export
#' 
#' @examples
update_codes <- function(codes,
                         dates = NULL,
                         classification,
                         date = NULL,
                         output = "code",
                         combine = TRUE,
                         report = FALSE,
                         graph = build_klass_graph(classification, date)) {

  if (is.null(dates) | length(dates) == 1) {
    
    if (length(output) == 1) {
      
      map_chr(.x     = codes,
              .f     = update_code,
              graph  = graph,
              date   = dates,
              output = output,
              combine = combine,
              report = report)
      
    } else {
      
      map(.x     = codes,
          .f     = update_code,
          graph  = graph,
          date   = dates,
          output = output,
          combine = combine,
          report = report)
      
    }
    
  } else {
    
    if (length(output) == 1) {
      
      map2_chr(.x     = codes,
               .y     = dates,
               .f     = update_code,
               graph  = graph,
               output = output, 
               combine = combine,
               report = report)
      
    } else {
      
      map2(.x     = codes,
           .y     = dates,
           .f     = update_code,
           graph  = graph,
           output = output,
           combine = combine,
           report = report)
      
    }
    
  }
  
}

#' @rdname update_codes
#' @export
UpdateKlass <- update_codes
