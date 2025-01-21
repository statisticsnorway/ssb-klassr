
#' Find the equivalents of a node at various dates
#'
#' @param node The node that we're finding the equivalents of
#' @param dates The dates that we want to find the equivalents in
#' @param graph The graph that the nodes come from
#'
#' @return A named list of `length(dates)` containing the equivalent nodes for
#'   each date. The names of the returned list are the dates provided in `date`,
#'   coerced to character.
#' 
find_equivalent_nodes <- function(node, dates, graph) {
  
  dates <- as.Date(dates)
  
  if (any(is.na(dates))) stop("Dates cannot be NA")
  if (!length(dates) > 1) stop("Need to provide at least two dates")
  
  # <- older                                                          newer ->
  # | ----------- min_date ---------------------------- max_date ----------- |
  # | 1. [=================================================================> |
  # | 2. [==========================================================]        |
  # | 3. [=========================================]                         |
  # | 4.                        [===================================]        |
  # | 5.                        [==================]                         |
  # | 6.                        [==========================================> |
  # |------------------------------------------------------------------------|
  
  min_date <- min(dates)
  max_date <- max(dates)
  
  # We only want to search for equivalent sets of nodes in a subset of the
  # graph, determined by max_date and min_date. We filter the nodes to include
  # only codes that were valid at one point within the period specified by
  # max_date and min_date. 
  #
  # See the table above for a visual explanation of the filtering logic used
  # here. The numbers in the table correspond to the numbered comments below.
  search_nodes <- igraph::V(graph)[
    # 1. Codes that were introduced before/on min_date and haven't expired
    (igraph::V(graph)$validFrom <= min_date & is.na(igraph::V(graph)$validTo)) |
    # 2. Codes that were introduced before/on min_date and expired after
    # max_date
    (igraph::V(graph)$validFrom <= min_date & 
       igraph::V(graph)$validTo >= max_date) |
    # 3. Codes that were introduced before min_date, and expired before/on
    # max_date and after/on min_date
    (igraph::V(graph)$validFrom <= min_date & 
       igraph::V(graph)$validTo <= max_date & 
       igraph::V(graph)$validTo >= min_date) |
    # 4. Codes that were introduced after/on min_date and before/on max_date,
    # and expired after/on max_date
    (igraph::V(graph)$validFrom >= min_date & 
       igraph::V(graph)$validFrom <= max_date & 
       igraph::V(graph)$validTo >= max_date) |
    # 5. Codes that were introduced after/on min_date and before/on max_date,
    # and expired before/on max_date and after/on min_date
    (igraph::V(graph)$validFrom >= min_date & 
       igraph::V(graph)$validFrom <= max_date & 
       igraph::V(graph)$validTo <= max_date & 
       igraph::V(graph)$validTo >= min_date) |
    # 6. Codes that were introduced after/on min_date and before/on max_date,
    # and haven't expired
    (igraph::V(graph)$validFrom >= min_date & 
       igraph::V(graph)$validFrom <= max_date & 
       is.na(igraph::V(graph)$validTo))
  ] 

  all_nodes <- igraph::bfs(graph = graph,
                           root = node,
                           mode = "all",
                           unreachable = FALSE,
                           restricted = search_nodes)[["order"]]
  
  result <- lapply(dates, \(date) {
    
    result_nodes <- all_nodes[
      date >= all_nodes$validFrom & 
        (date < all_nodes$validTo | is.na(all_nodes$validTo))
    ]
    
    result_nodes[order(result_nodes$code)]
    
  })
  
  names(result) <- as.character(dates)
  
  return(result)
  
}

#' Find equivalent sets of codes at specific dates in a Klass classification
#'
#' @param code A Klass code that you wish to know the equivalent sets of
#' @param date Optional. Use to specify the date this code was valid. If NULL,
#'   the most recent version of the code within the specified date interval will
#'   be used.
#' @param dates The dates that equivalent sets of codes should be found for.
#' @param classification The Klass ID to be used
#' @param graph Optional. Generating the graph using `klass_graph` manually
#'   beforehand and providing it in this parameter can save time if running
#'   `find_equivalents` multiple times in sequence.
#'
#' @return A data.frame with columns:
#' - `date` containing the input `dates`
#' - `code` containing the set of equivalent codes in each date
#' - `name` containing the names of each code
#' - `validFrom` and `validTo` values for each code returned
#' @export
#' @details This function provides a solution to the problem of split or
#'   combined codes in Klass classifications. When using `update_klass` to ask
#'   "what is this code in this version of the classification in this other
#'   version of the classification?", the answer is sometimes that the code has
#'   been split into two more codes (or combined from two or more codes, if
#'   trying to back-date a code), and therefore that the code cannot be updated.
#'
#'   The solution provided by `find_equivalents` is answering the question: "in
#'   these versions of the classification, which codes were equivalent to this
#'   code in this other version of the classification?".
#'
#'   Consider the following example of two codes combining into one. Here, `"a"`
#'   and `"b"` are valid at t1, and are combined into `"c"` at t2.
#'
#'   ```
#'   t1     t2
#'   a ──┰─> c
#'       ┃
#'   b ──┚
#'
#'   ```
#'
#'   `update_klass()` would inform us that `"a"` can be updated to `"c"` at t2,
#'   unless we specified `combine = FALSE`, in which case the result would be
#'   `NA`. `find_equivalents()` would inform us that the equivalent of `"a"` in
#'   t1 at t2 is `"c"`. Crucially, `find_equivalents` would also inform us that
#'   the equivalent of `"c"` in t2 at t1 is `"a"` and `"b"`.
#'
#'   We can also consider a code splitting into two. In this example, `"a"` is
#'   valid at t1, and splits into `"b"` and `"c"` at t2.
#'
#'   ```
#'   t1     t2
#'   a
#'   ├─────> b
#'   └─────> c
#'   ```
#'
#'   `update_klass` is unable to provide an updated code due to the split, and
#'   would return `NA`. `find_equivalents()` would inform us that the equivalent
#'   codes of `"a"` at t1 is `"b"` and `"c"` at t2.
#'
#'
#'   `find_equivalents` can handle more than two dates. In the following
#'   example, `"a"` splits into `"b"` and `"c"` at t2, and `"b"` and `"c"`
#'   combine into `"d"` at t3. `find_equivalents` can inform us that `"a"` is
#'   equivalent to `"b"` and `"c"` at t2, and `"d"` at t3.
#'
#'   ```
#'   t1     t2     t3
#'   a
#'   ├─────> b ┐
#'   └─────> c ┴─> d
#'   ```
#'
#'   `find_equivalents` will only search in the time range we specify. As a
#'   consequence, generating sets of equivalent codes over longer time spans
#'   will generally create larger sets than using shorter time spans.
#'   
#'   To illustrate this behavior, we can add a new code `"e"` to the previous
#'   example, and have `"d"` and `"e"` combine into `"f"` at t4. 
#'   
#'   ```
#'   t1     t2     t3     t4
#'   a
#'   ├─────> b ┐
#'   └─────> c ┴─> d ┐
#'   e ──────────────┴──> f
#'   ```
#'   
#'   Finding the
#'   equivalents of `"a"` in t1 at t2 and t3 returns the same sets as
#'   before:
#'   
#'   - t1: `"a"`
#'   - t2: `"b"` and `"c"`
#'   - t3: `"d"`
#'
#'   However, if we also wanted to know the equivalent set for t4, the result
#'   would be:
#'
#'   - t1: `"a"` and `"e"`
#'   - t2: `"b"`, `"c"` and `"e"`
#'   - t3: `"d"` and `"e"`
#'   - t4: `"f"`
#' 
find_equivalents <- function(code, 
                             date = NULL, 
                             dates, 
                             classification,
                             graph = klass_graph(classification)) {
  
  if (is.null(date)) {
    
    # Only search for nodes within the specified date interval
    # Refer to find_equivalent_nodes for an explanation of this filtering logic
    
    max_date <- max(dates)
    min_date <- min(dates)
    
    code_indices <- which(
      igraph::V(graph)$code == code & (
        (igraph::V(graph)$validFrom <= min_date & is.na(igraph::V(graph)$validTo)) |
          (igraph::V(graph)$validFrom <= min_date & 
             igraph::V(graph)$validTo >= max_date) |
          (igraph::V(graph)$validFrom <= min_date & 
             igraph::V(graph)$validTo <= max_date & 
             igraph::V(graph)$validTo >= min_date) |
          (igraph::V(graph)$validFrom >= min_date & 
             igraph::V(graph)$validFrom <= max_date & 
             igraph::V(graph)$validTo >= max_date) |
          (igraph::V(graph)$validFrom >= min_date & 
             igraph::V(graph)$validFrom <= max_date & 
             igraph::V(graph)$validTo <= max_date & 
             igraph::V(graph)$validTo >= min_date) |
          (igraph::V(graph)$validFrom >= min_date & 
             igraph::V(graph)$validFrom <= max_date & 
             is.na(igraph::V(graph)$validTo))
      )
    )
    
    highest_variant_index <-
      suppressWarnings(
        code_indices[which(igraph::V(graph)[code_indices]$variant ==
                          max(igraph::V(graph)[code_indices]$variant))]
      )
    
    node <- igraph::V(graph)[highest_variant_index]
    
  } else if (!is.na(as.Date(date))) {
    
    if (date > max(dates) | date < min(dates)) {
      
      stop("`date` must be in the range specified by `min(dates)` and `max(dates)`")
      
    }
    
    node <- igraph::V(graph)[
      igraph::V(graph)$code == !!code &
        igraph::V(graph)$validFrom <= date &
        (igraph::V(graph)$validTo >= date | is.na(igraph::V(graph)$validTo))
    ]
    
  } else {
    
    stop("Provided `date` could not be coerced to date.")
    
  }
  
  equivalents <- find_equivalent_nodes(node, dates, graph)
  
  equivalent_dfs <- list()
  
  for (i in seq_along(equivalents)) {
    
    df <- as.data.frame(igraph::vertex.attributes(graph, equivalents[[i]]))
    df$date <- dates[i]
    df <- df[c("date", "code", "label", "validFrom", "validTo")]
    names(df)[3] <- "name"
    
    equivalent_dfs[[i]] <- df
    
  }
  
  return(do.call(rbind, equivalent_dfs))
  
}