# Find the equivalent sets of a node at various dates

Find the equivalent sets of a node at various dates

## Usage

``` r
find_equivalent_nodes(node, dates, graph)
```

## Arguments

- node:

  The node that we're finding the equivalent sets of

- dates:

  The dates that we want to find the equivalent sets in

- graph:

  The graph that the nodes come from

## Value

A named list of `length(dates)` containing the equivalent nodes for each
date. The names of the returned list are the dates provided in `date`,
coerced to character.
