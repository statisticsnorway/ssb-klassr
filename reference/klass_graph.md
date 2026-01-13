# Build a directed graph of code changes based on a Klass classification

Build a directed graph of code changes based on a Klass classification

## Usage

``` r
klass_graph(classification, date = NULL)
```

## Arguments

- classification:

  The ID of the desired classification.

- date:

  The date which the edges of the graph should be directed towards.

  Defaults to the current year plus one, which ensures the graph is
  directed to the most recent codes.

## Value

An `igraph` object with the vertexes representing codes, and edges
representing changes between codes. The direction of the edges represent
changes towards the date specified in `date`.

## Examples

``` r
library(klassR)

# Build a graph directed towards the most recent codes
if (FALSE) { # \dontrun{
klass_131 <- klass_graph(131)
} # }

# Build a graph directed towards valid codes in 2020.
if (FALSE) { # \dontrun{
klass_131_2020 <- klass_graph(131, "2020-01-01")
} # }
```
