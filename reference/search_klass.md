# Search Klass

Search Klass

## Usage

``` r
search_klass(query, codelists = FALSE, size = 20)

SearchKlass(query, codelists = FALSE, size = 20)
```

## Arguments

- query:

  String with key word to search for

- codelists:

  True/False for whether to include codelists. Default = FALSE

- size:

  The number of results to show. Default = 20.

## Value

Data frame of possible classifications that match the query

## Examples

``` r
search_klass("occupation")
#>                         klass_name klass_nr
#> 1    Classification of occupations        7
#> 2 Standard for yrkesklassifisering        7
#> 3 Standard for yrkesklassifisering        7
```
