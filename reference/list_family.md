# Classification family list Print a list of all families and the number of classifications in each

Classification family list Print a list of all families and the number
of classifications in each

## Usage

``` r
list_family(family = NULL, codelists = FALSE, language = "nb")

ListFamily(family = NULL, codelists = FALSE, language = "nb")
```

## Arguments

- family:

  Input family ID number to get a list of classifications in that family

- codelists:

  True/False for whether to include codelists. Default = FALSE

- language:

  Two letter string for the requested language output. Default is Bokmål
  ("nb"). Nynorsk ("nn") and English ("en").

## Value

dataset containing a list of families

## Examples

``` r
list_family(family = 1)
#>                         klass_name klass_nr
#> 1 Standard for yrkesklassifisering        7
```
