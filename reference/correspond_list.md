# Correspondence list Print a list of correspondence tables for a given classification with source and target IDs

Correspondence list Print a list of correspondence tables for a given
classification with source and target IDs

## Usage

``` r
correspond_list(classification, date = NULL)

CorrespondList(klass, date = NULL)
```

## Arguments

- classification:

  Classification number

- date:

  Date for classification (format = "YYYY-mm-dd"). Default is current
  date

- klass:

  Deprecated; use `classification` instead.

## Value

Data frame with list of corrsepondence tables, source ID and target ID.

## Examples

``` r
# \donttest{
correspond_list("7")
#> Finding correspondence tables ...........................................
#>                                                                                           correspondence_name
#> 1 Pasient- og brukerrettede stillinger i helse- og omsorgstjenesten 2015 - Yrkesklassifisering 08 (STYRK-08) 
#> 2                                   Yrkesgrupper i omsorgstjenesten 2015 - Yrkesklassifisering 08 (STYRK-08) 
#> 3                           Yrkesgrupper i spesialisthelsetjenesten 2015 - Yrkesklassifisering 08 (STYRK-08) 
#> 4                                 Yrkesgrupper i tannhelsetjenesten 2015 - Yrkesklassifisering 08 (STYRK-08) 
#> 5                             Yrkeskatalogen, basert på STYRK 98 2016-12 - Yrkesklassifisering 08 (STYRK-08) 
#>   source_klass target_klass correspondence_table
#> 1         <NA>            7                 1250
#> 2         <NA>            7                 2500
#> 3         <NA>            7                 1007
#> 4         <NA>            7                 2482
#> 5          145            7                  426
# }
```
