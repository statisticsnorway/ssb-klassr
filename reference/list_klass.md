# Classification list Get a full list of all classifications and codelists

Classification list Get a full list of all classifications and codelists

## Usage

``` r
list_klass(codelists = FALSE, language = "nb")

ListKlass(codelists = FALSE, language = "nb")
```

## Arguments

- codelists:

  True/False for whether to include codelists. Default = FALSE

- language:

  Two letter string for the requested language output. Default is Bokmål
  ("nb"). Nynorsk ("nn") and English ("en").

## Value

A data frame containing a full list of classifications. The data frame
includes the classification name, number, family and type.

## Examples

``` r
head(list_klass(codelists = TRUE))
#>                                              klass_name klass_nr klass_family
#> 2                      Standard for yrkesklassifisering        7            1
#> 3                Kodeliste for arbeidstid (hel-/deltid)      149            1
#> 4                    Kodeliste for arbeidsmarkedsstatus      161            1
#> 5                 Kodeliste for arbeidsgiveravgiftstype      162            1
#> 6 Kodeliste for delpopulasjon for lønn og sysselsetting      163            1
#> 7                          Kodeliste for arbeidsforhold      164            1
#>       klass_type
#> 2 Klassifikasjon
#> 3      Kodeliste
#> 4      Kodeliste
#> 5      Kodeliste
#> 6      Kodeliste
#> 7      Kodeliste
```
