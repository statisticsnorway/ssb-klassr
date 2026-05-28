# Fetch Statistics Norway classification data using API

Fetch Statistics Norway classification data using API

## Usage

``` r
get_klass(
  classification,
  date = NULL,
  correspond = NULL,
  correspondID = NULL,
  variant = NULL,
  output_level = NULL,
  language = "nb",
  output_style = "normal",
  notes = FALSE,
  quiet = TRUE
)

GetKlass(
  klass,
  date = NULL,
  correspond = NULL,
  correspondID = NULL,
  variant = NULL,
  output_level = NULL,
  language = "nb",
  output_style = "normal",
  notes = FALSE,
  quiet = TRUE
)
```

## Arguments

- classification:

  Number/string of the classification ID/number. (use klass_list() to
  find this)

- date:

  String for the required date of the classification. Format must be
  "yyyy-mm-dd". For an inverval, provide two dates as a vector. If
  blank, will default to today's date.

- correspond:

  Number/string of the target classification for correspondence table
  (if a correspondence table is requested).

- correspondID:

  ID number of the correspondence table to retrieve. Use as an
  alternative to correspond.

- variant:

  The classification variant to fetch (if a variant is wanted).

- output_level:

  Number/string specifying the requested hierarchy level (optional).

- language:

  Two letter string for the requested language output. Default is Bokmål
  ("nb"). Nynorsk ("nn") and English ("en") also available for some
  classification.)

- output_style:

  String variable for the output type. Default is "normal". Specify
  "wide" for a wide formatted table output.

- notes:

  Logical for if notes should be returned as a column. Default FALSE

- quiet:

  Logical for whether to suppress the printing of the API address.
  Default TRUE.

- klass:

  Deprecated; use `classification` instead.

## Value

The function returns a data frame of the specified
classification/correspondence table. Output variables include: code,
parentCode, level, and name for standard lists. For correspondence
tables variables include: sourceCode, sourceName, targetCode and
targetName. For date correspondence tables variables include: oldCode,
oldName, newCode and newName. For "wide" output, code and name with
level suffixes is specified. For date ranges, validFromInRequestedRange
and validToInRequestedRange give the dates for the classification.
Variable ChangeOccured gives the effective date for classification
change in classification change tables.

## Examples

``` r
# Get classification for occupation classifications
head(get_klass(classification = "7"))
#>   code parentCode level                                        name
#> 1    0       <NA>     1                  Militære yrker og uoppgitt
#> 2   00          0     2 Uoppgitt / yrker som ikke kan identifiseres
#> 3  000         00     3 Uoppgitt / yrker som ikke kan identifiseres
#> 4 0000        000     4 Uoppgitt / yrker som ikke kan identifiseres
#> 5   01          0     2         Offiserer fra fenrik og høyere grad
#> 6  011         01     3         Offiserer fra fenrik og høyere grad
# Get classification for occupation classifications in English
head(get_klass(classification = "7", language = "en"))
#>   code parentCode level                                      name
#> 1    0       <NA>     1              Armed forces and unspecified
#> 2   00          0     2 Unspecified or unidentifiable occupations
#> 3  000         00     3 Unspecified or unidentifiable occupations
#> 4 0000        000     4 Unspecified or unidentifiable occupations
#> 5   01          0     2        Commissioned armed forces officers
#> 6  011         01     3        Commissioned armed forces officers
```
