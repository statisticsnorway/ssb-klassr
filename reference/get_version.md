# Get version number of a class given a date

Get version number of a class given a date

## Usage

``` r
get_version(classification = NULL, date = NULL, family = NULL, klassNr = FALSE)

GetVersion(klass = NULL, date = NULL, family = NULL, klassNr = FALSE)
```

## Arguments

- classification:

  Classification number

- date:

  Date for version to be valid

- family:

  Family ID number if a list of version number for all classes is
  desired

- klassNr:

  True/False for whether to output classification numbers. Default =
  FALSE

- klass:

  Deprecated; use `classification` instead.

## Value

Number, vector or data frame with version numbers and calssification
numbers if specified.

## Examples

``` r
get_version(7)
#> [1] "33"
```
