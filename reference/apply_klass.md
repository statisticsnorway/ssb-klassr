# Match and convert a classification

Match and convert a classification

## Usage

``` r
apply_klass(
  x,
  classification,
  date = NULL,
  variant = NULL,
  correspond = NULL,
  language = "nb",
  output_level = NULL,
  output = "name",
  format = TRUE
)

ApplyKlass(
  x,
  klass,
  date = NULL,
  variant = NULL,
  correspond = NULL,
  language = "nb",
  output_level = NULL,
  output = "name",
  format = TRUE
)
```

## Arguments

- x:

  Input vector of classification codes. Vector must match "code" column
  from a call to get_klass().

- classification:

  Classification number

- date:

  String for the required date of the classification. Format must be
  "yyyy-mm-dd". For an inverval, provide two dates as a vector. If
  blank, will default to today's date.

- variant:

  The classification variant to fetch (if a variant is wanted).

- correspond:

  ID number for target in correspondence table. For correspondence
  between two dates within the same classification, use correspond =
  TRUE.

- language:

  Default "nb" for Norwegian (Bokmål). Also "nn" (Nynorsk) and "en"
  (English available for some classifications)

- output_level:

  Desired output level

- output:

  String describing output. May be "name" (default), "code" or "both".

- format:

  Logical for whther to run formatting av input vector x (Default =
  TRUE), important to check if formatting is in one level.

- klass:

  Deprecated; use \`classification\` instead.

## Value

A vector or data frame is returned with names and/or code of the desired
output level.

## Examples

``` r
data(klassdata)
kommune_names <- apply_klass(
  x = klassdata$kommune,
  classification = 131,
  language = "en",
  format = FALSE
)
```
