# Internal function to create URL address

Internal function to create URL address

## Usage

``` r
MakeUrl(
  classification,
  correspond = NULL,
  correspondID = NULL,
  variant_name = NULL,
  type = "vanlig",
  fratil = FALSE,
  date = NULL,
  output_level_coding = NULL,
  language_coding = NULL
)
```

## Arguments

- classification:

  Classification number

- correspond:

  Target number for correspondence table

- variant_name:

  The name of the variant of the classification

- type:

  String describing type. "vanlig" for normal classification and "kor"
  for correspondence. Default = "vanlig"

- fratil:

  True/False for whether a date interval is to be used. Default = False

- date:

  Date(s) for classification

- output_level_coding:

  Coding for output level

- language_coding:

  Coding for language

## Value

String url adress
