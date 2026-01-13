# For a given Klass code, produce a table of dates describing the valid-from and valid-to dates of all versions of the code

For a given Klass code, produce a table of dates describing the
valid-from and valid-to dates of all versions of the code

## Usage

``` r
find_dates(code, api_alle, api_endringer)
```

## Arguments

- code:

  A Klass code

- api_alle:

  A table of all codes in the classification. See example.

- api_endringer:

  A table of all changes in the classification See example.

## Value

A `data.frame` with number of rows equal to the number of variants of
the combination of code and name, determined by the changes the code has
been involved in. The `data.frame` has two columns:

- "validFrom"

- "validTo"
