# Testdata for klassR package

A dataset containing variables for testing of Statistics Norways
classification API with the klassR package. Some observations are
missing or incorrect for testing and demonstrations.

## Usage

``` r
klassdata
```

## Format

A data frame containing 100 rows and 7 variables:

- ID:

  Identification number

- sex:

  1/2 variable for sex

- education:

  4-digit number for education standard ISCED97 (level and subject area)
  NUS (klass = 66) 2015.01.01

- kommune:

  4-digit code for Norwegian municipality (klass = 131). Based on
  2015.01.01

- kommune2:

  Numeric variable for Norwegian municipality with dropped leading
  zero's for testing (klass = 131). Based on 2015.01.01

- nace5:

  5-digit code for industry (NACE). Based on 01.01.2015 standard
  industry codes (klass = 7)

- occupation:

  4-digit occupation codes using standard for STYRK-08 (klass = 7)
  2015.01.01
