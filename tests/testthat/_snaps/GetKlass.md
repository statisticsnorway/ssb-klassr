# get_klass gives an informative error message when a variant is invalid

    Code
      get_klass(classification = 6, variant = 1, date = "2021-01-02")
    Condition
      Error in `f()`:
      ! x Failed to retrieve data from Klass
      i 404 Not Found when requesting https://data.ssb.no/api/klass/v1/variants/1
      i Klass responded: Classification Variant not found with id = 1

# get_klass gives an informative error message when a correspondence is missing

    Code
      get_klass(classification = 131, correspond = 556, date = "2020-01-01")
    Condition
      Error in `f()`:
      ! x Failed to retrieve data from Klass
      i 404 Not Found when requesting https://data.ssb.no/api/klass/v1/classifications/131/correspondsAt
      i Klass responded: Classification 'Standard for kommuneinndeling' has no correspondence table with Classification 'Kodeliste for avslutning av kvalifiseringsprogram, KVP'

# get_klass gives an informative error message when a date is too old

    Code
      get_klass(classification = 131, date = "1600-01-01")
    Condition
      Error in `get_klass()`:
      ! No codes were found for classification 131 with the current search parameters. The specified date (1600-01-01) may be too early.

