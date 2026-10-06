# get_klass gives an informative error message when a correspondence is missing

    Code
      get_klass(131, correspond = 556, date = "2020-01-01")
    Message
      Connection failed with error code 404:
      Classification 'Standard for kommuneinndeling' has no correspondence table with Classification 'Kodeliste for avslutning av kvalifiseringsprogram, KVP'
    Condition
      Error in `stop_quietly()`:

# get_klass gives an informative error message when a date is too old

    Code
      get_klass(131, date = "1600-01-01")
    Condition
      Error in `get_klass()`:
      ! No codes were found for classification 131 with the current search parameters. The specified date (1600-01-01) may be too early.

