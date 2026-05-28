# Introduction to klassR

## 1. Introduction

Do you have a Norwegian data set with codes for Standard Industrial
Classification that you want to find out what they mean? Or data with
Norwegian municipality numbers and no names? Or perhaps you want to
convert English standard occupations into Ny Norsk for a figure. These
are tasks which the R package **klassR** can help you with.

Statistics Norway’s [*KLASS*](https://www.ssb.no/en/klass/) is a central
database of classifications and code lists. An API makes it easy to
fetch these standards in different computing environments. **klassR**
provides an easy interface to fetch and apply these in R.

For Statistic Norway employees, the package is installed on most of our
platforms. For others, it can be installed from CRAN with:

``` r

install.packages("klassR")
```

CRAN is R’s central repository for thousands of useful packages. More
information on the requirements for **klassR** can be found on
[CRAN](https://CRAN.r-project.org/package=klassR)

To use the function in **klassR** the package must be called each time a
new R session is started. This can be done using:

``` r

library(klassR)
```

## 2. Search for a classification

To fetch a classification from *KLASS* you need the unique
classification number. This can be found in the URL of the *KLASS*
website or you can search for it in R using one of the following
functions.

### List all classifications

The function `list_klass` will fetch a list of all classifications. It
returns the classification name (`klass_name`), number (`klass_nr`) and
the classification family it belongs to (`klass_family`). The
classification type (`klass_type`) is also shown which indicates whether
it is a classification or code list.

``` r

list_klass()
```

| klass_name | klass_nr | klass_family | klass_type |
|:---|:---|:---|:---|
| Standard for yrkesklassifisering | 7 | 1 | Klassifikasjon |
| Standard for kjønn | 2 | 3 | Klassifikasjon |
| Standard for gruppering av familier | 17 | 3 | Klassifikasjon |
| Standard for sivilstand | 19 | 3 | Klassifikasjon |
| Standard for gruppering av husholdninger | 37 | 3 | Klassifikasjon |
| Standard for aldersinndeling | 282 | 3 | Klassifikasjon |

Code lists are classifications that used for national and internal
(Statistics Norway) publications. These can be included in the list
using the `codelist` parameter

``` r

list_klass(codelists = TRUE)
```

| klass_name | klass_nr | klass_family | klass_type |
|:---|:---|:---|:---|
| Standard for yrkesklassifisering | 7 | 1 | Klassifikasjon |
| Kodeliste for arbeidstid (hel-/deltid) | 149 | 1 | Kodeliste |
| Kodeliste for arbeidsmarkedsstatus | 161 | 1 | Kodeliste |
| Kodeliste for arbeidsgiveravgiftstype | 162 | 1 | Kodeliste |
| Kodeliste for delpopulasjon for lønn og sysselsetting | 163 | 1 | Kodeliste |
| Kodeliste for arbeidsforhold | 164 | 1 | Kodeliste |

### Search for a classification using a keyword

You can also search for a classification by a keyword using the
`search_klass` function. The first parameter here is the query to search
for.

``` r

search_klass(query = "ARENA")
```

| klass_name | klass_nr |
|:---|:---|
| Classification of Residential areas | 567 |
| Classification of STN area | 599 |
| Classification of maritime Areas | 892 |
| Classification of wild reindeer areas | 75 |
| Classification of agricultural area by use | 27 |
| Classification of major fishing areas (FAO) | 32 |
| Classification of Service areas - Emergency rooms | 435 |
| Classification of coastal and marine management areas | 895 |
| Classification of important Norwegian fishing areas | 33 |
| Classification of agricultural holdings by size of utilised agricultural area | 378 |
| Classification of productive forest area by development class | 70 |
| Classification of land use and land cover | 118 |
| Classification of productive forest area by site quality (H40) | 71 |
| Classification of forest owners with at last 25 decares productive forest area by owner group | 22 |
| Classification of type of building /cadastre | 31 |
| Classification of Standard Industrial Classification | 6 |
| Classification of Land classification system in the Norwegian general map series at scale 1:5 000 - 1:10 000 | 28 |
| Classification of energy products | 117 |
| Classification of target species in the sea | 34 |
| Classification of ports in Norway, goods and passenger transport | 398 |

Again, to include code lists in the search this should be specified

``` r

search_klass(query = "ARENA", codelists = TRUE)
```

| klass_name | klass_nr |
|:---|:---|
| Kodeliste for ARENA as_ytelse | 394 |
| Kodeliste for ARENA Tiltak | 386 |
| Kodeliste for ARENA as_f (arbeidssøkerstatus_fingruppe ) | 396 |
| Kodeliste for ARENA as_gr (arbeidssøkerstatus grovgruppe) | 395 |
| Kodeliste for ARENA as_stat (arbeidssøkerstatus, aktivitet og ytelse) | 393 |
| Classification of Residential areas | 567 |
| Codelist for nature hazard area and nature hazard suceptibility area | 723 |
| Classification of STN area | 599 |
| Classification of maritime Areas | 892 |
| Classification of wild reindeer areas | 75 |
| Codelist for Postal code areas | 616 |
| Classification of agricultural area by use | 27 |
| Codelist for nature hazard susceptibility areas | 721 |
| Classification of major fishing areas (FAO) | 32 |
| Classification of Service areas - Emergency rooms | 435 |
| Classification of coastal and marine management areas | 895 |
| Codelist for functional urban areas (FUA) | 551 |
| Classification of important Norwegian fishing areas | 33 |
| Codelist for property tax area in the municipalities | 260 |
| Codelist for Accessible areas for recreation and outdoor activities. | 305 |

Sometimes a classification or code list will appear several times. This
is due to that it occurs several times in different langauges in the
database.

## 3. Fetch a classification

To fetch a complete classification, use the `get_klass` function
together with the unique identifier. For example, to fetch the Standard
Industrial Classifications (*KLASS* number 6) we run:

``` r

industry <- get_klass(6)
head(industry)
```

| code | parentCode | level | name |
|:---|:---|:---|:---|
| 01 | A | 2 | Jordbruk og tjenester tilknyttet jordbruk, jakt og viltstell |
| 01.1 | 01 | 3 | Dyrking av ettårige vekster |
| 01.11 | 01.1 | 4 | Dyrking av korn, unntatt ris, belgvekster og oljeholdige vekster |
| 01.110 | 01.11 | 5 | Dyrking av korn, unntatt ris, belgvekster og oljeholdige vekster |
| 01.12 | 01.1 | 4 | Dyrking av ris |
| 01.120 | 01.12 | 5 | Dyrking av ris |

### Level

Classifications are often organised in a heirachical way. In the example
above, the Standard Industrial Classifications have different values for
*level*. To fetch a specific level, use the `output_level` parameter.
For example, to fetch only the top level Standard Industrial
Classification codes we use:

``` r

industry <- get_klass(6, output_level = 1)
head(industry)
```

| code | parentCode | level | name |
|:---|:---|:---|:---|
| A | NA | 1 | Jordbruk, skogbruk og fiske |
| B | NA | 1 | Bergverksdrift og utvinning |
| C | NA | 1 | Industri |
| D | NA | 1 | Forsyning av elektrisitet, gass, damp og kjøleluft |
| E | NA | 1 | Vannforsyning, avløps-, renovasjons- og oppryddingsvirksomhet |
| F | NA | 1 | Bygge- og anleggsvirksomhet |

### Language

In the above examples we have seen that the names are returned in
Norwegian (Bokmål). However, many of the classification in *KLASS* are
in multiple languages. The output language can be specified as Bokmål
(“nb”), Nynorsk (“nn”) or English (“en”) using the `language` parameter.
*Note: all 3 languages are not available for all classifcations.*

``` r

industry <- get_klass(6, output_level = 1, language = "en")
head(industry)
```

| code | parentCode | level | name |
|:---|:---|:---|:---|
| A | NA | 1 | Agriculture, forestry and fishing |
| B | NA | 1 | Mining and quarrying |
| C | NA | 1 | Manufacturing |
| D | NA | 1 | Electricity, gas, steam and air conditioning supply |
| E | NA | 1 | Water supply; sewerage, waste management and remediation activities |
| F | NA | 1 | Construction |

### Output format

The standard output style is ‘long’ where all levels of classifications
are listed down. An alternative format can be chosen using the parameter
`output_style='wide'`. This will give only one row per detailed
classification with the codes and names of the higher/broader levels
given as variables.

``` r

industry <- get_klass(6, output_style = "wide", language = "en")
head(industry, 2)
```

|  | code5 | name5 | code4 | name4 | code3 | name3 | code2 | name2 | code1 | name1 |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 4 | 01.110 | Growing of cereals, other than rice, leguminous crops and oil seeds | 01.11 | Growing of cereals, other than rice, leguminous crops and oil seeds | 01.1 | Growing of non-perennial crops | 01 | Crop and animal production, hunting and related service activities | A | Agriculture, forestry and fishing |
| 6 | 01.120 | Growing of rice | 01.12 | Growing of rice | 01.1 | Growing of non-perennial crops | 01 | Crop and animal production, hunting and related service activities | A | Agriculture, forestry and fishing |

### Notes

Some classifications have additional notes that can be fetched with the
classification. These can be included in the data using the option
`notes = T`.

``` r

industry <- get_klass(6, notes = T)
head(industry, 2)
```

| code | parentCode | level | name | notes |
|:---|:---|:---|:---|:---|
| 01 | A | 2 | Jordbruk og tjenester tilknyttet jordbruk, jakt og viltstell | Omfatter: Denne næringen omfatter to basisaktiviteter, produksjon av vegetabilske og animalske produkter. Næringshovedgruppe 01.5 Kombinert husdyrhold og planteproduksjon bryter med de vanlige prinsippene for identifisering av hovedaktivitet. Det er aksept for at mange gårder har et rimelig balansert forhold mellom plante- og husdyrproduksjon, og at en klassifisering i den ene eller andre kategorien vil bli tilfeldig. Næringen inkluderer jordløs dyrking, for eksempel ved bruk av hydroponiske og akvaponiske dyrkingsmetoder. I jordbruket kan det by på utfordringer ved fordelingen av verdiskapingen når en virksomhet både produserer en råvare og produserer en bearbeidet vare fra den egenproduserte råvaren. For eksempel når en virksomhet dyrker epler og produserer sider fra de selvdyrkede eplene, eller når en virksomhet dyrker bær og produserer syltetøy fra de selvdyrkede bærene. I slike tilfeller vil antall timer arbeidet være en passende erstatningsvariabel. Bruk av antall timer arbeidet ved slike vertikalt integrerte aktiviteter vil vanligvis føre til klassifisering av virksomheten i jordbruk. Det samme vil være tilfelle for andre jordbruksprodukter, der virksomheter vanligvis blir klassifisert i jordbruk, for å sikre likebehandling. Omfatter også: Næringen omfatter også tjenester tilknyttet jordbruk, jakt og fangst. Ekskluderer: Jordbruksaktiviteter omfatter normalt ikke bearbeiding av jordbruksprodukter (klassifisert i næringene 10, 11 og 12) med unntak av den bearbeiding som er nødvendig for å få omsatt varene på det primære markedet. En slik bearbeiding er inkludert her. Grunnarbeid, for eksempel anlegg av jordterrasser, drenering og lignende, grupperes under næringshovedområde F. Kjøpere og andelslag som driver med markedsføring av jordbruksprodukter grupperes under næringshovedområde G. Stell og vedlikehold av landskap grupperes under 81.30 Beplantning av hager og parkanlegg. |
| 01.1 | 01 | 3 | Dyrking av ettårige vekster | Omfatter: Denne næringshovedgruppen omfatter dyrking av ettårige vekster, dvs. Planter som ikke varer i mer enn to vekstsesonger. Den omfatter også dyrking av ettårige vekster med henblikk på produksjon av såfrø og såkorn. |

## 4. Applying a classification

If you have a data set and want to apply a classification to a variable
this is possible to do with `apply_klass`. This can be used to get the
name of a variable which is in code form for example.

There is a built in test data set in **klassR** called `klassdata`. It
contains fictitious persons with sex, education level, municipality
numbers, industry classification for workplace and occupation.

``` r

data(klassdata)
head(klassdata)
```

|  ID | sex | education | kommune | kommune2 | nace5 | occupation |
|----:|:----|:----------|:--------|:---------|:------|:-----------|
|   1 | 2   | 2799      | 0706    | 706      | 47710 | 5132       |
|   2 | 2   | 5620      | 1567    | 1567     | 86902 | NA         |
|   3 | 1   | 4010      | 1903    | 1903     | 41200 | 4177       |
|   4 | 1   | 1799      | 1003    | 1003     | 84120 | 3114       |
|   5 | 2   | NA        | 0806    | 806      | 87102 | 2411       |
|   6 | 1   | 5621      | 0301    | 301      | 88911 | 8141       |

We can use `apply_klass` to create a variable for the municipality names
(classification number 131) for the persons based on the codes. We
specify the vector of codes as the first parameter followed by the
unique classification number.

``` r

klassdata$kommune_names <- apply_klass(klassdata$kommune,
  classification = 131
)
head(klassdata)
```

|  ID | sex | education | kommune | kommune2 | nace5 | occupation | kommune_names |
|----:|:----|:----------|:--------|:---------|:------|:-----------|:--------------|
|   1 | 2   | 2799      | 0706    | 706      | 47710 | 5132       | Sandefjord    |
|   2 | 2   | 5620      | 1567    | 1567     | 86902 | NA         | Rindal        |
|   3 | 1   | 4010      | 1903    | 1903     | 41200 | 4177       | Harstad       |
|   4 | 1   | 1799      | 1003    | 1003     | 84120 | 3114       | Farsund       |
|   5 | 2   | NA        | 0806    | 806      | 87102 | 2411       | Skien         |
|   6 | 1   | 5621      | 0301    | 301      | 88911 | 8141       | Oslo          |

Again, the `language` and `output_level` can be specified.

## 5. Working with dates

Classifications will often change over time. The *KLASS* database
considers this and older classifications can be fetched using the `date`
parameter.

### Specify a specific date

Fetching or using a classification at a specific time point can be done
using the `date` parameter and specifying the date for which the version
of classification applies. The date format should be in the form
“yyyy-mm-dd”, for example “2022-05-27” for the 27th May, 2022.

There have been many changes to the regions in Norway (classification
number 106) over the past few years. We can see this by fetching the
classifications for these at different times

``` r

get_klass(106, date = "2019-01-01")
```

| code | parentCode | level | name               |
|:-----|:-----------|:------|:-------------------|
| 1    | NA         | 1     | Oslo og Akershus   |
| 2    | NA         | 1     | Hedmark og Oppland |
| 3    | NA         | 1     | Sør-Østlandet      |
| 4    | NA         | 1     | Agder og Rogaland  |
| 5    | NA         | 1     | Vestlandet         |
| 6    | NA         | 1     | Trøndelag          |
| 7    | NA         | 1     | Nord-Norge         |
| 9    | NA         | 1     | Uoppgitt           |

``` r

get_klass(106, date = "2020-01-01")
```

| code | parentCode | level | name                   |
|:-----|:-----------|:------|:-----------------------|
| 1    | NA         | 1     | Oslo og Viken          |
| 2    | NA         | 1     | Innlandet              |
| 3    | NA         | 1     | Agder og Sør-Østlandet |
| 4    | NA         | 1     | Vestlandet             |
| 5    | NA         | 1     | Trøndelag              |
| 6    | NA         | 1     | Nord-Norge             |
| 9    | NA         | 1     | Uoppgitt               |

### Time intervals

Sometime it may be useful to fetch all codes over a period of time. We
can do this by specifing two dates as a vector in the `date` paramter.

The following code fetched Norwegian regional codes between 1st January
2019 to the 1st January 2020. There are 26 different codes that show
both old and newer names.

``` r

get_klass(106, date = c("2019-01-01", "2020-01-01"))
```

| code | parentCode | level | name | validFromInRequestedRange | validToInRequestedRange |
|:---|:---|:---|:---|:---|:---|
| 1 | NA | 1 | Oslo og Akershus | 2018-01-01 | 2020-01-01 |
| 1 | NA | 1 | Oslo og Viken | 2020-01-01 | 2020-01-02 |
| 2 | NA | 1 | Innlandet | 2020-01-01 | 2020-01-02 |
| 2 | NA | 1 | Hedmark og Oppland | 2018-01-01 | 2020-01-01 |
| 3 | NA | 1 | Agder og Sør-Østlandet | 2020-01-01 | 2020-01-02 |
| 3 | NA | 1 | Sør-Østlandet | 2018-01-01 | 2020-01-01 |
| 4 | NA | 1 | Vestlandet | 2020-01-01 | 2020-01-02 |
| 4 | NA | 1 | Agder og Rogaland | 2018-01-01 | 2020-01-01 |
| 5 | NA | 1 | Vestlandet | 2018-01-01 | 2020-01-01 |
| 5 | NA | 1 | Trøndelag | 2020-01-01 | 2020-01-02 |
| 6 | NA | 1 | Trøndelag | 2018-01-01 | 2020-01-01 |
| 6 | NA | 1 | Nord-Norge | 2020-01-01 | 2020-01-02 |
| 7 | NA | 1 | Nord-Norge | 2018-01-01 | 2020-01-01 |
| 9 | NA | 1 | Uoppgitt | 2018-01-01 | 2020-01-02 |

### Changes in time

To fetch only the changes in a time period rather than all codes we can
specify `correspond=TRUE` allong with the time interval we are
interested in.

``` r

get_klass(106,
  date = c("2020-01-01", "2019-01-01"),
  correspond = TRUE
)
```

| sourceCode | sourceName | targetCode | targetName | changeOccurred |
|:---|:---|:---|:---|:---|
| NA | NA | 1 | Oslo og Akershus | 2020-01-01 |
| NA | NA | 3 | Sør-Østlandet | 2020-01-01 |
| NA | NA | 4 | Agder og Rogaland | 2020-01-01 |
| NA | NA | 5 | Vestlandet | 2020-01-01 |
| 1 | Oslo og Viken | NA | NA | 2020-01-01 |
| 2 | Innlandet | 2 | Hedmark og Oppland | 2020-01-01 |
| 3 | Agder og Sør-Østlandet | NA | NA | 2020-01-01 |
| 4 | Vestlandet | NA | NA | 2020-01-01 |
| 5 | Trøndelag | 6 | Trøndelag | 2020-01-01 |
| 6 | Nord-Norge | 7 | Nord-Norge | 2020-01-01 |

The table returned is a correspondents in codes and/or names in the time
interval specified. The `sourceCode` and `sourceName` refer to the
original name and coding. The `targetCode` and `targetName` refer to the
newer code and name. Notice there is not a simple 1:1 correspondence
between all of the regions. Here the municipality number would be needed
to map the changes more accurately.

### Future classification

Classification that are valid in the future are also included in
*KLASS*. They can be fetched out by specifying the future date. A
message will be shown to indicate that this is a future classification.
No additional parameters need to be specified.

## 6. Correspondence tables

In addition to small changes in time, some classifications will change
completely and a correspondence table is then defined within the *KLASS*
database. These can be fetched or applied using `get_klass` and
`apply_klass` functions together with the `correspond` parameter which
should give the unique classification number to convert into.

To fetch a correspondence table between municipality codes (131) and
greater regional codes (106) we can run:

``` r

get_klass(131, correspond = 106, date = "2020-01-01")
```

| sourceCode | sourceName  | targetCode | targetName    |
|:-----------|:------------|:-----------|:--------------|
| 0301       | Oslo        | 1          | Oslo og Viken |
| 3001       | Halden      | 1          | Oslo og Viken |
| 3002       | Moss        | 1          | Oslo og Viken |
| 3003       | Sarpsborg   | 1          | Oslo og Viken |
| 3004       | Fredrikstad | 1          | Oslo og Viken |
| 3005       | Drammen     | 1          | Oslo og Viken |

We can apply this correspondence between municipality and region in our
example data set using `apply_klass`.

``` r

klassdata$region <- apply_klass(klassdata$kommune,
  classification = 131,
  correspond = 106,
  date = "2020-01-01"
)
klassdata
```

|  ID | sex | education | kommune | kommune2 | nace5 | occupation | kommune_names | region        |
|----:|:----|:----------|:--------|:---------|:------|:-----------|:--------------|:--------------|
|   1 | 2   | 2799      | 0706    | 706      | 47710 | 5132       | Sandefjord    | NA            |
|   2 | 2   | 5620      | 1567    | 1567     | 86902 | NA         | Rindal        | NA            |
|   3 | 1   | 4010      | 1903    | 1903     | 41200 | 4177       | Harstad       | NA            |
|   4 | 1   | 1799      | 1003    | 1003     | 84120 | 3114       | Farsund       | NA            |
|   5 | 2   | NA        | 0806    | 806      | 87102 | 2411       | Skien         | NA            |
|   6 | 1   | 5621      | 0301    | 301      | 88911 | 8141       | Oslo          | Oslo og Viken |

## 7. Variants

It is also possible to fetch a variant of a classification. You need to
provide both the classification number and the variant number.

``` r

get_klass(6, variant = 1616, date = "2021-01-02")
```

| code | parentCode | level | name |
|:---|:---|:---|:---|
| 01 | 01-03 | 2 | Jordbruk og tjenester tilknyttet jordbruk, jakt og viltstell |
| 01-03 | NA | 1 | Jordbruk, skogbruk og fiske |
| 01.1 | 01 | 3 | Dyrking av ettårige vekster |
| 01.11 | 01.1 | 4 | Dyrking av korn (unntatt ris), belgvekster og oljeholdige vekster |
| 01.110 | 01.11 | 5 | Dyrking av korn (unntatt ris), belgvekster og oljeholdige vekster |
| 01.12 | 01.1 | 4 | Dyrking av ris |
