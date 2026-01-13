### Introduction

This repository is the repo for the R-package **klassR**. The package is
designed to provide an easy interface to retrieving classifications from
Statistic Norway’s classification database.

### Quick guide

To install the `klassR` package use

``` R
install.packages('klassR')
```

You can the load the package into the environment with

``` R
library(klassR)
```

To fetch a classification, use the `get_klass` function together with
the klass number from SSBs [KLASS](https://www.ssb.no/klass/). For
example occupation classifications (klass number 7) can be fetched with

``` R
get_klass(7)
```

A more in depth introduction to **klassR** can be found at:
<https://statisticsnorway.github.io/ssb-klassr/articles/klassR-vignette.html>
