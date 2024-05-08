---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# LCMStools

<!-- badges: start -->
<!-- badges: end -->

This package includes functions that I use frequently in my LC/MS or HPLC data flow.

## Installation

You can install the development version of LCMStools like so:

``` r
devtools::install_github('jmorim/LCMStools')
```

## Example

ConvertDtoMZML converts an Agilent MS1 data file (MS2 coming soon) to mzML.
mzML files can be read by packages like MetaboAnalyst and xcms.


```r
library(LCMStools)

ConvertDtoMZML(path='data', path.out='data/_mzML')
#> [1] "file.paths: "
#> [1] "cmd:  "
#> Error in system(command, as.integer(flag), f, stdout, stderr, timeout): character string expected as first argument
```
