
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCMStools

<!-- badges: start -->
<!-- badges: end -->

This package includes functions that I use frequently in my LC/MS or
HPLC data flow.

## Installation

You can install the development version of LCMStools like so:

``` r
devtools::install_github('jmorim/LCMStools')
```

## Example

ConvertDtoMZML converts an Agilent MS1 data file (MS2 coming soon) to
mzML. mzML files can be read by packages like MetaboAnalyst and xcms.

``` r
library(LCMStools)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.3.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)

# Convert .d files to .mzML
file = system.file(
  'extdata', '_d/0001.d', package='LCMStools'
)
convertDtoMZML(file=file, path.out=paste0(file, '/_mzML'))
#> [1] 0

# Calculate ppm windows
calcPPM(mz=198.1002, ppm = 20)
#> [1] 198.0962 198.1042

# Parsing sample info
file = system.file(
  'extdata', '_d/0001.d/AcqData/sample_info.xml', package='LCMStools'
)
parseSampleInfo(file)
#>                                                           Sample ID 
#>                                                                  "" 
#>                                                         Sample Name 
#>                                                              "200a" 
#>                                                           Rack Code 
#>                                                                  "" 
#>                                                       Rack Position 
#>                                                                  "" 
#>                                                          Plate Code 
#>                                                       "PlateOrVial" 
#>                                                      Plate Position 
#>                                                                  "" 
#>                                                     Sample Position 
#>                                                             "P3-A1" 
#>                                                              Method 
#> "D:\\MassHunter\\Methods\\morimoto\\meth_dev\\caffeine\\opt_dmrm.m" 
#>                                                  Override DA Method 
#>                                                                  "" 
#>                                                           Data File 
#>                                      "D:\\MassHunter\\Data\\0001.d" 
#>                                                         Sample Type 
#>                                                       "Calibration" 
#>                                                         Method Type 
#>                                                  "Acquisition Only" 
#>                                                    Balance Override 
#>                                                       "No Override" 
#>                                                        Inj Vol (µl) 
#>                                                                 "1" 
#>                                                  Equilib Time (min) 
#>                                                                 "0" 
#>                                                            Dilution 
#>                                                                 "1" 
#>                                                              Wt/Vol 
#>                                                                 "0" 
#>                                                             Comment 
#>                                                                  "" 
#>                                                             Barcode 
#>                                                                  "" 
#>                                                          Level Name 
#>                                                               "200" 
#>                                                         SampleGroup 
#>                                                                  "" 
#>                                                   SampleInformation 
#>                                                                  "" 
#>                                                          StreamInfo 
#>                                                              "LC 1" 
#>                                                             AcqTime 
#>                                 "2024-02-28T22:44:03.2325168-05:00" 
#>                                                 SampleLockedRunMode 
#>                                                                 "0" 
#>                                                    RunCompletedFlag 
#>                                                                "-1" 
#>                                                        OperatorName 
#>                                                                  "" 
#>                                                      InstrumentName 
#>                                                         "MIT 6495A" 
#>                                            CombinedExportOutputFile 
#>                                                                  ""

# Parsing MS method info
# single file
file = system.file(
  'extdata', '_d/0001.d/AcqData/opt_dmrm.m/192_1.xml', package='LCMStools'
)
ms.method <- parseMSMethod(xml.file=file)
ms.method$timeSegments$scanSegments[[1]]$scanElements
#> [[1]]
#> # A tibble: 1 × 21
#>   index compoundName isISTD ms1LowMz ms1Res ms2LowMz ms2Res dwell fragmentor
#>   <chr> <chr>        <chr>  <chr>    <chr>  <chr>    <chr>  <chr> <chr>     
#> 1 1     Caffeine     false  195      Unit   138      Unit   200   380       
#> # ℹ 12 more variables: collisionEnergy <chr>, deltaEMV <chr>,
#> #   cellAccVoltage <chr>, isPrimaryMRM <chr>, isTriggerMRM <chr>,
#> #   ignoreMRM <chr>, triggerMRMInfo <list>, funnelRFVoltageHP <chr>,
#> #   funnelRFVoltageLP <chr>, faimsCV <chr>, faimsDV <chr>, compoundGroup <chr>
#> 
#> [[2]]
#> # A tibble: 1 × 21
#>   index compoundName isISTD ms1LowMz ms1Res ms2LowMz ms2Res dwell fragmentor
#>   <chr> <chr>        <chr>  <chr>    <chr>  <chr>    <chr>  <chr> <chr>     
#> 1 1     Caffeine     false  195      Unit   110      Unit   200   380       
#> # ℹ 12 more variables: collisionEnergy <chr>, deltaEMV <chr>,
#> #   cellAccVoltage <chr>, isPrimaryMRM <chr>, isTriggerMRM <chr>,
#> #   ignoreMRM <chr>, triggerMRMInfo <list>, funnelRFVoltageHP <chr>,
#> #   funnelRFVoltageLP <chr>, faimsCV <chr>, faimsDV <chr>, compoundGroup <chr>
#> 
#> [[3]]
#> # A tibble: 1 × 21
#>   index compoundName isISTD ms1LowMz ms1Res ms2LowMz ms2Res dwell fragmentor
#>   <chr> <chr>        <chr>  <chr>    <chr>  <chr>    <chr>  <chr> <chr>     
#> 1 1     Theophyllne  false  181      Unit   124      Unit   200   380       
#> # ℹ 12 more variables: collisionEnergy <chr>, deltaEMV <chr>,
#> #   cellAccVoltage <chr>, isPrimaryMRM <chr>, isTriggerMRM <chr>,
#> #   ignoreMRM <chr>, triggerMRMInfo <list>, funnelRFVoltageHP <chr>,
#> #   funnelRFVoltageLP <chr>, faimsCV <chr>, faimsDV <chr>, compoundGroup <chr>
#> 
#> [[4]]
#> # A tibble: 1 × 21
#>   index compoundName isISTD ms1LowMz ms1Res ms2LowMz ms2Res dwell fragmentor
#>   <chr> <chr>        <chr>  <chr>    <chr>  <chr>    <chr>  <chr> <chr>     
#> 1 1     Theophyllne  false  181      Unit   96       Unit   200   380       
#> # ℹ 12 more variables: collisionEnergy <chr>, deltaEMV <chr>,
#> #   cellAccVoltage <chr>, isPrimaryMRM <chr>, isTriggerMRM <chr>,
#> #   ignoreMRM <chr>, triggerMRMInfo <list>, funnelRFVoltageHP <chr>,
#> #   funnelRFVoltageLP <chr>, faimsCV <chr>, faimsDV <chr>, compoundGroup <chr>

# multiple files
system.file("extdata", "_d", package = "LCMStools") |>
  dir(pattern = "192_1.xml", recursive = TRUE, full.names = TRUE) |>
  map_df(parseMSMethod)
#> # A tibble: 3 × 15
#>   version msInstrument ionSource tuneFile       stopMode stopTime collisionGasOn
#>   <chr>   <chr>        <chr>     <chr>          <chr>    <chr>    <chr>         
#> 1 2012002 QQQ_G6495A   XESI      "D:\\MassHunt… ByPumpT… 1        true          
#> 2 2012002 QQQ_G6495A   XESI      "D:\\MassHunt… ByPumpT… 1        true          
#> 3 2012002 QQQ_G6495A   XESI      "D:\\MassHunt… ByPumpT… 1        true          
#> # ℹ 8 more variables: APPILampOn <chr>, isTimeFilterEnabled <chr>,
#> #   timeFilterPeakWidth <chr>, timeSegments <tibble[,7]>,
#> #   instrumentCurves <chr>, chromatograms <tibble[,6]>,
#> #   beginDMRMWasteDivValveStartTime <chr>, endDMRMWasteDivValveStartTime <chr>
```
