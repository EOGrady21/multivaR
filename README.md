
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multivaR

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/Echisholm21/multivaR.svg?branch=master)](https://travis-ci.com/Echisholm21/multivaR)
<!-- badges: end -->

The goal of multivaR is to organize and simplify the process of creating
a multivariate analysis. This package can format data as well as
caluclate anomalies from raw data, once data is formatted the package
can be used to run Principal Components Analysis on multiple variables.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Echisholm21/multivaR")
```

multivaR is designed to work with other packages developed by the Data
Access team at Bedford Institute of Oceanogrpahy (BIO). Specifically,
this package is designed to work with data which is supplied through the
`azmpdata` package. This supplementary package can be installed by
using:

``` r
devtools::install_github("casaultb/azmpdata")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(multivaR)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
library(devtools)
#> Loading required package: usethis
## basic example code

# step 1: assemble data

# search for azmpdata
# if azmpdata is not downloaded:
if(!'azmpdata' %in% installed.packages()){devtools::install_github("casaultb/azmpdata")}
library(azmpdata)
#> 
#>  casaultb/azmpdata status:
#>  (Package ver: 0.2019.0.9100) Up to date
#>  (Data ver:2021-01-14 ) is up to date
query(search_key = '')
#>  [1] "Derived_Annual_Broadscale"          "Derived_Annual_Sections"           
#>  [3] "Derived_Annual_Stations"            "Derived_Monthly_Broadscale"        
#>  [5] "Derived_Monthly_Stations"           "Derived_Occupations_Sections"      
#>  [7] "Derived_Occupations_Stations"       "Discrete_Annual_Broadscale"        
#>  [9] "Discrete_Occupations_Sections"      "Discrete_Occupations_Stations"     
#> [11] "Phytoplankton_Annual_Stations"      "Phytoplankton_Occupations_Stations"
#> [13] "RemoteSensing_Annual_Broadscale"    "RemoteSensing_Weekly_Broadscale"   
#> [15] "Zooplankton_Annual_Sections"        "Zooplankton_Annual_Stations"       
#> [17] "Zooplankton_Occupations_Broadscale" "Zooplankton_Occupations_Sections"  
#> [19] "Zooplankton_Occupations_Stations"   "Zooplankton_Seasonal_Broadscale"   
#> [21] "Zooplankton_Seasonal_Sections"

# load in desired data frames
data("Derived_Occupations_Sections")
data("Zooplankton_Occupations_Sections")

# collate data frames

datfr <- right_join(Derived_Occupations_Sections, Zooplankton_Occupations_Sections) 
#> Joining, by = c("section", "station", "latitude", "longitude", "year", "month", "day")
```

If anomalies need to be calculated, multivaR has options for calculating
monthly, annual or seasonal anomalies with or without normalization.
Anomaly calculations use a reference period to calculate a climatology
(`climatologyYears`). This reference period is typically standardized,
please check that you are using the current standard climatology period
that is relevant to your data.

``` r
# monthly anomalies must be caluclated before annual anomalies (annual anomaly function simply combines monthly anomalies)

danomalies <- monthlyAnomaly(datfr, climatologyYears = c(1999, 2010), var = 'integrated_chlorophyll_0_100')

# annual anomalies function creates new data frame with just year and variable
chl_annual_anomalies <- annualAnomaly(danomalies, anomaly = 'integrated_chlorophyll_0_100_anomaly')

# if you are unsure which anomaly calculation to use you can use the wrapper function which will select the appropriate method 
anom_1 <- calculate_anomaly(data = datfr, anomalyType = 'monthly', climatologyYears = c(1999, 2010), var = 'integrated_chlorophyll_0_100', normalizedAnomaly = TRUE)
head(anom_1)
#>   section station latitude longitude year month day           event_id
#> 1     CSL    CSL1    46.95    -60.21 1999     4  15 18HU99005_99003155
#> 2     CSL    CSL2    47.02    -60.11 1999     4  15 18HU99005_99003161
#> 3      LL     LL2    45.65    -59.70 1999     4  15 18HU99005_99003146
#> 4      LL     LL6    44.47    -58.50 1999     4  14 18HU99005_99003115
#> 5      LL     LL7    44.13    -58.17 1999     4  14 18HU99005_99003109
#> 6      HL     HL5    43.18    -62.10 1999     4  12 18HU99005_99003091
#>   integrated_chlorophyll_0_100 integrated_nitrate_0_50
#> 1                      671.480                 88.2700
#> 2                      494.475                139.3200
#> 3                      270.295                135.9600
#> 4                       52.305                 36.8075
#> 5                       65.820                116.7955
#> 6                       79.950                201.3410
#>   integrated_nitrate_50_150 integrated_phosphate_0_50
#> 1                   129.750                   21.4750
#> 2                  1055.200                   16.5260
#> 3                   882.540                   16.9040
#> 4                    19.305                   15.8745
#> 5                  1325.300                   14.9350
#> 6                   655.405                   17.8340
#>   integrated_phosphate_50_150 integrated_silicate_0_50
#> 1                     11.6575                  382.905
#> 2                     57.1350                  160.415
#> 3                     52.2885                  141.743
#> 4                      5.3650                   83.653
#> 5                     44.4650                   89.151
#> 6                     28.1420                  137.347
#>   integrated_silicate_50_150 season               sample_id
#> 1                    152.705 Spring 99003157_99003102027002
#> 2                   1318.007 Spring 99003163_99003102028002
#> 3                   1014.163 Spring 99003142_99003102025002
#> 4                     42.676 Spring 99003117_99003102021002
#> 5                   1077.957 Spring 99003111_99003102020002
#> 6                    552.340 Spring 99003093_99003102017002
#>   Calanus_finmarchicus_abundance Pseudocalanus_abundance copepods non_copepods
#> 1                       1417.526                19845.36 174355.7   175780.000
#> 2                      16743.756                45306.63 256081.0    55155.901
#> 3                      38207.547                29716.98 237735.8    72196.981
#> 4                      22727.273                26223.78 192307.7   164335.664
#> 5                      42459.239                55197.01 362318.8   141621.377
#> 6                      49029.271                23416.97 172700.1     7317.802
#>   Arctic_Calanus_species warm_offshore_copepods warm_shelf_copepods
#> 1               8505.155                  0.000              0.0000
#> 2              26593.024                  0.000              0.0000
#> 3              26886.792                  0.000              0.0000
#> 4               1748.252                  0.000              0.0000
#> 5              15568.388               1415.308              0.0000
#> 6                  0.000                  0.000            731.7802
#>   zooplankton_meso_dry_weight zooplankton_macro_dry_weight
#> 1                          NA                           NA
#> 2                          NA                           NA
#> 3                          NA                           NA
#> 4                          NA                           NA
#> 5                          NA                           NA
#> 6                          NA                           NA
#>   zooplankton_total_dry_weight zooplankton_meso_wet_weight
#> 1                           NA                          NA
#> 2                           NA                          NA
#> 3                           NA                          NA
#> 4                           NA                    3.606364
#> 5                           NA                   28.001359
#> 6                           NA                   11.201718
#>   zooplankton_macro_wet_weight zooplankton_total_wet_weight
#> 1                   0.09276289                           NA
#> 2                           NA                           NA
#> 3                   1.13366038                           NA
#> 4                           NA                     3.604545
#> 5                   4.51675725                    32.518116
#> 6                           NA                    10.857130
#>   integrated_chlorophyll_0_100_normalizedAnomaly
#> 1                                      2.0232446
#> 2                                      1.2685040
#> 3                                      0.3126115
#> 4                                     -0.6168871
#> 5                                     -0.5592598
#> 6                                     -0.4990101
#>   integrated_chlorophyll_0_100_anomaly
#> 1                              474.500
#> 2                              297.495
#> 3                               73.315
#> 4                             -144.675
#> 5                             -131.160
#> 6                             -117.030


anom_2 <- calculate_anomaly(data = datfr, anomalyType = 'seasonal', climatologyYears = c(1999, 2010))
head(anom_2)
#>   type_name year season                       variable       value
#> 1       CSL 1999 Spring Calanus_finmarchicus_abundance -1.22078967
#> 2       CSL 1999 Spring Calanus_finmarchicus_abundance  0.77077700
#> 3        LL 1999 Spring Calanus_finmarchicus_abundance  0.39701826
#> 4        LL 1999 Spring Calanus_finmarchicus_abundance -0.06118276
#> 5        LL 1999 Spring Calanus_finmarchicus_abundance  0.52286420
#> 6        HL 1999 Spring Calanus_finmarchicus_abundance  0.02582455
```

Once data is assembled, PCA can be run

``` r
PCA(datfr[,9:15]) # subset dataframe to exclude metadata columns
#> Standard deviations (1, .., p=7):
#> [1] 1.6848330 1.4258073 1.0268635 0.8831113 0.3845731 0.3160633 0.2151381
#> 
#> Rotation (n x k) = (7 x 7):
#>                                      PC1        PC2         PC3         PC4
#> integrated_chlorophyll_0_100 -0.04458037 -0.4233590  0.74692663 -0.10672075
#> integrated_nitrate_0_50      -0.34682790 -0.2429350 -0.26962152  0.75247504
#> integrated_nitrate_50_150    -0.54580652  0.2260193  0.09348122  0.11407522
#> integrated_phosphate_0_50    -0.17981267 -0.4813154 -0.50649442 -0.42394932
#> integrated_phosphate_50_150  -0.50517512  0.1272963 -0.12146469 -0.47696671
#> integrated_silicate_0_50     -0.12684758 -0.6604202  0.06991854  0.04443296
#> integrated_silicate_50_150   -0.52554109  0.1632217  0.29066689 -0.01320110
#>                                      PC5        PC6         PC7
#> integrated_chlorophyll_0_100  0.41206803  0.2742135 -0.06706635
#> integrated_nitrate_0_50       0.31874123  0.1718770  0.22503531
#> integrated_nitrate_50_150     0.03315179 -0.1881391 -0.76991120
#> integrated_phosphate_0_50    -0.06599948  0.4784064 -0.25788486
#> integrated_phosphate_50_150   0.45829981 -0.3287557  0.41015096
#> integrated_silicate_0_50     -0.38319425 -0.6259318  0.04757690
#> integrated_silicate_50_150   -0.60520447  0.3621123  0.33927273
```

PCA results can be presented, plotted and analyzed as usual, using
packages outside of `multivaR`.
