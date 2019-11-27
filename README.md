UKgrid
======

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN status](https://www.r-pkg.org/badges/version/UKgrid)](https://cran.r-project.org/package=UKgrid)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->




An R data package with the UK [National Grid](https://en.wikipedia.org/wiki/National_Grid_(Great_Britain)) historical demand for electricity between April 2005 and October 2019


Overview
--------
The UKgrid dataset is an example of a multiple seasonality time series. This time series captures the demand for electricity and its components in the UK since April 2005 using half-hour intervals. In addition, the package provides a function to extract, subset and aggregate the series into `ts`, `xts`, `zoo`, `data.frame`, `data.table`, or `tbl`. 

The data was sourced from the National Grid UK [website](https://www.nationalgrid.com/uk)

<img src="man/figures/UK_natioal_demand.png" width="100%" />


Installation
------------

Install the stable version from [CRAN](https://CRAN.R-project.org/package=UKgrid):

``` r
install.packages("UKgrid")
```

or install the development version from [Github](https://github.com/RamiKrispin/UKgrid):

``` r
# install.packages("devtools")
devtools::install_github("RamiKrispin/UKgrid")
```


Usage
-----

``` r
library(UKgrid)

# Load the full dataset (data.frame format)
data("UKgrid")

# Extract only the demand field (ND - Net Demand) using xts format
extract_grid(type = "xts", 
             columns = "ND") 

# Extract the demand between 2016 and 2017 using tbl format
extract_grid(type = "tbl", 
             columns = "ND", 
             start = 2016, 
             end = 2017)

# Extract the first 10 days in 2018 and aggregate to hourly using zoo format
extract_grid(type = "zoo", 
             columns = "ND", 
             start = as.Date("2018-01-01"), 
             end = as.Date("2018-01-10"),
             aggregate = "hourly")
``` 
