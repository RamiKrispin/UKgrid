UKgrid
======

An R data package with the UK [National Grid](https://en.wikipedia.org/wiki/National_Grid_(Great_Britain)) historical demand for electricity between 2011 and 2018


![UK Grid](https://github.com/RamiKrispin/UKgrid/blob/master/vignettes/png/Rplot.png)


Overview
--------
The UKgrid dataset is an example of a multiple seasonality time series. This time series captures the demand for electricity and its components in the UK since 2011 using half-hour intervals. In addition, the package provides a function to extract, subset and aggregate the series into `ts`, `xts`, `zoo`, `data.frame`, `data.table`, or `tbl`. 

The data was sourced from the National Grid UK [website](https://www.nationalgrid.com/uk)



Installation
------------

Install the development version from [Github](https://github.com/RamiKrispin/UKgrid):

``` r
# install.packages("devtools")
devtools::install_github("RamiKrispin/TSstudio")
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
