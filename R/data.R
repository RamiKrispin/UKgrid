#' The UK National Electricity Transmission System Dataset
#'
#' The demand for electricity in the UK since 2011
#' Units: Thousands of units
#'
#'
#' @format Data frame with timestamp (half-hour intervals)
#' @source The UK natioanl grid \href{https://www.nationalgrid.com/uk}{link}
#' @keywords datasets
#' @examples
#' data(UKgrid)

"UKgrid"


#' Extracting and transforming data from the UKgrid Dataset
#' @param type A character, define the output type - c("xts", "zoo", "ts", "mts", "data.frame", "tbl", "data.table")
#' @param columns Selecting the columns names to be used from the UKgrid dataset,
#' can be either the numeric values of the columns index, or a string with the column names
#' @param start Defines the starting date and time of the data extractions,
#' could be either an integer with the year value (4-digits format) or Date/POSIXt obejct
#' @param end Defines the ending date and time of the data extractions,
#' could be either an integer with the year value (4-digits format) or Date/POSIXt obejct
#' @example
#' df <- extract_grid(type = "xts", columns = "ND", start = 2017)
#'

extract_grid <- function(type = "xts", columns = "ND", start = NULL, end = NULL){
  `%>%` <- magrittr::`%>%`
  UKgrid <- time_stamp <- NULL
  UKgrid <- UKgrid::UKgrid
  time_stamp <- "TIMESTAMP"
  # Error handling
  # Checking the values of the columns argument
  if(!base::is.null(columns)){
    if(base::is.character(columns)){
      if(!base::all(columns %in% base::names(UKgrid))){
        if(!base::any(columns %in% base::names(UKgrid))){
          stop("The list of column names is invalid, please check the 'columns' argument")
        } else{
          columns <- columns[base::which(columns %in% base::names(UKgrid))]
          warning("Some of the column names were not found, using only matched names")
        }
      }
      # Including the timestamp of the series
        if(!time_stamp %in% columns){
          columns <- c(time_stamp, columns)
        }
      # Getting the numeric value of the columns
      columns <- base::which(columns %in% base::names(UKgrid))
    } else if(base::is.numeric(columns)){
      if(!base::all(columns %in% 1:base::ncol(UKgrid))){
        if(!base::any(columns %in% 1:base::ncol(UKgrid))){
          stop("The list of column names is invalid, please check the 'columns' argument")
        } else{
          columns <- columns[base::which(columns %in% 1:base::ncol(UKgrid))]
          warning("Some of the column names were not found, using only matched names")
        }

      }

      if(!time_stamp %in% base::names(UKgrid)[columns]){
        columns <- c(base::which(colnames(UKgrid) == time_stamp), columns)
      }
    } else {
        stop("The 'columns argument is invalid")
      }
  } else {
    stop("The 'columns argument is invalid")
  }

  # Setting the time range
  # Start date
  if(!base::is.null(start)){
    # Case numeric - hence year value
    if(base::is.numeric(start)){
      if(start < lubridate::year(base::min(UKgrid$TIMESTAMP)) | start > lubridate::year(base::max(UKgrid$TIMESTAMP))){
        warning("The start argument is out of range, using the first available date to filter the data")
        start_date <- min(UKgrid$TIMESTAMP)
      } else{
        start_date <- UKgrid$TIMESTAMP[which(lubridate::year(UKgrid$TIMESTAMP) == start)[1]]
      }
      # Case date
    } else if(lubridate::is.Date(start)){
        if(start < base::min(UKgrid$TIMESTAMP) | start > max(UKgrid$TIMESTAMP)){
          warning("The start argument is out of range, using the first available date to filter the data")
          start_date <- min(UKgrid$TIMESTAMP)
        } else{
                start_date <- UKgrid$TIMESTAMP[which(base::as.Date(UKgrid$TIMESTAMP) == start)][1]
        }
      # Case POSIXt
    } else if(lubridate::is.POSIXt(start)){
      if(start < base::min(UKgrid$TIMESTAMP) | start > max(UKgrid$TIMESTAMP)){
        warning("The start argument is out of range, using the first available date to filter the data")
        start_date <- min(UKgrid$TIMESTAMP)
      } else if(start %in% UKgrid$TIMESTAMP){
          start_date <- start
        } else {
          start_date <- UKgrid$TIMESTAMP[which(UKgrid$TIMESTAMP > start)[1]]
          }
    }
  } else {
    start_date <- min(UKgrid$TIMESTAMP)
  }

  # End date
  if(!base::is.null(end)){
    # Case numeric - hence year value
    if(base::is.numeric(end)){
      if(end < lubridate::year(base::min(UKgrid$TIMESTAMP)) | end > lubridate::year(base::max(UKgrid$TIMESTAMP))){
        warning("The end argument is out of range, using the last available date to filter the data")
        end_date <- base::max(UKgrid$TIMESTAMP)
      } else {
        end_date <- UKgrid$TIMESTAMP[base::which(lubridate::year(UKgrid$TIMESTAMP) == end)[base::length(base::which(lubridate::year(UKgrid$TIMESTAMP) == end))]]
      }
      # Case date
    } else if(lubridate::is.Date(end)){
      if(end < base::min(UKgrid$TIMESTAMP) | end > base::max(UKgrid$TIMESTAMP)){
        warning("The end argument is out of range, using the last available date to filter the data")
        end_date <- base::max(UKgrid$TIMESTAMP)
      } else{
        end_date <- UKgrid$TIMESTAMP[base::which(base::as.Date(UKgrid$TIMESTAMP) == end)][base::length(base::which(base::as.Date(UKgrid$TIMESTAMP) == end))]
      }
      # Case POSIXt
    } else if(lubridate::is.POSIXt(end)){
      if(end < base::min(UKgrid$TIMESTAMP) | end > base::max(UKgrid$TIMESTAMP)){
        warning("The end argument is out of range, using the last available date to filter the data")
        end_date <- base::max(UKgrid$TIMESTAMP)
      } else if(end %in% UKgrid$TIMESTAMP){
          end_date <- end
        } else {
        end_date <- UKgrid$TIMESTAMP[base::which(UKgrid$TIMESTAMP < end)][base::length(base::which(UKgrid$TIMESTAMP < end))]
      }
    }
  } else {
    end_date <- base::max(UKgrid$TIMESTAMP)
  }




  df <- UKgrid %>% dplyr::select(c(time_stamp, columns)) %>%
    dplyr::filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

  if(type == "xts"){
    ts.obj <- xts::xts(df[, which(colnames(df) != time_stamp) ], order.by = df$TIMESTAMP)
  } else if(type == "zoo"){
    ts.obj <- zoo::zoo(df[, which(colnames(df) != time_stamp) ], order.by = df$TIMESTAMP)
  } else if(type == "ts"){
    ts.obj <- stats::ts(df[, which(colnames(df) != time_stamp) ],
                        start = c(lubridate::yday(start_date), 2 * lubridate::hour(start_date) + lubridate::minute(start_date) / 30 + 1),
                        frequency = 48)
  } else if(type == "data.frame"){
    ts.obj <- df
  } else if(type == "tbl"){
    ts.obj <- dplyr::as.tbl(df)
  } else if(type == "data.table"){
    ts.obj <- data.table::as.data.table(df)
  }

  return(ts.obj)
}
