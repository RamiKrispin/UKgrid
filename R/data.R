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
#' @param type A character, define the output type - c("xts", "zoo", "ts", "data.frame", "tbl")
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
  data(UKgrid)

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
        if(!"TIMESTAMP" %in% columns){
          columns <- c("TIMESTAMP", columns)
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

      if(!"TIMESTAMP" %in% base::names(UKgrid)[columns]){
        columns <- c(base::which(colnames(UKgrid) == "TIMESTAMP"), columns)
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
        if(start < base::min(df$TIMESTAMP) | start > max(df$TIMESTAMP)){
          warning("The start argument is out of range, using the first available date to filter the data")
          start_date <- min(df$TIMESTAMP)
        } else{
                start_date <- df$TIMESTAMP[which(base::as.Date(df$TIMESTAMP) == start)][1]
        }
      # Case POSIXt
    } else if(lubridate::is.POSIXt(start)){
      if(start < base::min(df$TIMESTAMP) | start > max(df$TIMESTAMP)){
        warning("The start argument is out of range, using the first available date to filter the data")
        start_date <- min(df$TIMESTAMP)
      } else if(start %in% df$TIMESTAMP){
          start_date <- start
        } else {
          start_date <- df$TIMESTAMP[which(df$TIMESTAMP > start)[1]]
          }
    }
  } else {
    start_date <- min(df$TIMESTAMP)
  }

  # End date
  if(!base::is.null(end)){
    # Case numeric - hence year value
    if(base::is.numeric(end)){
      if(end < lubridate::year(min(df$TIMESTAMP)) | end > lubridate::year(max(df$TIMESTAMP))){
        warning("The end argument is out of range, using the last available date to filter the data")
        end_date <- max(df$TIMESTAMP)
      } else {
        end_date <- df$TIMESTAMP[which(lubridate::year(df$TIMESTAMP) == end)[length(which(lubridate::year(df$TIMESTAMP) == end))]]
      }
      # Case date
    } else if(lubridate::is.Date(end)){
      if(end < base::min(df$TIMESTAMP) | end > max(df$TIMESTAMP)){
        warning("The end argument is out of range, using the last available date to filter the data")
        end_date <- base::max(df$TIMESTAMP)
      } else{
        end_date <- df$TIMESTAMP[which(base::as.Date(df$TIMESTAMP) == end)][length(which(base::as.Date(df$TIMESTAMP) == end))]
      }
      # Case POSIXt
    } else if(lubridate::is.POSIXt(end)){
      if(end < base::min(df$TIMESTAMP) | end > max(df$TIMESTAMP)){
        warning("The end argument is out of range, using the last available date to filter the data")
        end_date <- max(df$TIMESTAMP)
      } else if(end %in% df$TIMESTAMP){
          end_date <- end
        } else {
        end_date <- df$TIMESTAMP[base::which(df$TIMESTAMP < end)][base::length(base::which(df$TIMESTAMP < end))]
      }
    }
  } else {
    end_date <- max(df$TIMESTAMP)
  }




  df <- UKgrid %>% dplyr::select(c("TIMESTAMP", columns)) %>%
    dplyr::filter(TIMESTAMP >= start_date & TIMESTAMP <= end_date)

  if(type == "xts"){
    ts.obj <- xts::xts(df[, which(colnames(df) != "TIMESTAMP") ], order.by = df$TIMESTAMP)
  } else if(type == "zoo"){
    ts.obj <- zoo::zoo(df[, which(colnames(df) != "TIMESTAMP") ], order.by = df$TIMESTAMP)
  }
}
