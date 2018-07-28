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

extract_grid <- function(type = "xts", columns = "ND", start = NULL, end = NULL){
  data(UKgrid)
  df <- UKgrid
  if(!"TIMESTAMP" %in% columns){
    columns <- c("TIMESTAMP", columns)
  }

  # Setting the time range
  # Start date
  if(!base::is.null(start)){
    # Case numeric - hence year value
    if(base::is.numeric(start)){
      if(start < lubridate::year(min(df$TIMESTAMP)) | start > lubridate::year(max(df$TIMESTAMP))){
        warning("The start argument is out of range, using the first available date to filter the data")
        start_date <- min(df$TIMESTAMP)
      } else{
        start_date <- df$TIMESTAMP[which(lubridate::year(df$TIMESTAMP) == start)[1]]
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
    xts.obj <- xts::xts(df[, which(colnames(df) != "TIMESTAMP") ], order.by = df$TIMESTAMP)
  }
}
