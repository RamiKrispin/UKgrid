#' The UK National Electricity Transmission System Dataset
#'
#' The demand for electricity in the UK since 2011
#' Units: MW
#' Time zone: UTC
#'
#'
#' @format Data frame with timestamp (half-hour intervals)
#' @source The UK natioanl grid \href{https://www.nationalgrid.com/uk}{website}
#' @keywords datasets
#' @details Field descriptions, source National Grid UK \href{https://www.nationalgrid.com/uk}{website}
#'
#' TIMESTAMP - a POSIXt object (if not aggregate to daily frequency and above), the time stamp of the series observations
#'
#' ND - National Demand is calculated as a sum of generation based on National Grid operational generation metering
#'
#' I014_ND - Equivalent to ND (above) but calculated using settlement metered generation data from the I014 file where available
#'
#' TSD - Transmission System Demand, This is the Transmission System generation requirement and is equivalent to the Initial Transmission System Outturn (ITSDO) and Transmission System Demand Forecast on BM Reports. Transmission System Demand is equal to the ND plus the additional generation required to meet station load, pump storage pumping and interconnector exports
#'
#' I014_TSD - Equivalent to TSD (above), but calculated using settlement metered generation data from the I014 file where available
#'
#' ENGLAND_WALES_DEMAND - England and Wales Demand, as ND above but on an England and Wales basis
#'
#' EMBEDDED_WIND_GENERATION - Estimated Embedded Wind Generation, This is an estimate of the GB wind generation from wind farms which do not have Transmission System metering installed. These wind farms are embedded in the distribution network and invisible to National Grid. Their effect is to suppress the electricity demand during periods of high wind. The true output of these generators is not known so an estimate is provided based on National Grid’s best model
#'
#' EMBEDDED_WIND_CAPACITY - Estimated Embedded Wind Capacity, This is National Grid’s best view of the installed embedded wind capacity in GB. This is based on publically available information compiled from a variety of sources and is not the definitive view. It is consistent with the generation estimate provided above
#'
#' EMBEDDED_SOLAR_GENERATION - Estimated Embedded Solar Generation, As embedded wind generation above, but for solar generation
#'
#' EMBEDDED_SOLAR_CAPACITY - Embedded Solar Capacity, As embedded wind capacity above, but for solar generation
#'
#' @examples
#' data(UKgrid)
#' plot(UKgrid$TIMESTAMP, UKgrid$ND, type = "l")

"UKgrid"


#' Extracting and Aggregation of the UKgrid Dataset
#' @export extract_grid
#' @param type A character, define the output type - c(`tsibble`,`xts`, `zoo`, `ts`, `mts`, `data.frame`, `tbl`, `data.table`)
#' @param columns Selecting the columns names to be used from the UKgrid dataset,
#' can be either the numeric values of the columns index, or a string with the column names. Please see below the field descriptions
#' @param start Defines the starting date and time of the data extractions,
#' could be either an integer with the year value (4-digits format) or Date/POSIXt obejct
#' @param end Defines the ending date and time of the data extractions,
#' could be either an integer with the year value (4-digits format) or Date/POSIXt obejct
#' @param aggregate A string, if not NULL (default) aggregate up the series.
#' possible aggregation options are c("hourly", "daily", "weekly", "monthly", "quarterly", "yearly")
#' @param weekly_agg A string, define the week count methodology (according to the lubridate \code{\link[lubridate]{week}} functions setting)
#' to be used when the aggregation of the series set to weekly.
#' Possible options "index" (default), "week", "isoweek", or "epiweek".
#' @param na.rm A boolean, if TRUE will use the na.rm function to ignore any missing values in the aggregation process
#' @details Field descriptions, source National Grid UK \href{https://www.nationalgrid.com/uk}{website}
#'
#' TIMESTAMP - a POSIXt object (if not aggregate to daily frequency and above), the time stamp of the series observations
#'
#' ND - National Demand, National Demand is calculated as a sum of generation based on National Grid operational generation metering
#'
#' I014_ND - Equivalent to ND (above) but calculated using settlement metered generation data from the I014 file where available
#'
#' TSD - Transmission System Demand, This is the Transmission System generation requirement and is equivalent to the Initial Transmission System Outturn (ITSDO) and Transmission System Demand Forecast on BM Reports. Transmission System Demand is equal to the ND plus the additional generation required to meet station load, pump storage pumping and interconnector exports
#'
#' I014_TSD - Equivalent to TSD (above), but calculated using settlement metered generation data from the I014 file where available
#'
#' ENGLAND_WALES_DEMAND - England and Wales Demand, as ND above but on an England and Wales basis
#'
#' EMBEDDED_WIND_GENERATION - Estimated Embedded Wind Generation, This is an estimate of the GB wind generation from wind farms which do not have Transmission System metering installed. These wind farms are embedded in the distribution network and invisible to National Grid. Their effect is to suppress the electricity demand during periods of high wind. The true output of these generators is not known so an estimate is provided based on National Grid’s best model
#'
#' EMBEDDED_WIND_CAPACITY - Estimated Embedded Wind Capacity, This is National Grid’s best view of the installed embedded wind capacity in GB. This is based on publically available information compiled from a variety of sources and is not the definitive view. It is consistent with the generation estimate provided above
#'
#' EMBEDDED_SOLAR_GENERATION - Estimated Embedded Solar Generation, As embedded wind generation above, but for solar generation
#'
#' EMBEDDED_SOLAR_CAPACITY - Embedded Solar Capacity, As embedded wind capacity above, but for solar generation
#'
#' @examples
#' df <- extract_grid(type = "tsibble", columns = "ND", start = 2017)
#'

extract_grid <- function(type = "tsibble",
                         columns = "ND",
                         start = NULL,
                         end = NULL,
                         aggregate = NULL,
                         weekly_agg = "index",
                         na.rm = TRUE){


  . <- TIMESTAMP <- hour <- year <- month <- quarter <-  week <-  NULL
  `%>%` <- magrittr::`%>%`
  UKgrid <- time_stamp <- NULL
  UKgrid <- UKgrid::UKgrid
  time_stamp <- "TIMESTAMP"
  # Error handling

  if(!base::is.logical(na.rm)){
    warning("The value of the 'na.rm' argument is not valid, using default option - TRUE")
    na.rm <- TRUE
  }
  if(!base::is.null(aggregate)){
    if(!aggregate %in% c("hourly", "daily", "weekly", "monthly", "quarterly", "yearly")){
      warning("The value of the 'aggregate' argument is not valid and will be ignored")
      aggregate <- NULL
    } else if(aggregate == "weekly"){
      if(!weekly_agg %in% c("index", "week", "isoweek", "epiweek")){
        warning("The value of the 'weekly_agg' argument is not valid, using the default value - 'index'")
        weekly_agg <- "index"
      }
    }
  }



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
      columns <- base::which(base::names(UKgrid) %in% columns)
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

  col_names <- NULL
  col_names <- base::colnames(df)[base::which(base::colnames(df) != time_stamp)]

  if(base::is.null(aggregate)){
    df1 <- df
    frequency <- 48
    start <- c(1, 2 * lubridate::hour(start_date) + lubridate::minute(start_date) / 30 + 1)
  }  else if(aggregate == "hourly"){
    df$date <- base::as.Date(df$TIMESTAMP)
    df$hour <- lubridate::hour(df$TIMESTAMP)

    if(na.rm){
      df1 <- df %>%
        dplyr::select(-TIMESTAMP) %>%
        dplyr::group_by(date, hour) %>%
        dplyr::summarise_all(~{sum(.x, na.rm = any(!is.na(.x)))})
    } else {
      df1 <- df %>%
        dplyr::select(-TIMESTAMP) %>%
        dplyr::group_by(date, hour) %>%
        dplyr::summarise_all(base::list(base::sum), na.rm = FALSE)
    }


    df1$TIMESTAMP <- lubridate::ymd_h(paste(df1$date, df1$hour, sep = " "), tz = "UTC")
    df1$date <- df1$hour <- NULL
    df1 <- as.data.frame(df1[, c(base::which(base::colnames(df1) == time_stamp), base::which(base::colnames(df1) != time_stamp))])
    frequency <- 24
    start <- c(1, lubridate::hour(start_date))
  } else if(aggregate == "daily"){
    df$date <- base::as.Date(df$TIMESTAMP)
    df1 <- df %>% dplyr::select(-TIMESTAMP) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)

    df1$TIMESTAMP <- df1$date
    df1$date <-  NULL
    df1 <- as.data.frame(df1[, c(base::which(base::colnames(df1) == time_stamp), base::which(base::colnames(df1) != time_stamp))])
    frequency <- 365
    start <- c(lubridate::year(start_date), lubridate::yday(start_date))
  } else if(aggregate == "weekly"){
    df$date <- base::as.Date(df$TIMESTAMP)
    df$year <- lubridate::year(df$TIMESTAMP)

    if(weekly_agg != "index"){
    if(weekly_agg == "week"){
      df$week <- lubridate::week(df$TIMESTAMP)
    } else if(weekly_agg == "isoweek"){
      df$week <- lubridate::isoweek(df$TIMESTAMP)
    } else if(weekly_agg == "epiweek"){
      df$week <- lubridate::epiweek(df$TIMESTAMP)
    } else if(weekly_agg == "index"){
      df$week <- lubridate::isoweek(df$TIMESTAMP)
    }

    df1 <- df %>% dplyr::select(-TIMESTAMP, - date) %>%
      dplyr::group_by(year, week) %>%
      dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)
    } else if(weekly_agg == "index"){
      temp <- index <- NULL

      index =  seq.Date(from = as.Date(start_date),
                             to = as.Date(end_date),
                             by = "weeks" )

      temp <- df %>% dplyr::select(-TIMESTAMP, -year) %>%
      dplyr::group_by(date) %>%
        dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)
      temp$week <- base::rep(1:base::length(index), each = 7)[1:nrow(temp)]

      df1 <- temp %>% dplyr::select(-date) %>%
      dplyr::group_by(week) %>%
        dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)
    }

    df1$TIMESTAMP <- base::seq.Date(from = base::as.Date(start_date), by = "weeks", length.out = nrow(df1))
    df1$date <- df1$week <- df1$year <- NULL


    df1 <- as.data.frame(df1[, c(base::which(base::colnames(df1) == time_stamp), base::which(base::colnames(df1) != time_stamp))])
    frequency <- 52
    start <- c(lubridate::year(start_date), lubridate::week(start_date))
  } else if(aggregate == "monthly"){
    df$date <- base::as.Date(df$TIMESTAMP)
    df$month <- lubridate::month(df$TIMESTAMP)
    df$year <- lubridate::year(df$TIMESTAMP)
    df1 <- base::suppressMessages(df %>% dplyr::select(-TIMESTAMP, - date) %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)  %>%
      dplyr::left_join(df %>%
                         dplyr::group_by(year, month) %>%
                         dplyr::summarise(date = min(date, na.rm = na.rm))))

    df1$TIMESTAMP <- df1$date
    df1$date <- df1$month <- df1$year <- NULL
    df1 <- as.data.frame(df1[, c(base::which(base::colnames(df1) == time_stamp), base::which(base::colnames(df1) != time_stamp))])
    frequency <- 12
    start <- c(lubridate::year(start_date), lubridate::month(start_date) )
  } else if(aggregate == "quarterly"){
    df$date <- base::as.Date(df$TIMESTAMP)
    df$quarter <- lubridate::quarter(df$TIMESTAMP)
    df$year <- lubridate::year(df$TIMESTAMP)
    df1 <- base::suppressMessages(df %>% dplyr::select(-TIMESTAMP, - date) %>%
      dplyr::group_by(year, quarter) %>%
      dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)  %>%
      dplyr::left_join(df %>%
                         dplyr::group_by(year, quarter) %>%
                         dplyr::summarise(date = min(date, na.rm = na.rm))))

    df1$TIMESTAMP <- df1$date
    df1$date <- df1$quarter <- df1$year <- NULL
    df1 <- as.data.frame(df1[, c(base::which(base::colnames(df1) == time_stamp), base::which(base::colnames(df1) != time_stamp))])
    frequency <- 4
    start <- c(lubridate::year(start_date), lubridate::quarter(start_date) )
  } else if(aggregate == "yearly"){
    df$date <- base::as.Date(df$TIMESTAMP)
    df$year <- lubridate::year(df$TIMESTAMP)
    df1 <- base::suppressMessages(df %>% dplyr::select(-TIMESTAMP, - date) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise_all(base::list(base::sum), na.rm = na.rm)  %>%
      dplyr::left_join(df %>%
                         dplyr::group_by(year) %>%
                         dplyr::summarise(date = min(date, na.rm = na.rm))))

    df1$TIMESTAMP <- df1$date
    df1$date <- df1$year <- NULL
    df1 <- as.data.frame(df1[, c(base::which(base::colnames(df1) == time_stamp), base::which(base::colnames(df1) != time_stamp))])
    frequency <- 1
    start <- c(lubridate::year(start_date))
  }



  if(type == "xts"){
    ts.obj <- xts::xts(df1[, base::which(base::colnames(df1) != time_stamp) ],
                       order.by = df1$TIMESTAMP)
  } else if(type == "zoo"){
    ts.obj <- zoo::zoo(df1[, base::which(base::colnames(df1) != time_stamp) ],
                       order.by = df1$TIMESTAMP)
  } else if(type == "ts"){
    ts.obj <- stats::ts(df1[, base::which(base::colnames(df1) != time_stamp) ],
                        start = start,
                        frequency = frequency)
  } else if(type == "data.frame"){
    ts.obj <- df1
  } else if(type == "tbl"){
    ts.obj <- dplyr::as.tbl(df1)
  } else if(type == "data.table"){
    ts.obj <- data.table::as.data.table(df1)
  } else if(type == "tsibble"){

    if(!is.null(aggregate) && aggregate == "monthly"){
      ts.obj$TIMESTAMP <- tsibble::yearmonth(ts.obj$TIMESTAMP)
      ts.obj <- df1 %>% tsibble::as_tsibble(index = "TIMESTAMP")
    } else if(!is.null(aggregate) && aggregate == "quarterly"){
      ts.obj$TIMESTAMP <- tsibble::yearquarter(ts.obj$TIMESTAMP)
      ts.obj <- df1 %>% tsibble::as_tsibble(index = "TIMESTAMP")
    } else {
      ts.obj <- df1 %>% tsibble::as_tsibble(index = "TIMESTAMP")
    }
  }

  return(ts.obj)
}
