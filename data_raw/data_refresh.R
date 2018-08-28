# Load the UK nationalgrid data

df_11_16 <- data.table::fread("https://www.nationalgrid.com/sites/default/files/documents/DemandData_2011-2016.csv")
df_17 <- data.table::fread("https://www.nationalgrid.com/sites/default/files/documents/DemandData_2017.csv")
df_18 <- data.table::fread("https://www.nationalgrid.com/sites/default/files/documents/DemandData_2018_1.csv")


df_11_16$SETTLEMENT_DATE <- lubridate::dmy(df_11_16$SETTLEMENT_DATE)
df_17$SETTLEMENT_DATE <- lubridate::dmy(df_17$SETTLEMENT_DATE)
df_18$SETTLEMENT_DATE <- lubridate::dmy(df_18$SETTLEMENT_DATE)

df_11_16$I014_TSD.1 <- NULL

df <- rbind(df_11_16, df_17, df_18)

start_date <- min(df$SETTLEMENT_DATE)
end_date <- max(df$SETTLEMENT_DATE)

# Filling missing values
date_vec <- data.frame(SETTLEMENT_DATE = rep(seq.Date(from = start_date, to = end_date, by = "day"), each = 48))
date_vec$SETTLEMENT_PERIOD <- rep(1:48, times = nrow(date_vec) / 48)
df1 <- date_vec %>% dplyr::left_join(df)

df1$year <- lubridate::year(df1$SETTLEMENT_DATE)
df1$month <- lubridate::month(df1$SETTLEMENT_DATE)
df1$day <- lubridate::mday(df1$SETTLEMENT_DATE)
df1$minutes <- ifelse(df1$SETTLEMENT_PERIOD %% 2 == 0, "30","00")
df1$hours <- floor(df1$SETTLEMENT_PERIOD / 2 - 0.5)
df1$TIMESTAMP <- lubridate::ydm_hm(paste(df1$year, df1$day ,df1$month, df1$hours, df1$minutes, sep = "-"))

df1$year <- df1$month <- df1$day <- df1$minutes <- df1$hours <-
  df1$SETTLEMENT_DATE <- df1$SETTLEMENT_PERIOD <- NULL

col_time <- which(colnames(df1) == "TIMESTAMP")
col <- base::setdiff(1:ncol(df1), col_time)

df2 <- base::as.data.frame(df1)
col_names <- c(base::names(df2)[col_time], base::names(df2)[col])

UKgrid <- df2[, col_names]

devtools::use_data(UKgrid, overwrite = TRUE)
