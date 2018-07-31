# Load the UK nationalgrid data

df_11_16 <- data.table::fread("https://www.nationalgrid.com/sites/default/files/documents/DemandData_2011-2016.csv")
df_17 <- data.table::fread("https://www.nationalgrid.com/sites/default/files/documents/DemandData_2017.csv")
df_18 <- data.table::fread("https://www.nationalgrid.com/sites/default/files/documents/DemandData_2018_1.csv")

df_11_16$SETTLEMENT_DATE <- lubridate::dmy(df_11_16$SETTLEMENT_DATE)
df_17$SETTLEMENT_DATE <- lubridate::dmy(df_17$SETTLEMENT_DATE)
df_18$SETTLEMENT_DATE <- lubridate::dmy(df_18$SETTLEMENT_DATE)


df_17$I014_TSD.1 <- NA
df_18$I014_TSD.1 <- NA

df <- rbind(df_11_16, df_17, df_18)
df$year <- lubridate::year(df$SETTLEMENT_DATE)
df$month <- lubridate::month(df$SETTLEMENT_DATE)
df$day <- lubridate::mday(df$SETTLEMENT_DATE)
df$minutes <- ifelse(df$SETTLEMENT_PERIOD %% 2 == 0, "30","00")
df$hours <- floor(df$SETTLEMENT_PERIOD / 2 - 0.5)
df$TIMESTAMP <- lubridate::ydm_hm(paste(df$year, df$day ,df$month, df$hours, df$minutes, sep = "-"))

df$year <- df$month <- df$day <- df$minutes <- df$hours <-
  df$SETTLEMENT_DATE <- df$SETTLEMENT_PERIOD <- NULL

col_time <- which(colnames(df) == "TIMESTAMP")
col <- setdiff(1:ncol(df), col_time)

df1 <- as.data.frame(df)
col_names <- c(names(df)[col_time], names(df)[col])

UKgrid <- df1[, col_names]

devtools::use_data(UKgrid, overwrite = TRUE)
