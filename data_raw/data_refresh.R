# Load the UK nationalgrid data
rm(list = ls())
`%>%` <- magrittr::`%>%`
df_05 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2005_1542016460040.csv")
df_06 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2006_1542016477936.csv")
df_07 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2007_1542016490891.csv")
df_08 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2008_1542016504804.csv")
df_09 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2009_1542016517401.csv")
df_10 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2010_1542016528958.csv")
df_11 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2011_1542016545896.csv")
df_12 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2012_1542016557812.csv")
df_13 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2013_1542016569881.csv")
df_14 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2014_1542016583597.csv")
df_15 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2015_1542016605715.csv")
df_16 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=Demand_Data2016_1542016628412.csv")
df_17 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2017_1551263464434.csv")
df_18 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2018_1551263484189.csv")
df_19 <- data.table::fread("https://demandforecast.nationalgrid.com/efs_demand_forecast/downloadfile?filename=DemandData_2019_1572344306840.csv")



df_05$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_05$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_06$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_06$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_07$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_07$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_08$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_08$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_09$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_09$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_10$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_10$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_11$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_11$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_12$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_12$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_13$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_13$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_14$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_14$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_15$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_15$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_16$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_16$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_17$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_17$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_18$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_18$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")
df_19$SETTLEMENT_DATE <- base::as.Date(lubridate::dmy(df_19$SETTLEMENT_DATE, tz = "UTC"), tz = "UTC")

df_05$I014_TSD.1 <- NULL
df_06$I014_TSD.1 <- NULL
df_07$I014_TSD.1 <- NULL
df_08$I014_TSD.1 <- NULL
df_09$I014_TSD.1 <- NULL
df_10$I014_TSD.1 <- NULL
df_11$I014_TSD.1 <- NULL
df_12$I014_TSD.1 <- NULL
df_13$I014_TSD.1 <- NULL
df_14$I014_TSD.1 <- NULL
df_15$I014_TSD.1 <- NULL

df_19$NEMO_FLOW <- NULL

# Dropping duplicated

df_10a <- df_10 %>% dplyr::mutate(year = lubridate::year(SETTLEMENT_DATE)) %>% dplyr::filter(year == 2010) %>% dplyr::select(-year)

df <- rbind(df_05, df_06, df_07, df_08, df_09, df_10a,
            df_11, df_12, df_13, df_14, df_15,
            df_16, df_17, df_18, df_19)

start_date <- min(df$SETTLEMENT_DATE)
end_date <- max(df$SETTLEMENT_DATE)

start_time <- base::as.POSIXct(base::paste(start_date, "00:00:00", sep = " "), tz = "UTC")
end_time <- base::as.POSIXct(base::paste(end_date, "23:30:00", sep = " "), tz = "UTC")
# Filling missing values
date_vec <- data.frame(TIMESTAMP = seq.POSIXt(from = start_time, to = end_time, by = "30 min"))
date_vec$SETTLEMENT_DATE <- as.Date(date_vec$TIMESTAMP)
date_vec$SETTLEMENT_PERIOD <- base::as.numeric(lubridate::minute(date_vec$TIMESTAMP)) / 30 + base::as.numeric(lubridate::hour(date_vec$TIMESTAMP)) * 2 + 1
head(date_vec, 49)


UKgrid <- date_vec %>% dplyr::left_join(df, by = c("SETTLEMENT_DATE", "SETTLEMENT_PERIOD")) %>%
  dplyr::select(TIMESTAMP, ND, I014_ND, TSD, #I014_TSD,
                ENGLAND_WALES_DEMAND, EMBEDDED_WIND_GENERATION, EMBEDDED_WIND_CAPACITY,
                EMBEDDED_SOLAR_GENERATION, EMBEDDED_SOLAR_CAPACITY) %>%
  dplyr::arrange(TIMESTAMP)

plot(UKgrid$TIMESTAMP, UKgrid$ND, type = "l")

usethis::use_data(UKgrid, overwrite = TRUE, compress = "bzip2")
