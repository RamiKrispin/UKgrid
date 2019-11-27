context("Test extract_grid function")

test_that(desc = "Test basic object",
          {
            x <- UKgrid::extract_grid()
            expect_equal(tsibble::is_tsibble(x), TRUE)
            expect_equal(tsibble::index(x) == "TIMESTAMP", TRUE)
            expect_equal(lubridate::is.POSIXct(x$TIMESTAMP), TRUE)
          })


test_that(desc = "Using arguments - tsibble with hourly aggregation and two variables",
          {
            x <- UKgrid::extract_grid(type = "tsibble", columns = c("ND", "TSD"), aggregate = "hourly" )
            expect_equal(base::all(base::names(x) %in% c("TIMESTAMP", "ND", "TSD")), TRUE)
            expect_equal(tsibble::is_tsibble(x), TRUE)
            expect_equal(tsibble::index(x) == "TIMESTAMP", TRUE)
            expect_equal(lubridate::is.POSIXct(x$TIMESTAMP), TRUE)
          })

test_that(desc = "Using arguments - xts with monthly aggregation, pulling two variables",
          {
            x <- UKgrid::extract_grid(type = "xts", columns = c("ND", "TSD"), aggregate = "daily" )
            expect_equal(base::all(base::names(x) %in% c("TIMESTAMP", "ND", "TSD")), TRUE)
            expect_equal(xts::is.xts(x), TRUE)
            expect_equal(xts::indexClass(x) == "Date", TRUE)
          })



test_that(desc = "Using arguments - tsibble with monthly aggregation, pulling two variables",
          {
            x <- UKgrid::extract_grid(type = "tsibble", columns = c("ND", "TSD"), aggregate = "monthly" )
            expect_equal(base::all(base::names(x) %in% c("TIMESTAMP", "ND", "TSD")), TRUE)
            expect_equal(tsibble::is_tsibble(x), TRUE)
            expect_equal(tsibble::index(x) == "TIMESTAMP", TRUE)
            expect_equal(all(class(x$TIMESTAMP) %in% c("yearmonth", "Date")), TRUE)
          })

