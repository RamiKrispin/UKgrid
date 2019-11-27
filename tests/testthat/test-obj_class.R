context("Test extract_grid function")

test_that(desc = "Test object class",
          {
            expect_equal(TRUE, TRUE)
          })




# Test 1

x <- UKgrid::extract_grid()
tsibble::is_tsibble(x)
tsibble::index(x) == "TIMESTAMP"
lubridate::is.POSIXct(x$TIMESTAMP)


x <- UKgrid::extract_grid(type = "tsibble", columns = c("ND", "TSD"), aggregate = "hourly" )
all(names(x) %in% c("TIMESTAMP", "ND", "TSD"))
tsibble::is_tsibble(x)
tsibble::index(x) == "TIMESTAMP"
lubridate::is.POSIXct(x$TIMESTAMP)


x <- UKgrid::extract_grid(type = "tsibble", columns = c("ND", "TSD"), aggregate = "daily" )
all(names(x) %in% c("TIMESTAMP", "ND", "TSD"))
tsibble::is_tsibble(x)
tsibble::index(x) == "TIMESTAMP"
lubridate::is.Date(x$TIMESTAMP)

x <- UKgrid::extract_grid(type = "xts", columns = c("ND", "TSD"), aggregate = "daily" )
all(names(x) %in% c("TIMESTAMP", "ND", "TSD"))
xts::is.xts(x)
xts::indexClass(x) == "Date"


x <- UKgrid::extract_grid(type = "xts", columns = c("ND", "TSD"), aggregate = "monthly" )
all(names(x) %in% c("TIMESTAMP", "ND", "TSD"))
xts::is.xts(x)
xts::indexClass(x) == "Date"


x <- UKgrid::extract_grid(type = "tsibble", columns = c("ND", "TSD"), aggregate = "monthly" )
all(names(x) %in% c("TIMESTAMP", "ND", "TSD"))
tsibble::is_tsibble(x)
tsibble::index(x) == "TIMESTAMP"
all(class(x$TIMESTAMP) %in% c("yearmonth", "Date"))
