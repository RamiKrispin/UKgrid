context("Test extract_grid functio")

library(UKgrid)

test_that(desc = "Test object class",
          {
            expect_equal(tsibble::is_tsibble(extract_grid()), TRUE)
          })


