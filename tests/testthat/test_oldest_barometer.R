require(testthat)
devtools::load_all("../..")
date.min.barometer <- min(CEOdata()$Data, na.rm = TRUE)
  
context("Check that the transformations of the barometer produce meaningful dates.")
correct.date <- as.Date("2014-03-02")

test_that("Date of first presential barometer is 02/03/2014", {
            expect_identical(correct.date, date.min.barometer)
})

