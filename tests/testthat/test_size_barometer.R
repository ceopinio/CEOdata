require(testthat)
devtools::load_all("../..")
d <- CEOdata(raw = TRUE, extra_variables = FALSE)
context("Check that the merged barometer contains the correct number of observations and variables.")

dims.barometer <- dim(d)
obs.barometer <- as.integer(dims.barometer[1])
vars.barometer <- as.integer(dims.barometer[2])

test_that("Observations match the merged barometer", {
            correct.size <- 35838L # As of 220502
            expect_identical(correct.size, obs.barometer)
})

test_that("Variables match the merged barometer", {
            correct.size <- 896L # As of 200502
            expect_identical(correct.size, vars.barometer)
})

