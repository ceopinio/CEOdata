require(testthat)
devtools::load_all("../..")
context("Check that the conversion to factors is done properly, both in the barometer as well as individual REOs.")
d <- CEOdata()
d1031 <- CEOdata(reo = "1031")

c.barometer <- class(d$COMARCA)
c.1031 <- class(d1031$COMARCA)

test_that("Labelled SPSS variables are transformed into factors", {
            correct.type <- "factor"
            expect_identical(correct.type, c.barometer, c.1031)
})

