# tests/testthat/test_conversion_factors.R

test_that("CEOdata returns factor variables when raw = FALSE", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(
        codi_serie = "BOP_presencial",
        microdades_1 = "https://example.org/barometer.sav"
      )
    },
    CEOmetadata = function() {
      tibble::tibble(
        REO = "1031",
        `Microdades 1` = "https://example.org/reo1031.sav"
      )
    },
    ceodata_download_and_read = function(url, raw = FALSE) {
      tibble::tibble(COMARCA = factor(c("Barcelona", "Girona")))
    }
  )

  d <- CEOdata()
  d1031 <- CEOdata(reo = "1031")

  expect_s3_class(d$COMARCA, "factor")
  expect_s3_class(d1031$COMARCA, "factor")
})
