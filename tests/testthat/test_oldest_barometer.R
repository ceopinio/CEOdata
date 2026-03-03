# tests/testthat/test_oldest_barometer.R

test_that("Date of first presential barometer is 2014-03-02", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(
        codi_serie = "BOP_presencial",
        microdades_1 = "https://example.org/barometer.sav"
      )
    },
    ceodata_download_and_read = function(url, raw = FALSE) {
      tibble::tibble(
        ANY = c(2014, 2015, 2016),
        MES = c(3, 1, 12),
        DIA = c(2, 10, 1)
      )
    }
  )

  d <- CEOdata(raw = TRUE)

  expect_true(all(c("ANY", "MES", "DIA") %in% names(d)))

  built_date <- as.Date(sprintf(
    "%04d-%02d-%02d",
    as.integer(d$ANY),
    as.integer(d$MES),
    as.integer(d$DIA)
  ))

  date_min <- suppressWarnings(min(built_date, na.rm = TRUE))
  expect_identical(as.Date("2014-03-02"), date_min)
})
