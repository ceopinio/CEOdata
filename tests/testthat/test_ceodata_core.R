# tests/testthat/test_ceodata_core.R

test_that("CEOdata validates raw argument", {
  expect_error(
    CEOdata(raw = NA),
    "'raw' must be a single logical value"
  )

  expect_error(
    CEOdata(raw = "FALSE"),
    "'raw' must be a single logical value"
  )
})

test_that("CEOdata errors when both reo and series are provided", {
  expect_error(
    CEOdata(series = "BOP_presencial", reo = "1145"),
    "Provide either 'reo' or 'series', but not both"
  )
})

test_that("CEOdata REO mode errors for unknown REO", {
  local_mocked_bindings(
    CEOmetadata = function() {
      tibble::tibble(
        REO = c("100", "101"),
        `Microdades 1` = c("https://example.org/a.sav", "https://example.org/b.sav")
      )
    }
  )

  expect_error(
    CEOdata(reo = "999"),
    "There is no dataset available for REO 999"
  )
})

test_that("CEOdata REO mode prefers .sav over .zip links", {
  local_mocked_bindings(
    CEOmetadata = function() {
      tibble::tibble(
        REO = c("1145"),
        `Microdades 1` = c("https://example.org/reo1145.zip"),
        `Microdades 2` = c("https://example.org/reo1145.sav")
      )
    },
    ceodata_download_and_read = function(url, raw = FALSE) {
      list(url = url, raw = raw)
    }
  )

  out <- CEOdata(reo = "1145", raw = TRUE)

  expect_identical(out$url, "https://example.org/reo1145.sav")
  expect_identical(out$raw, TRUE)
})

test_that("CEOdata accumulated mode matches series case-insensitively", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(
        `codi serie` = c("BOP_presencial", "ALTRE"),
        `microdades 1` = c("https://example.org/bop.sav", "https://example.org/altre.sav")
      )
    },
    ceodata_download_and_read = function(url, raw = FALSE) {
      list(url = url, raw = raw)
    }
  )

  out <- CEOdata(series = "bop_presencial")

  expect_identical(out$url, "https://example.org/bop.sav")
  expect_identical(out$raw, FALSE)
})

test_that("CEOdata accumulated mode errors for unknown series", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(
        codi_serie = c("BOP_presencial"),
        microdades_1 = c("https://example.org/bop.sav")
      )
    }
  )

  expect_error(
    CEOdata(series = "NO_EXISTEIX"),
    "Unknown accumulated series: NO_EXISTEIX"
  )
})

test_that("CEOdata rejects CSV-only links", {
  local_mocked_bindings(
    CEOmetadata = function() {
      tibble::tibble(
        REO = c("200"),
        `Microdades 1` = c("https://example.org/reo200.csv")
      )
    }
  )

  expect_error(
    CEOdata(reo = "200"),
    "Only CSV links were found"
  )
})
