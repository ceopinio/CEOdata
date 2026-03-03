# tests/testthat/test_accumulated_meta_active_only.R

test_that("active_only keeps only active accumulated series", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(
        codi_serie = c("A", "B", "C"),
        estat = c("Serie activa", "Serie inactiva", "ACTIVA")
      )
    }
  )

  d <- CEOaccumulated_meta(active_only = TRUE)

  expect_identical(d$codi_serie, c("A", "C"))
})

test_that("active_only and series filter work together", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(
        codi_serie = c("A", "B", "C"),
        estat = c("Serie activa", "Serie inactiva", "Serie activa")
      )
    }
  )

  d <- CEOaccumulated_meta(series = c("B", "C"), active_only = TRUE)

  expect_identical(d$codi_serie, "C")
})

test_that("active_only handles missing estat column", {
  local_mocked_bindings(
    CEOaccumulated_metadata = function() {
      tibble::tibble(codi_serie = c("A", "B"))
    }
  )

  d <- CEOaccumulated_meta(active_only = TRUE)

  expect_identical(nrow(d), 0L)
})
