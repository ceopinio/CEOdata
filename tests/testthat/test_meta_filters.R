# tests/testthat/test_meta_filters.R

fake_ceo_metadata <- function() {
  tibble::tibble(
    REO = c("100", "101", "102"),
    `Titol enquesta` = c("Internet i politica", "Salut publica", "Educacio"),
    `Titol estudi` = c("Estudi A", "Estudi B", "Estudi C"),
    Objectius = c("Analitzar usos d'internet", "Hospitals", "Escoles"),
    Resum = c("Resum 1", "Resum 2", "Resum 3"),
    Descriptors = c("xarxes", "sanitat", "aules"),
    `Data d'alta al REO` = as.Date(c("2020-01-15", "2021-06-01", "2022-10-20")),
    Enllac = c("https://example.org/100", "https://example.org/101", "https://example.org/102"),
    `Microdades 1` = c("a.sav", "b.sav", "c.sav")
  )
}

test_that("CEOmeta search is case-insensitive and OR across terms", {
  local_mocked_bindings(
    CEOmetadata = function() fake_ceo_metadata()
  )

  d <- CEOmeta(search = c("internet", "SANITAT"))

  expect_identical(sort(as.character(d$REO)), c("100", "101"))
})

test_that("CEOmeta filters by date_start and date_end", {
  local_mocked_bindings(
    CEOmetadata = function() fake_ceo_metadata()
  )

  d <- CEOmeta(date_start = "2021-01-01", date_end = "2022-01-01")

  expect_identical(as.character(d$REO), "101")
})

test_that("CEOmeta browse safety limit skips opening when >10 and no force", {
  many_rows <- tibble::tibble(
    REO = as.character(1:11),
    `Titol enquesta` = rep("Titol", 11),
    `Titol estudi` = rep("Estudi", 11),
    Objectius = rep("Obj", 11),
    Resum = rep("Res", 11),
    Descriptors = rep("Desc", 11),
    `Data d'alta al REO` = as.Date("2021-01-01") + 0:10,
    Enllac = paste0("https://example.org/", 1:11),
    `Microdades 1` = rep("x.sav", 11)
  )

  local_mocked_bindings(
    CEOmetadata = function() many_rows
  )

  expect_message(
    d <- CEOmeta(browse = TRUE),
    "Browse request skipped"
  )
  expect_identical(nrow(d), 11L)
})
