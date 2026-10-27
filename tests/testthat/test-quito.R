quito_crudos <- function() {
  # The deliberately blank middle header tests column alignment.
  data.frame(
    V1 = c("Fecha", "hora local", "45292", "45292.9583333333", "45293"),
    V2 = c("Guaman\u00ed", "ug/m3", "10", "1000", "99"),
    V3 = c(NA, NA, "555", "555", "555"),
    V4 = c("Los Chillos", "ug/m3", "20", "-1", "88")
  )
}

test_that("station columns and every hour of the final day are preserved", {
  periodo <- .periodo("2024-01-01", "2024-01-01", NULL, 2004L)
  x <- .procesar_quito(quito_crudos(), periodo, NULL, "CO", "UTC")
  expect_named(x, c("date", "Guamani", "Los_Chillos"))
  expect_equal(x$Guamani, c(10, 1000))
  expect_equal(x$Los_Chillos, c(20, NA_real_))
  expect_equal(format(x$date, "%H:%M:%S", tz = "UTC"),
               c("00:00:00", "23:00:00"))
  expect_equal(attr(x, "unidades"), c(Guamani = "ug/m3", Los_Chillos = "ug/m3"))
})

test_that("Quito timezone interpretation is explicit and legacy-compatible", {
  periodo <- .periodo("2024-01-01", "2024-01-01", NULL, 2004L)
  legacy <- .procesar_quito(quito_crudos(), periodo, NULL, "TMP", "UTC")
  local <- .procesar_quito(quito_crudos(), periodo, NULL, "TMP",
                           "America/Guayaquil")
  expect_equal(as.numeric(local$date) - as.numeric(legacy$date), rep(5 * 3600, 2))
  expect_equal(local$Los_Chillos, c(20, -1))
  expect_identical(formals(extraer_Quito_variable)$percentil_limite, 0.999)
  expect_identical(formals(extraer_Quito_variable)$tz, "UTC")
})

test_that("missing data and duplicate timestamps cannot be mistaken for records", {
  periodo <- .periodo("2024-01-01", "2024-01-01", NULL, 2004L)
  x <- quito_crudos()
  x[4, 1] <- x[3, 1]
  expect_error(.procesar_quito(x, periodo, NULL, "CO", "UTC"), "Duplicate")
  local_mocked_bindings(.descargar = function(...) FALSE)
  expect_equal(nrow(extraer_Quito_variable("CO", year = 2024)), 0L)
})

test_that("station extraction retains the union of times and original units", {
  local_mocked_bindings(.leer_quito = function(variable, ...) {
    if (!variable %in% c("CO", "TMP")) return(.vacio_variable("UTC"))
    x <- data.frame(date = as.POSIXct("2024-01-01", tz = "UTC") +
                     if (variable == "CO") 0 else 3600,
                     Los_Chillos = if (variable == "CO") 2 else -3)
    attr(x, "unidades") <- c(Los_Chillos = if (variable == "CO") "mg/m3" else "C")
    x
  })
  x <- extraer_Quito_estacion("Los Chillos", year = 2024, percentil_limite = NULL)
  expect_named(x, c("date", "estacion", "CO", "TMP"))
  expect_equal(x$CO, c(2, NA_real_))
  expect_equal(x$TMP, c(NA_real_, -3))
  expect_equal(attr(x, "unidades"), c(CO = "mg/m3", TMP = "C"))
})

test_that("archive extraction and Excel reading work together and clean up", {
  fixture <- test_path("fixtures", "quito-synthetic.zip")
  temporal <- NULL
  local_mocked_bindings(.descargar = function(url, destino, query = NULL) {
    temporal <<- dirname(destino)
    file.copy(fixture, destino, overwrite = TRUE)
  })
  x <- extraer_Quito_variable("TMP", "2024-01-01", "2024-01-01",
                               percentil_limite = NULL,
                               tz = "America/Guayaquil")
  expect_named(x, c("date", "Guamani", "Los_Chillos"))
  expect_equal(x$Guamani, c(-3, 4))
  expect_equal(x$Los_Chillos, c(2, 6))
  expect_equal(format(x$date, "%H:%M", tz = "America/Guayaquil"),
               c("00:00", "23:00"))
  expect_false(dir.exists(temporal))
})

test_that("ISO dates and a single station header retain the first observation", {
  periodo <- .periodo("2024-01-01", "2024-01-01", NULL, 2004L)
  crudos <- data.frame(V1 = c("FECHA", "2024-01-01 00:00:00",
                               "2024-01-01 23:00:00", "2024-01-02 00:00:00"),
                        V2 = c("BELISARIO", "2", "4", "99"))
  x <- .procesar_quito(crudos, periodo, NULL, "CO", "America/Guayaquil")
  expect_equal(x$Belisario, c(2, 4))
  expect_equal(format(x$date, "%H:%M", tz = "America/Guayaquil"),
               c("00:00", "23:00"))
  expect_equal(attr(x, "unidades"), c(Belisario = NA_character_))
})

test_that("Excel date cells and absent unit rows are supported end to end", {
  fixture <- test_path("fixtures", "quito-date-cells.zip")
  local_mocked_bindings(.descargar = function(url, destino, query = NULL) {
    file.copy(fixture, destino, overwrite = TRUE)
  })
  x <- extraer_Quito_variable("CO", "2024-01-01", "2024-01-01",
                               percentil_limite = NULL,
                               tz = "America/Guayaquil")
  expect_equal(nrow(x), 2L)
  expect_equal(x$Belisario, c(2, 4))
  expect_equal(format(x$date, "%H:%M", tz = "America/Guayaquil"),
               c("00:00", "23:00"))
  expect_true(all(is.na(attr(x, "unidades"))))
})

test_that("mixed serial and ISO dates are interpreted as source wall clocks", {
  x <- .fechas_quito(c("45292", "2024-01-01T23:00:00", "2024-01-01",
                        "2024-01-01 12:30", "2024-02-30 00:00:00", "units"),
                     "America/Guayaquil")
  expect_equal(format(x[1:4], "%Y-%m-%d %H:%M", tz = "America/Guayaquil"),
               c("2024-01-01 00:00", "2024-01-01 23:00",
                 "2024-01-01 00:00", "2024-01-01 12:30"))
  expect_true(all(is.na(x[5:6])))
})
