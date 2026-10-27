test_that("invalid arguments are rejected before any network access", {
  local_mocked_bindings(.descargar = function(...) stop("NETWORK MUST NOT RUN"))
  for (year in list(NA_real_, Inf, c(2023, 2024), 2024.5, "2024")) {
    expect_error(extraer_Quito_variable(year = year), "year must")
  }
  for (percentil in list(NA_real_, Inf, c(0.9, 1), -1, 0, 1.1)) {
    expect_error(extraer_Quito_variable(percentil_limite = percentil),
                 "percentil_limite")
  }
  expect_error(extraer_Quito_variable(fecha_inicio = "2024-02-30"), "invalid")
  expect_error(extraer_Quito_variable(fecha_inicio = "2024-01-02",
                                      fecha_fin = "2024-01-01"), "must not exceed")
  expect_error(extraer_Quito_variable(contaminante = NA_character_), "variable")
  expect_error(extraer_Quito_variable(tz = "invalid"), "time-zone")
  expect_error(extraer_Quito_estacion("unknown"), "Unknown Quito")
  expect_error(extraer_Cuenca_estacion("MUN"), "Only the UDA")
  expect_error(extraer_Cuenca_variable("PM10"), "variable")
  expect_error(extraer_Cuenca_variable(cobertura_minima = NA_real_), "cobertura")
})

test_that("accent and space normalization is anchored to supported names", {
  expect_equal(.estacion_quito(" Guaman\u00ed ", TRUE), "Guamani")
  expect_equal(.estacion_quito("Los_Chillos", TRUE), "Los_Chillos")
  expect_error(.estacion_quito("Los Chillos unknown", TRUE), "Unknown")
})

test_that("upper-tail removal is optional and nonfinite values remain missing", {
  expect_equal(.filtrar_valores(c(1, 2, 1000, -1, Inf), NULL),
               c(1, 2, 1000, NA_real_, NA_real_))
  expect_equal(.filtrar_valores(c(1, 2, 1000), 0.5), c(1, 2, NA_real_))
  expect_equal(.filtrar_valores(c(NA, NA), 0.999), c(NA_real_, NA_real_))
})
