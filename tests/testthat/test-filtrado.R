descarga_cuenca_filtrado <- function(fechas, valores, unidad = "ppm") {
  force(fechas)
  force(valores)
  force(unidad)
  function(url, destino, query = NULL) {
    if (query$id == "314") {
      writeLines(paste0("1;2;", unidad), destino)
    } else {
      conservar <- fechas >= sub("T", " ", query$fini, fixed = TRUE) &
        fechas <= sub("T", " ", query$ffin, fixed = TRUE)
      writeLines(paste0("rows1.push(['", fechas[conservar],
                        "', parseFloat(", valores[conservar], ")]);",
                        collapse = ""), destino)
    }
    TRUE
  }
}

test_that("physical exclusions precede a single upper-tail sample quantile", {
  x <- .depurar_valores(c(-1, 0, 1, 2, 3, 1e7, NA, NaN, Inf),
                         0.999, "CO", "ppm")
  expect_equal(x$valores, c(NA, 0, 1, 2, NA, NA, NA, NA, NA))
  expect_equal(x$control$n_imposibles, 2L)
  expect_equal(x$control$n_no_finitos, 2L)
  expect_equal(x$control$n_faltantes_entrada, 1L)
  expect_equal(x$control$n_percentil, 1L)
  expect_equal(x$control$umbral_percentil, 2.997)
  expect_equal(x$control$n_validos, 3L)
  iguales <- .depurar_valores(rep(5, 10), 0.999, "CO", "ppm")
  expect_equal(iguales$valores, rep(5, 10))
  expect_equal(iguales$control$n_percentil, 0L)
})

test_that("unit-aware domains preserve legitimate signed temperatures", {
  expect_equal(.filtrar_valores(c(-300, -5, 0, 25), NULL,
                                variable = "TMP", unidad = "\u00b0C"),
               c(NA, -5, 0, 25))
  expect_equal(.filtrar_valores(c(-1, 0, 273.15), NULL,
                                variable = "TMP", unidad = "K"),
               c(NA, 0, 273.15))
  expect_equal(.filtrar_valores(c(-460, -30, 32), NULL,
                                variable = "TMP", unidad = "F"), c(NA, -30, 32))
  expect_equal(.filtrar_valores(c(-1, 0, 100, 101), NULL,
                                variable = "HUM", unidad = "%"), c(NA, 0, 100, NA))
  expect_equal(.filtrar_valores(c(-1, 0, 360, 361), NULL,
                                variable = "DIR", unidad = "degrees"),
               c(NA, 0, 360, NA))
  expect_equal(.filtrar_valores(c(0, 2 * pi, 7), NULL,
                                variable = "DIR", unidad = "rad"),
               c(0, 2 * pi, NA))
  expect_equal(.filtrar_valores(c(-1, 0, 750), NULL, variable = "PRE"),
               c(NA, NA, 750))
  expect_equal(.filtrar_valores(c(-1, 0, 1e9), NULL,
                                variable = "PM2.5", unidad = "ug/m3"),
               c(NA, 0, 1e9))
  expect_equal(.filtrar_valores(c(1e6, 1e6 + 1), NULL,
                                variable = "CO", unidad = "ppm"), c(1e6, NA))
  expect_equal(.filtrar_valores(c(1e9, 1e9 + 1), NULL,
                                variable = "NO2", unidad = "ppb"), c(1e9, NA))
  # Unknown units must not silently be assigned a Celsius-specific bound.
  expect_equal(.filtrar_valores(c(-300, -5), NULL, variable = "TMP"), c(-300, -5))
})

test_that("the audit accounts for missing, malformed and excluded observations", {
  x <- .depurar_valores(c("bad", NA, "NaN", "Inf", "-1", "1", "2", "9999999"),
                         0.999, "CO", "ppm")
  control <- x$control
  expect_equal(control$n_no_numericos, 1L)
  expect_equal(control$n_entrada,
               control$n_faltantes_entrada + control$n_no_numericos +
                 control$n_no_finitos + control$n_imposibles +
                 control$n_percentil + control$n_validos)
  expect_equal(x$valores, c(NA, NA, NA, NA, NA, 1, NA, NA))
})

test_that("the four exported extractors share the maintainer's P99.9 default", {
  for (f in list(extraer_Quito_variable, extraer_Quito_estacion,
                 extraer_Cuenca_variable, extraer_Cuenca_estacion)) {
    expect_identical(formals(f)$percentil_limite, 0.999)
  }
})

test_that("Quito thresholds are independent for differently scaled stations", {
  fechas <- format(seq(as.POSIXct("2024-01-01", tz = "UTC"), by = "hour",
                        length.out = 1001), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  crudos <- data.frame(
    V1 = c("FECHA", "unidad", fechas),
    V2 = c("Guamani", "mg/m3", rep("10", 1000), "1000"),
    V3 = c("Los Chillos", "mg/m3", rep("10000", 1000), "20000"))
  periodo <- .periodo("2024-01-01", "2024-02-11", NULL, 2004L)
  x <- .procesar_quito(crudos, periodo, 0.999, "CO", "UTC")
  expect_equal(nrow(x), 1001L)
  expect_true(is.na(tail(x$Guamani, 1)))
  expect_true(is.na(tail(x$Los_Chillos, 1)))
  expect_equal(sum(!is.na(x$Guamani)), 1000L)
  expect_equal(sum(!is.na(x$Los_Chillos)), 1000L)
  control <- attr(x, "control_calidad")
  expect_equal(control$umbral_percentil, c(10, 10000))
  expect_equal(control$n_percentil, c(1L, 1L))
  local_mocked_bindings(.leer_quito = function(variable, periodo, percentil, tz) {
    if (variable != "CO") return(.vacio_variable(tz))
    .procesar_quito(crudos, periodo, percentil, variable, tz)
  })
  estacion <- extraer_Quito_estacion("Los Chillos", "2024-01-01", "2024-02-11")
  control <- attr(estacion, "control_calidad")
  expect_equal(control$estacion, "Los_Chillos")
  expect_equal(control$variable, "CO")
  expect_equal(control$umbral_percentil, 10000)
})

test_that("Cuenca removes impossible raw observations before hourly completeness", {
  fechas <- format(as.POSIXct("2025-01-01", tz = "UTC") + (0:44) * 60,
                     "%Y-%m-%d %H:%M:%S", tz = "UTC")
  local_mocked_bindings(
    .descargar = descarga_cuenca_filtrado(fechas, c(rep(1, 44), 1e7)))
  x <- extraer_Cuenca_variable("CO", "2025-01-01", "2025-01-01")
  expect_equal(nrow(x), 24L)
  expect_true(all(is.na(x$UDA)))
  expect_equal(attr(x, "cobertura")$UDA[1], 44L)
  control <- attr(x, "control_calidad")
  expect_equal(control$etapa, c("original", "salida"))
  expect_equal(control$n_imposibles, c(1L, 0L))
  expect_equal(control$n_validos, c(44L, 0L))
})

test_that("Cuenca P99.9 is calculated once across downloaded chunks", {
  fechas <- format(seq(as.POSIXct("2025-01-01", tz = "UTC"), by = "day",
                        length.out = 14), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  local_mocked_bindings(
    .descargar = descarga_cuenca_filtrado(fechas, c(1:13, 1000)))
  x <- extraer_Cuenca_variable("CO", "2025-01-01", "2025-01-14",
                               resolucion = "original")
  expect_equal(x$UDA, c(1:13, NA_real_))
  control <- attr(x, "control_calidad")
  expect_equal(control$n_percentil, c(0L, 1L))
  expect_equal(control$percentil, c(NA_real_, 0.999))
  sin_percentil <- extraer_Cuenca_variable("CO", "2025-01-01", "2025-01-14",
                                           percentil_limite = NULL,
                                           resolucion = "original")
  expect_equal(sin_percentil$UDA, c(1:13, 1000))
  expect_equal(attr(sin_percentil, "control_calidad")$n_percentil, c(0L, 0L))
})

test_that("Cuenca upper-tail filtering targets the requested output resolution", {
  inicio <- as.POSIXct("2025-01-01", tz = "UTC")
  fechas <- format(inicio + rep(0:23, each = 45) * 3600 +
                     rep(0:44, 24) * 60, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  local_mocked_bindings(
    .descargar = descarga_cuenca_filtrado(fechas, rep(1:24, each = 45)))
  x <- extraer_Cuenca_variable("CO", "2025-01-01", "2025-01-01")
  expect_equal(x$UDA, c(1:23, NA_real_))
  expect_equal(attr(x, "cobertura")$UDA, rep(45L, 24))
  control <- attr(x, "control_calidad")
  expect_equal(control$resolucion, c("original", "horaria"))
  expect_equal(control$n_percentil, c(0L, 1L))
})
