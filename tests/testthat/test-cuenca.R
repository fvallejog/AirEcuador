cuenca_texto <- function(fechas, valores) {
  paste0("rows1.push(['", fechas, "', parseFloat(", valores, ")]);", collapse = "")
}

test_that("concentrations are parsed without evaluating remote JavaScript", {
  texto <- cuenca_texto(c("2025-01-01 00:00:00", "2025-01-01 00:01:00"),
                         c("6.47", "-1"))
  x <- .parsear_cuenca(texto, "America/Guayaquil")
  expect_equal(x$valor, c(6.47, NA_real_))
  expect_error(.parsear_cuenca(paste0(texto, "system('bad')"), "UTC"),
                 "format was not recognised")
  expect_error(.parsear_cuenca(cuenca_texto("2025-01-01 00:00:00",
                                           "system('bad')"), "UTC"), "format")
  expect_error(.parsear_cuenca(cuenca_texto("2025-02-30 00:00:00", "1"),
                               "UTC"), "Invalid timestamps")
  expect_equal(nrow(.parsear_cuenca("No existe datos para su consulta.", "UTC")), 0)
  expect_equal(nrow(.parsear_cuenca(paste0(texto, texto), "UTC")), 2)
  expect_error(.parsear_cuenca(cuenca_texto(rep("2025-01-01 00:00:00", 2),
                                           c("1", "2")), "UTC"), "Conflicting")
})

test_that("hourly completeness is counted by distinct valid minutes", {
  inicio <- as.POSIXct("2025-01-01 00:00:00", tz = "America/Guayaquil")
  datos <- data.frame(date = c(inicio + (0:44) * 60,
                               inicio + 3600 + (0:43) * 60),
                       valor = c(rep(10, 45), rep(20, 44)))
  x <- .agregar_cuenca(datos, c(inicio, inicio + 3 * 3600), 0.75,
                        "America/Guayaquil")
  expect_equal(x$UDA, c(10, NA_real_, NA_real_))
  expect_equal(attr(x, "cobertura")$UDA, c(45L, 44L, 0L))
  # Multiple observations in the same minute must not inflate coverage.
  repetidos <- rbind(datos[1:20, ], transform(datos[1:20, ], date = date + 1))
  x <- .agregar_cuenca(repetidos, c(inicio, inicio + 3600), 0.75, "UTC")
  expect_true(is.na(x$UDA))
  expect_equal(attr(x, "cobertura")$UDA, 20L)
})

test_that("chunk boundaries, ozone codes, units and the final day are handled", {
  llamadas <- list()
  local_mocked_bindings(.descargar = function(url, destino, query) {
    llamadas[[length(llamadas) + 1L]] <<- query
    if (query$id == "314") {
      writeLines("1;2;ppm", destino)
    } else {
      dia <- substr(query$fini, 1, 10)
      writeLines(cuenca_texto(paste(dia, "23:59:00"), "0.4"), destino)
    }
    TRUE
  })
  x <- extraer_Cuenca_variable("O3", "2025-01-01", "2025-01-08",
                               resolucion = "original")
  expect_equal(nrow(x), 2L)
  expect_equal(attr(x, "unidades"), c(UDA = "ppm"))
  expect_equal(llamadas[[1]]$contaminante, "OZONE")
  expect_equal(llamadas[[1]]$ffin, "2025-01-07T23:59:59")
  expect_equal(llamadas[[3]]$fini, "2025-01-08T00:00:00")
  expect_equal(format(x$date[2], "%Y-%m-%d %H:%M", tz = "America/Guayaquil"),
               "2025-01-08 23:59")
})

test_that("failed chunks cannot produce an apparently complete series", {
  contador <- 0L
  local_mocked_bindings(.descargar = function(url, destino, query) {
    if (query$id == "314") {
      writeLines("1;2;ppm", destino)
    } else {
      contador <<- contador + 1L
      if (contador == 2L) return(FALSE)
      writeLines(cuenca_texto("2025-01-01 00:00:00", "1"), destino)
    }
    TRUE
  })
  expect_message(x <- extraer_Cuenca_variable("CO", "2025-01-01", "2025-01-08"),
                   "incomplete download")
  expect_equal(nrow(x), 0L)
})

test_that("unit changes within one variable are rejected", {
  contador <- 0L
  local_mocked_bindings(.descargar = function(url, destino, query) {
    if (query$id == "314") {
      contador <<- contador + 1L
      writeLines(if (contador == 1L) "1;2;ppm" else "1;2;ug/m3", destino)
    } else {
      writeLines(cuenca_texto(paste(substr(query$fini, 1, 10), "00:00:00"), "1"),
                  destino)
    }
    TRUE
  })
  expect_message(x <- extraer_Cuenca_variable("CO", "2025-01-01", "2025-01-08"),
                   "unit changed")
  expect_equal(nrow(x), 0L)
})

test_that("station output reports individual pollutant units and coverage", {
  local_mocked_bindings(.leer_cuenca = function(variable, ...) {
    x <- data.frame(date = as.POSIXct("2025-01-01", tz = "America/Guayaquil"),
                     UDA = 1)
    attr(x, "unidades") <- c(UDA = if (variable == "PM2.5") "ug/m3" else "ppm")
    attr(x, "cobertura") <- data.frame(date = x$date, UDA = 60L)
    x
  })
  x <- extraer_Cuenca_estacion(year = 2025)
  expect_named(x, c("date", "estacion", "CO", "NO2", "O3", "PM2.5", "SO2"))
  expect_equal(x$estacion, "UDA")
  expect_equal(attr(x, "unidades")[["PM2.5"]], "ug/m3")
  expect_equal(attr(x, "unidades")[["O3"]], "ppm")
  expect_named(attr(x, "cobertura"), c("date", "CO", "NO2", "O3", "PM2.5", "SO2"))
})
