caire_json <- function(estacion, parametro, fecha, valor, unidad = "UG_M3") {
  datos <- data.frame(estacion = estacion, parametro = parametro, fecha = fecha,
                      valor = valor, unidad = unidad)
  jsonlite::toJSON(list(demasiadosDatos = FALSE, datos = datos),
                   auto_unbox = TRUE, na = "null")
}

test_that("CAIRE sends the captured JSON fields and aligns available stations", {
  llamadas <- list()
  local_mocked_bindings(POST = function(url, ..., body, encode) {
    expect_equal(url, "https://caire.emov.gob.ec/datos/datos/consulta")
    expect_identical(encode, "raw")
    consulta <- jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE)
    llamadas[[length(llamadas) + 1L]] <<- consulta
    texto <- if (length(llamadas) == 1L) {
      caire_json(c("ESTACION01", "ESTACION02", "ESTACION01"), "PM25",
                 c("2025/01/01 00:00:00", "2025/01/01 00:00:00", "2025/01/02 00:00:00"),
                 c(1, 5, -1))
    } else {
      caire_json(c("ESTACION01", "ESTACION02", "ESTACION02"), "PM25",
                 c("2025/01/08 23:00:00", "2025/01/08 23:00:00", "2025/01/09 00:00:00"),
                 c(100, 5, 1000))
    }
    structure(list(url = url, status_code = 200L,
                   headers = list("content-type" = "application/json"),
                   content = charToRaw(enc2utf8(texto))), class = "response")
  }, .package = "httr")

  x <- extraer_Cuenca_variable("PM2.5", "2025-01-01", "2025-01-08",
                               fuente = "CAIRE", tz = "UTC")
  expect_length(llamadas, 2L)
  expect_identical(llamadas[[1]]$tipoPromedio, 2L)
  expect_identical(llamadas[[1]]$parametros, list(list(id = 7L, acronimo = "PM25")))
  expect_identical(vapply(llamadas[[1]]$estaciones, function(e) e$id, 0L), 1:3)
  expect_identical(llamadas[[1]]$estaciones[[3L]], list(
    id = 3L, direccion = NULL, altura = NULL, latitud = -2.91641226,
    longitud = -79.03393596, nombre = "Sector Control Sur", nombreVisual = NULL
  ))
  expect_equal(llamadas[[1]]$fechaInicio,
               1000 * as.numeric(as.POSIXct("2025-01-01 05:00:00", tz = "UTC")))
  expect_equal(llamadas[[1]]$fechaFin + 1, llamadas[[2]]$fechaInicio)
  expect_named(x, c("date", "MUN", "CCA", "EIE"))
  expect_equal(format(x$date[c(1, 3)], "%Y-%m-%d %H:%M", tz = "UTC"),
               c("2025-01-01 05:00", "2025-01-09 04:00"))
  expect_identical(attr(x$date, "tzone"), "UTC")
  expect_equal(x$MUN, c(1, NA_real_, NA_real_))
  expect_equal(x$CCA, c(5, NA_real_, 5))
  expect_true(all(is.na(x$EIE)))
  expect_identical(attr(x, "unidades"), c(MUN = "ug/m3", CCA = "ug/m3", EIE = NA_character_))
  calidad <- attr(x, "control_calidad")
  expect_equal(calidad$n_imposibles, c(1, 0))
  expect_equal(calidad$n_percentil, c(1, 0))
})

test_that("the CAIRE station reader returns the available variables and their units", {
  local_mocked_bindings(.solicitar_caire = function(cuerpo) {
    expect_length(cuerpo$estaciones, 1L)
    expect_identical(cuerpo$estaciones[[1L]]$id, 3L)
    caire_json("ESTACION03", c("CO", "SO2"), "2025/01/01 00:00:00",
               c(0.51, 1.2), c("PPM", "UG_M3"))
  })
  x <- extraer_Cuenca_estacion("Sector Control Sur", "2025-01-01", "2025-01-01")
  expect_named(x, c("date", "estacion", "CO", "SO2"))
  expect_equal(x$estacion, "EIE")
  expect_equal(attr(x, "unidades"), c(CO = "PPM", SO2 = "ug/m3"))
  expect_null(attr(x, "cobertura"))
  expect_equal(attr(x, "control_calidad")$n_validos, c(1, 1))
})

test_that("CAIRE meteorology uses the captured IDs and retains provider units", {
  local_mocked_bindings(.solicitar_caire = function(cuerpo) {
    expect_identical(cuerpo$estaciones[[1L]]$id, 1L)
    expect_identical(vapply(cuerpo$parametros, function(p) p$id, 0L), 13:16)
    acronimos <- c("RAD_GLOBAL", "PRECIPITACION", "VELOCIDAD_VIENTO", "DIRECCION_VIENTO")
    expect_identical(vapply(cuerpo$parametros, function(p) p$acronimo, ""), acronimos)
    caire_json("ESTACION01", acronimos, "2025/01/01 00:00:00",
               c(200, 0, 2.2, 361), c("W_M2", "MM", "M_S", "GRADOS"))
  })
  x <- extraer_Cuenca_estacion("Centro Hist\u00f3rico", "2025-01-01", "2025-01-01",
                              tipo = "meteo")
  expect_named(x, c("date", "estacion", "RS", "LLU", "VEL", "DIR"))
  expect_identical(attr(x, "unidades"),
                   c(RS = "W_M2", LLU = "MM", VEL = "M_S", DIR = "GRADOS"))
  expect_true(is.na(x$DIR))
  expect_equal(attr(x, "control_calidad")$n_imposibles, c(0, 0, 0, 1))
})

test_that("empty CAIRE data and a failed chunk are reported separately", {
  local_mocked_bindings(.solicitar_caire = function(...) {
    '{"demasiadosDatos":false,"datos":[]}'
  })
  expect_message(
    x <- extraer_Cuenca_variable("PM2.5", "2025-01-01", "2025-01-01", fuente = "CAIRE"),
    "cero observaciones"
  )
  expect_named(x, c("date", "MUN", "CCA", "EIE"))
  expect_equal(nrow(x), 0L)

  llamadas <- 0L
  local_mocked_bindings(.solicitar_caire = function(...) {
    llamadas <<- llamadas + 1L
    if (llamadas == 2L) stop("HTTP 503")
    caire_json("ESTACION01", "PM25", "2025/01/01 00:00:00", 1)
  })
  expect_message(
    x <- extraer_Cuenca_variable("PM2.5", "2025-01-01", "2025-01-08", fuente = "CAIRE"),
    "HTTP 503"
  )
  expect_equal(nrow(x), 0L)
})
