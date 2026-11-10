.catalogo_caire <- data.frame(
  codigo = c("MUN", "CCA", "EIE"),
  id = 1:3,
  respuesta = c("ESTACION01", "ESTACION02", "ESTACION03"),
  nombre = c("Centro Hist\u00f3rico", "Sector Parque Industrial", "Sector Control Sur"),
  latitud = c(-2.897, -2.879789, -2.91641226),
  longitud = c(-79.003, -78.980213, -79.03393596),
  stringsAsFactors = FALSE
)

.parametros_caire <- data.frame(
  variable = c("CO", "NO2", "PM10", "PM2.5", "SO2", "RS", "LLU", "VEL", "DIR"),
  id = c(1L, 4L, 8L, 7L, 6L, 13L, 14L, 15L, 16L),
  acronimo = c("CO", "NO2", "PM10", "PM25", "SO2", "RAD_GLOBAL", "PRECIPITACION",
               "VELOCIDAD_VIENTO", "DIRECCION_VIENTO"),
  tipo = rep(c("aire", "meteo"), c(5L, 4L)),
  stringsAsFactors = FALSE
)

.tz_caire <- "America/Guayaquil"

.solicitar_caire <- function(cuerpo) {
  json <- jsonlite::toJSON(cuerpo, auto_unbox = TRUE, null = "null", digits = NA)
  respuesta <- httr::POST(
    "https://caire.emov.gob.ec/datos/datos/consulta",
    httr::content_type_json(), httr::accept_json(), httr::timeout(60),
    body = charToRaw(enc2utf8(json)), encode = "raw"
  )
  httr::stop_for_status(respuesta)
  httr::content(respuesta, "text", encoding = "UTF-8")
}

.parsear_caire <- function(texto, estaciones, variables) {
  respuesta <- jsonlite::fromJSON(texto)
  if (is.list(respuesta) && isTRUE(respuesta$demasiadosDatos)) {
    stop("CAIRE indica que hay demasiados datos para la consulta.")
  }
  if (!is.list(respuesta) || !identical(respuesta$demasiadosDatos, FALSE)) {
    stop("CAIRE devolvi\u00f3 una respuesta inesperada.")
  }
  crudos <- respuesta$datos
  if (identical(crudos, list())) {
    return(data.frame(date = as.POSIXct(character(), tz = .tz_caire),
                      estacion = character(), variable = character(),
                      valor = numeric(), unidad = character()))
  }
  campos <- c("estacion", "parametro", "fecha", "valor", "unidad")
  if (!is.data.frame(crudos) || !all(campos %in% names(crudos)) ||
      !is.atomic(crudos$valor)) {
    stop("CAIRE devolvi\u00f3 observaciones con otro formato.")
  }
  estacion <- .catalogo_caire$codigo[match(crudos$estacion, .catalogo_caire$respuesta)]
  variable <- .parametros_caire$variable[match(crudos$parametro, .parametros_caire$acronimo)]
  if (any(!estacion %in% estaciones) || any(!variable %in% variables)) {
    stop("CAIRE devolvi\u00f3 estaciones o par\u00e1metros ajenos a la consulta.")
  }
  fecha_texto <- trimws(crudos$fecha)
  formato <- "%Y/%m/%d %H:%M:%S"
  fecha <- as.POSIXct(fecha_texto, format = formato, tz = .tz_caire)
  if (anyNA(fecha) || any(format(fecha, formato, tz = .tz_caire) != fecha_texto)) {
    stop("CAIRE devolvi\u00f3 fechas inv\u00e1lidas o con otro formato.")
  }
  unidad <- trimws(crudos$unidad)
  unidad[unidad %in% "UG_M3"] <- "ug/m3"
  data.frame(date = fecha, estacion = estacion, variable = variable,
             valor = crudos$valor, unidad = unidad, stringsAsFactors = FALSE)
}

.leer_caire <- function(estaciones, variables, periodo, percentil, tz,
                        por_estacion = FALSE) {
  vacio <- if (por_estacion) .vacio_estacion(tz) else .vacio_variable(tz)
  if (!por_estacion) {
    for (estacion in estaciones) vacio[[estacion]] <- numeric()
  }
  tryCatch({
    sitios <- .catalogo_caire[match(estaciones, .catalogo_caire$codigo), ]
    parametros <- .parametros_caire[match(variables, .parametros_caire$variable), ]
    cuerpo <- list(
      estaciones = lapply(seq_len(nrow(sitios)), function(i) {
        list(id = sitios$id[i], direccion = NULL, altura = NULL,
             latitud = sitios$latitud[i], longitud = sitios$longitud[i],
             nombre = sitios$nombre[i], nombreVisual = NULL)
      }),
      parametros = lapply(seq_len(nrow(parametros)), function(i) {
        list(id = parametros$id[i], acronimo = parametros$acronimo[i])
      }),
      tipoPromedio = 2L
    )
    inicios <- seq(periodo$inicio, periodo$fin, by = 7L)
    partes <- vector("list", length(inicios))
    for (i in seq_along(inicios)) {
      tramo <- list(inicio = inicios[i], fin = min(inicios[i] + 6L, periodo$fin))
      limites <- as.numeric(.limites(tramo, .tz_caire)) * 1000
      cuerpo$fechaInicio <- limites[1L]
      cuerpo$fechaFin <- limites[2L] - 1
      if (i > 1L) Sys.sleep(0.25)
      partes[[i]] <- .parsear_caire(.solicitar_caire(cuerpo), estaciones, variables)
    }
    datos <- unique(do.call(rbind, partes))
    limites <- .limites(periodo, .tz_caire)
    datos <- datos[datos$date >= limites[1L] & datos$date < limites[2L], , drop = FALSE]
    if (!nrow(datos)) {
      message("CAIRE devolvi\u00f3 cero observaciones para el periodo solicitado.")
      return(vacio)
    }
    if (anyDuplicated(datos[c("date", "estacion", "variable")])) {
      stop("CAIRE devolvi\u00f3 observaciones incompatibles para la misma hora.")
    }

    attr(datos$date, "tzone") <- tz
    salida <- data.frame(date = sort(unique(datos$date)))
    if (por_estacion) salida$estacion <- rep(estaciones, nrow(salida))
    clave <- if (por_estacion) datos$variable else datos$estacion
    columnas <- if (por_estacion) intersect(variables, clave) else estaciones
    unidades <- stats::setNames(rep(NA_character_, length(columnas)), columnas)
    controles <- list()
    for (columna in columnas) {
      serie <- datos[clave == columna, , drop = FALSE]
      if (!nrow(serie)) {
        salida[[columna]] <- rep(NA_real_, nrow(salida))
        next
      }
      unidad <- unique(serie$unidad)
      if (length(unidad) != 1L) {
        stop("CAIRE devolvi\u00f3 unidades distintas para una misma serie.")
      }
      control <- .depurar_valores(serie$valor, percentil, serie$variable[1L], unidad)
      salida[[columna]] <- control$valores[match(salida$date, serie$date)]
      unidades[columna] <- unidad
      controles[[columna]] <- cbind(
        estacion = serie$estacion[1L], variable = serie$variable[1L],
        etapa = "salida", resolucion = "horaria", unidad = unidad, control$control
      )
    }
    attr(salida, "unidades") <- unidades
    attr(salida, "control_calidad") <- do.call(rbind, controles)
    attr(salida, "fuente") <- "EMOV EP - CAIRE"
    salida
  }, error = function(e) {
    message("No se pudo procesar la consulta de CAIRE (", conditionMessage(e), ").")
    vacio
  })
}
