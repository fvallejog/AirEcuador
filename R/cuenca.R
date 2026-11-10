#' Import observations for one variable in Cuenca
#'
#' The IERSE service provides observations for the Universidad del Azuay
#' station. The EMOV CAIRE service provides hourly observations for Centro
#' Historico (MUN), Sector Parque Industrial (CCA) and Sector Control Sur (EIE).
#'
#' @param contaminante A variable code. UDA accepts `CO`, `NO2`, `O3`,
#'   `PM2.5` and `SO2`. CAIRE accepts `CO`, `NO2`, `PM10`, `PM2.5` and `SO2`,
#'   and the meteorological variables `RS` (global radiation), `LLU`
#'   (precipitation), `VEL` (wind speed) and `DIR` (wind direction).
#' @param fecha_inicio,fecha_fin Inclusive calendar dates. The previous calendar
#'   month is requested by default. Data availability is determined by the
#'   provider and is not guaranteed for any particular year.
#' @param year An optional integer year by which both dates are overridden.
#' @param percentil_limite An optional percentile in `(0, 1]`, default `0.999`
#'   (P99.9). For each station and pollutant the type-7 sample quantile is
#'   calculated once over the requested interval, after physical checks and
#'   any hourly aggregation, at the returned resolution. Values strictly
#'   above it become `NA`. `NULL` disables only the percentile filter.
#' @param resolucion For UDA, either `"horaria"` (hourly means) or `"original"`
#'   (the timestamps and resolution supplied by IERSE). CAIRE returns
#'   provider-supplied hourly observations with either setting.
#' @param cobertura_minima The minimum fraction of 60 distinct minutes with
#'   valid observations required for an hourly mean. A value of `0.75`
#'   (45 minutes) is used by default. An `NA` mean is returned below this
#'   threshold. No interpolation is performed. This argument is ignored for
#'   `resolucion = "original"` and for CAIRE.
#' @param tz For UDA, the time zone used to interpret the displayed source
#'   timestamps. For CAIRE, the time zone used to display the returned times.
#'   CAIRE dates and source times use `"America/Guayaquil"`. Both readers default
#'   to this time zone.
#' @param fuente `"UDA"` (the default) or `"CAIRE"`.
#' @return A data frame with `date` and either `UDA` or `MUN`, `CCA` and `EIE`
#'   columns. CAIRE aligns observations by timestamp and uses `NA` for gaps
#'   within those timestamps. `attr(x, "unidades")` records the source units.
#'   For hourly UDA results, valid-minute counts are retained in
#'   `attr(x, "cobertura")` with `date` and `UDA` columns. `NA` observations are
#'   used for missing or insufficiently covered hours.
#'   `attr(x, "control_calidad")` records physical exclusions on original
#'   observations (`etapa = "original"`) and percentile exclusions on the
#'   output (`etapa = "salida"`); these stages must not be summed as if they
#'   contained distinct observations. An empty data frame is
#'   returned with a message when the service is unavailable, a request fails,
#'   the response format changes or no records are found. CAIRE reports quality
#'   counts for the hourly output stage.
#' @details Requests are divided into intervals of at most seven days.
#'   The UDA reader parses concentration records from the chart service.
#'   The CAIRE reader parses JSON observations from the public CAIRE portal
#'   <https://caire.emov.gob.ec/datosAutomaticas> and
#'   <https://caire.emov.gob.ec/datosMete>. Station and variable coverage
#'   varies over time. Negative and nonfinite concentrations, and fractions
#'   exceeding `1e6` ppm or `1e9` ppb when those units are reported, are replaced
#'   by `NA` before hourly aggregation and valid-minute counting. No arbitrary
#'   upper mass-concentration bound is inferred. The P99.9 screen is applied
#'   to the returned resolution, once after all chunks have been combined.
#'   Rows and timestamps are retained. A percentile screen is not a regulatory
#'   limit or proof of measurement error. The reader deduplicates identical
#'   observations and rejects conflicting observations for a series and time.
#' @examples
#' if (interactive()) {
#'   datos <- extraer_Cuenca_variable("PM2.5", "2025-01-01", "2025-01-01")
#'   head(datos)
#'   attr(datos, "unidades")
#'   head(attr(datos, "cobertura"))
#'   attr(datos, "control_calidad")
#'   caire <- extraer_Cuenca_variable("PM2.5", "2026-11-09", "2026-11-10",
#'                                   fuente = "CAIRE")
#'   head(caire)
#' }
#' @export
extraer_Cuenca_variable <- function(
    contaminante = "PM2.5",
    fecha_inicio = format(.ultimo_mes(), "%Y-%m-01"),
    fecha_fin = .ultimo_mes(), year = NULL, percentil_limite = 0.999,
    resolucion = c("horaria", "original"), cobertura_minima = 0.75,
    tz = "America/Guayaquil", fuente = c("UDA", "CAIRE")) {
  fuente <- match.arg(fuente)
  variables <- if (fuente == "UDA") .variables_cuenca else .parametros_caire$variable
  .validar_variable(contaminante, variables)
  periodo <- .periodo(fecha_inicio, fecha_fin, year, 1900L)
  .validar_percentil(percentil_limite)
  .validar_tz(tz)
  resolucion <- match.arg(resolucion)
  .validar_cobertura(cobertura_minima)
  if (fuente == "CAIRE") {
    return(.leer_caire(.catalogo_caire$codigo, contaminante, periodo,
                       percentil_limite, tz))
  }
  .leer_cuenca(contaminante, periodo, percentil_limite, resolucion,
               cobertura_minima, tz)
}

#' Import observations for one station in Cuenca
#'
#' The reader combines the supported variables by timestamp and returns
#' columns for those with observations in the requested period.
#'
#' @param estacion `"UDA"` (the default), `"MUN"`, `"CCA"` or `"EIE"`.
#'   The full station names are also accepted.
#' @param tipo `"aire"` (the default) for pollutant concentrations or
#'   `"meteo"` for CAIRE meteorological observations. UDA supports `"aire"`.
#' @inheritParams extraer_Cuenca_variable
#' @return A data frame with `date`, `estacion` and available variable columns.
#'   The `unidades` attribute records source units. For hourly UDA data,
#'   `cobertura` contains the valid-minute counts. `attr(x, "control_calidad")`
#'   contains a filtering summary for each returned series, with a separate
#'   original-observation stage for UDA. An empty result and a message
#'   indicate unavailable observations or a failed request.
#' @examples
#' if (interactive()) {
#'   datos <- extraer_Cuenca_estacion(fecha_inicio = "2025-01-01",
#'                                   fecha_fin = "2025-01-01")
#'   head(datos)
#'   control_sur <- extraer_Cuenca_estacion("EIE", "2026-11-04", "2026-11-09")
#'   head(control_sur)
#' }
#' @export
extraer_Cuenca_estacion <- function(
    estacion = "UDA", fecha_inicio = format(.ultimo_mes(), "%Y-%m-01"),
    fecha_fin = .ultimo_mes(), year = NULL, percentil_limite = 0.999,
    resolucion = c("horaria", "original"), cobertura_minima = 0.75,
    tz = "America/Guayaquil", tipo = c("aire", "meteo")) {
  codigos <- c("UDA", .catalogo_caire$codigo)
  nombres <- c("Universidad del Azuay", .catalogo_caire$nombre)
  claves <- stats::setNames(rep(codigos, 2L), c(codigos, .clave(nombres)))
  if (is.character(estacion) && length(estacion) == 1L) {
    estacion <- unname(claves[.clave(estacion)])
  } else {
    estacion <- NA_character_
  }
  if (is.na(estacion)) {
    stop("Estaci\u00f3n de Cuenca desconocida. Usa UDA, MUN, CCA o EIE.", call. = FALSE)
  }
  tipo <- match.arg(tipo)
  if (estacion == "UDA" && tipo == "meteo") {
    stop("Para consultar meteorolog\u00eda de Cuenca, usa una estaci\u00f3n CAIRE.",
         call. = FALSE)
  }
  periodo <- .periodo(fecha_inicio, fecha_fin, year, 1900L)
  .validar_percentil(percentil_limite)
  .validar_tz(tz)
  resolucion <- match.arg(resolucion)
  .validar_cobertura(cobertura_minima)
  if (estacion != "UDA") {
    variables <- .parametros_caire$variable[.parametros_caire$tipo == tipo]
    return(.leer_caire(estacion, variables, periodo, percentil_limite, tz,
                       por_estacion = TRUE))
  }
  partes <- coberturas <- controles <- list()
  unidades <- character()
  for (variable in .variables_cuenca) {
    datos <- .leer_cuenca(variable, periodo, percentil_limite, resolucion,
                          cobertura_minima, tz)
    if (!nrow(datos)) next
    nombre <- make.names(variable)
    unidades[nombre] <- attr(datos, "unidades")[["UDA"]]
    controles[[variable]] <- attr(datos, "control_calidad")
    cobertura <- attr(datos, "cobertura")
    if (!is.null(cobertura)) {
      names(cobertura)[2L] <- nombre
      coberturas[[variable]] <- cobertura
    }
    names(datos)[2L] <- nombre
    partes[[variable]] <- datos
  }
  if (!length(partes)) {
    message("No Cuenca observations were available for the requested interval.")
    return(.vacio_estacion(tz))
  }
  combinar <- function(x, y) merge(x, y, by = "date", all = TRUE)
  salida <- Reduce(combinar, partes)
  salida$estacion <- rep("UDA", nrow(salida))
  salida <- salida[c("date", "estacion", setdiff(names(salida),
                                                c("date", "estacion")))]
  attr(salida, "unidades") <- unidades
  attr(salida, "control_calidad") <- do.call(rbind, controles)
  attr(salida, "cobertura") <- if (length(coberturas)) {
    Reduce(combinar, coberturas)
  } else NULL
  attr(salida, "fuente") <- "IERSE - Universidad del Azuay, Cuenca"
  salida
}

.validar_cobertura <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x > 1) {
    stop("cobertura_minima must be one number between 0 and 1.", call. = FALSE)
  }
}

.parsear_cuenca <- function(texto, tz) {
  texto <- trimws(texto)
  if (texto %in% c("", "No existe datos para su consulta",
                    "No existe datos para su consulta.",
                    "No se encontraron registros v\u00e1lidos.")) {
    return(data.frame(date = as.POSIXct(character(), tz = tz),
                      valor = numeric()))
  }
  patron <- paste0("rows1[.]push\\(\\[['\"]",
                    "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})",
                    "['\"],\\s*parseFloat\\(([^()]*)\\)\\]\\);")
  registros <- regmatches(texto, gregexpr(patron, texto, perl = TRUE))[[1L]]
  if (!length(registros) || nzchar(trimws(gsub(patron, "", texto, perl = TRUE)))) {
    stop("The Cuenca concentration response format was not recognised.")
  }
  campos <- regmatches(registros, regexec(patron, registros, perl = TRUE))
  fechas_texto <- vapply(campos, `[`, character(1), 2L)
  valores_texto <- trimws(vapply(campos, `[`, character(1), 3L))
  valores_texto <- gsub("^['\"]|['\"]$", "", valores_texto)
  numero <- "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
  if (any(!grepl(numero, valores_texto) &
          !valores_texto %in% c("NaN", "null", "undefined", "NA"))) {
    stop("An unexpected concentration value was received.")
  }
  fechas <- as.POSIXct(fechas_texto, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if (anyNA(fechas) || any(format(fechas, "%Y-%m-%d %H:%M:%S", tz = tz) !=
                           fechas_texto)) {
    stop("Invalid timestamps were received from the Cuenca service.")
  }
  # Keep raw concentrations until units have been obtained. Physical checks
  # are then applied before aggregation and their exclusions can be audited.
  salida <- unique(data.frame(date = fechas,
                               valor = suppressWarnings(as.numeric(valores_texto))))
  if (anyDuplicated(salida$date)) {
    stop("Conflicting Cuenca observations were received at the same time.")
  }
  salida[order(salida$date), , drop = FALSE]
}

.leer_cuenca <- function(variable, periodo, percentil, resolucion, cobertura, tz) {
  carpeta <- tempfile("AirEcuador-Cuenca-")
  dir.create(carpeta)
  on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
  url <- "https://ierse.uazuay.edu.ec/proyectos/sos/libudauda/ajax.php"
  codigo <- switch(variable, O3 = "OZONE", "PM2.5" = "PM25", variable)
  inicios <- seq(periodo$inicio, periodo$fin, by = 7L)
  partes <- list()
  unidad <- NULL
  tryCatch({
    for (i in seq_along(inicios)) {
      fin <- min(inicios[i] + 6L, periodo$fin)
      consulta <- list(id = "310", contaminante = codigo,
                        fini = paste0(inicios[i], "T00:00:00"),
                        ffin = paste0(fin, "T23:59:59"))
      destino <- file.path(carpeta, "respuesta.txt")
      if (i > 1L) Sys.sleep(0.25)
      if (!.descargar(url, destino, consulta)) {
        stop("An incomplete download was discarded; the interval was not returned.")
      }
      texto <- paste(readLines(destino, warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
      parte <- .parsear_cuenca(texto, tz)
      if (!nrow(parte)) next
      consulta$id <- "314"
      Sys.sleep(0.25)
      if (!.descargar(url, destino, consulta)) {
        stop("The concentration unit could not be retrieved.")
      }
      metadatos <- strsplit(paste(readLines(destino, warn = FALSE,
                                           encoding = "UTF-8"),
                                  collapse = ""), ";", fixed = TRUE)[[1L]]
      if (length(metadatos) != 3L || !nzchar(trimws(metadatos[3L]))) {
        stop("The concentration unit response format was not recognised.")
      }
      unidad_actual <- trimws(metadatos[3L])
      if (!is.null(unidad) && !identical(unidad, unidad_actual)) {
        stop("The reported concentration unit changed within the interval.")
      }
      unidad <- unidad_actual
      partes[[length(partes) + 1L]] <- parte
    }
    if (!length(partes)) {
      message("No Cuenca observations were available for ", variable, ".")
      return(.vacio_variable(tz))
    }
    datos <- unique(do.call(rbind, partes))
    if (anyDuplicated(datos$date)) stop("Conflicting timestamps were received.")
    limites <- .limites(periodo, tz)
    datos <- datos[datos$date >= limites[1L] & datos$date < limites[2L], ,
                   drop = FALSE]
    if (!nrow(datos)) return(.vacio_variable(tz))
    originales <- .depurar_valores(datos$valor, NULL, variable, unidad)
    datos$valor <- originales$valores
    if (resolucion == "horaria") {
      salida <- .agregar_cuenca(datos, limites, cobertura, tz)
    } else {
      salida <- datos[order(datos$date), , drop = FALSE]
      names(salida)[2L] <- "UDA"
    }
    depurado <- .depurar_valores(salida$UDA, percentil, variable, unidad)
    salida$UDA <- depurado$valores
    rownames(salida) <- NULL
    attr(salida, "unidades") <- c(UDA = unidad)
    attr(salida, "control_calidad") <- rbind(
      cbind(estacion = "UDA", variable = variable, etapa = "original",
            resolucion = "original", unidad = unidad, originales$control),
      cbind(estacion = "UDA", variable = variable, etapa = "salida",
            resolucion = resolucion, unidad = unidad, depurado$control))
    attr(salida, "fuente") <- "IERSE - Universidad del Azuay, Cuenca"
    salida
  }, error = function(e) {
    message("Cuenca data for ", variable, " could not be processed: ",
            conditionMessage(e))
    .vacio_variable(tz)
  })
}

.agregar_cuenca <- function(datos, limites, cobertura, tz) {
  minuto <- floor(as.numeric(datos$date) / 60) * 60
  grupos <- split(datos$valor, minuto)
  media <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  minutos <- data.frame(tiempo = as.numeric(names(grupos)),
                         valor = vapply(grupos, media, numeric(1)))
  origen <- as.numeric(limites[1L])
  horas <- floor((minutos$tiempo - origen) / 3600) * 3600 + origen
  grupos <- split(minutos$valor, horas)
  n <- vapply(grupos, function(x) sum(!is.na(x)), integer(1))
  medias <- vapply(grupos, media, numeric(1))
  medias[n < max(1L, ceiling(60 * cobertura))] <- NA_real_
  rejilla <- seq(as.numeric(limites[1L]), as.numeric(limites[2L]) - 3600,
                 by = 3600)
  indice <- match(rejilla, as.numeric(names(grupos)))
  salida <- data.frame(date = as.POSIXct(rejilla, origin = "1970-01-01", tz = tz),
                        UDA = unname(medias[indice]))
  cuentas <- unname(n[indice])
  cuentas[is.na(cuentas)] <- 0L
  attr(salida, "cobertura") <- data.frame(date = salida$date, UDA = cuentas)
  salida
}
