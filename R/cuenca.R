#' Import air pollutant observations from Cuenca
#'
#' Historical observations from the Universidad del Azuay (UDA) station are
#' downloaded through the public service used by the IERSE website. This
#' station is identified explicitly; coverage of the EMOV station network is
#' not implied. PM10 and meteorological variables are not provided by this
#' service.
#'
#' @param contaminante One of `CO`, `NO2`, `O3`, `PM2.5` or `SO2`.
#' @param fecha_inicio,fecha_fin Inclusive calendar dates. The previous calendar
#'   month is requested by default. Data availability is determined by the
#'   provider and is not guaranteed for any particular year.
#' @param year An optional integer year by which both dates are overridden.
#' @param percentil_limite An optional percentile in `(0, 1]`. No upper
#'   percentile filter is applied by default. When supplied, filtering is
#'   applied to the returned resolution, after aggregation.
#' @param resolucion Either `"horaria"` (hourly means) or `"original"` (the
#'   timestamps and resolution supplied by the provider).
#' @param cobertura_minima The minimum fraction of 60 distinct minutes with
#'   valid observations required for an hourly mean. A value of `0.75`
#'   (45 minutes) is used by default. An `NA` mean is returned below this
#'   threshold. No interpolation is performed. This argument is ignored for
#'   `resolucion = "original"`.
#' @param tz A time zone by which the displayed source times are interpreted.
#'   Ecuadorian local time (`"America/Guayaquil"`) is assumed by default.
#'   This assumption should be verified with the provider for critical uses.
#' @return A data frame with `date` and `UDA` columns. The source unit is
#'   retained in `attr(x, "unidades")`; no unit conversion is performed.
#'   For hourly results, valid-minute counts are retained in
#'   `attr(x, "cobertura")` with `date` and `UDA` columns. `NA` observations are
#'   used for missing or insufficiently covered hours. An empty data frame is
#'   returned with a message when the service is unavailable, a request fails,
#'   the response format changes or no records are found.
#' @details Requests are divided into intervals of at most seven days.
#'   Only concentration records are parsed; remotely supplied JavaScript is
#'   never evaluated. Negative and nonfinite concentrations are replaced by
#'   `NA`. Repeated, identical observations are deduplicated. Conflicting
#'   observations at an identical timestamp are rejected.
#' @examples
#' if (interactive()) {
#'   datos <- extraer_Cuenca_variable("PM2.5", "2025-01-01", "2025-01-01")
#'   head(datos)
#'   attr(datos, "unidades")
#'   head(attr(datos, "cobertura"))
#' }
#' @export
extraer_Cuenca_variable <- function(
    contaminante = "PM2.5",
    fecha_inicio = format(.ultimo_mes(), "%Y-%m-01"),
    fecha_fin = .ultimo_mes(), year = NULL, percentil_limite = NULL,
    resolucion = c("horaria", "original"), cobertura_minima = 0.75,
    tz = "America/Guayaquil") {
  .validar_variable(contaminante, .variables_cuenca)
  periodo <- .periodo(fecha_inicio, fecha_fin, year, 1900L)
  .validar_percentil(percentil_limite)
  .validar_tz(tz)
  resolucion <- match.arg(resolucion)
  .validar_cobertura(cobertura_minima)
  .leer_cuenca(contaminante, periodo, percentil_limite, resolucion,
               cobertura_minima, tz)
}

#' Import all supported pollutants for the UDA station in Cuenca
#'
#' Concentrations of CO, NO2, O3, PM2.5 and SO2 are downloaded separately and
#' combined by time. Unavailable pollutants are reported and omitted.
#'
#' @param estacion The station name `"UDA"` or `"Universidad del Azuay"`.
#' @inheritParams extraer_Cuenca_variable
#' @return A data frame with `date`, `estacion` and available pollutant
#'   columns. Original units and hourly valid-minute counts are retained in
#'   the `unidades` and `cobertura` attributes, respectively. An empty data frame
#'   is returned with a message if no observations are available.
#' @examples
#' if (interactive()) {
#'   datos <- extraer_Cuenca_estacion(fecha_inicio = "2025-01-01",
#'                                   fecha_fin = "2025-01-01")
#'   head(datos)
#' }
#' @export
extraer_Cuenca_estacion <- function(
    estacion = "UDA", fecha_inicio = format(.ultimo_mes(), "%Y-%m-01"),
    fecha_fin = .ultimo_mes(), year = NULL, percentil_limite = NULL,
    resolucion = c("horaria", "original"), cobertura_minima = 0.75,
    tz = "America/Guayaquil") {
  if (!is.character(estacion) || length(estacion) != 1L || is.na(estacion) ||
      !.clave(estacion) %in% c("UDA", "UNIVERSIDADDELAZUAY")) {
    stop("Only the UDA station is supported by this Cuenca service.",
         call. = FALSE)
  }
  periodo <- .periodo(fecha_inicio, fecha_fin, year, 1900L)
  .validar_percentil(percentil_limite)
  .validar_tz(tz)
  resolucion <- match.arg(resolucion)
  .validar_cobertura(cobertura_minima)
  partes <- coberturas <- list()
  unidades <- character()
  for (variable in .variables_cuenca) {
    datos <- .leer_cuenca(variable, periodo, percentil_limite, resolucion,
                          cobertura_minima, tz)
    if (!nrow(datos)) next
    nombre <- make.names(variable)
    unidades[nombre] <- attr(datos, "unidades")[["UDA"]]
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
  salida <- unique(data.frame(date = fechas,
                               valor = .filtrar_valores(valores_texto, NULL)))
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
    if (resolucion == "horaria") {
      salida <- .agregar_cuenca(datos, limites, cobertura, tz)
    } else {
      salida <- datos[order(datos$date), , drop = FALSE]
      names(salida)[2L] <- "UDA"
    }
    salida$UDA <- .filtrar_valores(salida$UDA, percentil)
    rownames(salida) <- NULL
    attr(salida, "unidades") <- c(UDA = unidad)
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
  horas <- floor(minutos$tiempo / 3600) * 3600
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
