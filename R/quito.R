.leer_quito <- function(variable, periodo, percentil, tz) {
  carpeta <- tempfile("AirEcuador-")
  dir.create(carpeta)
  on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
  comprimido <- file.path(carpeta, "observaciones.rar")
  url <- paste0("https://datosambiente.quito.gob.ec/datos/", variable, ".rar")
  if (!.descargar(url, comprimido)) return(.vacio_variable(tz))
  tryCatch({
    indice <- archive::archive(comprimido)
    rutas <- gsub("\\\\", "/", indice$path)
    if (any(grepl("^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)", rutas))) {
      stop("Unsafe archive paths were detected.")
    }
    candidatos <- indice$path[grepl("\\.xlsx?$", rutas, ignore.case = TRUE)]
    preferidos <- candidatos[toupper(basename(candidatos)) %in%
                              toupper(paste0(variable, c(".xlsx", ".xls")))]
    if (length(preferidos) == 1L) candidatos <- preferidos
    if (length(candidatos) != 1L) {
      stop("A unique spreadsheet could not be identified in the archive.")
    }
    archive::archive_extract(comprimido, dir = carpeta, files = candidatos)
    archivo <- file.path(carpeta, candidatos)
    crudos <- suppressMessages(readxl::read_excel(
      archivo, sheet = 1, col_names = FALSE, col_types = "text",
      .name_repair = "minimal"))
    .procesar_quito(as.data.frame(crudos), periodo, percentil, variable, tz)
  }, error = function(e) {
    message("Quito data for ", variable, " could not be processed: ",
            conditionMessage(e))
    .vacio_variable(tz)
  })
}

.procesar_quito <- function(crudos, periodo, percentil, variable, tz) {
  if (nrow(crudos) < 2L || ncol(crudos) < 2L) {
    stop("The expected station header and observations were not found.")
  }
  encabezados <- as.character(unlist(crudos[1L, ], use.names = FALSE))
  columnas <- which(!is.na(encabezados) & nzchar(trimws(encabezados)) &
                     seq_along(encabezados) > 1L)
  if (!length(columnas)) stop("No station headers were found.")
  estaciones <- .estacion_quito(encabezados[columnas])
  if (anyDuplicated(estaciones)) stop("Duplicate station headers were found.")
  fechas_candidatas <- .fechas_quito(crudos[[1L]][-1L], tz)
  primera <- which(!is.na(fechas_candidatas))[1L]
  if (is.na(primera) || primera > 2L) {
    stop("The expected dates were not found after the station header.")
  }
  # An optional unit row is distinguished from the first observation by date.
  unidades <- if (primera == 2L) {
    as.character(unlist(crudos[2L, columnas, drop = FALSE], use.names = FALSE))
  } else {
    rep(NA_character_, length(columnas))
  }
  datos <- crudos[-seq_len(primera), , drop = FALSE]
  fechas <- fechas_candidatas[seq.int(primera, length(fechas_candidatas))]
  limites <- .limites(periodo, tz)
  conservar <- !is.na(fechas) & fechas >= limites[1L] & fechas < limites[2L]
  salida <- data.frame(date = fechas[conservar])
  if (!nrow(salida)) return(.vacio_variable(tz))
  if (anyDuplicated(salida$date)) stop("Duplicate observation times were found.")
  controles <- vector("list", length(columnas))
  for (i in seq_along(columnas)) {
    depurado <- .depurar_valores(datos[[columnas[i]]][conservar], percentil,
                                 variable, unidades[i])
    salida[[estaciones[i]]] <- depurado$valores
    controles[[i]] <- cbind(
      estacion = estaciones[i], variable = variable, etapa = "salida",
      resolucion = "horaria", unidad = unidades[i], depurado$control)
  }
  salida <- salida[order(salida$date), , drop = FALSE]
  rownames(salida) <- NULL
  attr(salida, "unidades") <- stats::setNames(unidades, estaciones)
  attr(salida, "control_calidad") <- do.call(rbind, controles)
  attr(salida, "fuente") <- "Secretaria de Ambiente de Quito"
  salida
}

.fechas_quito <- function(x, tz) {
  x <- trimws(as.character(x))
  salida <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = tz)
  numero <- "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
  serial <- !is.na(x) & grepl(numero, x)
  if (any(serial)) {
    reloj <- format(as.POSIXct(round((as.numeric(x[serial]) - 25569) * 86400),
                              origin = "1970-01-01", tz = "UTC"),
                    "%Y-%m-%d %H:%M:%S", tz = "UTC")
    salida[serial] <- as.POSIXct(reloj, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  }
  # Excel date cells are converted to ISO text by readxl's text-column reader.
  texto <- !is.na(x) & grepl(paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}",
    "([ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?$"), x)
  if (any(texto)) {
    reloj <- sub("T", " ", x[texto], fixed = TRUE)
    solo_fecha <- nchar(reloj) == 10L
    reloj[solo_fecha] <- paste(reloj[solo_fecha], "00:00:00")
    sin_segundos <- nchar(reloj) == 16L
    reloj[sin_segundos] <- paste0(reloj[sin_segundos], ":00")
    fechas <- as.POSIXct(reloj, format = "%Y-%m-%d %H:%M:%S", tz = tz)
    validas <- !is.na(fechas) &
      format(fechas, "%Y-%m-%d %H:%M:%S", tz = tz) == reloj
    fechas[!validas] <- as.POSIXct(NA, tz = tz)
    salida[texto] <- fechas
  }
  salida
}
