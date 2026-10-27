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
  if (nrow(crudos) < 3L || ncol(crudos) < 2L) {
    stop("The expected station and unit headers were not found.")
  }
  encabezados <- as.character(unlist(crudos[1L, ], use.names = FALSE))
  columnas <- which(!is.na(encabezados) & nzchar(trimws(encabezados)) &
                     seq_along(encabezados) > 1L)
  if (!length(columnas)) stop("No station headers were found.")
  estaciones <- .estacion_quito(encabezados[columnas])
  if (anyDuplicated(estaciones)) stop("Duplicate station headers were found.")
  unidades <- as.character(unlist(crudos[2L, columnas, drop = FALSE],
                                  use.names = FALSE))
  datos <- crudos[-c(1L, 2L), , drop = FALSE]
  serial <- suppressWarnings(as.numeric(datos[[1L]]))
  reloj <- format(as.POSIXct(round((serial - 25569) * 86400),
                            origin = "1970-01-01", tz = "UTC"),
                  "%Y-%m-%d %H:%M:%S", tz = "UTC")
  fechas <- as.POSIXct(reloj, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  limites <- .limites(periodo, tz)
  conservar <- !is.na(fechas) & fechas >= limites[1L] & fechas < limites[2L]
  salida <- data.frame(date = fechas[conservar])
  if (!nrow(salida)) return(.vacio_variable(tz))
  if (anyDuplicated(salida$date)) stop("Duplicate observation times were found.")
  for (i in seq_along(columnas)) {
    salida[[estaciones[i]]] <- .filtrar_valores(
      datos[[columnas[i]]][conservar], percentil, variable == "TMP")
  }
  salida <- salida[order(salida$date), , drop = FALSE]
  rownames(salida) <- NULL
  attr(salida, "unidades") <- stats::setNames(unidades, estaciones)
  attr(salida, "fuente") <- "Secretaria de Ambiente de Quito"
  salida
}
