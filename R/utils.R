.variables_quito <- c("CO", "NO2", "O3", "PM10", "PM2.5", "SO2", "LLU", "DIR",
                      "HUM", "IUV", "PRE", "RS", "TMP", "VEL")
.variables_cuenca <- c("CO", "NO2", "O3", "PM2.5", "SO2")
.estaciones_quito <- c("Los_Chillos", "Guamani", "Tumbaco", "Belisario",
                       "Carapungo", "Centro", "Cotocollao", "El_Camal",
                       "San_Antonio", "Condado", "Turubamba", "Chillogallo",
                       "Jipijapa")

.ultimo_mes <- function() {
  as.Date(format(Sys.Date(), "%Y-%m-01")) - 1L
}

.validar_variable <- function(x, opciones) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% opciones) {
    stop("A single variable must be supplied: ", paste(opciones, collapse = ", "),
         ".", call. = FALSE)
  }
}

.validar_tz <- function(tz) {
  if (!is.character(tz) || length(tz) != 1L || is.na(tz) ||
      !tz %in% c("UTC", "GMT", OlsonNames())) {
    stop("A valid time-zone name must be supplied.", call. = FALSE)
  }
}

.validar_percentil <- function(x) {
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
                      x <= 0 || x > 1)) {
    stop("percentil_limite must be NULL or one number in (0, 1].", call. = FALSE)
  }
}

.periodo <- function(inicio, fin, year, minimo) {
  actual <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1L || !is.finite(year) ||
        year != floor(year) || year < minimo || year > actual) {
      stop("year must be one integer between ", minimo, " and ", actual, ".",
           call. = FALSE)
    }
    inicio <- sprintf("%04d-01-01", year)
    fin <- sprintf("%04d-12-31", year)
  }
  convertir <- function(x) {
    if (length(x) != 1L || is.na(x) ||
        (!inherits(x, "Date") &&
         (!is.character(x) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)))) {
      stop("Dates must be single Date objects or YYYY-MM-DD strings.",
           call. = FALSE)
    }
    fecha <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
    if (is.na(fecha) || (is.character(x) && format(fecha, "%Y-%m-%d") != x)) {
      stop("An invalid calendar date was supplied.", call. = FALSE)
    }
    fecha
  }
  inicio <- convertir(inicio)
  fin <- convertir(fin)
  if (inicio > fin) stop("fecha_inicio must not exceed fecha_fin.", call. = FALSE)
  if (fin > Sys.Date()) {
    fin <- Sys.Date()
    message("The requested end date was limited to the current date.")
  }
  if (inicio > fin) stop("The requested interval lies in the future.", call. = FALSE)
  list(inicio = inicio, fin = fin)
}

.clave <- function(x) {
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1", "aeiouunAEIOUUN", trimws(x))
  gsub("[^A-Z0-9]", "", toupper(x))
}

.estacion_quito <- function(x, validar = FALSE) {
  if (!is.character(x) || (validar && (length(x) != 1L || is.na(x)))) {
    stop("A single Quito station name must be supplied.", call. = FALSE)
  }
  indice <- match(.clave(x), .clave(.estaciones_quito))
  resultado <- .estaciones_quito[indice]
  faltan <- is.na(indice)
  resultado[faltan] <- make.names(x[faltan])
  if (validar && any(faltan)) {
    stop("Unknown Quito station. Supported names: ",
         paste(.estaciones_quito, collapse = ", "), ".", call. = FALSE)
  }
  resultado
}

.vacio_variable <- function(tz) {
  data.frame(date = as.POSIXct(character(), tz = tz))
}

.vacio_estacion <- function(tz) {
  data.frame(date = as.POSIXct(character(), tz = tz), estacion = character())
}

.limites <- function(periodo, tz) {
  c(as.POSIXct(paste(periodo$inicio, "00:00:00"), tz = tz),
    as.POSIXct(paste(periodo$fin + 1L, "00:00:00"), tz = tz))
}

.filtrar_valores <- function(x, percentil, permitir_negativos = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  if (!permitir_negativos) x[!is.na(x) & x < 0] <- NA_real_
  if (!is.null(percentil) && any(!is.na(x))) {
    limite <- stats::quantile(x, percentil, na.rm = TRUE, names = FALSE)
    x[!is.na(x) & x > limite] <- NA_real_
  }
  x
}

.descargar <- function(url, destino, query = NULL) {
  # Quito's established request header is retained: that request was accepted
  # from the maintainer's R session, while the workbook reader needed repair.
  agente <- if (startsWith(url, "https://datosambiente.quito.gob.ec/")) {
    paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
           "AppleWebKit/537.36 (KHTML, like Gecko) ",
           "Chrome/91.0.4472.124 Safari/537.36")
  } else {
    "AirEcuador (https://github.com/fvallejog/AirEcuador)"
  }
  respuesta <- tryCatch(httr::GET(
    url, query = query, httr::write_disk(destino, overwrite = TRUE),
    httr::timeout(120), httr::user_agent(agente)),
    error = function(e) {
      message("The remote resource could not be downloaded: ",
              conditionMessage(e))
      NULL
    })
  if (is.null(respuesta)) return(FALSE)
  if (httr::status_code(respuesta) != 200L) {
    message("The remote resource was unavailable (HTTP ",
            httr::status_code(respuesta), "): ", url)
    return(FALSE)
  }
  if (!file.exists(destino) || file.info(destino)$size == 0) {
    message("An empty response was received from the remote resource.")
    return(FALSE)
  }
  TRUE
}
