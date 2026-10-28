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

.limites_fisicos <- function(variable = NULL, unidad = NA_character_,
                            permitir_negativos = FALSE) {
  inferior <- if (permitir_negativos || identical(variable, "TMP")) -Inf else 0
  superior <- Inf
  abierto <- identical(variable, "PRE")
  unidad_texto <- if (length(unidad) == 1L && !is.na(unidad)) trimws(unidad) else ""
  clave <- .clave(unidad_texto)
  if (identical(variable, "TMP")) {
    if (clave %in% c("C", "DEGC", "CELSIUS", "GRADOSC", "GRADOSCELSIUS")) {
      inferior <- -273.15
    } else if (clave %in% c("K", "KELVIN")) {
      inferior <- 0
    } else if (clave %in% c("F", "DEGF", "FAHRENHEIT", "GRADOSF")) {
      inferior <- -459.67
    }
  } else if (identical(variable, "HUM")) {
    if (unidad_texto == "%" || clave %in% c("PCT", "PERCENT", "PORCENTAJE")) {
      superior <- 100
    } else if (clave %in% c("1", "FRACTION", "FRACCION")) {
      superior <- 1
    }
  } else if (identical(variable, "DIR")) {
    if (unidad_texto %in% c("\u00b0", "\u00ba") ||
        clave %in% c("DEG", "DEGREES", "DEGREE", "GRADOS")) {
      superior <- 360
    } else if (clave %in% c("RAD", "RADIAN", "RADIANS", "RADIANES")) {
      superior <- 2 * pi
    }
  } else if (!is.null(variable) && variable %in% .variables_cuenca) {
    if (clave %in% c("PPM", "PPMV")) superior <- 1e6
    if (clave %in% c("PPB", "PPBV")) superior <- 1e9
  }
  list(inferior = inferior, superior = superior, inferior_abierto = abierto)
}

.depurar_valores <- function(x, percentil, variable = NULL,
                            unidad = NA_character_, permitir_negativos = FALSE) {
  original <- x
  x <- suppressWarnings(as.numeric(x))
  faltantes <- sum(is.na(original) & !is.nan(original))
  no_numericos <- sum(!is.na(original) & is.na(x) & !is.nan(x))
  no_finitos <- sum(is.infinite(x) | is.nan(x))
  x[!is.finite(x)] <- NA_real_
  limites <- .limites_fisicos(variable, unidad, permitir_negativos)
  imposibles <- !is.na(x) & (x < limites$inferior | x > limites$superior |
                              (limites$inferior_abierto & x == limites$inferior))
  x[imposibles] <- NA_real_
  umbral <- NA_real_
  extremos <- rep(FALSE, length(x))
  if (!is.null(percentil) && any(!is.na(x))) {
    umbral <- stats::quantile(x, percentil, na.rm = TRUE, names = FALSE, type = 7)
    extremos <- !is.na(x) & x > umbral
    x[extremos] <- NA_real_
  }
  control <- data.frame(
    n_entrada = length(x), n_faltantes_entrada = faltantes,
    n_no_numericos = no_numericos, n_no_finitos = no_finitos,
    limite_fisico_inferior = limites$inferior,
    limite_fisico_superior = limites$superior,
    inferior_abierto = limites$inferior_abierto,
    n_imposibles = sum(imposibles),
    percentil = if (is.null(percentil)) NA_real_ else percentil,
    umbral_percentil = umbral, n_percentil = sum(extremos),
    n_validos = sum(!is.na(x)))
  list(valores = x, control = control)
}

.filtrar_valores <- function(x, percentil, permitir_negativos = FALSE,
                            variable = NULL, unidad = NA_character_) {
  .depurar_valores(x, percentil, variable, unidad, permitir_negativos)$valores
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
