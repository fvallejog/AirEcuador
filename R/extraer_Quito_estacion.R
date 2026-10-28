#' Import Quito observations for one station
#'
#' Available air quality and meteorological variables are combined by time.
#' Individual unavailable variables are reported and omitted. No files are
#' retained outside the R session's temporary directory.
#'
#' @param estacion A Quito station name, for example `"Los Chillos"`,
#'   `"Guamani"` or `"Belisario"`. Spaces, accents and underscores are accepted.
#' @inheritParams extraer_Quito_variable
#' @return A data frame with `date`, `estacion` and available variable columns.
#'   Original units are retained in the `unidades` attribute. An empty data
#'   frame is returned with a message if no observations are available.
#'   `attr(x, "control_calidad")` contains the filtering summary for the
#'   requested station and each returned variable.
#' @examples
#' if (interactive()) {
#'   datos <- extraer_Quito_estacion("Los Chillos", "2024-01-01", "2024-01-01",
#'                                 percentil_limite = 0.999,
#'                                 tz = "America/Guayaquil")
#'   head(datos)
#' }
#' @export
extraer_Quito_estacion <- function(
    estacion = "Los Chillos", fecha_inicio = "2004-01-01",
    fecha_fin = .ultimo_mes(), year = NULL, percentil_limite = 0.999,
    tz = "UTC") {
  estacion <- .estacion_quito(estacion, validar = TRUE)
  periodo <- .periodo(fecha_inicio, fecha_fin, year, 2004L)
  .validar_percentil(percentil_limite)
  .validar_tz(tz)
  partes <- controles <- list()
  unidades <- character()
  for (variable in .variables_quito) {
    datos <- .leer_quito(variable, periodo, percentil_limite, tz)
    if (!nrow(datos) || !estacion %in% names(datos)) next
    nombre <- make.names(variable)
    parte <- datos[c("date", estacion)]
    names(parte)[2L] <- nombre
    partes[[variable]] <- parte
    unidad <- attr(datos, "unidades")[[estacion]]
    unidades[nombre] <- if (is.null(unidad)) NA_character_ else unidad
    control <- attr(datos, "control_calidad")
    if (!is.null(control)) {
      controles[[variable]] <- control[control$estacion == estacion, , drop = FALSE]
    }
  }
  if (!length(partes)) {
    message("No observations were available for ", estacion, ".")
    return(.vacio_estacion(tz))
  }
  salida <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), partes)
  salida$estacion <- rep(estacion, nrow(salida))
  salida <- salida[c("date", "estacion", setdiff(names(salida),
                                                c("date", "estacion")))]
  attr(salida, "unidades") <- unidades
  attr(salida, "control_calidad") <- do.call(rbind, controles)
  attr(salida, "fuente") <- "Secretaria de Ambiente de Quito"
  salida
}
