#' Import Quito observations for one variable
#'
#' An official Quito archive is downloaded and observations from all reported
#' stations are returned. Existing argument names and station columns are
#' retained for compatibility with AirEcuador 0.2.0.
#'
#' @param contaminante A variable code: `CO`, `NO2`, `O3`, `PM10`, `PM2.5`,
#'   `SO2`, `LLU`, `DIR`, `HUM`, `IUV`, `PRE`, `RS`, `TMP` or `VEL`.
#' @param fecha_inicio,fecha_fin Inclusive calendar dates, supplied as `Date`
#'   objects or `YYYY-MM-DD` strings. The last day of the preceding month is
#'   used when `fecha_fin` is omitted.
#' @param year An optional integer year. Both date arguments are overridden.
#' @param percentil_limite An optional percentile in `(0, 1]`. Values above the
#'   station- and variable-specific sample quantile are replaced by `NA`.
#'   The default is `0.999` (P99.9), calculated after physical checks over the
#'   requested interval, using R's type-7 quantile. Values equal to the
#'   threshold are retained. `NULL` disables only the percentile filter.
#' @param tz A time zone used to interpret the source's Excel wall-clock times.
#'   The legacy default `"UTC"` retains the numeric timestamps produced by
#'   version 0.2.0; no conversion from Ecuadorian time was performed in that
#'   version. Use `"America/Guayaquil"` when Ecuadorian local times are intended,
#'   and convert the resulting instants to UTC for comparison with WRF.
#' @return A data frame with a `POSIXct` `date` column and one numeric column per
#'   station. An empty data frame is returned with an informative message if
#'   the remote resource is unavailable or its format cannot be processed.
#'   Original units, when supplied in a separate header row, are retained in
#'   the `unidades` attribute; otherwise `NA` is recorded. No concentration
#'   unit conversion is performed. Per-station counts, physical bounds and
#'   percentile thresholds are retained in `attr(x, "control_calidad")`.
#' @details All hours of `fecha_fin` are included. Negative pollutant values
#'   and negative values of nonnegative meteorological variables are replaced
#'   by `NA`; atmospheric pressure must be positive. Additional measurement
#'   domain checks depend on reported units: temperature must not be below
#'   absolute zero; humidity in percent must be in `[0, 100]`; wind direction
#'   must be in `[0, 360]` degrees or `[0, 2*pi]` radians; concentrations in ppm
#'   and ppb must not exceed `1e6` and `1e9`, respectively. Unknown units are
#'   not guessed and no arbitrary upper mass-concentration limit is imposed.
#'   Valid negative Celsius temperatures are retained. Percentile thresholds
#'   are never pooled across stations or variables. Exclusions become `NA`;
#'   timestamps and rows are retained. A sample percentile is a statistical
#'   screen, not a regulatory limit or proof of measurement error.
#' @examples
#' if (interactive()) {
#'   datos <- extraer_Quito_variable("CO", "2024-01-01", "2024-01-01",
#'                                  percentil_limite = 0.999,
#'                                  tz = "America/Guayaquil")
#'   head(datos)
#'   attr(datos, "control_calidad")
#' }
#' @export
extraer_Quito_variable <- function(
    contaminante = "CO", fecha_inicio = "2004-01-01",
    fecha_fin = .ultimo_mes(), year = NULL, percentil_limite = 0.999,
    tz = "UTC") {
  .validar_variable(contaminante, .variables_quito)
  periodo <- .periodo(fecha_inicio, fecha_fin, year, 2004L)
  .validar_percentil(percentil_limite)
  .validar_tz(tz)
  .leer_quito(contaminante, periodo, percentil_limite, tz)
}
