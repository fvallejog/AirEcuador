# AirEcuador 0.3.0.9000

* All four extractors apply `percentil_limite = 0.999` by default, independently
  per station and variable, after physical checks. Cuenca's previous NULL
  default is changed to the maintainer's P99.9 policy. Only values strictly
  above the type-7 sample quantile of the requested output interval are
  replaced by NA; rows and timestamps are preserved.
* Unit-aware physical/measurement-domain checks are applied before percentile
  estimation. Cuenca original observations are checked before aggregation
  and hourly completeness counting. Per-series bounds, exclusion counts and
  percentile thresholds are exposed in the `control_calidad` attribute.
* `mapa_estaciones(ciudad = "Quito")` retains the existing default map and
  accepts `ciudad = "Cuenca"` for UDA and the three EMOV-CAIRE reference sites
  MUN, CCA and EIE. Providers and downloader status are identified. UDA's
  reference location is taken from IERSE page metadata. Historical EMOV
  coordinate sources and pending 2024--2025 location
  verification are identified in Cuenca popups. EMOV data downloads remain
  unimplemented; the Cuenca extraction functions still query UDA.
* The initial CRAN upload remains unconfirmed and is on hold at the
  maintainer's request while Cuenca coverage is reviewed. This development
  version has not been submitted to CRAN.

# AirEcuador 0.3.0

* Cuenca observations from the Universidad del Azuay station, published by
  IERSE, are supported through `extraer_Cuenca_variable()` and
  `extraer_Cuenca_estacion()`. EMOV network coverage is not implied.
* CO, NO2, O3, PM2.5 and SO2 are supported for Cuenca. The original units are
  retained. Minute concentrations can be returned unchanged or aggregated to
  hourly means with an explicit minimum valid-minute coverage requirement.
  Missing hours are represented by NA; no interpolation is performed.
* No upper percentile filter is applied to Cuenca by default. The Quito
  default of 0.999 is retained for compatibility and can now be disabled
  explicitly with `percentil_limite = NULL`.
* The non-CRAN dependency `lhmetools` is replaced by the CRAN package `archive`.
  No separate 7-Zip installation is required. Excel files are read with
  `readxl`; both .xls and .xlsx files are supported.
* Quito station headers are matched to their original column positions,
  including when blank columns are present. Accented station names are
  accepted. All hours of the final date are retained. Signed temperatures
  are preserved. Unexpected duplicate timestamps are reported.
* Quito spreadsheets with one station-header row or with an additional unit
  row are supported. Excel date cells, ISO date strings and numeric Excel
  serials are recognised. The first observation is retained when a unit row
  is absent; unreported units are represented by NA.
* The established Quito download user agent is retained for compatibility
  with the request confirmed to work from the maintainer's R session.
* Time-zone interpretation is exposed through `tz`. The Quito default of UTC
  is retained to reproduce legacy numeric timestamps. This is a wall-clock
  interpretation, not a conversion from Ecuadorian local time. Explicit
  `tz = "America/Guayaquil"` is recommended for local records.
* Network and remote-format failures are reported through messages and empty
  results. Temporary files are removed on success and failure.
* English reference documentation, a MIT license file, offline regression
  tests and checks for multiple R platforms are added. Mapping is retained
  through the optional dependency `leaflet`.
