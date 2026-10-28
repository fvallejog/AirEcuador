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
