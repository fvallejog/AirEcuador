# AirEcuador

Air quality observations from Quito and Cuenca, Ecuador, are imported into R.
Meteorological observations are also supported for Quito.

This is development version 0.3.0.9000. The initial 0.3.0 CRAN upload remains
unconfirmed and is on hold while Cuenca coverage is reviewed with the maintainer.
This development version has not been submitted to CRAN.

## Installation

```r
install.packages("remotes")
remotes::install_github("fvallejog/AirEcuador@feat/mapa-ciudades")
library(AirEcuador)
```

The previous dependency on `lhmetools` is no longer required. RAR archives are
read through `archive`. On Linux, `libarchive-dev` (Debian/Ubuntu) or
`libarchive-devel` (Fedora) may be required when that dependency is compiled
from source. Mapping is available when the optional package `leaflet` has
been installed.

## Sources and coverage

| City | Provider and station coverage | Variables | Returned resolution |
| --- | --- | --- | --- |
| Quito | [Secretaria de Ambiente](https://datosambiente.quito.gob.ec/); stations reported in each archive | CO, NO2, O3, PM10, PM2.5, SO2, LLU, DIR, HUM, IUV, PRE, RS, TMP, VEL | Original hourly observations |
| Cuenca | [IERSE, Universidad del Azuay](https://ierse.uazuay.edu.ec/aire/); UDA station | CO, NO2, O3, PM2.5, SO2 | Original timestamps or hourly means |

The Cuenca IERSE service is linked from the
[municipal air-quality catalogue](https://cuencaendatos.cuenca.gob.ec/dataset/calidad-del-aire).
Its displayed station is Universidad del Azuay. Coverage of the EMOV network
is not claimed by these functions; the CAIRE-EMOV portal is a separate service.
PM10 and meteorological records are not exposed by the UDA service.
Ambato is not included.

Observations are retrieved on request. No historical monitoring database is
redistributed with the package. Data rights, quality control and revisions
are governed by the original providers; the MIT license applies to the package
code. The full requested historical range is not guaranteed to be available.

## Quito

```r
co <- extraer_Quito_variable(
  "CO", fecha_inicio = "2024-01-01", fecha_fin = "2024-01-31",
  percentil_limite = NULL, tz = "America/Guayaquil"
)

chillos <- extraer_Quito_estacion(
  "Los Chillos", year = 2024,
  percentil_limite = NULL, tz = "America/Guayaquil"
)

attr(co, "unidades")
```

Original argument names and output column names are retained. All hours of the
last requested date are included. Files are downloaded and extracted only to
the session's temporary directory and are removed after processing.

## Cuenca: UDA station

```r
pm25 <- extraer_Cuenca_variable(
  "PM2.5", fecha_inicio = "2025-01-01", fecha_fin = "2025-01-07",
  cobertura_minima = 0.75
)
head(pm25)                    # date, UDA
attr(pm25, "unidades")        # Original provider unit
head(attr(pm25, "cobertura")) # Distinct valid minutes in each hour

uda <- extraer_Cuenca_estacion(
  fecha_inicio = "2025-01-01", fecha_fin = "2025-01-07"
)
head(uda)                    # date, estacion, CO, NO2, O3, PM2.5, SO2

original <- extraer_Cuenca_variable(
  "PM2.5", "2025-01-01", "2025-01-01", resolucion = "original"
)
```

Concentrations are parsed from the service used by the public IERSE chart;
remotely supplied JavaScript is never evaluated. Requests are divided into
intervals of at most seven days and are spaced to limit server load. A failed
chunk causes that variable's whole request to be discarded with a message.

For hourly output, subminute observations are first averaged by minute, so
repeated samples cannot inflate completeness. At least 45 distinct valid
minutes are required by the default threshold. Insufficiently covered and
missing hours are returned as `NA`. No values are interpolated. Coverage is
calculated before any optional percentile filter of the hourly means.

## Station maps

```r
mapa_estaciones()                  # Quito, retained default
mapa_estaciones(ciudad = "Quito")
mapa_estaciones(ciudad = "Cuenca")
```

The Cuenca map contains the three EMOV-CAIRE sites requested for review:

| Code | Displayed site | Data download through AirEcuador |
| --- | --- | --- |
| MUN | Centro Historico / Municipio | Pending |
| CCA | Sector Parque Industrial / Colegio Carlos Arizaga | Pending |
| EIE | Sector Control Sur / Escuela Ignacio Escandon | Pending |

These are reference locations, with coordinates attributed to EMOV (2013)
in Table I of [Abad and Mejia (2017)](https://www.researchgate.net/publication/336209902).
Verification of the locations for 2024--2025 is pending; the coordinate
source and date are shown in the popups. Current station operation is not
checked. Mapping a site does not implement its data download. The existing
`extraer_Cuenca_*()` functions query the separate UDA station, which is not
one of the three EMOV sites above.

## Time and units

Cuenca times are interpreted as Ecuadorian local time (`America/Guayaquil`)
by default. This interpretation of the displayed times is an explicit
assumption and should be confirmed with the provider for critical uses.

For compatibility, the old Quito default `tz = "UTC"` is retained. In version
0.2.0, Excel wall-clock values were labelled as UTC without conversion from
Ecuadorian local time. The same numeric timestamps are reproduced by this
default. For records interpreted as local time, `tz = "America/Guayaquil"`
should be supplied explicitly; UTC instants can then be displayed as follows:

```r
format(co$date, tz = "UTC", usetz = TRUE)
```

Units, when reported in a separate header row, are retained in
`attr(x, "unidades")` for Quito. When no unit row is supplied, `NA` is recorded;
units are not inferred from concentrations. Cuenca units are retrieved from
the provider's measurement metadata. Units are not harmonized between
cities. In particular, Cuenca gas measurements must not be treated as mass
concentrations when ppm or ppb have been reported. No mass-to-volume or
temperature/pressure conversion is performed by the package.

## Quality filters and unavailable services

Negative pollutant concentrations and nonfinite values are replaced by `NA`.
Signed Quito temperature observations are retained. An upper percentile filter
is **not** applied to Cuenca unless one is requested. The old Quito default
`percentil_limite = 0.999` is retained for compatibility. For analysis of high
pollution episodes, `percentil_limite = NULL` is recommended unless a filter
has been independently justified. These transformations are not a substitute
for the provider's quality assurance.

Network failures and changed source formats are reported through messages and
empty results. Empty results must be checked before downstream analysis:

```r
if (nrow(pm25) == 0L) {
  message("No observations were returned; the source and dates should be checked.")
}
```

## Development checks

Offline regression tests are included for dates, station alignment, time-zone
interpretation, units, hourly completeness and remote-service failures.
Separate live checks can be run explicitly by the maintainer:

```sh
Rscript dev/validate-live.R
R CMD build .
R CMD check --as-cran AirEcuador_0.3.0.9000.tar.gz
```

Checks on Linux, Windows, macOS and R-devel are configured in GitHub Actions.
The current results should be reviewed before submission. CRAN acceptance is
determined by its maintainers.

Package maintenance: Fidel Vallejo, <fidel.vallejo@unach.edu.ec>.
