# AirEcuador

AirEcuador imports air quality and meteorological observations from Quito
and Cuenca, Ecuador, into R.

This is development version 0.3.0.9001. The initial 0.3.0 CRAN upload is
awaiting maintainer confirmation while the updated Cuenca reader is tested.

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
| Cuenca | [EMOV CAIRE](https://caire.emov.gob.ec/datosAutomaticas), MUN, CCA and EIE stations | CO, NO2, PM10, PM2.5, SO2 | Provider-supplied hourly observations |
| Cuenca | [EMOV CAIRE meteorology](https://caire.emov.gob.ec/datosMete) | RS, LLU, VEL, DIR | Provider-supplied hourly observations |

The municipal air-quality catalogue links to the IERSE service for
Universidad del Azuay. [Municipal catalogue](https://cuencaendatos.cuenca.gob.ec/dataset/calidad-del-aire).

Observations are retrieved on request. No historical monitoring database is
redistributed with the package. Data rights, quality control and revisions
are governed by the original providers; the MIT license applies to the package
code. The full requested historical range is not guaranteed to be available.

## Quito

```r
co <- extraer_Quito_variable(
  "CO", fecha_inicio = "2024-01-01", fecha_fin = "2024-01-31",
  percentil_limite = 0.999, tz = "America/Guayaquil"
)

chillos <- extraer_Quito_estacion(
  "Los Chillos", year = 2024,
  percentil_limite = 0.999, tz = "America/Guayaquil"
)

attr(co, "unidades")
```

Original argument names and output column names are retained. All hours of the
last requested date are included. Files are downloaded and extracted only to
the session's temporary directory and are removed after processing.

## Cuenca IERSE station

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

## Cuenca EMOV stations

```r
pm25_caire <- extraer_Cuenca_variable(
  "PM2.5", "2026-11-09", "2026-11-10", fuente = "CAIRE"
)
head(pm25_caire)  # date, MUN, CCA, EIE

control_sur <- extraer_Cuenca_estacion("EIE", "2026-11-04", "2026-11-09")
attr(control_sur, "unidades")
attr(control_sur, "control_calidad")

met_centro <- extraer_Cuenca_estacion("MUN", "2026-11-09", "2026-11-10",
                                     tipo = "meteo")
attr(met_centro, "unidades")
```

CAIRE supplies hourly observations. The reader aligns stations at the
returned timestamps, fills gaps with `NA`, and reports source units.
The station reader includes the variables returned for that station and period.
During a November 2026 validation, the portal returned PM2.5 for MUN and
CCA and returned CO and SO2 for EIE.

The meteorology reader uses `RS` for global radiation, `LLU` for precipitation,
`VEL` for wind speed and `DIR` for wind direction. The CAIRE portal capture
identified these parameters for MUN. The provider determines the coverage
for each station and date.

## Station maps

```r
mapa_estaciones()                  # Quito, retained default
mapa_estaciones(ciudad = "Quito")
mapa_estaciones(ciudad = "Cuenca")
```

The Cuenca map retains UDA alongside the three EMOV-CAIRE sites:

| Code | Displayed site | Data download through AirEcuador |
| --- | --- | --- |
| MUN | Centro Historico / Municipio | CAIRE |
| CCA | Sector Parque Industrial / Colegio Carlos Arizaga | CAIRE |
| EIE | Sector Control Sur / Escuela Ignacio Escandon | CAIRE |
| UDA | Universidad del Azuay / IERSE | IERSE |

The three EMOV sites use reference coordinates attributed to EMOV (2013)
in Table I of [Abad and Mejia (2017)](https://www.researchgate.net/publication/336209902).
Verification of the locations for 2024--2025 is pending; the coordinate
source and date appear in the popups. UDA's reference location comes from the
geographic metadata of its portal. The map popups identify each provider.
[IERSE air-quality portal](https://ierse.uazuay.edu.ec/aire/).

## Time and units

The UDA reader interprets the displayed times as Ecuadorian local time
(`America/Guayaquil`) by default. The CAIRE reader uses Cuenca local time
for request dates and source timestamps. Its `tz` argument controls how the
returned timestamps are displayed.

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

The maintainer's filtering policy is applied to Quito and Cuenca: physical
and measurement-domain checks first, then P99.9 separately for each station
and variable. `percentil_limite = 0.999` is the default in all four extractors.
The type-7 sample quantile is estimated over the requested interval at the
returned resolution, after physical exclusions. Values strictly above the
threshold become `NA`; values equal to it are retained. Dates and rows are
preserved. The percentile is a chosen statistical screen, not a regulatory
limit or proof that a high concentration is erroneous. In short intervals,
the fraction removed can exceed 0.1% because sample quantiles are interpolated.

Nonfinite and negative pollutant values are excluded. Nonnegative
meteorological variables reject negatives, and pressure rejects zero.
Additional checks use the reported units:

| Variable | Recognized unit | Accepted measurement domain |
| --- | --- | --- |
| TMP | Celsius / kelvin / Fahrenheit | At or above absolute zero; valid negative Celsius/Fahrenheit values are retained |
| HUM | Percent / fraction | 0--100 / 0--1 |
| DIR | Degrees / radians | 0--360 / 0--2*pi |
| Pollutant concentration | ppm / ppb | 0--1e6 / 0--1e9 |

Unit-dependent bounds are not guessed when units are missing or unrecognized.
No arbitrary upper limit is assigned to mass concentrations, rainfall, wind
speed or radiation. These checks do not replace the provider's quality flags
or instrument-specific validation. Celsius and kelvin interpretation follows
the [SI temperature scale](https://www.nist.gov/pml/owm/si-units-temperature).

For UDA, the reader checks original observations before calculating minute
and hourly averages and counting valid minutes. For CAIRE, the reader
checks the provider's hourly observations. Both readers calculate one
percentile threshold for each series after combining all chunks.

The applied bounds, counts and threshold are available for review:

```r
attr(co, "control_calidad")
attr(pm25, "control_calidad")
```

Each record identifies its station, variable, unit, stage and resolution,
with input counts, missing/non-numeric/nonfinite values, physical exclusions,
the percentile threshold, percentile exclusions and remaining valid values.
UDA reports original and output stages separately. CAIRE reports the hourly
output stage. `percentil_limite = NULL` disables the percentile screen while
retaining physical checks.

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
R CMD check --as-cran AirEcuador_0.3.0.9001.tar.gz
```

Checks on Linux, Windows, macOS and R-devel are configured in GitHub Actions.
The current results should be reviewed before submission. CRAN acceptance is
determined by its maintainers.

Package maintenance: Fidel Vallejo, <fidel.vallejo@unach.edu.ec>.
