# Cuenca scope for maintainer review

The requested EMOV-CAIRE coverage comprises the three sites below. IERSE
supplies the observations for the fourth Cuenca station, UDA.

| Code | Site | Documented reference latitude | Documented reference longitude |
| --- | --- | --- | --- |
| MUN | Centro Historico / Municipio | -2.897001 | -79.003000 |
| CCA | Sector Parque Industrial / Colegio Carlos Arizaga | -2.880532 | -78.980024 |
| EIE | Sector Control Sur / Escuela Ignacio Escandon | -2.916303 | -79.034047 |

Names and sites are presented separately by the official CAIRE portal:
<https://caire.emov.gob.ec/ica>.

On 2026-11-10 the maintainer identified the CAIRE sections for automatic
stations (<https://caire.emov.gob.ec/datosAutomaticas>) and meteorology
(<https://caire.emov.gob.ec/datosMete>). Both requests returned HTTP 503
before the page headers from this environment.

The maintainer's 2026-11-10 XLSX export contains a `Reporte` worksheet with
`Estaciones`, `Parametro`, `Fecha`, `Valores` and `Unidades`. Centro
Historico has 11 consecutive hourly records for each of `PM25` and `PM10`,
in `ug/m3`, with timestamps written as `YYYY/MM/DD HH:MM:SS` and no time-zone
field. The export confirms the workbook format for MUN. The provider export
is retained outside the repository.

The maintainer's browser capture records HTTP 200 for POST requests to
<https://caire.emov.gob.ec/datos/datos/consulta> and
<https://caire.emov.gob.ec/datos/datos/descargaDatos>. The package
reader uses `consulta` for the JSON observations. The observed hourly request
uses `tipoPromedio = 2`,
`parametros` objects with `id = 7, acronimo = PM25` and `id = 8, acronimo =
PM10`, and `fechaInicio`/`fechaFin` in Unix milliseconds. The station
objects in `estaciones` contain `id`, `direccion`, `altura`, `latitud`,
`longitud`, `nombre` and `nombreVisual`; the captured address, elevation
and display-name fields are null.

| Station | CAIRE request ID | Portal latitude | Portal longitude |
| --- | --- | --- | --- |
| MUN | 1 | -2.897 | -79.003 |
| CCA | 2 | -2.879789 | -78.980213 |
| EIE | 3 | -2.91641226 | -79.03393596 |

The capture shows PM25 selected across all three stations and CO (ID 1),
SO2 (ID 6) and PM25 selected for EIE with hourly averaging. A separate MUN
capture selects NO2 (ID 4) and SO2 with `tipoPromedio = 5`. Its aggregation
label has not been verified. The selections do not establish coverage for
all periods. The table documents portal coordinates on 2026-11-10.

The response contains `datos` records with `estacion`, `parametro`, `fecha`,
`valor`, `valorSinFactor` and `unidad`, alongside chart data and a size flag.
The 22 supplied records match the XLSX export. `ESTACION01` identifies MUN
and `UG_M3` matches `ug/m3`. Request start `1794279600000` is 2026-11-10
03:00 UTC, which corresponds to the first returned timestamp, 2026-11-09
22:00 in `America/Guayaquil`. Calendar-day requests should use Cuenca
midnight when generating the Unix millisecond bounds.

Both POST routes
returned HTTP 503 when replayed from this environment with the three-station
PM25 payload. Tests with simulated responses verified the request fields,
station mapping and separate reporting of empty results and HTTP errors.

The maintainer replayed the hourly PM25 query separately for each station
using `fechaInicio = 1794279600000` and `fechaFin = 1794365999999`.
The API returned 12 finite values for MUN (`ESTACION01`), 13 for CCA
(`ESTACION02`) and an empty result for EIE. Extending the same EIE request
to seven days (`fechaInicio = 1793761200000`) and selecting CO, SO2 and
PM25 returned 109 finite values each for CO and SO2 and zero PM25 rows.
Both series identified EIE as `ESTACION03`.

The meteorology capture selects MUN with hourly averaging and parameters
`RAD_GLOBAL` (ID 13), `PRECIPITACION` (ID 14), `VELOCIDAD_VIENTO` (ID 15)
and `DIRECCION_VIENTO` (ID 16). The candidate maps those acronyms to
`RS`, `LLU`, `VEL` and `DIR`. Replaying the payload against `consulta`
from this environment returned HTTP 503. Live meteorology records and
their units still require verification.

The historical reference coordinates in the first table are attributed to EMOV
(2013) in Table I of Abad and Mejia (2017), "Estimacion de la concentracion
de material particulado menor a 10 micras a traves de sensores remotos en el
area urbana de la ciudad de Cuenca":
<https://www.researchgate.net/publication/336209902_EJE_07-07_Estimacion_de_la_concentracion_de_material_particulado_menor_a_10_micras_a_traves_de_sensores_remotos_en_el_area_urbana_de_la_ciudad_de_Cuenca>.
Current locations for the requested 2024--2025 period have not been verified.
The map identifies the coordinate source and this limitation in its popups.

UDA is also mapped at latitude -2.9190715, longitude -79.0005447. These are
the `place:location:latitude` and `place:location:longitude` values retrieved
from <https://ierse.uazuay.edu.ec/aire/> on 2026-10-28. They identify a
reference location published by the portal; the precise position of the
instrument has not been verified. Its popup identifies IERSE/UDA and its
existing downloader separately from EMOV.

The provider's 2022 report, tables 2--4, distinguishes the automatic stations
MUN, CCA and EIE from the microsensor network. Their records must not be
combined without identifying the monitoring method:
<https://ierse.uazuay.edu.ec/proyectos/links_doc_contaminantes/Informes-Calidad-Aire/Informe_Calidad_Aire_Cuenca_2022.pdf>.

## Pending before a CRAN submission is reconsidered

- Confirm the three locations for 2024--2025 with the provider's metadata.
- Verify the installed candidate with live CAIRE queries from the maintainer's
  R session and check the requested historical coverage.
- Verify Quito and Cuenca maps visually, including their source popups.
- Resolve the previous unconfirmed CRAN upload before any replacement upload.
- Obtain the maintainer's explicit approval of the reviewed candidate.

On 2026-10-28 direct access to the CAIRE website returned HTTP 503 from this
environment, and the web retrieval service timed out. This is not evidence
that the service is unavailable from the maintainer's connection.
