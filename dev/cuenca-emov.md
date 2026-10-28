# Cuenca scope for maintainer review

The requested EMOV-CAIRE coverage comprises the three sites below. At the
maintainer's request, UDA is retained as a fourth, separately identified
IERSE station. The UDA downloader does not implement the EMOV stations.

| Code | Site | Documented reference latitude | Documented reference longitude |
| --- | --- | --- | --- |
| MUN | Centro Historico / Municipio | -2.897001 | -79.003000 |
| CCA | Sector Parque Industrial / Colegio Carlos Arizaga | -2.880532 | -78.980024 |
| EIE | Sector Control Sur / Escuela Ignacio Escandon | -2.916303 | -79.034047 |

Names and sites are presented separately by the official CAIRE portal:
<https://caire.emov.gob.ec/ica>.

The coordinates above are historical reference sites, attributed to EMOV
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
- Inspect the official concentration download, station identifiers, units,
  time interpretation, available variables and historical coverage.
- Verify actual downloads from each requested station, with the maintainer.
- Verify Quito and Cuenca maps visually, including their source popups.
- Resolve the previous unconfirmed CRAN upload before any replacement upload.
- Obtain the maintainer's explicit approval of the reviewed candidate.

On 2026-10-28 direct access to the CAIRE website returned HTTP 503 from this
environment, and the web retrieval service timed out. This is not evidence
that the service is unavailable from the maintainer's connection.
No EMOV concentration downloader has been implemented or claimed as working.
