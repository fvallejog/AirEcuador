This submission candidate has not yet been submitted to CRAN.

AirEcuador has been distributed through GitHub. Support for Cuenca observations
from the Universidad del Azuay station, published by IERSE, is added in 0.3.0.
The non-CRAN dependency lhmetools is removed. The existing exported Quito
functions and their legacy defaults are retained.

Validation completed on 2026-10-27:

* 80 offline test expectations passed without failures, warnings or skips on
  Ubuntu 24.04 with R 4.3.3.
* R CMD check --as-cran --no-manual completed with 0 errors, 0 warnings and
  3 notes: new submission / Quito source URL HTTP 403; optional leaflet not
  installed locally; current time could not be verified from this environment.
* Live Cuenca checks for CO, NO2, O3, PM2.5 and SO2 returned 24 valid hourly
  means each for 2025-01-01. Original units were retrieved: ppm for the gases
  and ug/m3 for PM2.5.
* Direct Quito downloads returned HTTP 403 from this environment. An official
  CO.rar downloaded by the maintainer was then supplied on 2026-10-27. Its
  archive extraction, spreadsheet reading, date filtering and station output
  were verified through the exported function with the download step replaced
  by the supplied file. For 2024-01-01, 24 hourly rows and eight station columns
  were returned. Local-file processing is verified; direct HTTP access from
  the maintainer's R session has not been verified.
* The same HTTP 403 was reproduced by the separate live Quito check on a
  GitHub-hosted Ubuntu runner. This failure was observed after the package
  check itself completed with Status: OK.
* R CMD check --as-cran --no-manual completed with Status: OK on GitHub-hosted
  Linux, Windows and macOS using current R release (4.6.1) before the official
  archive format adjustment. These checks must be repeated for the corrected
  candidate. The R-devel jobs were still installing dependencies when this
  status was recorded.
* A development source tarball was also built locally with R 4.6.1. The local
  offline tests above were run with R 4.3.3; no local R 4.6.1 test pass is
  claimed because the older native dependency builds were incompatible.
* Additional live Cuenca spot checks returned minute observations for
  2026-10-01. No observations were returned for the tested intervals on
  2014-01-01 and 2024-06-01. This does not establish the earliest available
  record, and full EMOV historical coverage is not claimed.

Checks on current R release, R-devel, Windows and macOS are configured in
.github/workflows/R-CMD-check.yaml. Final R-devel results must be reviewed
before the submission comments are finalized.

External data are accessed only at explicit user request. Examples are guarded
by interactive(), and tests do not access the network. Temporary downloads
are removed on both successful and failed calls. Unavailable services and
unexpected remote formats are handled by messages and empty results.

No provider observations are bundled with the package. The test archive
contains entirely synthetic observations.
