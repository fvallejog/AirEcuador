This submission candidate has not yet been submitted to CRAN.

AirEcuador has been distributed through GitHub. Support for Cuenca observations
from the Universidad del Azuay station, published by IERSE, is added in 0.3.0.
The non-CRAN dependency lhmetools is removed. The existing exported Quito
functions and their legacy defaults are retained.

Validation completed on 2026-10-27:

* 71 offline test expectations passed without failures, warnings or skips on
  Ubuntu 24.04 with R 4.3.3.
* Live Cuenca checks for CO, NO2, O3, PM2.5 and SO2 returned 24 valid hourly
  means each for 2025-01-01. Original units were retrieved: ppm for the gases
  and ug/m3 for PM2.5.
* Quito live checks could not be completed from this environment: the official
  archive endpoint returned HTTP 403. An end-to-end synthetic archive and
  spreadsheet test passed. Confirmation against a current official Quito
  archive is still required before submission.

Checks on current R release, R-devel, Windows and macOS are configured in
.github/workflows/R-CMD-check.yaml. Their results must be reviewed before the
submission comments are finalized. No cross-platform pass is claimed here.

External data are accessed only at explicit user request. Examples are guarded
by interactive(), and tests do not access the network. Temporary downloads
are removed on both successful and failed calls. Unavailable services and
unexpected remote formats are handled by messages and empty results.

No provider observations are bundled with the package. The test archive
contains entirely synthetic observations.
