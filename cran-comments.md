## Submission

This is the first CRAN submission of AirEcuador (version 0.3.0).

## Test environments

* GitHub-hosted Ubuntu: current R release (4.6.1) and R-devel.
* GitHub-hosted Windows: current R release.
* GitHub-hosted macOS: current R release.
* Ubuntu 24.04: 80 offline expectations passed with R 4.3.3.
* Maintainer's Windows/RStudio session: version 0.3.0 downloaded and processed
  Quito CO observations successfully on 2026-10-28.

Package checks passed on all four GitHub configurations for commit
a396e2e3764a7091fd1da5a32e744881dcca0086. A dedicated source-package job builds
the submission tarball with current R and checks that exact tarball using
--as-cran --no-manual. The final report is included with the release artifact.

## External sources

The official Quito data URL may return HTTP 403 to some cloud connections.
It was successfully accessed from the maintainer's RStudio session. The
corrected exported function returned 48 hourly rows and eight station columns
for 2024-01-01 through 2024-01-02, with both calendar days included.

An official CO.rar supplied by the maintainer was also checked independently.
Excel date cells, ISO date strings, numeric Excel serials and the optional unit
header are supported. Unreported units are returned as NA.

For Cuenca, live checks returned 24 valid hourly means for each of CO, NO2,
O3, PM2.5 and SO2 on 2025-01-01. Original units were retrieved from the IERSE
service. Coverage is explicitly limited to its Universidad del Azuay station;
the EMOV station network and Ambato are not claimed.

Internet access is performed only on explicit function calls. Examples are
guarded by interactive(), and automated tests are offline. Unavailable
resources and changed formats are handled with messages and empty results.
Temporary files are removed on success and failure.

No provider observations are included in the package. The two small test
archives contain synthetic workbooks only.

## Dependencies and compatibility

All required dependencies are available on CRAN. The former non-CRAN
dependency lhmetools is replaced by archive. Existing Quito function names,
station columns, percentile defaults and legacy numeric timestamps are
retained. Local-time interpretation is available explicitly through tz.
