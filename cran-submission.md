# AirEcuador 0.3.0 submission

Maintainer: Fidel Vallejo <fidel.vallejo@unach.edu.ec>

## Current status

The checked source archive was uploaded and submitted through the CRAN web
form on 2026-10-28 (upload identifier 360569). The portal reached Step 3
(Confirmation) and reported that a confirmation email had been sent to the
maintainer.

**Submission on hold: do not confirm or resubmit.** On 2026-10-28 the
maintainer confirmed that the email link had not been accepted and requested
review of the Cuenca stations before proceeding. Changes to GitHub do not
modify the already uploaded 0.3.0 archive. Development version 0.3.0.9000
is separate and has not been submitted to CRAN.

**Maintainer email confirmation is pending.** The portal states that the
submission is not complete until the maintainer follows the link in that
email. After confirmation the package will be passed to CRAN for review.
Acceptance or publication has not been granted.

The reviewed changes were merged into main through pull request #1:
<https://github.com/fvallejog/AirEcuador/pull/1>.
Merge commit: 55532a0d55a0656965de9487b62eac19ab7063ea.

## Validation and submitted archive

The maintainer confirmed successful download and processing from Windows
RStudio on 2026-10-28 using AirEcuador 0.3.0: 48 rows and nine columns were
returned for Quito CO observations from 2024-01-01 through 2024-01-02, with
timestamps from 00:00 on the first day through 23:00 on the second day.

All package checks at commit 4106c0f57ad05ee35e5317710ba11343c96027c7 passed on
Linux R-release, Linux R-devel, Windows R-release and macOS R-release.
The exact submitted source archive was built with R 4.6.1 and checked using
`--as-cran --no-manual`: 0 errors, 0 warnings and one NOTE (New submission).
The package files in the archive were compared with the reviewed checkout.

- Archive: `AirEcuador_0.3.0.tar.gz`
- SHA-256: `7b3604105f0ca4d074af4b4b361eac057814ae76ac527069668ba92253c1311f`
- MD5: `753d4676524473332a18aaac4c841014`
- Build and check run: <https://github.com/fvallejog/AirEcuador/actions/runs/40711720676>
- Retained artifact: `AirEcuador-0.3.0-CRAN-source` (ID 12826686283)

No provider data are bundled. Cuenca coverage is limited to the UDA station
published by IERSE. Ambato is excluded.
