# CRAN submission preparation

Version: 0.3.0

Maintainer: Fidel Vallejo <fidel.vallejo@unach.edu.ec>

The source package must be built from the reviewed release commit with a
current R release. The exact resulting tarball must be checked with
`R CMD check --as-cran` on current R-devel or current R release, with any
remaining notes explained in cran-comments.md.

Before submission:

1. Results for Linux release, R-devel, Windows release and macOS release must
   be reviewed in GitHub Actions.
2. Quito extraction was verified against the official CO.rar supplied by the
   maintainer on 2026-10-27. The one-row header and Excel date-cell layout are
   now supported, with 24 hourly rows and eight station columns recovered for
   2024-01-01. Direct HTTP access returned 403 from this environment and a
   GitHub-hosted runner. On 2026-10-28 the existing installed function was
   confirmed to download and extract the archive from the maintainer's
   RStudio session; its subsequent date conversion failed. The corrected
   reader returned 48 hourly rows for the maintainer's exact two-day interval
   when the supplied archive was used, and the working Quito request user
   agent is retained in the candidate.
3. Cuenca source coverage must be understood as the UDA station published by
   IERSE. Its gas units and the local-time assumption must be retained in any
   scientific analysis. EMOV network coverage must not be claimed.
4. The final check results and any remaining notes must be entered into
   cran-comments.md, and a source tarball must be rebuilt and rechecked if
   package contents have changed.

The reviewed tarball is submitted at <https://CRAN.R-project.org/submit.html>.
The maintainer confirmation email must then be accepted before processing.
Acceptance is determined by the CRAN maintainers.

A source tarball was built with current R 4.6.1. Package checks after the
official archive format adjustment passed on Linux, Windows, macOS and
R-devel for commit 13bcaa312906a336b6d7c61e92ce1a3c91345776. The subsequent
request-header compatibility adjustment must be included in the final
checked commit and source tarball.
No tarball should be presented as a completed CRAN submission until the
submission and maintainer-confirmation steps have actually been completed.
