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
2. Quito extraction must be verified against at least one current official
   RAR archive. The official source returned HTTP 403 from the development
   environment and from a GitHub-hosted runner on 2026-10-27; synthetic
   regression tests alone do not establish current source compatibility.
   An official RAR downloaded through a working connection can be supplied
   for this verification.
3. Cuenca source coverage must be understood as the UDA station published by
   IERSE. Its gas units and the local-time assumption must be retained in any
   scientific analysis. EMOV network coverage must not be claimed.
4. The final check results and any remaining notes must be entered into
   cran-comments.md, and a source tarball must be rebuilt and rechecked if
   package contents have changed.

The reviewed tarball is submitted at <https://CRAN.R-project.org/submit.html>.
The maintainer confirmation email must then be accepted before processing.
Acceptance is determined by the CRAN maintainers.

A source tarball has been built with current R 4.6.1. Package checks on
current R release completed with Status: OK on Linux, Windows and macOS.
The separate real-source Quito verification remains blocked by HTTP 403,
and R-devel results were pending when this status was recorded.
No tarball should be presented as a completed CRAN submission until the
submission and maintainer-confirmation steps have actually been completed.
