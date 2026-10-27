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
   environment on 2026-10-27; synthetic regression tests alone do not establish
   current source compatibility.
3. Cuenca source coverage must be understood as the UDA station published by
   IERSE. Its gas units and the local-time assumption must be retained in any
   scientific analysis. EMOV network coverage must not be claimed.
4. The final check results and any remaining notes must be entered into
   cran-comments.md, and a source tarball must be rebuilt and rechecked if
   package contents have changed.

The reviewed tarball is submitted at <https://CRAN.R-project.org/submit.html>.
The maintainer confirmation email must then be accepted before processing.
Acceptance is determined by the CRAN maintainers.

The tarball built with R 4.3.3 in this environment is a development check
artifact. It must not be presented as a completed CRAN submission.
