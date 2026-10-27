# An explicit maintainer check; this file is excluded from the source package.
# Run from the repository root after installation: Rscript dev/validate-live.R
library(AirEcuador)
for (variable in c("CO", "NO2", "O3", "PM2.5", "SO2")) {
  x <- extraer_Cuenca_variable(variable, "2025-01-01", "2025-01-01")
  stopifnot(nrow(x) == 24L, any(is.finite(x$UDA)),
            all(attr(x, "cobertura")$UDA <= 60L),
            length(attr(x, "unidades")[["UDA"]]) == 1L)
  cat(variable, ": ", nrow(x), " hours; unit = ", attr(x, "unidades")[["UDA"]],
      "; valid hours = ", sum(!is.na(x$UDA)), "\n", sep = "")
}
q <- extraer_Quito_variable("CO", "2024-01-01", "2024-01-01",
                            percentil_limite = NULL,
                            tz = "America/Guayaquil")
stopifnot(nrow(q) == 24L)
cat("Quito: ", nrow(q), " hourly rows\n", sep = "")
