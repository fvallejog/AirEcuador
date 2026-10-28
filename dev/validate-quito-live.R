# Explicit source verification, separate from offline CRAN examples and tests.
library(AirEcuador)
q <- extraer_Quito_variable("CO", "2024-01-01", "2024-01-01",
                            percentil_limite = NULL,
                            tz = "America/Guayaquil")
stopifnot(nrow(q) == 24L, ncol(q) > 1L,
          any(is.finite(as.matrix(q[-1L]))),
          identical(format(q$date[c(1L, 24L)], "%H:%M",
                           tz = "America/Guayaquil"), c("00:00", "23:00")))
cat("Quito official archive: ", nrow(q), " hourly rows; ", ncol(q) - 1L,
    " station columns\n", sep = "")
print(attr(q, "unidades"))
