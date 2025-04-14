
# AirEcuador

Este paquete proporciona herramientas para descargar, extraer y analizar datos ambientales de Quito, como concentraciones de contaminantes (PM2.5, CO, NO2, etc.), a partir de los datos abiertos de la Secretaría de Ambiente de Quito.

## Instalación

Puedes instalar `AirEcuador` desde GitHub usando `devtools`:

```R
# Instalar devtools si no lo tienes
if (!require(devtools)) install.packages("devtools")

# Instalar AirEcuador
devtools::install_github("fvallejog/AirEcuador")
