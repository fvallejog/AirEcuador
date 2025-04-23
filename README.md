AirEcuador
Este paquete proporciona herramientas para descargar, extraer y analizar datos ambientales de Quito, como concentraciones de contaminantes (PM2.5, CO, NO2, etc.), a partir de los datos abiertos de la Secretaría de Ambiente de Quito.

Instalación
Puedes instalar AirEcuador desde GitHub usando devtools:

# Instalar devtools si no lo tienes
if (!require(devtools)) install.packages("devtools")

# Instalar AirEcuador
devtools::install_github("fvallejog/AirEcuador")

# Cargar el paquete
library(AirEcuador)

Uso Básico
Extraer datos por estación

datos <- extraer_Quito_estacion(estacion = "Los_Chillos", fecha_inicio = "2024-01-01", fecha_fin = "2024-06-30")
head(datos)

Extraer datos por contaminante
datos_co <- extraer_Quito_variable(contaminante = "CO", year = 2024)
head(datos_co)

Visualizar las estaciones en un mapa
mapa_estaciones()

Ejemplos Avanzados con openair y ggplot2

1. Resumen de contaminantes con openair

Usa openair para generar un resumen estadístico de los contaminantes en una estación específica:

library(openair)

# Extraer datos de Los Chillos
datos <- extraer_Quito_estacion(estacion = "Los_Chillos", year = 2024)

# Renombrar la columna 'date' para que sea compatible con openair
datos <- dplyr::rename(datos, date = date)

# Generar un resumen de estadísticas para todos los contaminantes
summaryPlot(datos, pollutant = c("CO", "PM2.5", "NO2"))

2. Rosa de contaminación con openair

Analiza la dirección del viento y su relación con los niveles de CO usando una rosa de contaminación:

library(openair)

# Extraer datos de CO y dirección del viento (DIR)
datos_co <- extraer_Quito_variable(contaminante = "CO", year = 2024)
datos_dir <- extraer_Quito_variable(contaminante = "DIR", year = 2024)

# Combinar los datos
datos <- dplyr::inner_join(datos_co, datos_dir, by = "date") %>%
  dplyr::select(date, Los_Chillos.x, Los_Chillos.y) %>%
  dplyr::rename(CO = Los_Chillos.x, ws = Los_Chillos.y)

# Crear una rosa de contaminación
pollutionRose(datos, pollutant = "CO", ws = "ws", main = "Rosa de Contaminación CO en Los Chillos (2024)")

3. Serie temporal con ggplot2

Visualiza la serie temporal de CO para múltiples estaciones usando ggplot2:

library(ggplot2)
library(dplyr)
library(tidyr)

# Extraer datos de CO
datos_co <- extraer_Quito_variable(contaminante = "CO", year = 2024)

# Transformar a formato largo para ggplot2
datos_long <- datos_co %>%
  tidyr::pivot_longer(cols = -date, names_to = "estacion", values_to = "CO")

# Crear la gráfica
ggplot(datos_long, aes(x = date, y = CO, color = estacion)) +
  geom_line() +
  labs(title = "Niveles de CO en Quito (2024)", x = "Fecha", y = "CO (ppm)") +
  theme_minimal()

Dependencias

Este paquete requiere los siguientes paquetes de R:

httr
dplyr
lhmetools
openxlsx
lubridate
leaflet
tidyr

Paquetes sugeridos para análisis avanzados:
openair
ggplot2

Licencia

Este proyecto está bajo la licencia MIT.

Autor

Fidel Vallejo
GitHub: fvallejog
fidel.vallejo@unach.edu.ec
