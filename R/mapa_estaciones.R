#' Generar un mapa interactivo de las estaciones de monitoreo en Quito
#'
#' Esta función crea un mapa interactivo con las ubicaciones de las estaciones de monitoreo
#' de calidad del aire en Quito, utilizando la librería leaflet. Las estaciones vigentes
#' se muestran con marcadores verdes.
#'
#' @return Un objeto de mapa interactivo (leaflet) con las estaciones marcadas.
#' @examples
#' \dontrun{
#' # Generar el mapa de estaciones
#' mapa_estaciones()
#' }
#' @import leaflet
#' @export
mapa_estaciones <- function() {
  # Asegurarse de que el paquete leaflet esté disponible
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("El paquete 'leaflet' es necesario para generar el mapa. Por favor, instálalo con: install.packages('leaflet')")
  }
  
  # Crear un data frame con las estaciones vigentes
  estaciones <- data.frame(
    pais = "Ecuador",
    ciudad = "Quito",
    nombre = c("BELISARIO", "CARAPUNGO", "CENTRO", "COTOCOLLAO", "EL_CAMAL", 
               "GUAMANI", "LOS_CHILLOS", "TUMBACO", "SAN_ANTONIO"),
    longitud = c(-78.495986, -78.449809, -78.514005, -78.497222, -78.510000, 
                 -78.553416, -78.455248, -78.403253, -78.448001),
    latitud = c(-0.184719, -0.095472, -0.221393, -0.107777, -0.250000, 
                -0.333949, -0.297062, -0.214933, -0.009222),
    vigente = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Crear el mapa con leaflet
  m <- leaflet::leaflet(data = estaciones) %>%
    # Añadir un mapa base (puedes cambiar a providers$Esri.WorldImagery para vista satelital)
    leaflet::addTiles() %>%
    leaflet::addProviderTiles(providers$OpenTopoMap) %>%
    # Añadir marcadores para las estaciones vigentes
    leaflet::addCircleMarkers(
      lng = ~longitud,
      lat = ~latitud,
      popup = ~paste0("<b>", nombre, "</b><br>",
                      "Ciudad: ", ciudad, "<br>",
                      "Coordenadas: (", round(latitud, 6), ", ", round(longitud, 6), ")<br>",
                      "Estado: Vigente"),
      label = ~as.character(nombre),
      labelOptions = leaflet::labelOptions(
        permanent = FALSE,  # Mostrar etiqueta solo al pasar el cursor
        textsize = "12px",
        direction = "auto"
      ),
      color = "green",  # Color para estaciones vigentes
      radius = 8,
      stroke = TRUE,
      fillOpacity = 0.8
    ) %>%
    # Centrar el mapa en Quito
    leaflet::setView(lng = -78.5, lat = -0.2, zoom = 11)
  
  # Devolver el mapa
  return(m)
}