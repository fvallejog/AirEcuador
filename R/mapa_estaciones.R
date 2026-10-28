#' Display the Quito monitoring stations on an interactive map
#'
#' Locations of the nine Quito stations included in AirEcuador 0.2.0 are
#' displayed. Current station operation is not verified by this function.
#' The optional package 'leaflet' is required for rendering.
#'
#' @return A `leaflet` HTML widget. Map tiles are downloaded by the browser
#'   when the widget is displayed.
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   mapa <- mapa_estaciones()
#' }
#' @export
mapa_estaciones <- function() {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The optional package 'leaflet' must be installed to display the map.",
         call. = FALSE)
  }
  estaciones <- data.frame(
    pais = "Ecuador", ciudad = "Quito",
    nombre = c("BELISARIO", "CARAPUNGO", "CENTRO", "COTOCOLLAO", "EL_CAMAL",
                "GUAMANI", "LOS_CHILLOS", "TUMBACO", "SAN_ANTONIO"),
    longitud = c(-78.495986, -78.449809, -78.514005, -78.497222, -78.510000,
                  -78.553416, -78.455248, -78.403253, -78.448001),
    latitud = c(-0.184719, -0.095472, -0.221393, -0.107777, -0.250000,
                 -0.333949, -0.297062, -0.214933, -0.009222),
    stringsAsFactors = FALSE
  )
  mapa <- leaflet::leaflet(data = estaciones)
  mapa <- leaflet::addProviderTiles(mapa, "OpenTopoMap")
  mapa <- leaflet::addCircleMarkers(
    mapa, lng = estaciones$longitud, lat = estaciones$latitud,
    popup = paste0("<b>", estaciones$nombre, "</b><br>Quito"),
    label = estaciones$nombre,
    labelOptions = leaflet::labelOptions(permanent = FALSE, textsize = "12px",
                                         direction = "auto"),
    color = "green", radius = 8, stroke = TRUE, fillOpacity = 0.8)
  leaflet::setView(mapa, lng = -78.5, lat = -0.2, zoom = 11)
}
