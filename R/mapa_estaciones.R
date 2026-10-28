#' Display monitoring station locations for a city
#'
#' Reference locations of monitoring stations in Quito or Cuenca are displayed.
#' Calling the function without arguments retains the Quito map.
#' The optional package 'leaflet' is required for rendering.
#'
#' @param ciudad A single city name: `"Quito"` (the default) or `"Cuenca"`.
#'   Case and surrounding whitespace are ignored.
#' @details The nine Quito locations are retained from AirEcuador 0.2.0.
#'   Cuenca shows the IERSE/UDA station and three EMOV-CAIRE sites: Centro
#'   Historico (MUN), Sector Parque Industrial (CCA) and Sector Control Sur
#'   (EIE). The EMOV reference
#'   coordinates are attributed to EMOV (2013) in Table I of Abad and
#'   Mejia (2017); verification of their locations for 2024--2025 is pending.
#'   UDA's reference location is taken from the IERSE air-quality page's
#'   geographic metadata; the precise instrument position is not verified.
#'   Sources and location limitations are displayed in the Cuenca popups.
#'
#'   A mapped site does not imply a supported data download. EMOV downloads
#'   have not been implemented: [extraer_Cuenca_variable()] and
#'   [extraer_Cuenca_estacion()] query the separate UDA station.
#'   Current station operation is not verified by this function.
#' @references Abad, L. and Mejia, D. (2017). Estimacion de la concentracion
#'   de material particulado menor a 10 micras a traves de sensores remotos
#'   en el area urbana de la ciudad de Cuenca, Table I.
#'   <https://www.researchgate.net/publication/336209902>.
#'   IERSE/UDA air-quality portal: <https://ierse.uazuay.edu.ec/aire/>.
#' @return A `leaflet` HTML widget. Map tiles are downloaded by the browser
#'   when the widget is displayed.
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   quito <- mapa_estaciones()
#'   cuenca <- mapa_estaciones(ciudad = "Cuenca")
#' }
#' @export
mapa_estaciones <- function(ciudad = "Quito") {
  estaciones <- .estaciones_mapa(ciudad)
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The optional package 'leaflet' must be installed to display the map.",
         call. = FALSE)
  }
  nota <- ifelse(
    is.na(estaciones$anio_coordenadas),
    "Ubicaciones heredadas del paquete; operacion actual sin verificar.",
    paste0("Coordenadas de referencia: EMOV ", estaciones$anio_coordenadas,
           ".<br>Ubicacion 2024-2025 pendiente de verificar.",
           "<br>Descarga EMOV pendiente en AirEcuador; ",
           "las funciones Cuenca consultan UDA.",
           "<br><a href=\"", estaciones$fuente_coordenadas,
           "\" target=\"_blank\" rel=\"noopener noreferrer\">",
           "Fuente de coordenadas (Tabla I)</a>"))
  es_uda <- estaciones$codigo == "UDA"
  if (any(es_uda)) {
    nota[es_uda] <- paste0(
      "Ubicacion de referencia: metadatos geograficos del portal IERSE.",
      "<br>Posicion exacta del instrumento sin verificar.",
      "<br>Descarga implementada mediante las funciones Cuenca (UDA).",
      "<br><a href=\"", estaciones$fuente_coordenadas[es_uda],
      "\" target=\"_blank\" rel=\"noopener noreferrer\">Portal IERSE/UDA</a>")
  }
  mapa <- leaflet::leaflet(data = estaciones)
  mapa <- leaflet::addProviderTiles(mapa, "OpenTopoMap")
  mapa <- leaflet::addCircleMarkers(
    mapa, lng = estaciones$longitud, lat = estaciones$latitud,
    popup = paste0("<b>", estaciones$nombre, " (", estaciones$codigo,
                   ")</b><br>", estaciones$ciudad, "<br>Red: ",
                   estaciones$red, "<br>", nota),
    label = estaciones$nombre,
    labelOptions = leaflet::labelOptions(permanent = FALSE, textsize = "12px",
                                         direction = "auto"),
    color = ifelse(estaciones$descarga_implementada, "green", "#b35c00"),
    radius = 8, stroke = TRUE, fillOpacity = 0.8)
  if (estaciones$ciudad[1L] == "Quito") {
    return(leaflet::setView(mapa, lng = -78.5, lat = -0.2, zoom = 11))
  }
  leaflet::fitBounds(
    mapa, lng1 = min(estaciones$longitud) - 0.008,
    lat1 = min(estaciones$latitud) - 0.008,
    lng2 = max(estaciones$longitud) + 0.008,
    lat2 = max(estaciones$latitud) + 0.008)
}

.catalogo_estaciones_mapa <- function() {
  quito <- data.frame(
    ciudad = "Quito", red = "Secretaria de Ambiente - Quito",
    codigo = c("BELISARIO", "CARAPUNGO", "CENTRO", "COTOCOLLAO", "EL_CAMAL",
                "GUAMANI", "LOS_CHILLOS", "TUMBACO", "SAN_ANTONIO"),
    nombre = c("BELISARIO", "CARAPUNGO", "CENTRO", "COTOCOLLAO", "EL_CAMAL",
                "GUAMANI", "LOS_CHILLOS", "TUMBACO", "SAN_ANTONIO"),
    longitud = c(-78.495986, -78.449809, -78.514005, -78.497222, -78.510000,
                  -78.553416, -78.455248, -78.403253, -78.448001),
    latitud = c(-0.184719, -0.095472, -0.221393, -0.107777, -0.250000,
                 -0.333949, -0.297062, -0.214933, -0.009222),
    anio_coordenadas = NA_integer_, fuente_coordenadas = NA_character_,
    descarga_implementada = TRUE,
    stringsAsFactors = FALSE
  )
  cuenca <- data.frame(
    ciudad = "Cuenca", red = "EMOV - CAIRE",
    codigo = c("MUN", "CCA", "EIE"),
    nombre = c("Centro Hist\u00f3rico", "Sector Parque Industrial",
                "Sector Control Sur"),
    longitud = c(-79.003000, -78.980024, -79.034047),
    latitud = c(-2.897001, -2.880532, -2.916303),
    anio_coordenadas = 2013L,
    fuente_coordenadas = paste0(
      "https://www.researchgate.net/publication/336209902_",
      "EJE_07-07_Estimacion_de_la_concentracion_de_material_particulado_",
      "menor_a_10_micras_a_traves_de_sensores_remotos_en_el_area_urbana_",
      "de_la_ciudad_de_Cuenca"),
    descarga_implementada = FALSE,
    stringsAsFactors = FALSE)
  uda <- data.frame(
    ciudad = "Cuenca", red = "IERSE - Universidad del Azuay",
    codigo = "UDA", nombre = "Universidad del Azuay",
    longitud = -79.0005447, latitud = -2.9190715,
    anio_coordenadas = NA_integer_,
    fuente_coordenadas = "https://ierse.uazuay.edu.ec/aire/",
    descarga_implementada = TRUE, stringsAsFactors = FALSE)
  rbind(quito, cuenca, uda)
}

.estaciones_mapa <- function(ciudad = "Quito") {
  catalogo <- .catalogo_estaciones_mapa()
  ciudades <- unique(catalogo$ciudad)
  error <- paste0("ciudad must be one of: ", paste(ciudades, collapse = ", "),
                  ".")
  if (!is.character(ciudad) || length(ciudad) != 1L || is.na(ciudad)) {
    stop(error, call. = FALSE)
  }
  indice <- match(tolower(trimws(ciudad)), tolower(ciudades))
  if (is.na(indice)) stop(error, call. = FALSE)
  salida <- catalogo[catalogo$ciudad == ciudades[indice], , drop = FALSE]
  rownames(salida) <- NULL
  salida
}
