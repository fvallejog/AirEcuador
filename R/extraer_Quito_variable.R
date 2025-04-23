#' Extraer y procesar datos ambientales de Quito por contaminante o variable meteorológica
#'
#' Esta función descarga, descomprime y procesa datos ambientales de Quito
#' (como PM2.5, CO, NO2, etc.) desde los datos abiertos de la Secretaría de
#' Ambiente de Quito, para un contaminante o variable meteorológica y un rango de fechas.
#'
#' @param contaminante Carácter, el contaminante a extraer (e.g., "CO", "PM2.5", "NO2").
#' @param fecha_inicio Fecha (formato "YYYY-MM-DD") desde la cual extraer datos (por defecto: "2004-01-01").
#' @param fecha_fin Fecha (formato "YYYY-MM-DD") hasta la cual extraer datos (por defecto: último mes disponible).
#' @param year Numérico, año completo para el cual extraer datos (opcional). Si se especifica, sobrescribe fecha_inicio y fecha_fin.
#' @param percentil_limite Numérico, percentil para filtrar valores extremos (por defecto 0.999).
#' @return Un data frame con las concentraciones del contaminante para todas las estaciones,
#' con columnas: date, y una columna por cada estación con los valores del contaminante.
#' @examples
#' \dontrun{
#' # Extraer datos de CO para un rango de fechas
#' datos <- extraer_Quito_variable(contaminante = "CO", fecha_inicio = "2024-01-01", fecha_fin = "2024-12-31")
#' head(datos)
#' # Extraer datos de CO para un año completo
#' datos <- extraer_Quito_variable(contaminante = "CO", year = 2024)
#' head(datos)
#' }
#' @import httr
#' @import dplyr
#' @import lhmetools
#' @import openxlsx
#' @import lubridate
#' @export
extraer_Quito_variable <- function(contaminante = "CO", 
                                   fecha_inicio = "2004-01-01", 
                                   fecha_fin = format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m-%d"), 
                                   year = NULL,
                                   percentil_limite = 0.999) {
  # Asegurarse de que los paquetes estén disponibles
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("El paquete 'httr' es necesario para descargar los datos. Por favor, instálalo con: install.packages('httr')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("El paquete 'dplyr' es necesario para procesar los datos. Por favor, instálalo con: install.packages('dplyr')")
  }
  
  # Definir enlaces y fechas mínimas para cada contaminante
  base_url <- "https://datosambiente.quito.gob.ec/datos/"
  enlaces <- list(
    CO = paste0(base_url, "CO.rar"),
    NO2 = paste0(base_url, "NO2.rar"),
    O3 = paste0(base_url, "O3.rar"),
    PM10 = paste0(base_url, "PM10.rar"),
    "PM2.5" = paste0(base_url, "PM2.5.rar"),
    SO2 = paste0(base_url, "SO2.rar"),
    LLU = paste0(base_url, "LLU.rar"),
    DIR = paste0(base_url, "DIR.rar"),
    HUM = paste0(base_url, "HUM.rar"),
    IUV = paste0(base_url, "IUV.rar"),
    PRE = paste0(base_url, "PRE.rar"),
    RS = paste0(base_url, "RS.rar"),
    TMP = paste0(base_url, "TMP.rar"),
    VEL = paste0(base_url, "VEL.rar")
  )
  
  # Definir fechas mínimas para cada contaminante
  fechas_minimas <- list(
    CO = as.Date("2004-01-01"),
    NO2 = as.Date("2004-01-01"),
    O3 = as.Date("2004-01-01"),
    PM10 = as.Date("2004-01-01"),
    "PM2.5" = as.Date("2004-08-01"),  # PM2.5 comienza en agosto de 2004
    SO2 = as.Date("2004-01-01"),
    LLU = as.Date("2004-01-01"),
    DIR = as.Date("2004-01-01"),
    HUM = as.Date("2004-01-01"),
    IUV = as.Date("2004-01-01"),
    PRE = as.Date("2004-01-01"),
    RS = as.Date("2004-01-01"),
    TMP = as.Date("2004-01-01"),
    VEL = as.Date("2004-01-01")
  )
  
  # Validar contaminante
  if (length(contaminante) != 1 || !contaminante %in% names(enlaces)) {
    stop("Debe especificar exactamente un contaminante válido. Opciones: ", paste(names(enlaces), collapse = ", "))
  }
  
  # Si se especifica year, ajustar fecha_inicio y fecha_fin
  if (!is.null(year)) {
    if (!is.numeric(year) || year < 2004 || year > year(Sys.Date())) {
      stop("El año debe estar entre 2004 y ", year(Sys.Date()))
    }
    fecha_inicio <- as.Date(paste0(year, "-01-01"))
    fecha_fin <- as.Date(paste0(year, "-12-31"))
    message("Año especificado: ", year, ". Ajustando fechas: ", fecha_inicio, " a ", fecha_fin)
  }
  
  # Validar fechas
  fecha_inicio <- as.Date(fecha_inicio)
  fecha_fin <- as.Date(fecha_fin)
  if (fecha_inicio > fecha_fin) {
    stop("La fecha de inicio no puede ser mayor que la fecha de fin")
  }
  
  # Verificar fecha mínima para el contaminante
  fecha_minima <- fechas_minimas[[contaminante]]
  if (fecha_inicio < fecha_minima) {
    message("Nota: Los datos para ", contaminante, " comienzan en ", fecha_minima, 
            ". Ajustando fecha de inicio a ", fecha_minima, ".")
    fecha_inicio <- fecha_minima
  }
  
  if (fecha_fin > Sys.Date()) {
    message("Nota: La fecha de fin es posterior a la fecha actual (", Sys.Date(), 
            "). Ajustando fecha de fin al último mes disponible: ", 
            format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m-%d"))
    fecha_fin <- as.Date(format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m-%d"))
  }
  
  # Descargar archivo con manejo de errores
  enlace <- enlaces[[contaminante]]
  temp_rar <- tempfile(fileext = ".rar")
  message("Descargando datos de ", contaminante, "...")
  response <- tryCatch(
    suppressMessages(httr::GET(enlace, 
                               write_disk(temp_rar, overwrite = TRUE),
                               user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"))),
    error = function(e) {
      stop("No se pudo descargar el archivo para el contaminante ", contaminante, 
           ". Error: ", e$message, "\nVerifica el enlace: ", enlace)
    }
  )
  if (httr::status_code(response) != 200) {
    stop("Error al descargar: ", enlace, 
         " (código de estado: ", httr::status_code(response), 
         "). Posibles causas: archivo no encontrado, servidor no disponible, o problemas de conexión.")
  }
  
  # Crear un directorio temporal limpio
  timestamp <- gsub("[^0-9]", "", Sys.time())
  temp_dir <- file.path(tempdir(), paste0("extract_", timestamp))
  dir.create(temp_dir, showWarnings = FALSE)
  message("Extrayendo archivo .rar a: ", temp_dir)
  
  # Descomprimir usando lhmetools::unrar
  extracted_files <- tryCatch(
    suppressMessages(lhmetools::unrar(file = temp_rar, dest_dir = temp_dir, overwrite = TRUE, quiet = TRUE)),
    error = function(e) {
      stop("Error al descomprimir con lhmetools::unrar: ", e$message, 
           "\nAsegúrate de tener 7-Zip instalado (puedes instalarlo con installr::install.7zip()).")
    }
  )
  message("Archivos extraídos del .rar: ", paste(extracted_files, collapse = ", "))
  
  # Buscar archivo Excel
  archivo_excel <- file.path(temp_dir, paste0(contaminante, ".xlsx"))
  if (!file.exists(archivo_excel)) {
    message("No se encontró ", contaminante, ".xlsx en la raíz. Buscando cualquier .xlsx o .xls...")
    archivo_excel <- list.files(temp_dir, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE, recursive = TRUE)[1]
    if (is.na(archivo_excel)) {
      stop("No se encontró Excel (.xlsx o .xls) en: ", enlace, ". Archivos presentes: ", paste(extracted_files, collapse = ", "))
    }
  }
  message("Archivo Excel encontrado: ", archivo_excel)
  
  # Leer el archivo Excel con openxlsx
  message("Leyendo archivo Excel: ", archivo_excel)
  datos_crudos <- tryCatch(
    suppressMessages(openxlsx::read.xlsx(archivo_excel, sheet = 1, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)),
    error = function(e) {
      stop("Error al leer el archivo Excel: ", e$message)
    }
  )
  
  # Inspeccionar los encabezados (fila 1)
  encabezados_crudos <- datos_crudos[1, ]
  message("Encabezados crudos: ", paste(encabezados_crudos, collapse = ", "))
  
  # Inspeccionar las unidades (fila 2)
  unidades <- datos_crudos[2, ]
  message("Unidades (fila 2): ", paste(unidades, collapse = ", "))
  
  # Procesar nombres de estaciones (columna 2 en adelante)
  nombres_columnas <- as.character(unlist(encabezados_crudos[-1]))
  nombres_columnas <- nombres_columnas[!is.na(nombres_columnas) & nombres_columnas != ""]
  nombres_columnas <- trimws(nombres_columnas)
  nombres_columnas <- toupper(nombres_columnas)
  nombres_columnas <- gsub("[^A-Z0-9]", "_", nombres_columnas)
  
  # Estandarizar nombres de estaciones usando el mismo mapa que en la función anterior
  estacion_map <- list(
    "LOS_CHILLOS|LOSCHILLOS" = "Los_Chillos",
    "GUAMANI|GUAMANÍ|GUAMAN_" = "Guamani",
    "TUMBACO" = "Tumbaco",
    "BELISARIO" = "Belisario",
    "CARAPUNGO" = "Carapungo",
    "CENTRO" = "Centro",
    "COTOCOLLAO" = "Cotocollao",
    "EL_CAMAL|ELCAMAL" = "El_Camal",
    "SAN_ANTONIO|SANANTONIO" = "San_Antonio",
    "CONDADO" = "Condado",
    "TURUBAMBA" = "Turubamba",
    "CHILLOGALLO" = "Chillogallo",
    "JIPIJAPA" = "Jipijapa"
  )
  
  nombres_columnas_estandar <- nombres_columnas
  for (i in seq_along(nombres_columnas)) {
    for (patron in names(estacion_map)) {
      if (grepl(patron, nombres_columnas[i])) {
        nombres_columnas_estandar[i] <- estacion_map[[patron]]
        break
      }
    }
  }
  
  # Asegurarse de que no haya nombres vacíos después de las transformaciones
  nombres_columnas_estandar <- nombres_columnas_estandar[nombres_columnas_estandar != "" & !is.na(nombres_columnas_estandar)]
  if (length(nombres_columnas_estandar) == 0) {
    stop("No se encontraron nombres de estaciones válidos para el contaminante ", contaminante)
  }
  
  # Depuración: Mostrar los nombres de las columnas
  message("Nombres de columnas para ", contaminante, ": ", paste(nombres_columnas_estandar, collapse = ", "))
  
  # Leer datos desde la fila 3 (excluyendo las primeras 2 filas)
  datos <- datos_crudos[-c(1:2), ]
  
  # Asignar nombres de columnas, asegurándose de que coincidan con los nombres estandarizados
  nombres_columnas_completos <- c("fecha", nombres_columnas_estandar)
  if (ncol(datos) < length(nombres_columnas_completos)) {
    nombres_columnas_completos <- nombres_columnas_completos[1:ncol(datos)]
  } else if (ncol(datos) > length(nombres_columnas_completos)) {
    datos <- datos[, 1:length(nombres_columnas_completos), drop = FALSE]
  }
  colnames(datos) <- nombres_columnas_completos
  
  # Depuración: Mostrar los nombres de las columnas después de asignar
  message("Nombres de columnas en datos (antes de mutate): ", paste(colnames(datos), collapse = ", "))
  
  # Convertir fechas numéricas de Excel a POSIXct
  datos <- as.data.frame(datos) %>%
    dplyr::mutate(date = as.POSIXct((as.numeric(fecha) - 25569) * 86400, origin = "1970-01-01", tz = "UTC")) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::select(-fecha)
  
  # Verificar las primeras fechas convertidas
  message("Primeras 5 fechas convertidas: ", paste(head(datos$date, 5), collapse = ", "))
  
  # Filtrar por rango de fechas
  datos <- datos %>%
    dplyr::filter(date >= fecha_inicio & date <= fecha_fin)
  if (nrow(datos) == 0) {
    stop("No hay datos para el rango de fechas especificado (", fecha_inicio, " a ", fecha_fin, 
         ") para el contaminante ", contaminante)
  }
  
  # Convertir valores a numérico y manejar valores negativos
  datos <- datos %>%
    dplyr::mutate(across(all_of(nombres_columnas_estandar), as.numeric)) %>%
    dplyr::mutate(across(all_of(nombres_columnas_estandar), ~if_else(. < 0, NA_real_, .)))
  
  # Filtrado al percentil 0.999 por cada estación
  for (estacion in nombres_columnas_estandar) {
    if (sum(!is.na(datos[[estacion]])) > 0) {
      limite <- quantile(datos[[estacion]], percentil_limite, na.rm = TRUE)
      message("Percentil ", percentil_limite, " para ", estacion, ": ", limite)
      datos <- datos %>%
        dplyr::mutate(!!estacion := if_else(.data[[estacion]] > limite, NA_real_, .data[[estacion]]))
    }
  }
  
  # Mostrar cantidad de datos no NA por estación después del filtrado
  datos_no_na <- colSums(!is.na(datos[, nombres_columnas_estandar, drop = FALSE]))
  message("Datos no NA por estación después del filtrado: ")
  for (estacion in names(datos_no_na)) {
    message(estacion, ": ", datos_no_na[estacion])
  }
  
  # Depuración: Mostrar los nombres de las columnas finales
  message("Nombres de columnas en datos (final): ", paste(colnames(datos), collapse = ", "))
  
  # Limpiar archivos temporales
  unlink(temp_rar)
  unlink(temp_dir, recursive = TRUE)
  
  # Mensaje final y summary
  message("Datos listos para el contaminante ", contaminante, " en el rango ", fecha_inicio, " a ", fecha_fin, ".")
  print(summary(datos))
  
  return(datos)
}