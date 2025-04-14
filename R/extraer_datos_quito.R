#' Extraer y procesar datos ambientales de Quito
#'
#' Esta función descarga, descomprime y procesa datos ambientales de Quito
#' (como PM2.5, CO, NO2, etc.) desde los datos abiertos de la Secretaría de
#' Ambiente de Quito, para un año y contaminante específicos.
#'
#' @param contaminante Carácter, el contaminante a extraer (e.g., "PM2.5", "CO", "NO2").
#' Opciones disponibles: CO, NO2, O3, PM10, PM2.5, SO2, LLU, DIR, HUM, IUV, PRE, RS, TMP, VEL.
#' @param year Numérico, el año de los datos a extraer (2004-2024).
#' @param percentil_limite Numérico, percentil para filtrar valores extremos (por defecto 0.999).
#' @return Un data frame con las concentraciones del contaminante por estación y fecha,
#' filtrado para el año especificado.
#' @examples
#' \dontrun{
#' # Extraer datos de PM2.5 para 2024
#' datos <- extraer_datos_quito(contaminante = "PM2.5", year = 2024)
#' head(datos)
#' }
#' @import httr
#' @import lhmetools
#' @import openxlsx
#' @import dplyr
#' @import lubridate
#' @export
extraer_datos_quito <- function(contaminante = "CO", year = 2024, percentil_limite = 0.999) {
  # Definir enlaces para cada contaminante
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
  
  # Validar contaminante
  if (length(contaminante) != 1 || !contaminante %in% names(enlaces)) {
    stop("Debe especificar exactamente un contaminante válido. Opciones: ", paste(names(enlaces), collapse = ", "))
  }
  
  # Validar año
  if (!is.numeric(year) || year < 2004) {
    stop("No existe datos antes del 01 de enero del 2004")
  }
  if (year > 2024) {
    stop("No existe datos completos después del 31 de diciembre del 2024")
  }
  
  # Descargar archivo con manejo de errores
  enlace <- enlaces[[contaminante]]
  temp_rar <- tempfile(fileext = ".rar")
  message("Descargando archivo: ", enlace)
  response <- tryCatch(
    GET(enlace, write_disk(temp_rar, overwrite = TRUE)),
    error = function(e) {
      stop("No se pudo descargar el archivo para el contaminante ", contaminante, ". Verifica el enlace: ", enlace)
    }
  )
  if (status_code(response) != 200) {
    stop("Error al descargar: ", enlace, " (código de estado: ", status_code(response), ")")
  }
  
  # Crear un directorio temporal limpio con un nombre válido
  timestamp <- gsub("[^0-9]", "", Sys.time()) # Eliminar caracteres no numéricos
  temp_dir <- file.path(tempdir(), paste0("extract_", timestamp))
  dir.create(temp_dir, showWarnings = FALSE)
  message("Extrayendo archivo .rar a: ", temp_dir)
  
  # Descomprimir usando lhmetools::unrar
  message("Descomprimiendo con lhmetools::unrar...")
  extracted_files <- tryCatch(
    lhmetools::unrar(file = temp_rar, dest_dir = temp_dir, overwrite = TRUE, quiet = FALSE),
    error = function(e) {
      stop("Error al descomprimir con lhmetools::unrar: ", e$message, 
           "\nAsegúrate de tener 7-Zip instalado (puedes instalarlo con installr::install.7zip()).")
    }
  )
  message("Archivos extraídos del .rar: ", paste(extracted_files, collapse = ", "))
  
  # Buscar archivo Excel (usar el nombre exacto PM2.5.xlsx)
  archivo_excel <- file.path(temp_dir, "PM2.5.xlsx")
  if (!file.exists(archivo_excel)) {
    message("No se encontró PM2.5.xlsx en la raíz. Buscando cualquier .xlsx o .xls...")
    archivo_excel <- list.files(temp_dir, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE, recursive = TRUE)[1]
    if (is.na(archivo_excel)) {
      stop("No se encontró Excel (.xlsx o .xls) en: ", enlace, ". Archivos presentes: ", paste(extracted_files, collapse = ", "))
    }
  }
  message("Archivo Excel encontrado: ", archivo_excel)
  
  # Leer el archivo Excel con openxlsx
  message("Leyendo archivo Excel: ", archivo_excel)
  datos_crudos <- tryCatch(
    openxlsx::read.xlsx(archivo_excel, sheet = 1, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE),
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
  
  # Verificar estaciones detectadas
  message("Estaciones detectadas: ", paste(nombres_columnas, collapse = ", "))
  message("Número de estaciones: ", length(nombres_columnas))
  
  # Leer datos desde la fila 3 (excluyendo las primeras 2 filas)
  datos <- datos_crudos[-c(1:2), ]
  colnames(datos) <- c("fecha", nombres_columnas)
  
  # Inspeccionar las primeras fechas
  message("Primeras 5 fechas crudas: ", paste(head(datos$fecha, 5), collapse = ", "))
  
  # Convertir fechas numéricas de Excel a POSIXct
  datos <- as.data.frame(datos) %>%
    mutate(date = as.POSIXct((as.numeric(fecha) - 25569) * 86400, origin = "1970-01-01", tz = "UTC")) %>%
    filter(!is.na(date)) %>%
    select(-fecha)
  
  # Verificar las primeras fechas convertidas
  message("Primeras 5 fechas convertidas: ", paste(head(datos$date, 5), collapse = ", "))
  
  # Filtrar por año
  datos <- datos %>%
    filter(year(date) == year)
  if (nrow(datos) == 0) {
    stop("No hay datos para el año ", year, " para el contaminante ", contaminante)
  }
  
  # Convertir valores a numérico y manejar valores negativos
  datos <- datos %>%
    mutate(across(all_of(nombres_columnas), as.numeric)) %>%
    mutate(across(all_of(nombres_columnas), ~if_else(. < 0, NA_real_, .)))
  
  # Filtrado al percentil 0.999 por cada estación
  for (estacion in nombres_columnas) {
    if (sum(!is.na(datos[[estacion]])) > 0) {
      limite <- quantile(datos[[estacion]], percentil_limite, na.rm = TRUE)
      message("Percentil 0.999 para ", estacion, ": ", limite)
      datos <- datos %>%
        mutate(!!estacion := if_else(.data[[estacion]] > limite, NA_real_, .data[[estacion]]))
    }
  }
  
  # Mostrar cantidad de datos no NA por estación después del filtrado
  datos_no_na <- colSums(!is.na(datos[, nombres_columnas, drop = FALSE]))
  message("Datos no NA por estación después del filtrado: ")
  for (estacion in names(datos_no_na)) {
    message(estacion, ": ", datos_no_na[estacion])
  }
  
  # Limpiar archivos temporales
  unlink(temp_rar)
  unlink(temp_dir, recursive = TRUE)
  
  return(datos)
}