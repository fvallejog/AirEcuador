#' Extraer y procesar datos ambientales de Quito por estación
#'
#' Esta función descarga, descomprime y procesa datos ambientales de Quito
#' (como PM2.5, CO, NO2, etc.) desde los datos abiertos de la Secretaría de
#' Ambiente de Quito, para una estación específica y un rango de fechas.
#'
#' @param estacion Carácter, el nombre de la estación a extraer (e.g., "Los Chillos", "Guamani", "Tumbaco").
#' @param fecha_inicio Fecha (formato "YYYY-MM-DD") desde la cual extraer datos (por defecto: "2004-01-01").
#' @param fecha_fin Fecha (formato "YYYY-MM-DD") hasta la cual extraer datos (por defecto: último mes disponible).
#' @param year Numérico, año completo para el cual extraer datos (opcional). Si se especifica, sobrescribe fecha_inicio y fecha_fin.
#' @param percentil_limite Numérico, percentil para filtrar valores extremos (por defecto 0.999).
#' @return Un data frame con las concentraciones de todos los contaminantes para la estación especificada,
#' con columnas: date, estacion, y una columna por cada contaminante (CO, NO2, etc.).
#' @examples
#' \dontrun{
#' # Extraer datos de todos los contaminantes para Los Chillos hasta febrero 2025
#' datos <- extraer_Quito_estacion(estacion = "Los Chillos", fecha_fin = "2025-02-28")
#' head(datos)
#' # Extraer datos para un año completo
#' datos <- extraer_Quito_estacion(estacion = "Los Chillos", year = 2024)
#' head(datos)
#' }
#' @import httr
#' @import dplyr
#' @import lhmetools
#' @import openxlsx
#' @import lubridate
#' @export
extraer_Quito_estacion <- function(estacion = "Los Chillos", 
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
  contaminantes <- c("CO", "NO2", "O3", "PM10", "PM2.5", "SO2", "LLU", "DIR", "HUM", "IUV", "PRE", "RS", "TMP", "VEL")
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
  enlaces <- setNames(paste0(base_url, contaminantes, ".rar"), contaminantes)
  
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
  
  # Ajustar fecha_fin si es posterior a la fecha actual
  if (fecha_fin > Sys.Date()) {
    message("Nota: La fecha de fin es posterior a la fecha actual (", Sys.Date(), 
            "). Ajustando fecha de fin al último mes disponible: ", 
            format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m-%d"))
    fecha_fin <- as.Date(format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m-%d"))
  }
  
  # Estandarizar el nombre de la estación
  estacion <- trimws(estacion)
  estacion <- toupper(estacion)
  estacion <- gsub("[^A-Z0-9]", "_", estacion)
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
  estacion_estandar <- estacion
  for (patron in names(estacion_map)) {
    if (grepl(patron, estacion)) {
      estacion_estandar <- estacion_map[[patron]]
      break
    }
  }
  
  # Lista para almacenar los datos de cada contaminante
  datos_por_contaminante <- list()
  
  # Iterar sobre cada contaminante
  for (contaminante in contaminantes) {
    # Verificar fecha mínima para el contaminante
    fecha_minima <- fechas_minimas[[contaminante]]
    if (fecha_inicio < fecha_minima) {
      message("Nota: Los datos para ", contaminante, " comienzan en ", fecha_minima, 
              ". Ajustando fecha de inicio a ", fecha_minima, ".")
      fecha_inicio_contaminante <- fecha_minima
    } else {
      fecha_inicio_contaminante <- fecha_inicio
    }
    
    message("Descargando datos de ", contaminante, " para ", estacion_estandar, "...")
    
    # Descargar archivo
    enlace <- enlaces[[contaminante]]
    temp_rar <- tempfile(fileext = ".rar")
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
    
    # Crear directorio temporal
    timestamp <- gsub("[^0-9]", "", Sys.time())
    temp_dir <- file.path(tempdir(), paste0("extract_", timestamp))
    dir.create(temp_dir, showWarnings = FALSE)
    
    # Descomprimir
    extracted_files <- tryCatch(
      suppressMessages(lhmetools::unrar(file = temp_rar, dest_dir = temp_dir, overwrite = TRUE, quiet = TRUE)),
      error = function(e) {
        stop("Error al descomprimir con lhmetools::unrar: ", e$message, 
             "\nAsegúrate de tener 7-Zip instalado (puedes instalarlo con installr::install.7zip()).")
      }
    )
    
    # Buscar archivo Excel
    archivo_excel <- file.path(temp_dir, paste0(contaminante, ".xlsx"))
    if (!file.exists(archivo_excel)) {
      archivo_excel <- list.files(temp_dir, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE, recursive = TRUE)[1]
      if (is.na(archivo_excel)) {
        stop("No se encontró Excel (.xlsx o .xls) en: ", enlace, ". Archivos presentes: ", paste(extracted_files, collapse = ", "))
      }
    }
    
    # Leer el archivo Excel
    datos_crudos <- tryCatch(
      suppressMessages(openxlsx::read.xlsx(archivo_excel, sheet = 1, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)),
      error = function(e) {
        stop("Error al leer el archivo Excel: ", e$message)
      }
    )
    
    # Inspeccionar encabezados y unidades
    encabezados_crudos <- datos_crudos[1, ]
    unidades <- datos_crudos[2, ]
    
    # Procesar nombres de estaciones
    nombres_columnas <- as.character(unlist(encabezados_crudos[-1]))
    nombres_columnas <- nombres_columnas[!is.na(nombres_columnas) & nombres_columnas != ""]
    nombres_columnas <- trimws(nombres_columnas)
    nombres_columnas <- toupper(nombres_columnas)
    nombres_columnas <- gsub("[^A-Z0-9]", "_", nombres_columnas)
    
    # Estandarizar nombres de estaciones
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
      message("No se encontraron nombres de estaciones válidos para el contaminante ", contaminante, ". Saltando...")
      unlink(temp_rar)
      unlink(temp_dir, recursive = TRUE)
      next
    }
    
    # Depuración: Mostrar los nombres de las columnas
    message("Nombres de columnas para ", contaminante, ": ", paste(nombres_columnas_estandar, collapse = ", "))
    
    # Verificar si la estación existe
    if (!estacion_estandar %in% nombres_columnas_estandar) {
      message("La estación ", estacion_estandar, " no se encontró para el contaminante ", contaminante, ". Saltando...")
      unlink(temp_rar)
      unlink(temp_dir, recursive = TRUE)
      next
    }
    
    # Leer datos desde la fila 3
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
    
    # Convertir fechas y seleccionar la estación
    datos <- as.data.frame(datos) %>%
      dplyr::mutate(date = as.POSIXct((as.numeric(fecha) - 25569) * 86400, origin = "1970-01-01", tz = "UTC")) %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::select(date, !!estacion_estandar)
    
    # Limpiar el nombre del contaminante para evitar problemas
    contaminante_limpio <- make.names(contaminante)
    
    # Renombrar la columna con el nombre del contaminante
    datos <- datos %>%
      dplyr::rename(!!contaminante_limpio := !!estacion_estandar)
    
    # Depuración: Mostrar los nombres después de renombrar
    message("Nombres de columnas en datos (después de renombrar): ", paste(colnames(datos), collapse = ", "))
    
    # Filtrar por rango de fechas
    datos <- datos %>%
      dplyr::filter(date >= fecha_inicio_contaminante & date <= fecha_fin)
    if (nrow(datos) == 0) {
      message("No hay datos para el rango de fechas especificado (", fecha_inicio_contaminante, 
              " a ", fecha_fin, ") para el contaminante ", contaminante)
      unlink(temp_rar)
      unlink(temp_dir, recursive = TRUE)
      next
    }
    
    # Convertir valores a numérico y manejar valores negativos
    datos <- datos %>%
      dplyr::mutate(!!contaminante_limpio := as.numeric(.data[[contaminante_limpio]])) %>%
      dplyr::mutate(!!contaminante_limpio := if_else(.data[[contaminante_limpio]] < 0, NA_real_, .data[[contaminante_limpio]]))
    
    # Filtrado al percentil
    if (sum(!is.na(datos[[contaminante_limpio]])) > 0) {
      limite <- quantile(datos[[contaminante_limpio]], percentil_limite, na.rm = TRUE)
      datos <- datos %>%
        dplyr::mutate(!!contaminante_limpio := if_else(.data[[contaminante_limpio]] > limite, NA_real_, .data[[contaminante_limpio]]))
    }
    
    # Depuración: Mostrar los nombres después de todas las transformaciones
    message("Nombres de columnas en datos (final): ", paste(colnames(datos), collapse = ", "))
    
    # Almacenar los datos
    datos_por_contaminante[[contaminante]] <- datos
    
    # Limpiar archivos temporales
    unlink(temp_rar)
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Combinar los datos de todos los contaminantes
  if (length(datos_por_contaminante) == 0) {
    stop("No se encontraron datos para la estación ", estacion_estandar, " en el rango de fechas especificado.")
  }
  
  datos_combinados <- datos_por_contaminante[[1]]
  if (length(datos_por_contaminante) > 1) {
    for (i in 2:length(datos_por_contaminante)) {
      # Asegurarse de que el data frame a combinar tenga nombres válidos
      current_data <- datos_por_contaminante[[i]]
      valid_names <- names(current_data)[!is.na(names(current_data)) & names(current_data) != ""]
      current_data <- current_data[, valid_names, drop = FALSE]
      message("Nombres de columnas en current_data (antes de join): ", paste(colnames(current_data), collapse = ", "))
      datos_combinados <- datos_combinados %>%
        dplyr::full_join(current_data, by = "date")
    }
  }
  
  # Asegurarse de que datos_combinados tenga nombres válidos antes de mutate
  valid_names <- names(datos_combinados)[!is.na(names(datos_combinados)) & names(datos_combinados) != ""]
  datos_combinados <- datos_combinados[, valid_names, drop = FALSE]
  message("Nombres de columnas en datos_combinados (antes de mutate final): ", paste(colnames(datos_combinados), collapse = ", "))
  
  # Añadir columna de estación
  datos_combinados <- datos_combinados %>%
    dplyr::mutate(estacion = estacion_estandar) %>%
    dplyr::select(date, estacion, dplyr::everything())
  
  # Mensaje final y summary
  message("Datos listos para la estación ", estacion_estandar, " en el rango ", fecha_inicio, " a ", fecha_fin, ".")
  print(summary(datos_combinados))
  
  return(datos_combinados)
}