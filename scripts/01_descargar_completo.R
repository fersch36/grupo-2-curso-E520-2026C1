## ============================================================
## DESCARGA COMPLETA EPH — T1 2016 hasta último disponible
## Todas las columnas + etiquetas de valor + CAES
##
## Estructura:
##   1. Librerías
##   2. Carpetas y rutas
##   3. Diccionarios
##   4. Descarga de microdatos (un .rds por trimestre)
##   5. Unión y etiquetado
##   6. Control de calidad
##   7. Guardado final
## ============================================================


## ============================================================
## 1. LIBRERÍAS
## ============================================================

library(tidyverse)
library(eph)
library(haven)


## ============================================================
## 2. CARPETAS Y RUTAS
## ============================================================

dir.create("data",                    showWarnings = FALSE)
dir.create("data/diccionarios_eph",   showWarnings = FALSE)
dir.create("data/microdatos_raw",     showWarnings = FALSE)


## ============================================================
## 3. DICCIONARIOS — se guardan una sola vez
## ============================================================

diccionarios <- list(
  caes                    = eph::caes,
  CNO                     = eph::CNO,
  diccionario_aglomerados = eph::diccionario_aglomerados,
  diccionario_regiones    = eph::diccionario_regiones,
  canastas_reg_example    = eph::canastas_reg_example,
  centroides_aglomerados  = eph::centroides_aglomerados
)

for (nombre in names(diccionarios)) {
  ruta <- file.path("data/diccionarios_eph", paste0(nombre, ".csv"))
  if (file.exists(ruta)) {
    message(paste("⏭️  Diccionario ya existe:", ruta))
  } else {
    write_csv(diccionarios[[nombre]], ruta)
    message(paste("✅ Diccionario guardado:", ruta))
  }
}


## ============================================================
## 4. DESCARGA DE MICRODATOS — un .rds por trimestre
##    Si el archivo ya existe en data/microdatos_raw/, no lo
##    vuelve a descargar. Esto permite cortar y retomar.
##    Si falla por límite de conexiones, limpia y reintenta.
## ============================================================

closeAllConnections()

cronograma <- expand.grid(anio = 2016:2025, trimestre = 1:4) %>%
  arrange(anio, trimestre)

for (i in seq_len(nrow(cronograma))) {
  
  ano <- cronograma$anio[i]
  tri <- cronograma$trimestre[i]
  archivo <- file.path("data/microdatos_raw", paste0(ano, "_T", tri, ".rds"))
  
  if (file.exists(archivo)) {
    message(paste("⏭️  Ya descargado:", ano, "- T", tri))
    next
  }
  
  message(paste(">>> Descargando:", ano, "- T", tri))
  
  exito <- FALSE
  for (intento in 1:3) {
    
    resultado <- tryCatch({
      
      df <- get_microdata(year = ano, period = tri, type = "individual")
      names(df) <- toupper(names(df))
      
      ## Parches históricos de nombres de columnas
      if ("CAT_OCUP"  %in% names(df) & !("CATEGORIA"  %in% names(df)))
        df <- df %>% mutate(CATEGORIA = CAT_OCUP)
      if ("PP04D_COL" %in% names(df) & !("PP04D_COD" %in% names(df)))
        df <- df %>% mutate(PP04D_COD = PP04D_COL)
      
      ## Guardar raw (sin etiquetas, sin transformar)
      saveRDS(df, archivo)
      closeAllConnections()
      message(paste("✅ Guardado:", archivo))
      TRUE
      
    }, error = function(e) {
      closeAllConnections()
      if (grepl("128 connections", e$message) && intento < 3) {
        message(paste("🔄 Conexiones llenas, reintentando (", intento, "/ 3 ):", ano, "-T", tri))
      } else {
        message(paste("⚠️  Se saltó:", ano, "-T", tri, "| Motivo:", e$message))
      }
      FALSE
    })
    
    if (resultado) { exito <- TRUE; break }
  }
}

cat("\n--- Archivos descargados ---\n")
print(list.files("data/microdatos_raw"))


# df_test <- readRDS("data/microdatos_raw/2016_T1.rds")
# names(df_test)[str_detect(names(df_test), "PP04")]
# names(df_test)
# df_2023 <- readRDS("data/microdatos_raw/2023_T2.rds")
# names(df_2023)[str_detect(names(df_2023), "(?i)pp04|PP04|caes|CAES|rama|RAMA")]
# 
# class(df_test)
# length(df_test)
# nrow(df_test)
# 
# nrow(df_2023)
# "PP04B_COD" %in% names(df_2023)


## ============================================================
## 5. UNIÓN Y ETIQUETADO
## ============================================================

archivos_rds <- list.files("data/microdatos_raw", pattern = "\\.rds$", full.names = TRUE)
archivos_rds <- sort(archivos_rds)

message(paste("\n>>> Uniendo", length(archivos_rds), "trimestres..."))

base_completa_eph <- map_dfr(archivos_rds, function(archivo) {
  
  df <- readRDS(archivo)
  
  ## Saltear archivos vacíos (ej: 2016_T1)
  if (nrow(df) == 0 || ncol(df) == 0) {
    message(paste("⏭️  Vacío, se saltea:", basename(archivo)))
    return(NULL)
  }
  
  nombre <- tools::file_path_sans_ext(basename(archivo))
  partes <- str_match(nombre, "(\\d{4})_T(\\d)")
  ano <- as.integer(partes[2])
  tri <- as.integer(partes[3])
  
  ## Etiquetas categóricas
  df <- organize_labels(df, type = "individual")
  
  ## Etiquetas CAES (solo si el trimestre tiene las columnas)
  if ("PP04B_COD" %in% names(df)) {
    tryCatch(
      { df <- organize_caes(df) },
      error = function(e) message(paste("⚠️  organize_caes falló en", nombre, ":", e$message))
    )
  } else {
    message(paste("ℹ️  Sin PP04B_COD en", nombre, "— se omite organize_caes"))
  }
  
  ## Convertir labelled → texto plano
  df <- df %>%
    mutate(across(everything(), function(x) {
      if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
        as.character(haven::as_factor(x))
      } else {
        x
      }
    }))
  
  ## Columnas de período
  df <- df %>%
    mutate(
      Anio       = ano,
      Trimestre  = tri,
      periodo_id = nombre
    ) %>%
    relocate(periodo_id, Anio, Trimestre, .before = 1)
  
  message(paste("✅ Unido:", nombre, "(", nrow(df), "filas )"))
  return(df)
})

message(paste("✅ Base completa:",
              nrow(base_completa_eph), "filas |",
              ncol(base_completa_eph), "columnas"))

## ============================================================
## 6. CONTROL DE CALIDAD
## ============================================================

cat("\n========== FILAS POR AÑO ==========\n")
print(table(base_completa_eph$Anio))

cat("\n========== ESTADO ==========\n")
print(table(base_completa_eph$ESTADO, useNA = "always"))

cat("\n========== GÉNERO (CH04) ==========\n")
print(table(base_completa_eph$CH04, useNA = "always"))

cat("\n========== NIVEL EDUCATIVO (NIVEL_ED) ==========\n")
print(table(base_completa_eph$NIVEL_ED, useNA = "always"))

cat("\n========== REGIÓN ==========\n")
print(table(base_completa_eph$REGION, useNA = "always"))

cat("\n========== SECTOR (caes_eph_label) ==========\n")
print(table(base_completa_eph$caes_eph_label, useNA = "always"))

cat("\n========== REGISTRO (PP07H) ==========\n")
print(table(base_completa_eph$PP07H, useNA = "always"))

## Abrir en el visor de RStudio
View(base_completa_eph)


## ============================================================
## 7. GUARDADO FINAL
## ============================================================

saveRDS(base_completa_eph, here("data", "base_completa_eph_etiquetada.rds"))
message("✅ Guardada en data/babase_completa_eph_etiquetada.rds")
