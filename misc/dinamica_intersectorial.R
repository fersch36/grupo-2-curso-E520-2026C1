## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: Construcción de la base unificada
## Dataset: base_completa_eph (2T2016 - 4T2025)
## Variables: Género, Tramo Edad, Registro, Sector Laboral,
##            Nivel Educativo (NIVEL_ED), Región, Ingreso, Pondera
## ============================================================

library(tidyverse)
library(eph)

# -------------------------------------------------------
# GRILLA TEMPORAL: 2T-2016 al 4T-2025
# -------------------------------------------------------
cronograma <- expand.grid(anio = 2016:2025, trimestre = 1:4) %>%
  filter(!(anio == 2016 & trimestre == 1)) %>%
  arrange(anio, trimestre)


# -------------------------------------------------------
# FUNCIÓN DE PROCESAMIENTO POR TRIMESTRE
# -------------------------------------------------------
procesar_trimestre <- function(ano, tri) {
  message(paste(">>> Procesando:", ano, "- T", tri))
  
  tryCatch({
    df <- get_microdata(year = ano, period = tri, type = "individual")
    names(df) <- toupper(names(df))
    
    # --- Parches de nombres históricos ---
    if ("CAT_OCUP" %in% names(df) & !("CATEGORIA" %in% names(df))) {
      df <- df %>% mutate(CATEGORIA = CAT_OCUP)
    }
    if ("PP04D_COL" %in% names(df) & !("PP04D_COD" %in% names(df))) {
      df <- df %>% mutate(PP04D_COD = PP04D_COL)
    }
    
    df_procesada <- df %>%
      mutate(across(any_of(c("ESTADO", "CATEGORIA", "CH04", "PP07H")), as.character)) %>%
      # Ocupados (ESTADO=1) y Asalariados (CATEGORIA=3)
      filter(ESTADO == "1", CATEGORIA == "3") %>%
      mutate(
        
        Anio      = ANO4,
        Trimestre = TRIMESTRE,
        
        # --- GÉNERO ---
        Genero = case_when(
          CH04 == "1" ~ "Varón",
          CH04 == "2" ~ "Mujer",
          TRUE ~ NA_character_
        ),
        
        # --- TRAMO ETARIO ---
        Edad = as.numeric(CH06),
        Tramo_Edad = case_when(
          Edad >= 16 & Edad <= 25 ~ "16-25",
          Edad >= 26 & Edad <= 46 ~ "26-46",
          Edad >= 47              ~ "47+",
          TRUE ~ NA_character_
        ),
        
        # --- REGISTRO LABORAL ---
        Registro = case_when(
          PP07H == "1" ~ "Registrado",
          PP07H == "2" ~ "No Registrado",
          TRUE ~ NA_character_
        ),
        
        # --- SECTOR LABORAL (CAES Rev.2) ---
        Rama_Texto = as.character(as.numeric(PP04D_COD)),
        Sector_Laboral = case_when(
          str_starts(Rama_Texto, "10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33") ~ "Industria",
          str_starts(Rama_Texto, "41|42|43")       ~ "Construcción",
          str_starts(Rama_Texto, "45|46|47")       ~ "Comercio",
          str_starts(Rama_Texto, "97")             ~ "Servicio Doméstico",
          str_starts(Rama_Texto, "85|86")          ~ "Educación y Salud",
          str_starts(Rama_Texto, "84")             ~ "Administración Pública",
          str_starts(Rama_Texto, "49|50|51|52|53") ~ "Transporte y Almacenamiento",
          str_starts(Rama_Texto, "5|6|7|8|9|35|36|37|38|39") ~ "Minería, Energía y Agro",
          TRUE ~ "Otros Servicios / Actividades"
        ),
        
        # --- NIVEL EDUCATIVO ---
        # Se usa NIVEL_ED directamente: es la variable estable en toda la serie 2016-2025.
        # Valores: 1=Primaria incompleta, 2=Primaria completa, 3=Secundaria incompleta,
        #          4=Secundaria completa, 5=Superior universitaria incompleta,
        #          6=Superior universitaria completa, 7=Sin instrucción
        Nivel_Educativo = case_when(
          as.numeric(NIVEL_ED) == 1 ~ "Primaria incompleta",
          as.numeric(NIVEL_ED) == 2 ~ "Primaria completa",
          as.numeric(NIVEL_ED) == 3 ~ "Secundaria incompleta",
          as.numeric(NIVEL_ED) == 4 ~ "Secundaria completa",
          as.numeric(NIVEL_ED) == 5 ~ "Superior incompleta",
          as.numeric(NIVEL_ED) == 6 ~ "Superior completa",
          as.numeric(NIVEL_ED) == 7 ~ "Sin instrucción",
          TRUE ~ NA_character_
        ),
        
        # Versión agrupada (más útil para grafos y análisis)
        Nivel_Educativo_Agrup = case_when(
          as.numeric(NIVEL_ED) %in% c(1, 7) ~ "Hasta primaria incompleta",
          as.numeric(NIVEL_ED) == 2         ~ "Primaria completa",
          as.numeric(NIVEL_ED) == 3         ~ "Secundaria incompleta",
          as.numeric(NIVEL_ED) == 4         ~ "Secundaria completa",
          as.numeric(NIVEL_ED) %in% c(5, 6) ~ "Superior",
          TRUE ~ NA_character_
        ),
        
        # --- REGIÓN ---
        # REGION: 1=GBA, 40=NOA, 41=NEA, 42=Cuyo, 43=Pampeana, 44=Patagonia
        Region = case_when(
          as.numeric(REGION) == 1  ~ "GBA",
          as.numeric(REGION) == 40 ~ "NOA",
          as.numeric(REGION) == 41 ~ "NEA",
          as.numeric(REGION) == 42 ~ "Cuyo",
          as.numeric(REGION) == 43 ~ "Pampeana",
          as.numeric(REGION) == 44 ~ "Patagonia",
          TRUE ~ NA_character_
        ),
        
        # --- INGRESO Y PONDERADOR ---
        Ingreso_Mensual = as.numeric(P21),
        PONDERA         = as.numeric(PONDERA)
        
      ) %>%
      filter(
        !is.na(Genero),
        !is.na(Tramo_Edad),
        !is.na(Registro),
        !is.na(Nivel_Educativo),
        !is.na(Region),
        !is.na(Ingreso_Mensual),
        Ingreso_Mensual > 0
      ) %>%
      select(
        Anio, Trimestre,
        Genero, Tramo_Edad, Registro,
        Sector_Laboral,
        Nivel_Educativo, Nivel_Educativo_Agrup,
        Region,
        Ingreso_Mensual, PONDERA
      )
    
    return(df_procesada)
    
  }, error = function(e) {
    message(paste("⚠️  Se saltó:", ano, "-T", tri, "| Motivo:", e$message))
    return(NULL)
  })
}


# -------------------------------------------------------
# DESCARGA Y PROCESAMIENTO MASIVO (2T2016 - 4T2025)
# -------------------------------------------------------
base_completa_eph <- map2_dfr(
  cronograma$anio,
  cronograma$trimestre,
  ~procesar_trimestre(.x, .y)
)


# -------------------------------------------------------
# CONTROL DE CALIDAD — correr siempre después de la descarga
# -------------------------------------------------------

cat("\n--- Filas por año (debe haber de 2016 a 2025) ---\n")
print(table(base_completa_eph$Anio))

cat("\n--- Distribución Registro ---\n")
print(table(base_completa_eph$Registro, useNA = "always"))

cat("\n--- Distribución Región ---\n")
print(table(base_completa_eph$Region, useNA = "always"))

cat("\n--- Distribución Nivel Educativo Agrupado ---\n")
print(table(base_completa_eph$Nivel_Educativo_Agrup, useNA = "always"))

cat("\n--- Distribución Sector Laboral ---\n")
print(table(base_completa_eph$Sector_Laboral, useNA = "always"))






