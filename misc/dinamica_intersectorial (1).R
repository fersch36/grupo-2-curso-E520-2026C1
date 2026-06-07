

## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: Construcción del panel longitudinal y tabla de traspasos
## Output:  tabla_traspasos (una fila = un traspaso detectado)
## ============================================================

library(tidyverse)
library(eph)
library(lubridate)

## ============================================================
## ETAPA 1: MICRODATOS RAW CON IDENTIFICADORES DE PERSONA
## ============================================================

cronograma <- expand.grid(anio = 2016:2025, trimestre = 1:4) %>%
  filter(!(anio == 2016 & trimestre == 1)) %>%
  arrange(anio, trimestre)

bajar_trimestre_raw <- function(ano, tri) {
  message(paste(">>> Descargando:", ano, "- T", tri))
  tryCatch({
    df <- get_microdata(year = ano, period = tri, type = "individual")
    names(df) <- toupper(names(df))
    
    # Parches históricos de nombres de variables
    if ("CAT_OCUP" %in% names(df) & !("CATEGORIA" %in% names(df)))
      df <- df %>% mutate(CATEGORIA = CAT_OCUP)
    if ("PP04D_COL" %in% names(df) & !("PP04D_COD" %in% names(df)))
      df <- df %>% mutate(PP04D_COD = PP04D_COL)
    
    df %>%
      mutate(across(any_of(c("ESTADO", "CATEGORIA", "CH04", "PP07H")), as.character)) %>%
      filter(ESTADO == "1", CATEGORIA == "3") %>%
      mutate(
        Anio      = as.integer(ANO4),
        Trimestre = as.integer(TRIMESTRE),
        Genero = case_when(
          CH04 == "1" ~ "Varón",
          CH04 == "2" ~ "Mujer",
          TRUE ~ NA_character_
        ),
        Tramo_Edad = case_when(
          as.numeric(CH06) >= 16 & as.numeric(CH06) <= 25 ~ "16-25",
          as.numeric(CH06) >= 26 & as.numeric(CH06) <= 46 ~ "26-46",
          as.numeric(CH06) >= 47                          ~ "47+",
          TRUE ~ NA_character_
        ),
        Registro = case_when(
          PP07H == "1" ~ "Registrado",
          PP07H == "2" ~ "No Registrado",
          TRUE ~ NA_character_
        ),
        Sector_Laboral = case_when(
          str_starts(as.character(as.numeric(PP04D_COD)),
                     "10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32|33") ~ "Industria",
          str_starts(as.character(as.numeric(PP04D_COD)), "41|42|43")       ~ "Construcción",
          str_starts(as.character(as.numeric(PP04D_COD)), "45|46|47")       ~ "Comercio",
          str_starts(as.character(as.numeric(PP04D_COD)), "97")             ~ "Servicio Doméstico",
          str_starts(as.character(as.numeric(PP04D_COD)), "85|86")          ~ "Educación y Salud",
          str_starts(as.character(as.numeric(PP04D_COD)), "84")             ~ "Administración Pública",
          str_starts(as.character(as.numeric(PP04D_COD)), "49|50|51|52|53") ~ "Transporte y Almacenamiento",
          str_starts(as.character(as.numeric(PP04D_COD)), "5|6|7|8|9|35|36|37|38|39") ~ "Minería, Energía y Agro",
          TRUE ~ "Otros Servicios / Actividades"
        ),
        Nivel_Educativo_Agrup = case_when(
          as.numeric(NIVEL_ED) %in% c(1, 7) ~ "Hasta primaria incompleta",
          as.numeric(NIVEL_ED) == 2         ~ "Primaria completa",
          as.numeric(NIVEL_ED) == 3         ~ "Secundaria incompleta",
          as.numeric(NIVEL_ED) == 4         ~ "Secundaria completa",
          as.numeric(NIVEL_ED) %in% c(5, 6) ~ "Superior",
          TRUE ~ NA_character_
        ),
        Region = case_when(
          as.numeric(REGION) == 1  ~ "GBA",
          as.numeric(REGION) == 40 ~ "NOA",
          as.numeric(REGION) == 41 ~ "NEA",
          as.numeric(REGION) == 42 ~ "Cuyo",
          as.numeric(REGION) == 43 ~ "Pampeana",
          as.numeric(REGION) == 44 ~ "Patagonia",
          TRUE ~ NA_character_
        ),
        Ingreso_Mensual = as.numeric(P21),
        PONDERA         = as.numeric(PONDERA)
      ) %>%
      filter(
        !is.na(Genero), !is.na(Tramo_Edad), !is.na(Registro),
        !is.na(Sector_Laboral), !is.na(Nivel_Educativo_Agrup),
        !is.na(Region), !is.na(Ingreso_Mensual), Ingreso_Mensual > 0
      ) %>%
      select(
        CODUSU, NRO_HOGAR, COMPONENTE,
        Anio, Trimestre,
        Genero, Tramo_Edad, Registro,
        Sector_Laboral, Nivel_Educativo_Agrup, Region,
        Ingreso_Mensual, PONDERA
      )
  }, error = function(e) {
    message(paste("⚠️  Se saltó:", ano, "-T", tri, "|", e$message))
    return(NULL)
  })
}

microdatos_raw <- map2(
  cronograma$anio,
  cronograma$trimestre,
  ~bajar_trimestre_raw(.x, .y)
)
names(microdatos_raw) <- paste0(cronograma$anio, "_T", cronograma$trimestre)

message("✅ Microdatos raw descargados.")


## ============================================================
## ETAPA 2: CBT HISTÓRICA (promedio trimestral)
## ============================================================

cbt_historica <- get_poverty_lines() %>%
  mutate(
    Anio      = as.integer(year(periodo)),
    Trimestre = as.integer(quarter(periodo))
  ) %>%
  group_by(Anio, Trimestre) %>%
  summarise(CBT = mean(CBT, na.rm = TRUE), .groups = "drop") %>%
  filter(Anio <= 2025)

message("✅ CBT histórica lista.")


## ============================================================
## ETAPA 3: DETECCIÓN DE TRASPASOS
## ============================================================

pares <- tibble(
  Anio_t = cronograma$anio,
  Tri_t  = cronograma$trimestre
) %>%
  mutate(
    Anio_t1 = if_else(Tri_t == 4L, Anio_t + 1L, Anio_t),
    Tri_t1  = if_else(Tri_t == 4L, 1L, Tri_t + 1L)
  ) %>%
  filter(Anio_t1 <= 2025)

detectar_traspasos <- function(anio_t, tri_t, anio_t1, tri_t1) {
  
  key_t  <- paste0(anio_t,  "_T", tri_t)
  key_t1 <- paste0(anio_t1, "_T", tri_t1)
  
  df_t  <- microdatos_raw[[key_t]]
  df_t1 <- microdatos_raw[[key_t1]]
  
  if (is.null(df_t) | is.null(df_t1)) return(NULL)
  
  df_t %>%
    inner_join(df_t1,
               by     = c("CODUSU", "NRO_HOGAR", "COMPONENTE"),
               suffix = c("_t", "_t1")) %>%
    # Descartar matches imposibles (distinto género = error de match)
    filter(Genero_t == Genero_t1) %>%
    # Solo los que cambiaron de sector
    filter(Sector_Laboral_t != Sector_Laboral_t1) %>%
    # Join con CBT
    left_join(cbt_historica, by = c("Anio_t" = "Anio", "Trimestre_t" = "Trimestre")) %>%
    rename(CBT_t = CBT) %>%
    left_join(cbt_historica, by = c("Anio_t1" = "Anio", "Trimestre_t1" = "Trimestre")) %>%
    rename(CBT_t1 = CBT) %>%
    mutate(
      CBT_consumidas_t  = Ingreso_Mensual_t  / CBT_t,
      CBT_consumidas_t1 = Ingreso_Mensual_t1 / CBT_t1,
      Delta_CBT         = CBT_consumidas_t1 - CBT_consumidas_t,
      Mejora            = if_else(Delta_CBT > 0, "Mejoró", "Empeoró"),
      Anio_traspaso     = anio_t,
      Trimestre_traspaso = tri_t,
      Formalizacion     = (Registro_t == "No Registrado" & Registro_t1 == "Registrado")
    ) %>%
    select(
      CODUSU, NRO_HOGAR, COMPONENTE,
      Anio_traspaso, Trimestre_traspaso,
      Genero                = Genero_t,
      Tramo_Edad            = Tramo_Edad_t,
      Registro_t, Registro_t1,
      Nivel_Educativo_Agrup = Nivel_Educativo_Agrup_t,
      Region                = Region_t,
      Sector_origen         = Sector_Laboral_t,
      Sector_destino        = Sector_Laboral_t1,
      Ingreso_t             = Ingreso_Mensual_t,
      Ingreso_t1            = Ingreso_Mensual_t1,
      CBT_t, CBT_t1,
      CBT_consumidas_t, CBT_consumidas_t1,
      Delta_CBT, Mejora, Formalizacion,
      PONDERA               = PONDERA_t
    )
}

tabla_traspasos <- pmap_dfr(
  list(pares$Anio_t, pares$Tri_t, pares$Anio_t1, pares$Tri_t1),
  ~detectar_traspasos(..1, ..2, ..3, ..4)
)

message(paste("✅ TABLA DE TRASPASOS LISTA. Traspasos detectados:", nrow(tabla_traspasos)))


## ============================================================
## CONTROL DE CALIDAD
## ============================================================

cat("\n--- Traspasos por año ---\n")
print(table(tabla_traspasos$Anio_traspaso))

cat("\n--- Traspasos por sector origen ---\n")
print(sort(table(tabla_traspasos$Sector_origen), decreasing = TRUE))

cat("\n--- Distribución Delta CBT ---\n")
print(summary(tabla_traspasos$Delta_CBT))

cat("\n--- Mejora vs Empeora ---\n")
print(table(tabla_traspasos$Mejora))

cat("\n--- Formalizaciones detectadas ---\n")
print(table(tabla_traspasos$Formalizacion))


