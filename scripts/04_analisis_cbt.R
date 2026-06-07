## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: 04_analisis_cbt.R
## Input:  data/base_completa_eph_etiquetada.csv
##         data/cbt_historica.rds (se descarga si no existe)
## Output: gráficos en plots/ + tablas en data/diagnostico_cbt.txt
## ============================================================

library(tidyverse)
library(scales)
library(here)
library(glue)
library(eph)
library(lubridate)

source(here("scripts", "utils.R"))

options(dplyr.summarise.inform = FALSE)

# -------------------------------------------------------
# 0. CONFIGURACIÓN
# -------------------------------------------------------

RUTA_BASE <- here("data", "base_completa_eph_etiquetada.rds")
RUTA_CBT  <- here("data", "cbt_historica.rds")

GUARDAR_PLOTS <- TRUE
CARPETA_PLOTS <- here("plots", "04_analisis_cbt")
ANCHO  <- 10
ALTO   <- 6
DPI    <- 150

if (GUARDAR_PLOTS) {
  dir.create(CARPETA_PLOTS, recursive = TRUE, showWarnings = FALSE)
  cat(glue("📁 Gráficos se guardarán en: {CARPETA_PLOTS}\n\n"))
}

guardar <- function(g, nombre) {
  print(g)
  if (GUARDAR_PLOTS) {
    ruta <- file.path(CARPETA_PLOTS, paste0(nombre, ".png"))
    ggsave(ruta, plot = g, width = ANCHO, height = ALTO, dpi = DPI)
    cat(glue("  💾 {nombre}.png\n"))
  }
}

theme_set(tema_eph())


# -------------------------------------------------------
# 1. CBT HISTÓRICA
# -------------------------------------------------------
# Si ya existe en disco la levanta sin conexión a internet.
# Si no existe, la descarga de INDEC y la guarda para la próxima.

if (file.exists(RUTA_CBT)) {
  cat(">>> Levantando CBT desde disco...\n")
  cbt_raw <- readRDS(RUTA_CBT)
} else {
  cat(">>> Descargando CBT histórica desde INDEC...\n")
  cbt_raw <- get_poverty_lines()
  saveRDS(cbt_raw, RUTA_CBT)
  cat(glue("✅ CBT guardada en {RUTA_CBT}\n"))
}

cbt_historica <- cbt_raw %>%
  mutate(
    Anio      = as.integer(year(periodo)),
    Trimestre = as.integer(quarter(periodo))
  ) %>%
  group_by(Anio, Trimestre) %>%
  summarise(CBT = mean(CBT, na.rm = TRUE), .groups = "drop")

cat(glue("✅ CBT lista: {nrow(cbt_historica)} trimestres\n\n"))


# -------------------------------------------------------
# 2. CARGA DE LA BASE COMPLETA
# -------------------------------------------------------
# Se leen solo las columnas necesarias para no cargar los ~2GB completos.
# Ver decisiones_metodologicas.md para la justificación de cada variable.

cat(">>> Leyendo base completa EPH...\n")

# Columnas que efectivamente se usan — el resto se descarta después de leer.
# Ver decisiones_metodologicas.md para la justificación de cada variable.
COLUMNAS <- c(
  "CODUSU", "NRO_HOGAR", "COMPONENTE",   # identificación de persona (panel)
  "Anio", "Trimestre",                    # ubicación temporal
  "ESTADO", "CAT_OCUP",                  # filtro universo
  "caes_division_cod",                   # sector de actividad (CAES Rev.2)
  "P21",                                 # ingreso ocupación principal
  "PP07H",                               # registro laboral (proxy informalidad)
  "CH04",                                # género
  "CH06",                                # edad
  "NIVEL_ED",                            # nivel educativo
  "REGION",                              # región geográfica
  "PONDERA"                              # ponderador
)

raw <- readRDS(RUTA_BASE) %>%
  select(any_of(COLUMNAS))

cat(glue("✅ Base cargada: {nrow(raw)} filas | {ncol(raw)} columnas\n\n"))


# -------------------------------------------------------
# 3. PREPARACIÓN Y JOIN CON CBT
# -------------------------------------------------------

df <- raw %>%
  mutate(across(any_of(c("ESTADO", "CAT_OCUP", "CH04", "PP07H")), as.character)) %>%
  filter(
    str_detect(ESTADO,   regex("Ocupado", ignore_case = TRUE)),
    str_detect(CAT_OCUP, regex("Obrero",  ignore_case = TRUE))
  ) %>%
  mutate(
    Anio      = as.integer(Anio),
    Trimestre = as.integer(Trimestre),

    Sector = clasificar_sector(caes_division_cod),

    Genero = case_when(
      CH04 == "Mujer" ~ "Mujer",
      CH04 == "Varon" ~ "Varón",
      TRUE ~ NA_character_
    ),

    Registro = case_when(
      PP07H == "Si" ~ "Registrado",
      PP07H == "No" ~ "No Registrado",
      TRUE ~ NA_character_
    ),

    Tramo_Edad = case_when(
      as.numeric(CH06) >= 16 & as.numeric(CH06) <= 25 ~ "16-25",
      as.numeric(CH06) >= 26 & as.numeric(CH06) <= 46 ~ "26-46",
      as.numeric(CH06) >= 47                          ~ "47+",
      TRUE ~ NA_character_
    ),

    Nivel_Ed = case_when(
      str_detect(NIVEL_ED, regex("incompleta|Sin instruc", ignore_case = TRUE)) &
        str_detect(NIVEL_ED, regex("rimaria|instruc",      ignore_case = TRUE)) ~ "Hasta primaria inc.",
      str_detect(NIVEL_ED, regex("Primaria completa",      ignore_case = TRUE)) ~ "Primaria completa",
      str_detect(NIVEL_ED, regex("Secundaria incompleta",  ignore_case = TRUE)) ~ "Secundaria inc.",
      str_detect(NIVEL_ED, regex("Secundaria completa",    ignore_case = TRUE)) ~ "Secundaria completa",
      str_detect(NIVEL_ED, regex("Universit|Superior|Terci", ignore_case = TRUE)) ~ "Superior",
      TRUE ~ "Otro / NS"
    ),

    Region = case_when(
      str_detect(REGION, regex("Buenos Aires|GBA|Partidos", ignore_case = TRUE)) ~ "GBA",
      str_detect(REGION, regex("Pampeana",    ignore_case = TRUE)) ~ "Pampeana",
      str_detect(REGION, regex("Patagonia",   ignore_case = TRUE)) ~ "Patagonia",
      str_detect(REGION, regex("Cuyo",        ignore_case = TRUE)) ~ "Cuyo",
      str_detect(REGION, regex("Noroeste|NOA",ignore_case = TRUE)) ~ "NOA",
      str_detect(REGION, regex("Noreste|NEA", ignore_case = TRUE)) ~ "NEA",
      TRUE ~ REGION
    ),

    Ingreso = as.numeric(P21),
    PONDERA = as.numeric(PONDERA),
    Edad    = as.numeric(CH06)
  ) %>%
  filter(!is.na(Ingreso), Ingreso > 0) %>%
  left_join(cbt_historica, by = c("Anio", "Trimestre")) %>%
  mutate(CBT_consumidas = Ingreso / CBT)

# Control: trimestres sin CBT
sin_cbt <- df %>% filter(is.na(CBT)) %>% count(Anio, Trimestre)
if (nrow(sin_cbt) > 0) {
  cat("⚠️  Trimestres sin CBT disponible:\n")
  print(sin_cbt)
}

cat(glue(
  "✅ Base lista: {nrow(df)} asalariados ocupados | ",
  "{n_distinct(df$Anio)} años | ",
  "{n_distinct(interaction(df$Anio, df$Trimestre))} trimestres\n\n"
))

# Guardar panel procesado con CBT para no releer el CSV en scripts posteriores
RUTA_PANEL <- here("data", "processed", "panel_cbt.rds")
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
saveRDS(df, RUTA_PANEL)
cat(glue("✅ Panel con CBT guardado en {RUTA_PANEL}\n\n"))


# -------------------------------------------------------
# 4. TABLAS DE DIAGNÓSTICO
# -------------------------------------------------------

RUTA_DIAGNOSTICO <- here("data", "diagnostico_cbt.txt")
options(width = 200)
sink(RUTA_DIAGNOSTICO)

cat("=======================================================\n")
cat("DIAGNÓSTICO — ANÁLISIS EN CANASTAS BÁSICAS (CBT)\n")
cat("=======================================================\n")

cat("\n--- Resumen por sector (CBT consumidas) ---\n")
resumen_sector <- df %>%
  filter(!is.na(Sector), !is.na(Registro), !is.na(CBT_consumidas)) %>%
  group_by(Sector) %>%
  summarise(
    N              = n(),
    CBT_Media      = round(weighted.mean(CBT_consumidas, PONDERA, na.rm = TRUE), 2),
    CBT_Mediana    = round(median(CBT_consumidas, na.rm = TRUE), 2),
    Pct_NR         = round(mean(Registro == "No Registrado", na.rm = TRUE) * 100, 1),
    Edad_Media     = round(mean(Edad, na.rm = TRUE), 1),
    Pct_Mujer      = round(mean(Genero == "Mujer", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(CBT_Media))
print(resumen_sector)

cat("\n--- CBT mediana por sector y año ---\n")
print(
  df %>%
    filter(!is.na(Sector), !is.na(CBT_consumidas)) %>%
    group_by(Anio, Sector) %>%
    summarise(CBT_Mediana = round(median(CBT_consumidas, na.rm = TRUE), 2), .groups = "drop") %>%
    pivot_wider(names_from = Anio, values_from = CBT_Mediana),
  n = Inf
)

cat("\n--- Brecha de género en CBT por sector ---\n")
print(
  df %>%
    filter(!is.na(Genero), !is.na(Sector), !is.na(CBT_consumidas)) %>%
    group_by(Sector, Genero) %>%
    summarise(CBT_Media = round(weighted.mean(CBT_consumidas, PONDERA, na.rm = TRUE), 2), .groups = "drop") %>%
    pivot_wider(names_from = Genero, values_from = CBT_Media) %>%
    mutate(Brecha_Pct = round((Varón - Mujer) / Varón * 100, 1)) %>%
    arrange(desc(Brecha_Pct)),
  n = Inf
)

cat("\n--- CBT por sector y registro laboral ---\n")
print(
  df %>%
    filter(!is.na(Registro), !is.na(Sector), !is.na(CBT_consumidas)) %>%
    group_by(Sector, Registro) %>%
    summarise(CBT_Media = round(weighted.mean(CBT_consumidas, PONDERA, na.rm = TRUE), 2), .groups = "drop") %>%
    pivot_wider(names_from = Registro, values_from = CBT_Media) %>%
    mutate(Prima_Registro = round(Registrado - `No Registrado`, 2)) %>%
    arrange(desc(Prima_Registro)),
  n = Inf
)

sink()
cat(glue("✅ Diagnóstico guardado en {RUTA_DIAGNOSTICO}\n\n"))


# -------------------------------------------------------
# 5. GRÁFICOS
# -------------------------------------------------------

## G1: CBT media por sector -------------------------------------------------------
g1 <- resumen_sector %>%
  mutate(Sector = fct_reorder(Sector, CBT_Media)) %>%
  ggplot(aes(x = CBT_Media, y = Sector, fill = Sector)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = round(CBT_Media, 1)),
    hjust = -0.15, size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = colores_sectores) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Canastas básicas mensuales por sector",
    subtitle = "Ingreso medio real expresado en CBT — asalariados ocupados 2016–2025",
    caption  = "Fuente: EPH-INDEC. CBT: get_poverty_lines(). Promedio ponderado (PONDERA).",
    x = "Canastas básicas (CBT)", y = NULL
  )

guardar(g1, "04_01_cbt_por_sector")


## G2: Evolución temporal de CBT por sector --------------------------------------
g2 <- df %>%
  filter(!is.na(Sector), !is.na(CBT_consumidas)) %>%
  group_by(Anio, Sector) %>%
  summarise(CBT_Mediana = median(CBT_consumidas, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Anio, y = CBT_Mediana, color = Sector)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colores_sectores) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(
    title    = "Evolución del poder adquisitivo real por sector (2016–2025)",
    subtitle = "Mediana de CBT consumidas por año",
    caption  = "Fuente: EPH-INDEC. CBT: get_poverty_lines().",
    x = NULL, y = "Canastas básicas (CBT)", color = NULL
  ) +
  theme(legend.position = "right")

guardar(g2, "04_02_evolucion_cbt_sector")


## G3: Brecha de género en CBT por sector ----------------------------------------
brecha_sector <- df %>%
  filter(!is.na(Genero), !is.na(Sector), !is.na(CBT_consumidas)) %>%
  group_by(Sector, Genero) %>%
  summarise(CBT_Media = weighted.mean(CBT_consumidas, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genero, values_from = CBT_Media) %>%
  filter(!is.na(Varón), !is.na(Mujer)) %>%
  mutate(
    Brecha_Pct = (Varón - Mujer) / Varón * 100,
    Sector     = fct_reorder(Sector, Brecha_Pct)
  )

g3 <- brecha_sector %>%
  ggplot(aes(x = Brecha_Pct, y = Sector,
             fill = if_else(Brecha_Pct > 0, "Varón gana más", "Mujer gana más"))) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_text(
    aes(
      label = paste0(if_else(Brecha_Pct > 0, "+", ""), round(Brecha_Pct, 1), "%"),
      hjust = if_else(Brecha_Pct > 0, -0.15, 1.15)
    ),
    size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = c("Varón gana más" = colores_genero["Varón"],
                               "Mujer gana más" = colores_genero["Mujer"])) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0.2, 0.2))
  ) +
  labs(
    title    = "Brecha salarial de género por sector (en CBT)",
    subtitle = "% de diferencia en poder adquisitivo real (varón − mujer) / varón",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = NULL, y = NULL
  )

guardar(g3, "04_03_brecha_genero_cbt")


## G4: CBT por sector y tramo etario ---------------------------------------------
g4 <- df %>%
  filter(!is.na(Tramo_Edad), !is.na(Sector), !is.na(CBT_consumidas)) %>%
  group_by(Sector, Tramo_Edad) %>%
  summarise(CBT_Media = weighted.mean(CBT_consumidas, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sector = fct_reorder(Sector, CBT_Media, mean)) %>%
  ggplot(aes(x = CBT_Media, y = Sector, color = Tramo_Edad, shape = Tramo_Edad)) +
  geom_point(size = 3.5, alpha = 0.85, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("16-25" = "#E84855", "26-46" = "#2C3E7A", "47+" = "#1D9E75")) +
  scale_shape_manual(values = c("16-25" = 17, "26-46" = 16, "47+" = 15)) +
  labs(
    title    = "Poder adquisitivo real por sector y tramo etario",
    subtitle = "CBT consumidas medias — asalariados ocupados 2016–2025",
    caption  = "Fuente: EPH-INDEC.",
    x = "Canastas básicas (CBT)", y = NULL,
    color = "Tramo etario", shape = "Tramo etario"
  )

guardar(g4, "04_04_cbt_tramo_etario")


## G5: Prima de registro por sector ----------------------------------------------
# Diferencia en CBT entre registrados y no registrados dentro de cada sector
prima_registro <- df %>%
  filter(!is.na(Registro), !is.na(Sector), !is.na(CBT_consumidas)) %>%
  group_by(Sector, Registro) %>%
  summarise(CBT_Media = weighted.mean(CBT_consumidas, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Registro, values_from = CBT_Media) %>%
  filter(!is.na(Registrado), !is.na(`No Registrado`)) %>%
  mutate(
    Prima = Registrado - `No Registrado`,
    Sector = fct_reorder(Sector, Prima)
  )

g5 <- prima_registro %>%
  ggplot(aes(x = Prima, y = Sector)) +
  geom_col(fill = colores_registro["Registrado"], width = 0.7) +
  geom_text(
    aes(label = round(Prima, 2)),
    hjust = -0.15, size = 3.2, color = "grey30"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Prima salarial del registro formal por sector",
    subtitle = "Diferencia en CBT consumidas entre registrados y no registrados",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "Diferencia en canastas básicas (CBT)", y = NULL
  )

guardar(g5, "04_05_prima_registro")


## G6: Evolución de CBT por registro (registrado vs no registrado) ---------------
g6 <- df %>%
  filter(!is.na(Registro), !is.na(CBT_consumidas)) %>%
  group_by(Anio, Registro) %>%
  summarise(CBT_Mediana = median(CBT_consumidas, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Anio, y = CBT_Mediana, color = Registro)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_registro) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(
    title    = "Evolución del poder adquisitivo: registrados vs no registrados",
    subtitle = "Mediana de CBT consumidas por año",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = NULL, y = "Canastas básicas (CBT)", color = NULL
  )

guardar(g6, "04_06_evolucion_registro")


cat("\n✅ Análisis CBT completado.\n")
