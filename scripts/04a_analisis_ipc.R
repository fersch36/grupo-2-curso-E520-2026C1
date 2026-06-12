## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: 04.1_analisis_ipc.R
## Versión alternativa de 04_analisis_cbt.R, usando IPC en vez de CBT
## como deflactor del ingreso.
##
## Input:  data/base_completa_eph_etiquetada.rds
##         data/serie_ipc_divisiones.csv (IPC Nacional - Nivel general,
##                base diciembre 2016 = 100)
## Output: data/processed/panel_ipc.rds
##         data/diagnostico_ipc.txt
##         plots/04.1_analisis_ipc/
## ============================================================

library(tidyverse)
library(scales)
library(here)
library(glue)
library(lubridate)

source(here("scripts", "utils.R"))
getwd()
options(dplyr.summarise.inform = FALSE)

# -------------------------------------------------------
# 0. CONFIGURACIÓN
# -------------------------------------------------------

RUTA_BASE <- here("data", "base_completa_eph_etiquetada.rds")
RUTA_IPC  <- here("data", "serie_ipc_divisiones.csv")

GUARDAR_PLOTS <- TRUE
CARPETA_PLOTS <- here("plots", "04.1_analisis_ipc")
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
# 1. SERIE DE IPC TRIMESTRAL (NACIONAL, NIVEL GENERAL)
# -------------------------------------------------------
# Fuente: serie_ipc_divisiones.csv (INDEC), Codigo == "0" (Nivel general),
# Region == "Nacional", base diciembre 2016 = 100.
#
# La serie nacional post-reforma arranca en 201612. Esto NO es la serie
# 2007-2016 cuestionada (esa fue discontinuada) — es la serie vigente desde
# la reforma metodológica de fines de 2016, por lo que no aplica la objeción
# que originalmente descartó al IPC para este proyecto.
#
# Para cubrir 2T2016 (abr-jun 2016) y parte de 3T/4T2016, que quedan antes
# de la base, se extiende la serie hacia atrás (abr-nov 2016) con variaciones
# mensuales tomadas de normalizada_IPC_y_a_red_completa.R.
#
# ⚠️ PENDIENTE DE DOCUMENTAR: el origen de estas 8 variaciones mensuales
# (¿IPC-CABA? ¿qué fuente?) no está registrado en decisiones_metodologicas.md.
# Antes de usar este script para la presentación final, agregar la cita de
# fuente correspondiente en esa sección.

cat(">>> Construyendo serie de IPC trimestral...\n")

ipc_raw <- read_delim(
  RUTA_IPC,
  delim  = ";",
  locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
  col_types = cols(Codigo = col_character(), .default = col_guess())
)

ipc_nacional <- ipc_raw %>%
  filter(Codigo == "0", Region == "Nacional") %>%
  transmute(Periodo = as.integer(Periodo), Indice_IPC)

# Extensión abr-nov 2016 (retropropagación desde dic-2016 = 100)
# vm[i] = variación % del mes i respecto del mes i-1
vm_2016 <- c(3.4, 4.2, 3.1, 2.0, 0.2, 1.1, 2.4, 1.6)  # abr..nov 2016
periodos_2016 <- 201604:201611

indice_2016 <- numeric(length(vm_2016) + 1)
indice_2016[length(indice_2016)] <- 100  # dic-2016 (conocido)
for (i in length(vm_2016):1) {
  indice_2016[i] <- indice_2016[i + 1] / (1 + vm_2016[i] / 100)
}
indice_2016 <- indice_2016[-length(indice_2016)]  # descartar dic (ya está en ipc_nacional)

ipc_extension_2016 <- tibble(
  Periodo    = periodos_2016,
  Indice_IPC = indice_2016
)

ipc_mensual <- bind_rows(ipc_extension_2016, ipc_nacional) %>%
  distinct(Periodo, .keep_all = TRUE) %>%
  arrange(Periodo)

cat(glue("✅ IPC mensual: {nrow(ipc_mensual)} períodos ({min(ipc_mensual$Periodo)}-{max(ipc_mensual$Periodo)})\n"))

# Agregación a trimestres (promedio simple del índice mensual)
ipc_trimestral <- ipc_mensual %>%
  mutate(
    Anio      = Periodo %/% 100L,
    Mes       = Periodo %% 100L,
    Trimestre = ((Mes - 1L) %/% 3L) + 1L
  ) %>%
  group_by(Anio, Trimestre) %>%
  summarise(IPC = mean(Indice_IPC, na.rm = TRUE), .groups = "drop")

cat(glue("✅ IPC trimestral: {nrow(ipc_trimestral)} trimestres\n\n"))


# -------------------------------------------------------
# 2. CARGA DE LA BASE COMPLETA
# -------------------------------------------------------
# Mismas columnas y mismo universo que 04_analisis_cbt.R
# Ver decisiones_metodologicas.md para la justificación de cada variable.

cat(">>> Leyendo base completa EPH...\n")

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
# 3. PREPARACIÓN Y JOIN CON IPC
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
  left_join(ipc_trimestral, by = c("Anio", "Trimestre")) %>%
  mutate(
    # Ingreso expresado en pesos constantes de diciembre 2016
    Ingreso_Real = Ingreso / (IPC / 100)
  )

# Control: trimestres sin IPC
sin_ipc <- df %>% filter(is.na(IPC)) %>% count(Anio, Trimestre)
if (nrow(sin_ipc) > 0) {
  cat("⚠️  Trimestres sin IPC disponible:\n")
  print(sin_ipc)
}

cat(glue(
  "✅ Base lista: {nrow(df)} asalariados ocupados | ",
  "{n_distinct(df$Anio)} años | ",
  "{n_distinct(interaction(df$Anio, df$Trimestre))} trimestres\n\n"
))

# Guardar panel procesado con IPC para no releer el CSV en scripts posteriores
RUTA_PANEL <- here("data", "processed", "panel_ipc.rds")
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
saveRDS(df, RUTA_PANEL)
cat(glue("✅ Panel con IPC guardado en {RUTA_PANEL}\n\n"))


# -------------------------------------------------------
# 4. TABLAS DE DIAGNÓSTICO
# -------------------------------------------------------

RUTA_DIAGNOSTICO <- here("data", "diagnostico_ipc.txt")
options(width = 200)
sink(RUTA_DIAGNOSTICO)

cat("=======================================================\n")
cat("DIAGNÓSTICO — ANÁLISIS EN PESOS CONSTANTES (IPC, base dic-2016)\n")
cat("=======================================================\n")

cat("\n--- Resumen por sector (Ingreso real, $ dic-2016) ---\n")
resumen_sector <- df %>%
  filter(!is.na(Sector), !is.na(Registro), !is.na(Ingreso_Real)) %>%
  group_by(Sector) %>%
  summarise(
    N                = n(),
    Ingreso_Media    = round(weighted.mean(Ingreso_Real, PONDERA, na.rm = TRUE), 0),
    Ingreso_Mediana  = round(median(Ingreso_Real, na.rm = TRUE), 0),
    Pct_NR           = round(mean(Registro == "No Registrado", na.rm = TRUE) * 100, 1),
    Edad_Media       = round(mean(Edad, na.rm = TRUE), 1),
    Pct_Mujer        = round(mean(Genero == "Mujer", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Ingreso_Media))
print(resumen_sector)

cat("\n--- Ingreso real mediano por sector y año ($ dic-2016) ---\n")
print(
  df %>%
    filter(!is.na(Sector), !is.na(Ingreso_Real)) %>%
    group_by(Anio, Sector) %>%
    summarise(Ingreso_Mediana = round(median(Ingreso_Real, na.rm = TRUE), 0), .groups = "drop") %>%
    pivot_wider(names_from = Anio, values_from = Ingreso_Mediana),
  n = Inf
)

cat("\n--- Brecha de género en ingreso real por sector ---\n")
print(
  df %>%
    filter(!is.na(Genero), !is.na(Sector), !is.na(Ingreso_Real)) %>%
    group_by(Sector, Genero) %>%
    summarise(Ingreso_Media = round(weighted.mean(Ingreso_Real, PONDERA, na.rm = TRUE), 0), .groups = "drop") %>%
    pivot_wider(names_from = Genero, values_from = Ingreso_Media) %>%
    mutate(Brecha_Pct = round((Varón - Mujer) / Varón * 100, 1)) %>%
    arrange(desc(Brecha_Pct)),
  n = Inf
)

cat("\n--- Ingreso real por sector y registro laboral ---\n")
print(
  df %>%
    filter(!is.na(Registro), !is.na(Sector), !is.na(Ingreso_Real)) %>%
    group_by(Sector, Registro) %>%
    summarise(Ingreso_Media = round(weighted.mean(Ingreso_Real, PONDERA, na.rm = TRUE), 0), .groups = "drop") %>%
    pivot_wider(names_from = Registro, values_from = Ingreso_Media) %>%
    mutate(Prima_Registro = round(Registrado - `No Registrado`, 0)) %>%
    arrange(desc(Prima_Registro)),
  n = Inf
)

sink()
cat(glue("✅ Diagnóstico guardado en {RUTA_DIAGNOSTICO}\n\n"))


# -------------------------------------------------------
# 5. GRÁFICOS
# -------------------------------------------------------

## G1: Ingreso real medio por sector ----------------------------------------------
g1 <- resumen_sector %>%
  mutate(Sector = fct_reorder(Sector, Ingreso_Media)) %>%
  ggplot(aes(x = Ingreso_Media, y = Sector, fill = Sector)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = label_comma(big.mark = ".")(Ingreso_Media)),
    hjust = -0.15, size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = colores_sectores) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Ingreso real medio por sector",
    subtitle = "Ingreso medio expresado en pesos constantes de dic-2016 — asalariados ocupados 2016–2025",
    caption  = "Fuente: EPH-INDEC. Deflactor: IPC Nacional Nivel General (base dic-2016=100). Promedio ponderado (PONDERA).",
    x = "Ingreso real ($ dic-2016)", y = NULL
  )

guardar(g1, "04.1_01_ingreso_real_por_sector")


## G2: Evolución temporal del ingreso real por sector ------------------------------
g2 <- df %>%
  filter(!is.na(Sector), !is.na(Ingreso_Real)) %>%
  group_by(Anio, Sector) %>%
  summarise(Ingreso_Mediana = median(Ingreso_Real, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Anio, y = Ingreso_Mediana, color = Sector)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colores_sectores) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Evolución del poder adquisitivo real por sector (2016–2025)",
    subtitle = "Mediana del ingreso real ($ dic-2016) por año",
    caption  = "Fuente: EPH-INDEC. Deflactor: IPC Nacional Nivel General (base dic-2016=100).",
    x = NULL, y = "Ingreso real ($ dic-2016)", color = NULL
  ) +
  theme(legend.position = "right")

guardar(g2, "04.1_02_evolucion_ingreso_real_sector")


## G3: Brecha de género en ingreso real por sector ----------------------------------
brecha_sector <- df %>%
  filter(!is.na(Genero), !is.na(Sector), !is.na(Ingreso_Real)) %>%
  group_by(Sector, Genero) %>%
  summarise(Ingreso_Media = weighted.mean(Ingreso_Real, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genero, values_from = Ingreso_Media) %>%
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
    title    = "Brecha salarial de género por sector (ingreso real)",
    subtitle = "% de diferencia en poder adquisitivo real (varón − mujer) / varón",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = NULL, y = NULL
  )

guardar(g3, "04.1_03_brecha_genero_ingreso_real")


## G4: Ingreso real por sector y tramo etario -----------------------------------
g4 <- df %>%
  filter(!is.na(Tramo_Edad), !is.na(Sector), !is.na(Ingreso_Real)) %>%
  group_by(Sector, Tramo_Edad) %>%
  summarise(Ingreso_Media = weighted.mean(Ingreso_Real, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sector = fct_reorder(Sector, Ingreso_Media, mean)) %>%
  ggplot(aes(x = Ingreso_Media, y = Sector, color = Tramo_Edad, shape = Tramo_Edad)) +
  geom_point(size = 3.5, alpha = 0.85, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("16-25" = "#E84855", "26-46" = "#2C3E7A", "47+" = "#1D9E75")) +
  scale_shape_manual(values = c("16-25" = 17, "26-46" = 16, "47+" = 15)) +
  scale_x_continuous(labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Poder adquisitivo real por sector y tramo etario",
    subtitle = "Ingreso real medio ($ dic-2016) — asalariados ocupados 2016–2025",
    caption  = "Fuente: EPH-INDEC.",
    x = "Ingreso real ($ dic-2016)", y = NULL,
    color = "Tramo etario", shape = "Tramo etario"
  )

guardar(g4, "04.1_04_ingreso_real_tramo_etario")


## G5: Prima de registro por sector ----------------------------------------------
prima_registro <- df %>%
  filter(!is.na(Registro), !is.na(Sector), !is.na(Ingreso_Real)) %>%
  group_by(Sector, Registro) %>%
  summarise(Ingreso_Media = weighted.mean(Ingreso_Real, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Registro, values_from = Ingreso_Media) %>%
  filter(!is.na(Registrado), !is.na(`No Registrado`)) %>%
  mutate(
    Prima  = Registrado - `No Registrado`,
    Sector = fct_reorder(Sector, Prima)
  )

g5 <- prima_registro %>%
  ggplot(aes(x = Prima, y = Sector)) +
  geom_col(fill = colores_registro["Registrado"], width = 0.7) +
  geom_text(
    aes(label = label_comma(big.mark = ".")(round(Prima, 0))),
    hjust = -0.15, size = 3.2, color = "grey30"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Prima salarial del registro formal por sector",
    subtitle = "Diferencia en ingreso real ($ dic-2016) entre registrados y no registrados",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "Diferencia en ingreso real ($ dic-2016)", y = NULL
  )

guardar(g5, "04.1_05_prima_registro")


## G6: Evolución del ingreso real por registro (registrado vs no registrado) -------
g6 <- df %>%
  filter(!is.na(Registro), !is.na(Ingreso_Real)) %>%
  group_by(Anio, Registro) %>%
  summarise(Ingreso_Mediana = median(Ingreso_Real, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Anio, y = Ingreso_Mediana, color = Registro)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_registro) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Evolución del poder adquisitivo: registrados vs no registrados",
    subtitle = "Mediana del ingreso real ($ dic-2016) por año",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = NULL, y = "Ingreso real ($ dic-2016)", color = NULL
  )

guardar(g6, "04.1_06_evolucion_registro")


cat("\n✅ Análisis IPC completado.\n")
