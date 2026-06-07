## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: 3_explorar_muestra.R
## Input:  data/muestra_eph.csv
## Output: gráficos en plots/ + tablas de diagnóstico en consola
## ============================================================

library(tidyverse)
library(scales)
library(here)
library(glue)

source(here("scripts", "utils.R"))

# -------------------------------------------------------
# 0. CONFIGURACIÓN
# -------------------------------------------------------

RUTA_MUESTRA <- here("data", "muestra_eph.csv")

# ¿Guardar los gráficos como PNG en plots/?
# Cambiar a TRUE para activar — se crea la carpeta automáticamente si no existe
GUARDAR_PLOTS <- FALSE
CARPETA_PLOTS <- here("plots")
ANCHO  <- 10    # pulgadas
ALTO   <- 6     # pulgadas
DPI    <- 150

if (GUARDAR_PLOTS) {
  dir.create(CARPETA_PLOTS, recursive = TRUE, showWarnings = FALSE)
  cat(glue("📁 Gráficos se guardarán en: {CARPETA_PLOTS}\n\n"))
}

# Helper: muestra el gráfico y opcionalmente lo guarda
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
# 1. CARGA Y PREPARACIÓN
# -------------------------------------------------------

raw <- read_csv(RUTA_MUESTRA, show_col_types = FALSE)

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
  filter(!is.na(Ingreso), Ingreso > 0)

cat(glue(
  "✅ Muestra lista: {nrow(df)} asalariados ocupados | ",
  "{n_distinct(df$Anio)} años | ",
  "{n_distinct(interaction(df$Anio, df$Trimestre))} trimestres\n\n"
))


# -------------------------------------------------------
# 2. TABLAS DE DIAGNÓSTICO
# -------------------------------------------------------
# Esta sección está pensada para pegar en el chat y analizar.
# Los números son nominales — la transformación a CBT va en 4_analisis_cbt.R

RUTA_DIAGNOSTICO <- here("data", "diagnostico_muestra.txt")
options(width = 200)
sink(RUTA_DIAGNOSTICO)

cat("=======================================================\n")
cat("DIAGNÓSTICO — DISTRIBUCIONES CLAVE\n")
cat("=======================================================\n")

cat("\n--- Filas por año (¿cubre toda la serie?) ---\n")
print(table(df$Anio))

cat("\n--- Distribución sectorial ---\n")
print(sort(table(df$Sector), decreasing = TRUE))

cat("\n--- NAs por variable clave ---\n")
vars_clave <- c("Sector", "Genero", "Registro", "Tramo_Edad", "Nivel_Ed", "Region", "Ingreso")
na_tabla <- df %>%
  summarise(across(all_of(vars_clave), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NAs") %>%
  mutate(Pct_NA = round(NAs / nrow(df) * 100, 1)) %>%
  arrange(desc(NAs))
print(na_tabla)

cat("\n--- Resumen por sector (ingreso nominal) ---\n")
resumen_sector <- df %>%
  filter(!is.na(Sector), !is.na(Registro)) %>%
  group_by(Sector) %>%
  summarise(
    N             = n(),
    Ingreso_Medio = round(weighted.mean(Ingreso, PONDERA, na.rm = TRUE)),
    Ingreso_Median= round(median(Ingreso, na.rm = TRUE)),
    Pct_NR        = round(mean(Registro == "No Registrado", na.rm = TRUE) * 100, 1),
    Edad_Media    = round(mean(Edad, na.rm = TRUE), 1),
    Pct_Mujer     = round(mean(Genero == "Mujer", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Ingreso_Medio))
print(resumen_sector)

cat("\n--- Ingreso por sector y año (mediana nominal) ---\n")
print(
  df %>%
    filter(!is.na(Sector)) %>%
    group_by(Anio, Sector) %>%
    summarise(Ingreso_Median = round(median(Ingreso, na.rm = TRUE)), .groups = "drop") %>%
    pivot_wider(names_from = Anio, values_from = Ingreso_Median),
  n = Inf
)

cat("\n--- Informalidad por sector y género ---\n")
print(
  df %>%
    filter(!is.na(Registro), !is.na(Genero), !is.na(Sector)) %>%
    group_by(Sector, Genero) %>%
    summarise(Pct_NR = round(mean(Registro == "No Registrado") * 100, 1), .groups = "drop") %>%
    pivot_wider(names_from = Genero, values_from = Pct_NR),
  n = Inf
)

cat("\n--- Distribución por región y sector ---\n")
print(
  df %>%
    filter(!is.na(Region), !is.na(Sector)) %>%
    count(Region, Sector) %>%
    pivot_wider(names_from = Sector, values_from = n, values_fill = 0),
  n = Inf
)

cat("\n--- Nivel educativo por sector (%) ---\n")
print(
  df %>%
    filter(!is.na(Nivel_Ed), !is.na(Sector), Nivel_Ed != "Otro / NS") %>%
    count(Sector, Nivel_Ed) %>%
    group_by(Sector) %>%
    mutate(Pct = round(n / sum(n) * 100, 1)) %>%
    select(-n) %>%
    pivot_wider(names_from = Nivel_Ed, values_from = Pct, values_fill = 0),
  n = Inf
)

cat("\n--- Personas con 2+ observaciones (candidatos a traspaso) ---\n")
panel_ids <- df %>%
  count(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  filter(n >= 2)
cat(glue("Personas únicas con 2+ apariciones: {nrow(panel_ids)}\n\n"))

traspasos_muestra <- df %>%
  semi_join(panel_ids, by = c("CODUSU", "NRO_HOGAR", "COMPONENTE")) %>%
  arrange(CODUSU, NRO_HOGAR, COMPONENTE, Anio, Trimestre) %>%
  group_by(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  mutate(Sector_prev = lag(Sector)) %>%
  ungroup() %>%
  filter(!is.na(Sector_prev), Sector != Sector_prev)

cat(glue("Traspasos detectados en la muestra: {nrow(traspasos_muestra)}\n\n"))

cat("--- Top flujos de traspaso ---\n")
print(
  traspasos_muestra %>%
    count(Sector_prev, Sector, name = "N") %>%
    arrange(desc(N)) %>%
    head(15)
)


# -------------------------------------------------------
# 3. GRÁFICOS
# -------------------------------------------------------

## G1: Ingreso medio ponderado por sector ----------------------------------------
g1 <- resumen_sector %>%
  mutate(Sector = fct_reorder(Sector, Ingreso_Medio)) %>%
  ggplot(aes(x = Ingreso_Medio, y = Sector, fill = Sector)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = dollar(Ingreso_Medio, prefix = "$", big.mark = ".", decimal.mark = ",")),
    hjust = -0.1, size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = colores_sectores) +
  scale_x_continuous(
    labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title    = "Ingreso mensual medio por sector (nominal)",
    subtitle = "Asalariados ocupados — promedio ponderado (PONDERA)",
    caption  = "Fuente: muestra EPH 2T2016–4T2025. Valores nominales, no comparables entre años.",
    x = NULL, y = NULL
  )

guardar(g1, "01_ingreso_por_sector")


## G2: Informalidad por sector ---------------------------------------------------
g2 <- resumen_sector %>%
  mutate(
    Sector   = fct_reorder(Sector, Pct_NR),
    Color_NR = if_else(Pct_NR > 40, "alto", if_else(Pct_NR > 20, "medio", "bajo"))
  ) %>%
  ggplot(aes(x = Pct_NR, y = Sector, fill = Color_NR)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(Pct_NR, 1), "%")),
    hjust = -0.15, size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = c("alto" = "#E24B4A", "medio" = "#BA7517", "bajo" = "#1D9E75")) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title    = "Tasa de informalidad por sector",
    subtitle = "% de trabajadores no registrados (sin descuento jubilatorio)",
    caption  = "Fuente: muestra EPH 2T2016–4T2025",
    x = NULL, y = NULL
  )

guardar(g2, "02_informalidad_por_sector")


## G3: Brecha salarial de género por sector --------------------------------------
brecha_sector <- df %>%
  filter(!is.na(Genero), !is.na(Sector)) %>%
  group_by(Sector, Genero) %>%
  summarise(Ingreso_Medio = weighted.mean(Ingreso, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Genero, values_from = Ingreso_Medio) %>%
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
    title    = "Brecha salarial de género por sector",
    subtitle = "% de diferencia en ingreso medio (varón − mujer) / varón",
    caption  = "Fuente: muestra EPH 2T2016–4T2025",
    x = NULL, y = NULL
  )

guardar(g3, "03_brecha_salarial_genero")


## G4: Composición de género por sector (stacked 100%) ---------------------------
g4 <- df %>%
  filter(!is.na(Genero), !is.na(Sector)) %>%
  count(Sector, Genero) %>%
  group_by(Sector) %>%
  mutate(Pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(Sector = fct_reorder(Sector, Pct * (Genero == "Mujer"), sum)) %>%
  ggplot(aes(x = Pct, y = Sector, fill = Genero)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = if_else(Pct > 0.08, paste0(round(Pct * 100), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = colores_genero) +
  scale_x_continuous(labels = label_percent()) +
  labs(
    title    = "Composición de género por sector",
    subtitle = "Proporción de asalariados por sector y género",
    caption  = "Fuente: muestra EPH 2T2016–4T2025",
    x = NULL, y = NULL
  )

guardar(g4, "04_composicion_genero")


## G5: Distribución del ingreso por sector (boxplot) -----------------------------
g5 <- df %>%
  filter(!is.na(Sector)) %>%
  mutate(Sector = fct_reorder(Sector, Ingreso, median)) %>%
  ggplot(aes(x = Ingreso, y = Sector, fill = Sector)) +
  geom_boxplot(
    outlier.size = 0.8, outlier.alpha = 0.4,
    width = 0.6, show.legend = FALSE
  ) +
  scale_fill_manual(values = colores_sectores) +
  scale_x_log10(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  labs(
    title    = "Distribución del ingreso por sector (escala log, nominal)",
    subtitle = "Mediana, rango intercuartil y outliers",
    caption  = "Fuente: muestra EPH 2T2016–4T2025. Valores nominales, no comparables entre años.",
    x = NULL, y = NULL
  )

guardar(g5, "05_distribucion_ingreso_boxplot")


## G6: Informalidad por sector y género ------------------------------------------
g6 <- df %>%
  filter(!is.na(Genero), !is.na(Registro), !is.na(Sector)) %>%
  group_by(Sector, Genero) %>%
  summarise(Pct_NR = mean(Registro == "No Registrado") * 100, .groups = "drop") %>%
  mutate(Sector = fct_reorder(Sector, Pct_NR, mean)) %>%
  ggplot(aes(x = Pct_NR, y = Sector, fill = Genero)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = colores_genero) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Informalidad por sector y género",
    subtitle = "% de trabajadores no registrados",
    caption  = "Fuente: muestra EPH 2T2016–4T2025",
    x = NULL, y = NULL
  )

guardar(g6, "06_informalidad_genero_sector")


## G7: Flujos de traspaso detectados ---------------------------------------------
flujos <- traspasos_muestra %>%
  count(Sector_prev, Sector, name = "Traspasos") %>%
  filter(Traspasos >= 3) %>%
  arrange(desc(Traspasos)) %>%
  mutate(
    Flujo = paste0(
      str_trunc(Sector_prev, 20, "right"),
      " → ",
      str_trunc(Sector,      20, "right")
    ),
    Flujo = fct_reorder(Flujo, Traspasos)
  )

g7 <- flujos %>%
  ggplot(aes(x = Traspasos, y = Flujo)) +
  geom_col(fill = colores_proyecto["primario"], width = 0.7) +
  geom_text(aes(label = Traspasos), hjust = -0.2, size = 3.2, color = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Principales flujos de traspaso sectorial detectados",
    subtitle = glue(
      "{nrow(traspasos_muestra)} traspasos en {n_distinct(traspasos_muestra$CODUSU)} personas ",
      "(muestra EPH — panel rotante)"
    ),
    caption  = "Fuente: muestra EPH 2T2016–4T2025. Solo flujos con ≥3 casos.",
    x = "Número de traspasos", y = NULL
  )

guardar(g7, "07_flujos_traspaso")


## G8: Nivel educativo por sector (heatmap) --------------------------------------
orden_ed <- c(
  "Hasta primaria inc.", "Primaria completa",
  "Secundaria inc.",     "Secundaria completa", "Superior"
)

g8 <- df %>%
  filter(Nivel_Ed %in% orden_ed, !is.na(Sector)) %>%
  count(Sector, Nivel_Ed) %>%
  group_by(Sector) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(Nivel_Ed = factor(Nivel_Ed, levels = orden_ed)) %>%
  ggplot(aes(
    x    = Nivel_Ed,
    y    = fct_reorder(Sector, Pct * (Nivel_Ed == "Superior"), sum),
    fill = Pct
  )) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(Pct), "%")), size = 3, color = "white") +
  scale_fill_gradient(low = "#E6F1FB", high = "#0C447C", name = "% del sector") +
  scale_x_discrete(guide = guide_axis(angle = 25)) +
  labs(
    title    = "Composición educativa por sector",
    subtitle = "% de trabajadores de cada sector según nivel educativo",
    caption  = "Fuente: muestra EPH 2T2016–4T2025",
    x = NULL, y = NULL
  ) +
  theme(legend.position = "right")

guardar(g8, "08_nivel_educativo_heatmap")


## G9: Ingreso por sector, género y tramo etario ---------------------------------
g9 <- df %>%
  filter(!is.na(Genero), !is.na(Tramo_Edad), !is.na(Sector)) %>%
  group_by(Sector, Genero, Tramo_Edad) %>%
  summarise(Ingreso_Medio = weighted.mean(Ingreso, PONDERA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sector = fct_reorder(Sector, Ingreso_Medio, mean)) %>%
  ggplot(aes(x = Ingreso_Medio, y = Sector, color = Genero, shape = Tramo_Edad)) +
  geom_point(size = 3, alpha = 0.85, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = colores_genero) +
  scale_shape_manual(values = c("16-25" = 17, "26-46" = 16, "47+" = 15)) +
  scale_x_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  labs(
    title    = "Ingreso medio por sector, género y tramo etario (nominal)",
    subtitle = "Cada punto = combinación sector × género × tramo",
    caption  = "Fuente: muestra EPH 2T2016–4T2025. Valores nominales, no comparables entre años.",
    x = NULL, y = NULL,
    color = "Género", shape = "Tramo etario"
  )

guardar(g9, "09_ingreso_genero_edad_sector")


# -------------------------------------------------------
# 4. ADVERTENCIA SOBRE "OTROS SERVICIOS"
# -------------------------------------------------------
pct_otros <- mean(df$Sector == "Otros Servicios / Actividades") * 100

if (pct_otros > 20) {
  cat(glue(
    "\n⚠️  ATENCIÓN: {round(pct_otros, 1)}% de los asalariados cayó en ",
    "'Otros Servicios / Actividades'.\n",
    "   Revisar el clasificador sectorial en utils.R.\n\n"
  ))
}

sink()
cat(glue("\n✅ Diagnóstico guardado en {RUTA_DIAGNOSTICO}\n"))
cat("\n✅ Análisis exploratorio completado.\n")
