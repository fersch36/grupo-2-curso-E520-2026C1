## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: 05_traspasos.R
## Input:  data/processed/panel_cbt.rds (output de 04_analisis_cbt.R)
## Output: data/processed/tabla_traspasos.rds
##         data/diagnostico_traspasos.txt
##         plots/05_traspasos/
## ============================================================

library(tidyverse)
library(scales)
library(here)
library(glue)

source(here("scripts", "utils.R"))

options(dplyr.summarise.inform = FALSE)

# -------------------------------------------------------
# 0. CONFIGURACIÓN
# -------------------------------------------------------

RUTA_PANEL     <- here("data", "processed", "panel_cbt.rds")
RUTA_TRASPASOS <- here("data", "processed", "tabla_traspasos.rds")
RUTA_DIAGN     <- here("data", "diagnostico_traspasos.txt")
CARPETA_PLOTS  <- here("plots", "05_traspasos")

GUARDAR_PLOTS <- TRUE

dir.create(CARPETA_PLOTS, recursive = TRUE, showWarnings = FALSE)

guardar <- function(g, nombre) {
  print(g)
  if (GUARDAR_PLOTS) {
    ruta <- file.path(CARPETA_PLOTS, paste0(nombre, ".png"))
    ggsave(ruta, plot = g, width = 10, height = 6, dpi = 150)
    cat(glue("  💾 {nombre}.png\n"))
  }
}

theme_set(tema_eph())


# -------------------------------------------------------
# 1. CARGA DEL PANEL
# -------------------------------------------------------

cat(">>> Leyendo panel con CBT...\n")
panel <- readRDS(RUTA_PANEL)
cat(glue("✅ Panel cargado: {nrow(panel)} observaciones\n\n"))


# -------------------------------------------------------
# 2. DETECCIÓN DE TRASPASOS
# -------------------------------------------------------
# Un traspaso = misma persona (CODUSU + NRO_HOGAR + COMPONENTE)
# observada en dos trimestres consecutivos en sectores distintos.
# Se usa t_index para alinear períodos: anio * 4 + (trimestre - 1)

cat(">>> Detectando traspasos...\n")

panel_idx <- panel %>%
  mutate(
    t_index = Anio * 4L + (Trimestre - 1L),
    id      = paste(CODUSU, NRO_HOGAR, COMPONENTE, sep = "_")
  )

# Versión desplazada un período para el join
panel_sig <- panel_idx %>%
  mutate(t_index = t_index - 1L)

traspasos <- inner_join(
  panel_idx,
  panel_sig,
  by     = c("id", "t_index"),
  suffix = c("_t", "_t1")
) %>%
  # Validación de identidad: mismo género y edad coherente
  filter(
    Genero_t == Genero_t1,
    (Edad_t1 - Edad_t) %in% c(0L, 1L)
  ) %>%
  # Solo los que cambiaron de sector
  filter(Sector_t != Sector_t1) %>%
  mutate(
    Delta_CBT     = CBT_consumidas_t1 - CBT_consumidas_t,
    Mejora        = if_else(Delta_CBT > 0, "Mejoró", "Empeoró"),
    Formalizacion = (Registro_t == "No Registrado" & Registro_t1 == "Registrado"),
    Anio_traspaso      = Anio_t,
    Trimestre_traspaso = Trimestre_t
  ) %>%
  transmute(
    id,
    Anio_traspaso, Trimestre_traspaso,
    Genero             = Genero_t,
    Tramo_Edad         = Tramo_Edad_t,
    Nivel_Ed           = Nivel_Ed_t,
    Region             = Region_t,
    Registro_t, Registro_t1,
    Sector_origen      = Sector_t,
    Sector_destino     = Sector_t1,
    Ingreso_t, Ingreso_t1,
    CBT_t              = CBT_consumidas_t,
    CBT_t1             = CBT_consumidas_t1,
    Delta_CBT, Mejora, Formalizacion,
    PONDERA            = PONDERA_t
  )

cat(glue("✅ Traspasos detectados: {nrow(traspasos)}\n\n"))

# Guardar
saveRDS(traspasos, RUTA_TRASPASOS)
cat(glue("✅ Tabla guardada en {RUTA_TRASPASOS}\n\n"))


# -------------------------------------------------------
# 3. DIAGNÓSTICO
# -------------------------------------------------------

options(width = 200)
sink(RUTA_DIAGN)

cat("=======================================================\n")
cat("DIAGNÓSTICO — TRASPASOS INTERSECTORIALES\n")
cat("=======================================================\n")

cat(glue("\nTotal de traspasos detectados: {nrow(traspasos)}\n"))
cat(glue("Personas únicas: {n_distinct(traspasos$id)}\n"))

cat("\n--- Traspasos por año ---\n")
print(table(traspasos$Anio_traspaso))

cat("\n--- Mejoró vs Empeoró ---\n")
print(table(traspasos$Mejora))

cat("\n--- Formalizaciones detectadas ---\n")
print(table(traspasos$Formalizacion))

cat("\n--- Delta CBT: resumen estadístico ---\n")
print(summary(traspasos$Delta_CBT))

cat("\n--- Top sectores de origen ---\n")
print(sort(table(traspasos$Sector_origen), decreasing = TRUE))

cat("\n--- Top sectores de destino ---\n")
print(sort(table(traspasos$Sector_destino), decreasing = TRUE))

cat("\n--- Delta CBT medio por sector origen ---\n")
print(
  traspasos %>%
    group_by(Sector_origen) %>%
    summarise(
      N         = n(),
      Delta_CBT = round(mean(Delta_CBT, na.rm = TRUE), 3),
      Pct_Mejora = round(mean(Mejora == "Mejoró") * 100, 1)
    ) %>%
    arrange(desc(Delta_CBT)),
  n = Inf
)

cat("\n--- Delta CBT medio por sector destino ---\n")
print(
  traspasos %>%
    group_by(Sector_destino) %>%
    summarise(
      N          = n(),
      Delta_CBT  = round(mean(Delta_CBT, na.rm = TRUE), 3),
      Pct_Mejora = round(mean(Mejora == "Mejoró") * 100, 1)
    ) %>%
    arrange(desc(Delta_CBT)),
  n = Inf
)

cat("\n--- Top 20 flujos de traspaso ---\n")
print(
  traspasos %>%
    count(Sector_origen, Sector_destino, name = "N") %>%
    arrange(desc(N)) %>%
    head(20),
  n = Inf
)

cat("\n--- Delta CBT por flujo (mínimo 30 casos) ---\n")
print(
  traspasos %>%
    group_by(Sector_origen, Sector_destino) %>%
    summarise(
      N          = n(),
      Delta_CBT  = round(mean(Delta_CBT, na.rm = TRUE), 3),
      Pct_Mejora = round(mean(Mejora == "Mejoró") * 100, 1),
      .groups    = "drop"
    ) %>%
    filter(N >= 30) %>%
    arrange(desc(Delta_CBT)),
  n = Inf
)

cat("\n--- Delta CBT por género ---\n")
print(
  traspasos %>%
    group_by(Genero) %>%
    summarise(
      N          = n(),
      Delta_CBT  = round(mean(Delta_CBT, na.rm = TRUE), 3),
      Pct_Mejora = round(mean(Mejora == "Mejoró") * 100, 1)
    ),
  n = Inf
)

cat("\n--- Delta CBT por tramo etario ---\n")
print(
  traspasos %>%
    group_by(Tramo_Edad) %>%
    summarise(
      N          = n(),
      Delta_CBT  = round(mean(Delta_CBT, na.rm = TRUE), 3),
      Pct_Mejora = round(mean(Mejora == "Mejoró") * 100, 1)
    ),
  n = Inf
)

cat("\n--- Delta CBT por registro de origen ---\n")
print(
  traspasos %>%
    group_by(Registro_t) %>%
    summarise(
      N          = n(),
      Delta_CBT  = round(mean(Delta_CBT, na.rm = TRUE), 3),
      Pct_Mejora = round(mean(Mejora == "Mejoró") * 100, 1)
    ),
  n = Inf
)

cat("\n--- Formalizaciones por sector origen ---\n")
print(
  traspasos %>%
    filter(Formalizacion) %>%
    count(Sector_origen, Sector_destino) %>%
    arrange(desc(n)),
  n = Inf
)

sink()
cat(glue("✅ Diagnóstico guardado en {RUTA_DIAGN}\n\n"))


# -------------------------------------------------------
# 4. GRÁFICOS
# -------------------------------------------------------

## G1: Distribución del Delta CBT -----------------------------------------------
g1 <- traspasos %>%
  ggplot(aes(x = Delta_CBT, fill = Mejora)) +
  geom_histogram(bins = 60, alpha = 0.85) +
  geom_vline(xintercept = 0, linewidth = 0.6, linetype = "dashed", color = "grey30") +
  scale_fill_manual(values = c("Mejoró" = "#1D9E75", "Empeoró" = "#E24B4A")) +
  scale_x_continuous(limits = c(-5, 5)) +
  labs(
    title    = "Distribución del Delta CBT en traspasos intersectoriales",
    subtitle = "Variación en canastas básicas al cambiar de sector",
    caption  = "Fuente: EPH-INDEC 2016–2025. Se excluyen valores extremos (|Delta| > 5 CBT).",
    x = "Delta CBT (canastas ganadas/perdidas)", y = "Frecuencia", fill = NULL
  )

guardar(g1, "05_01_distribucion_delta_cbt")


## G2: Delta CBT medio por sector origen ----------------------------------------
g2 <- traspasos %>%
  group_by(Sector_origen) %>%
  summarise(
    Delta_CBT = mean(Delta_CBT, na.rm = TRUE),
    N         = n()
  ) %>%
  mutate(
    Sector_origen = fct_reorder(Sector_origen, Delta_CBT),
    Color         = if_else(Delta_CBT > 0, "positivo", "negativo")
  ) %>%
  ggplot(aes(x = Delta_CBT, y = Sector_origen, fill = Color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_text(
    aes(
      label = round(Delta_CBT, 2),
      hjust = if_else(Delta_CBT > 0, -0.15, 1.15)
    ),
    size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = c("positivo" = "#1D9E75", "negativo" = "#E24B4A")) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title    = "Delta CBT medio según sector de origen",
    subtitle = "Los que salen de este sector, ¿mejoran o empeoran?",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "Delta CBT medio", y = NULL
  )

guardar(g2, "05_02_delta_cbt_origen")


## G3: Delta CBT medio por sector destino ----------------------------------------
g3 <- traspasos %>%
  group_by(Sector_destino) %>%
  summarise(
    Delta_CBT = mean(Delta_CBT, na.rm = TRUE),
    N         = n()
  ) %>%
  mutate(
    Sector_destino = fct_reorder(Sector_destino, Delta_CBT),
    Color          = if_else(Delta_CBT > 0, "positivo", "negativo")
  ) %>%
  ggplot(aes(x = Delta_CBT, y = Sector_destino, fill = Color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey50") +
  geom_text(
    aes(
      label = round(Delta_CBT, 2),
      hjust = if_else(Delta_CBT > 0, -0.15, 1.15)
    ),
    size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = c("positivo" = "#1D9E75", "negativo" = "#E24B4A")) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title    = "Delta CBT medio según sector de destino",
    subtitle = "Los que llegan a este sector, ¿mejoran o empeoran?",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "Delta CBT medio", y = NULL
  )

guardar(g3, "05_03_delta_cbt_destino")


## G4: % que mejoró por flujo (mínimo 50 casos) ----------------------------------
g4 <- traspasos %>%
  group_by(Sector_origen, Sector_destino) %>%
  summarise(
    N          = n(),
    Pct_Mejora = mean(Mejora == "Mejoró") * 100,
    .groups    = "drop"
  ) %>%
  filter(N >= 50) %>%
  mutate(
    Flujo = paste0(
      str_trunc(Sector_origen,  18, "right"), " → ",
      str_trunc(Sector_destino, 18, "right")
    ),
    Flujo = fct_reorder(Flujo, Pct_Mejora),
    Color = if_else(Pct_Mejora >= 50, "positivo", "negativo")
  ) %>%
  ggplot(aes(x = Pct_Mejora, y = Flujo, fill = Color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 50, linewidth = 0.4, linetype = "dashed", color = "grey50") +
  geom_text(
    aes(label = paste0(round(Pct_Mejora), "%")),
    hjust = -0.15, size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(values = c("positivo" = "#1D9E75", "negativo" = "#E24B4A")) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "% de trabajadores que mejoraron su poder adquisitivo por flujo",
    subtitle = "Solo flujos con ≥50 casos",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "% que mejoró", y = NULL
  )

guardar(g4, "05_04_pct_mejora_por_flujo")


## G5: Delta CBT por género ------------------------------------------------------
g5 <- traspasos %>%
  filter(!is.na(Genero)) %>%
  ggplot(aes(x = Delta_CBT, fill = Genero)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey30") +
  scale_fill_manual(values = colores_genero) +
  scale_x_continuous(limits = c(-5, 5)) +
  labs(
    title    = "Distribución del Delta CBT por género",
    subtitle = "¿Varones y mujeres se benefician igual al cambiar de sector?",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "Delta CBT", y = "Densidad", fill = NULL
  )

guardar(g5, "05_05_delta_cbt_genero")


## G6: Delta CBT por tramo etario ------------------------------------------------
g6 <- traspasos %>%
  filter(!is.na(Tramo_Edad)) %>%
  ggplot(aes(x = Delta_CBT, fill = Tramo_Edad)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey30") +
  scale_fill_manual(values = c("16-25" = "#E84855", "26-46" = "#2C3E7A", "47+" = "#1D9E75")) +
  scale_x_continuous(limits = c(-5, 5)) +
  labs(
    title    = "Distribución del Delta CBT por tramo etario",
    subtitle = "¿La edad influye en el resultado del traspaso?",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = "Delta CBT", y = "Densidad", fill = NULL
  )

guardar(g6, "05_06_delta_cbt_edad")


## G7: Evolución de traspasos por año -------------------------------------------
g7 <- traspasos %>%
  count(Anio_traspaso, Mejora) %>%
  group_by(Anio_traspaso) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = Anio_traspaso, y = Pct, color = Mejora)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Mejoró" = "#1D9E75", "Empeoró" = "#E24B4A")) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Evolución del resultado de los traspasos (2016–2025)",
    subtitle = "% de traspasos que mejoraron vs empeoraron el poder adquisitivo",
    caption  = "Fuente: EPH-INDEC 2016–2025.",
    x = NULL, y = NULL, color = NULL
  )

guardar(g7, "05_07_evolucion_traspasos")


cat("\n✅ Análisis de traspasos completado.\n")
