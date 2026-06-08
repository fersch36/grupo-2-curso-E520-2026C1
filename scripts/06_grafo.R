## ============================================================
## TRABAJO FINAL - DINÁMICA SALARIAL INTERSECTORIAL (EPH)
## Script: 06_grafo.R
## Input:  data/processed/tabla_traspasos.rds (output de 05_traspasos.R)
## Output: plots/06_grafo/
##         data/processed/aristas_grafo.rds
## ============================================================
## Descripción:
##   Construye un grafo dirigido donde:
##   - Nodos = sectores económicos
##   - Aristas = flujos de traspasos entre sectores
##   - Grosor de arista = volumen de traspasos (N)
##   - Color de arista = Delta CBT medio del flujo
##   - Tamaño de nodo = total de trabajadores que pasaron por ese sector
##
## Se generan tres visualizaciones:
##   G1: Grafo principal (todos los flujos con N >= MIN_FLUJO)
##   G2: Grafo de flujos con ganancia neta (Delta CBT > 0)
##   G3: Grafo de flujos con pérdida neta (Delta CBT <= 0)
## ============================================================

library(tidyverse)
library(ggraph)
library(igraph)
library(here)
library(glue)
library(scales)

source(here("scripts", "utils.R"))

# -------------------------------------------------------
# 0. CONFIGURACIÓN
# -------------------------------------------------------

RUTA_TRASPASOS <- here("data", "processed", "tabla_traspasos.rds")
RUTA_ARISTAS   <- here("data", "processed", "aristas_grafo.rds")
CARPETA_PLOTS  <- here("plots", "06_grafo")

# Umbral mínimo de traspasos para incluir un flujo en el grafo.
# Flujos con N < MIN_FLUJO se omiten para evitar ruido visual.
MIN_FLUJO <- 50

GUARDAR_PLOTS <- TRUE

dir.create(CARPETA_PLOTS, recursive = TRUE, showWarnings = FALSE)

guardar <- function(g, nombre, ancho = 12, alto = 10) {
  print(g)
  if (GUARDAR_PLOTS) {
    ruta <- file.path(CARPETA_PLOTS, paste0(nombre, ".png"))
    ggsave(ruta, plot = g, width = ancho, height = alto, dpi = 150, bg = "white")
    cat(glue("  💾 {nombre}.png\n"))
  }
}

theme_set(tema_eph())


# -------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# -------------------------------------------------------

cat(">>> Leyendo tabla de traspasos...\n")
traspasos <- readRDS(RUTA_TRASPASOS)
cat(glue("✅ Tabla cargada: {nrow(traspasos)} traspasos\n\n"))


# ── 1a. Construir tabla de aristas ────────────────────────────────────────────
# Cada arista resume un flujo origen → destino con:
#   N          = número de traspasos
#   Delta_CBT  = Delta CBT medio ponderado (simple, sin expandir con PONDERA
#                porque el análisis es de individuos en panel, no poblacional)
#   Pct_Mejora = % que mejoró el poder adquisitivo

aristas <- traspasos %>%
  group_by(Sector_origen, Sector_destino) %>%
  summarise(
    N          = n(),
    Delta_CBT  = mean(Delta_CBT, na.rm = TRUE),
    Pct_Mejora = mean(Mejora == "Mejoró") * 100,
    .groups    = "drop"
  ) %>%
  filter(N >= MIN_FLUJO) %>%
  # Normalizar grosor entre 0.3 y 4 para ggraph
  mutate(
    Grosor = rescale(N, to = c(0.3, 4))
  )

cat(glue("Aristas incluidas (N >= {MIN_FLUJO}): {nrow(aristas)}\n"))

saveRDS(aristas, RUTA_ARISTAS)
cat(glue("✅ Aristas guardadas en {RUTA_ARISTAS}\n\n"))


# ── 1b. Construir tabla de nodos ──────────────────────────────────────────────
# El tamaño del nodo refleja cuántos traspasos involucraron ese sector
# (como origen O como destino).

nodos <- tibble(
  Sector = unique(c(aristas$Sector_origen, aristas$Sector_destino))
) %>%
  left_join(
    traspasos %>%
      count(Sector = Sector_origen, name = "N_salida"),
    by = "Sector"
  ) %>%
  left_join(
    traspasos %>%
      count(Sector = Sector_destino, name = "N_entrada"),
    by = "Sector"
  ) %>%
  replace_na(list(N_salida = 0, N_entrada = 0)) %>%
  mutate(
    N_total = N_salida + N_entrada,
    Color   = colores_sectores[Sector]
  )


# ── 1c. Nombres cortos para el grafo ─────────────────────────────────────────
# Etiquetas más compactas para que no se superpongan en el gráfico

abreviar_sector <- function(x) {
  case_when(
    x == "Otros Servicios / Actividades" ~ "Otros\nServicios",
    x == "Administración Pública"        ~ "Adm.\nPública",
    x == "Educación y Salud"             ~ "Educación\ny Salud",
    x == "Servicio Doméstico"            ~ "Servicio\nDoméstico",
    x == "Transporte y Almacenamiento"   ~ "Transporte y\nAlmacenamiento",
    x == "Minería, Energía y Agro"       ~ "Minería,\nEnergía y Agro",
    x == "Servicios Profesionales e IT"  ~ "Serv. Prof.\ne IT",
    TRUE                                 ~ x
  )
}

nodos <- nodos %>%
  mutate(Label = abreviar_sector(Sector))


# -------------------------------------------------------
# 2. CONSTRUCCIÓN DEL OBJETO igraph
# -------------------------------------------------------

grafo <- graph_from_data_frame(
  d        = aristas %>% select(from = Sector_origen, to = Sector_destino,
                                N, Delta_CBT, Pct_Mejora, Grosor),
  vertices = nodos   %>% select(name = Sector, N_total, Color, Label),
  directed = TRUE
)


# -------------------------------------------------------
# 3. PALETA DE COLOR PARA ARISTAS (Delta CBT)
# -------------------------------------------------------
# Divergente: rojo (pérdida) → blanco (sin cambio) → verde (ganancia)
# Se centra en 0 para que la lectura sea intuitiva.

paleta_delta <- colorRampPalette(c("#E24B4A", "#F5F5F5", "#1D9E75"))(101)

# Función que mapea un vector numérico a colores de la paleta
delta_a_color <- function(valores, rango_simetrico = NULL) {
  if (is.null(rango_simetrico)) {
    lim <- max(abs(valores), na.rm = TRUE)
  } else {
    lim <- rango_simetrico
  }
  idx <- round((valores / lim + 1) / 2 * 100) + 1
  idx <- pmax(1, pmin(101, idx))
  paleta_delta[idx]
}


# -------------------------------------------------------
# 4. GRÁFICO 1: GRAFO COMPLETO
# -------------------------------------------------------

cat(">>> Generando G1: grafo completo...\n")

# Layout manual para reproducibilidad y legibilidad
# Se posicionan los sectores según su peso en el mercado laboral argentino:
# públicos/formales arriba, informales/doméstico abajo, industriales a la izq.
set.seed(42)

g1 <- ggraph(grafo, layout = "stress") +

  # ── Aristas ──────────────────────────────────────────────────────────────────
  geom_edge_arc(
    aes(
      width     = Grosor,
      color     = Delta_CBT,
      alpha     = N
    ),
    arrow         = arrow(length = unit(2.5, "mm"), type = "closed"),
    end_cap       = circle(6, "mm"),
    start_cap     = circle(6, "mm"),
    strength      = 0.25,
    show.legend   = TRUE
  ) +

  # ── Nodos ────────────────────────────────────────────────────────────────────
  geom_node_point(
    aes(size = N_total, color = Color),
    alpha = 0.95
  ) +

  # ── Etiquetas de nodos ───────────────────────────────────────────────────────
  geom_node_label(
    aes(label = Label),
    size          = 2.8,
    fontface      = "bold",
    label.padding = unit(0.15, "lines"),
    label.size    = 0.2,
    fill          = "white",
    alpha         = 0.85,
    repel         = FALSE
  ) +

  # ── Escalas ──────────────────────────────────────────────────────────────────
  scale_edge_color_gradient2(
    low      = "#E24B4A",
    mid      = "#F0EEE9",
    high     = "#1D9E75",
    midpoint = 0,
    name     = "Delta CBT medio",
    guide    = guide_colorbar(
      title.position = "top",
      barwidth       = 8,
      barheight      = 0.5
    )
  ) +
  scale_edge_width(
    range  = c(0.3, 3.5),
    name   = "N traspasos",
    guide  = guide_legend(
      title.position = "top",
      override.aes   = list(alpha = 1)
    )
  ) +
  scale_edge_alpha(range = c(0.4, 1), guide = "none") +
  scale_size(
    range  = c(4, 14),
    name   = "Traspasos\n(total)",
    guide  = guide_legend(title.position = "top")
  ) +
  scale_color_identity() +

  # ── Etiquetas del gráfico ────────────────────────────────────────────────────
  labs(
    title    = "Movilidad intersectorial del trabajo en Argentina (2016–2025)",
    subtitle = glue(
      "Grafo dirigido: flechas = flujos de trabajadores entre sectores (N ≥ {MIN_FLUJO})\n",
      "Color de arista = variación media del poder adquisitivo (Δ CBT) · Grosor = volumen de traspasos"
    ),
    caption  = "Fuente: EPH-INDEC 2016–2025. Elaboración: Grupo 2 — Curso E520."
  ) +

  # ── Tema ─────────────────────────────────────────────────────────────────────
  tema_eph() +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_blank()
  )

guardar(g1, "06_01_grafo_completo", ancho = 14, alto = 11)


# -------------------------------------------------------
# 5. GRÁFICO 2: SOLO FLUJOS CON GANANCIA (Delta CBT > 0)
# -------------------------------------------------------

cat(">>> Generando G2: grafo de flujos con ganancia...\n")

aristas_pos <- aristas %>% filter(Delta_CBT > 0)

grafo_pos <- graph_from_data_frame(
  d        = aristas_pos %>% select(from = Sector_origen, to = Sector_destino,
                                    N, Delta_CBT, Pct_Mejora, Grosor),
  vertices = nodos        %>% select(name = Sector, N_total, Color, Label),
  directed = TRUE
)

g2 <- ggraph(grafo_pos, layout = "stress") +
  geom_edge_arc(
    aes(width = Grosor, color = Delta_CBT, alpha = N),
    arrow       = arrow(length = unit(2.5, "mm"), type = "closed"),
    end_cap     = circle(6, "mm"),
    start_cap   = circle(6, "mm"),
    strength    = 0.25
  ) +
  geom_node_point(aes(size = N_total, color = Color), alpha = 0.95) +
  geom_node_label(
    aes(label = Label),
    size = 2.8, fontface = "bold",
    label.padding = unit(0.15, "lines"), label.size = 0.2,
    fill = "white", alpha = 0.85
  ) +
  scale_edge_color_gradient(
    low  = "#A8DADC",
    high = "#1D9E75",
    name = "Delta CBT medio",
    guide = guide_colorbar(title.position = "top", barwidth = 8, barheight = 0.5)
  ) +
  scale_edge_width(range = c(0.3, 3.5), name = "N traspasos",
                   guide = guide_legend(title.position = "top",
                                        override.aes = list(alpha = 1))) +
  scale_edge_alpha(range = c(0.4, 1), guide = "none") +
  scale_size(range = c(4, 14), name = "Traspasos\n(total)",
             guide = guide_legend(title.position = "top")) +
  scale_color_identity() +
  labs(
    title    = "Flujos con ganancia de poder adquisitivo (Δ CBT > 0)",
    subtitle = glue("Solo flujos con N ≥ {MIN_FLUJO} y Delta CBT positivo"),
    caption  = "Fuente: EPH-INDEC 2016–2025. Elaboración: Grupo 2 — Curso E520."
  ) +
  tema_eph() +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_blank()
  )

guardar(g2, "06_02_grafo_ganancia", ancho = 14, alto = 11)


# -------------------------------------------------------
# 6. GRÁFICO 3: SOLO FLUJOS CON PÉRDIDA (Delta CBT <= 0)
# -------------------------------------------------------

cat(">>> Generando G3: grafo de flujos con pérdida...\n")

aristas_neg <- aristas %>% filter(Delta_CBT <= 0)

grafo_neg <- graph_from_data_frame(
  d        = aristas_neg %>% select(from = Sector_origen, to = Sector_destino,
                                    N, Delta_CBT, Pct_Mejora, Grosor),
  vertices = nodos        %>% select(name = Sector, N_total, Color, Label),
  directed = TRUE
)

g3 <- ggraph(grafo_neg, layout = "stress") +
  geom_edge_arc(
    aes(width = Grosor, color = Delta_CBT, alpha = N),
    arrow       = arrow(length = unit(2.5, "mm"), type = "closed"),
    end_cap     = circle(6, "mm"),
    start_cap   = circle(6, "mm"),
    strength    = 0.25
  ) +
  geom_node_point(aes(size = N_total, color = Color), alpha = 0.95) +
  geom_node_label(
    aes(label = Label),
    size = 2.8, fontface = "bold",
    label.padding = unit(0.15, "lines"), label.size = 0.2,
    fill = "white", alpha = 0.85
  ) +
  scale_edge_color_gradient(
    low  = "#E24B4A",
    high = "#FAD7A0",
    name = "Delta CBT medio",
    guide = guide_colorbar(title.position = "top", barwidth = 8, barheight = 0.5)
  ) +
  scale_edge_width(range = c(0.3, 3.5), name = "N traspasos",
                   guide = guide_legend(title.position = "top",
                                        override.aes = list(alpha = 1))) +
  scale_edge_alpha(range = c(0.4, 1), guide = "none") +
  scale_size(range = c(4, 14), name = "Traspasos\n(total)",
             guide = guide_legend(title.position = "top")) +
  scale_color_identity() +
  labs(
    title    = "Flujos con pérdida de poder adquisitivo (Δ CBT ≤ 0)",
    subtitle = glue("Solo flujos con N ≥ {MIN_FLUJO} y Delta CBT nulo o negativo"),
    caption  = "Fuente: EPH-INDEC 2016–2025. Elaboración: Grupo 2 — Curso E520."
  ) +
  tema_eph() +
  theme(
    plot.background  = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_blank()
  )

guardar(g3, "06_03_grafo_perdida", ancho = 14, alto = 11)


# -------------------------------------------------------
# 7. GRÁFICO 4: HEATMAP DE FLUJOS (complemento tabular)
# -------------------------------------------------------
# El grafo de red puede ser difícil de leer para todos los flujos.
# Este heatmap lo complementa: filas = origen, columnas = destino,
# color = Delta CBT medio.

cat(">>> Generando G4: heatmap de flujos...\n")

# Calcular todos los flujos (sin umbral para el heatmap)
flujos_todos <- traspasos %>%
  group_by(Sector_origen, Sector_destino) %>%
  summarise(
    N         = n(),
    Delta_CBT = mean(Delta_CBT, na.rm = TRUE),
    .groups   = "drop"
  )

# Orden de sectores por Delta CBT medio como destino (de mejor a peor)
orden_destino <- flujos_todos %>%
  group_by(Sector_destino) %>%
  summarise(Delta_CBT_med = mean(Delta_CBT, na.rm = TRUE)) %>%
  arrange(desc(Delta_CBT_med)) %>%
  pull(Sector_destino)

orden_origen <- flujos_todos %>%
  group_by(Sector_origen) %>%
  summarise(Delta_CBT_med = mean(Delta_CBT, na.rm = TRUE)) %>%
  arrange(Delta_CBT_med) %>%
  pull(Sector_origen)

# Etiquetas cortas (sin salto de línea para el heatmap)
abreviar_corto <- function(x) {
  case_when(
    x == "Otros Servicios / Actividades" ~ "Otros Servicios",
    x == "Administración Pública"        ~ "Adm. Pública",
    x == "Educación y Salud"             ~ "Educ. y Salud",
    x == "Servicio Doméstico"            ~ "Serv. Doméstico",
    x == "Transporte y Almacenamiento"   ~ "Transporte",
    x == "Minería, Energía y Agro"       ~ "Minería/Energía/Agro",
    x == "Servicios Profesionales e IT"  ~ "Serv. Prof. e IT",
    TRUE                                 ~ x
  )
}

g4 <- flujos_todos %>%
  filter(N >= 30) %>%                     # umbral más bajo para ver más flujos
  mutate(
    Origen  = factor(abreviar_corto(Sector_origen),
                     levels = abreviar_corto(orden_origen)),
    Destino = factor(abreviar_corto(Sector_destino),
                     levels = abreviar_corto(orden_destino))
  ) %>%
  ggplot(aes(x = Destino, y = Origen, fill = Delta_CBT)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(
      label = if_else(N >= MIN_FLUJO, as.character(N), ""),
      color = abs(Delta_CBT) > 0.3
    ),
    size = 3
  ) +
  scale_fill_gradient2(
    low      = "#E24B4A",
    mid      = "#F5F5F5",
    high     = "#1D9E75",
    midpoint = 0,
    name     = "Δ CBT medio",
    guide    = guide_colorbar(
      title.position = "top",
      barwidth       = 10,
      barheight      = 0.6
    )
  ) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey40"),
                     guide = "none") +
  labs(
    title    = "Heatmap de flujos intersectoriales",
    subtitle = glue(
      "Color = Δ CBT medio · Números = N traspasos (solo flujos con N ≥ {MIN_FLUJO})\n",
      "Filas ordenadas por Δ CBT como origen (peor → mejor); columnas por Δ CBT como destino (mejor → peor)"
    ),
    caption  = "Fuente: EPH-INDEC 2016–2025. Se incluyen flujos con N ≥ 30.",
    x = "Sector de destino", y = "Sector de origen"
  ) +
  tema_eph() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom"
  )

guardar(g4, "06_04_heatmap_flujos", ancho = 12, alto = 9)


# -------------------------------------------------------
# 8. NOTA METODOLÓGICA EN CONSOLA
# -------------------------------------------------------

cat("\n")
cat("=======================================================\n")
cat("NOTAS METODOLÓGICAS — 06_grafo.R\n")
cat("=======================================================\n")
cat(glue("Umbral mínimo de flujo aplicado al grafo: N >= {MIN_FLUJO}\n"))
cat(glue("Aristas en el grafo principal: {nrow(aristas)}\n"))
cat(glue("Nodos en el grafo: {nrow(nodos)}\n"))
cat("\n")
cat("Decisiones de diseño:\n")
cat("  · Grosor de arista = N de traspasos (rescalado 0.3–3.5)\n")
cat("  · Color de arista  = Delta CBT medio (divergente rojo-blanco-verde)\n")
cat("  · Tamaño de nodo   = N total de traspasos que involucraron al sector\n")
cat("  · Color de nodo    = paleta de proyecto (colores_sectores en utils.R)\n")
cat("  · Layout           = 'stress' (Fruchterman-Reingold mejorado, seed=42)\n")
cat("\n")
cat("⚠ PENDIENTE: agregar 'Servicios Profesionales e IT' a colores_sectores\n")
cat("   en utils.R para que el nodo tenga color de proyecto (actualmente NA).\n")
cat("   Sugerencia: colores_sectores['Servicios Profesionales e IT'] <- '#F4A261'\n")
cat("\n✅ Script 06_grafo.R completado.\n")
