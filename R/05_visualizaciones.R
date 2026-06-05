# 05_visualizaciones.R
# Gráficos con ggplot2 y ggraph
# ------------------------------

library(tidyverse)
library(ggraph)
library(igraph)
library(scales)
library(here)
source(here("R", "utils.R"))

ml  <- readRDS(here("data", "processed", "indicadores_mercado_laboral.rds"))
ing <- readRDS(here("data", "processed", "indicadores_ingreso.rds"))
dec <- readRDS(here("data", "processed", "deciles_serie.rds"))

# ── 1. Serie de tasas de mercado laboral ─────────────────────────────────────
p1 <- ml |>
  pivot_longer(cols = starts_with("tasa"), names_to = "indicador",
               values_to = "valor") |>
  mutate(indicador = recode(indicador,
    tasa_desempleo    = "Desempleo",
    tasa_empleo       = "Empleo",
    tasa_informalidad = "Informalidad"
  )) |>
  ggplot(aes(x = periodo, y = valor, color = indicador, group = indicador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = unname(colores_proyecto[c("primario","secundario","acento")])) +
  labs(
    title    = "Indicadores de mercado laboral — EPH INDEC",
    subtitle = "Serie trimestral. Total urbano nacional.",
    x = NULL, y = NULL, color = NULL,
    caption  = "Fuente: EPH-INDEC. Elaboración propia con paquete {eph}."
  ) +
  tema_eph() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_grafico(p1, "01_serie_mercado_laboral")

# ── 2. Evolución del Gini ─────────────────────────────────────────────────────
p2 <- ing |>
  ggplot(aes(x = periodo, y = gini, group = 1)) +
  geom_area(alpha = 0.15, fill = colores_proyecto["primario"]) +
  geom_line(color = colores_proyecto["primario"], linewidth = 1.2) +
  geom_point(color = colores_proyecto["primario"], size = 2.5) +
  scale_y_continuous(limits = c(0.3, 0.6)) +
  labs(
    title   = "Coeficiente de Gini — Ingresos laborales",
    subtitle = "Ocupados con ingreso declarado. Total urbano nacional.",
    x = NULL, y = "Gini",
    caption = "Fuente: EPH-INDEC. Elaboración propia con paquete {eph}."
  ) +
  tema_eph() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_grafico(p2, "02_serie_gini")

# ── 3. Evolución brecha D10/D1 ────────────────────────────────────────────────
p3 <- ing |>
  ggplot(aes(x = periodo, y = brecha_d10_d1, group = 1)) +
  geom_col(fill = colores_proyecto["secundario"], alpha = 0.8) +
  labs(
    title    = "Brecha de ingresos D10 / D1",
    subtitle = "Cuántas veces gana más el decil superior respecto al inferior.",
    x = NULL, y = "Razón D10/D1",
    caption  = "Fuente: EPH-INDEC. Elaboración propia con paquete {eph}."
  ) +
  tema_eph() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_grafico(p3, "03_brecha_d10_d1")

# ── 4. Heatmap de deciles por período ────────────────────────────────────────
p4 <- dec |>
  ggplot(aes(x = periodo, y = factor(decil), fill = ingreso)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#d4e9f7", high = colores_proyecto["primario"],
                      labels = dollar_format(prefix = "$", big.mark = ".")) +
  labs(
    title    = "Ingreso por decil a lo largo del tiempo",
    subtitle = "Ingreso laboral nominal. Ocupados con ingreso declarado.",
    x = NULL, y = "Decil", fill = "Ingreso",
    caption  = "Fuente: EPH-INDEC. Elaboración propia con paquete {eph}."
  ) +
  tema_eph() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_grafico(p4, "04_heatmap_deciles")

message("Todos los gráficos exportados a outputs/graficos/")
