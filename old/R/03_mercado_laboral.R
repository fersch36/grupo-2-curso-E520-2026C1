# 03_mercado_laboral.R
# Indicadores de mercado laboral por período
# -------------------------------------------

library(tidyverse)
library(here)
source(here("R", "utils.R"))

panel <- readRDS(here("data", "processed", "panel_limpio.rds"))

# ── Función auxiliar: media ponderada ────────────────────────────────────────
media_pond <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

# ── Tasa de desempleo ─────────────────────────────────────────────────────────
tasa_desempleo <- panel |>
  filter(pea) |>
  group_by(periodo) |>
  summarise(
    tasa_desempleo = media_pond(condicion == "Desocupado", pondera),
    .groups = "drop"
  )

# ── Tasa de empleo ────────────────────────────────────────────────────────────
tasa_empleo <- panel |>
  filter(ch06 >= 14) |>   # población en edad de trabajar
  group_by(periodo) |>
  summarise(
    tasa_empleo = media_pond(condicion == "Ocupado", pondera),
    .groups = "drop"
  )

# ── Tasa de informalidad ──────────────────────────────────────────────────────
tasa_informalidad <- panel |>
  filter(condicion == "Ocupado", cat_ocup == 3) |>  # solo asalariados
  group_by(periodo) |>
  summarise(
    tasa_informalidad = media_pond(informal, pondera),
    .groups = "drop"
  )

# ── Consolidar ───────────────────────────────────────────────────────────────
indicadores_ml <- tasa_desempleo |>
  left_join(tasa_empleo,       by = "periodo") |>
  left_join(tasa_informalidad, by = "periodo")

saveRDS(indicadores_ml, here("data", "processed", "indicadores_mercado_laboral.rds"))
guardar_tabla(indicadores_ml, "indicadores_mercado_laboral")
message("Indicadores de mercado laboral listos.")
