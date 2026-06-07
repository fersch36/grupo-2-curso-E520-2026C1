# 04_distribucion_ingreso.R
# Indicadores de distribución del ingreso por período
# -----------------------------------------------------

library(tidyverse)
library(here)
source(here("R", "utils.R"))

panel <- readRDS(here("data", "processed", "panel_limpio.rds"))

# ── Filtro: ocupados con ingreso declarado ───────────────────────────────────
ocupados <- panel |>
  filter(condicion == "Ocupado", !is.na(p21), p21 > 0)

# ── Coeficiente de Gini ───────────────────────────────────────────────────────
gini_pond <- function(ingreso, peso) {
  ord  <- order(ingreso)
  x    <- ingreso[ord]
  w    <- peso[ord]
  N    <- sum(w)
  cum  <- cumsum(w) / N
  lc   <- cumsum(x * w) / sum(x * w)
  # Aproximación trapezoidal
  1 - 2 * sum(diff(cum) * (lc[-length(lc)] + lc[-1]) / 2)
}

gini_serie <- ocupados |>
  group_by(periodo) |>
  summarise(gini = gini_pond(p21, pondera), .groups = "drop")

# ── Deciles de ingreso ────────────────────────────────────────────────────────
deciles_serie <- ocupados |>
  group_by(periodo) |>
  reframe(
    decil   = 1:10,
    ingreso = Hmisc::wtd.quantile(p21, weights = pondera,
                                  probs = seq(0.1, 1, by = 0.1))
  )

# ── Brecha D10/D1 ─────────────────────────────────────────────────────────────
brecha_serie <- deciles_serie |>
  filter(decil %in% c(1, 10)) |>
  pivot_wider(names_from = decil, values_from = ingreso,
              names_prefix = "d") |>
  mutate(brecha_d10_d1 = d10 / d1)

# ── Consolidar ───────────────────────────────────────────────────────────────
indicadores_ingreso <- gini_serie |>
  left_join(brecha_serie |> select(periodo, brecha_d10_d1), by = "periodo")

saveRDS(deciles_serie,        here("data", "processed", "deciles_serie.rds"))
saveRDS(indicadores_ingreso,  here("data", "processed", "indicadores_ingreso.rds"))
guardar_tabla(indicadores_ingreso, "indicadores_ingreso")
message("Indicadores de distribución del ingreso listos.")
