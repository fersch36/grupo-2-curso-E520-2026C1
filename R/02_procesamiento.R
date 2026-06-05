# 02_procesamiento.R
# Limpieza y preparación de los microdatos
# -----------------------------------------

library(eph)
library(tidyverse)
library(janitor)
library(here)
source(here("R", "utils.R"))

datos_crudos <- readRDS(here("data", "raw", "microdatos_individuales.rds"))

# ── Agregar etiquetas oficiales y columna de período ─────────────────────────
datos_limpios <- datos_crudos |>
  imap(function(df, nombre_periodo) {
    df |>
      organize_labels() |>
      clean_names() |>
      mutate(periodo = nombre_periodo)
  })

# ── Concatenar todos los períodos ────────────────────────────────────────────
panel <- bind_rows(datos_limpios) |>
  mutate(
    periodo = factor(periodo, levels = names(datos_crudos)),
    condicion = case_when(
      estado == 1 ~ "Ocupado",
      estado == 2 ~ "Desocupado",
      estado == 3 ~ "Inactivo",
      TRUE        ~ NA_character_
    ),
    pea      = estado %in% c(1, 2),
    informal = (cat_ocup == 3 & pp07h == 2)
  )

saveRDS(panel, here("data", "processed", "panel_limpio.rds"))
message("Panel procesado guardado en data/processed/")
