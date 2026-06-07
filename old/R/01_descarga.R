# 01_descarga.R
# Descarga de microdatos EPH via paquete {eph}
# ---------------------------------------------

library(eph)
library(tidyverse)
library(here)
source(here("R", "utils.R"))

# ── Descargar microdatos individuales para todos los períodos ─────────────────
periodos <- expand_grid(anio = ANIOS, trimestre = TRIMESTRES)

datos_crudos <- periodos |>
  pmap(function(anio, trimestre) {
    message(glue::glue("Descargando {anio} T{trimestre}..."))
    tryCatch(
      get_microdata(year = anio, trimester = trimestre, type = "individual"),
      error = function(e) {
        warning(glue::glue("No disponible: {anio} T{trimestre}"))
        NULL
      }
    )
  }) |>
  set_names(map2_chr(periodos$anio, periodos$trimestre, etiqueta_periodo))

# Eliminar períodos no disponibles
datos_crudos <- compact(datos_crudos)

message(glue::glue("{length(datos_crudos)} períodos descargados correctamente."))

# ── Guardar en data/raw/ ──────────────────────────────────────────────────────
saveRDS(datos_crudos, here("data", "raw", "microdatos_individuales.rds"))
message("Microdatos guardados en data/raw/")

