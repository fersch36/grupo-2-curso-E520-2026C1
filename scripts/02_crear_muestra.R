## ============================================================
## CREAR MUESTRA DE EPH PARA ANÁLISIS
## Requiere: base_completa_eph en memoria
## Output:   data/muestra_eph_para_claude.csv
## ============================================================

library(tidyverse)

set.seed(42)

## -------------------------------------------------------
## 1. PERSONAS DE PANEL — aparecen en más de un trimestre
##    (son las que importan para el análisis de traspasos)
## -------------------------------------------------------

personas_panel <- base_completa_eph %>%
  count(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  filter(n >= 2)

## -------------------------------------------------------
## 2. SAMPLEAR ~500 PERSONAS DE PANEL (cadena completa)
## -------------------------------------------------------

ids_panel <- personas_panel %>%
  slice_sample(n = 500)

muestra_panel <- base_completa_eph %>%
  semi_join(ids_panel, by = c("CODUSU", "NRO_HOGAR", "COMPONENTE"))

## -------------------------------------------------------
## 3. AGREGAR ~2000 FILAS ALEATORIAS DEL RESTO
## -------------------------------------------------------

muestra_random <- base_completa_eph %>%
  anti_join(ids_panel, by = c("CODUSU", "NRO_HOGAR", "COMPONENTE")) %>%
  slice_sample(n = 2000)

## -------------------------------------------------------
## 4. UNIR Y GUARDAR
## -------------------------------------------------------

muestra_final <- bind_rows(muestra_panel, muestra_random)

cat("Filas totales:", nrow(muestra_final), "\n")
cat("Personas de panel:", n_distinct(muestra_panel$CODUSU), "\n")
cat("Años cubiertos:", paste(sort(unique(muestra_final$Anio)), collapse = ", "), "\n")
cat("Sectores:", n_distinct(muestra_final$caes_eph_label), "\n")

dir.create("data", showWarnings = FALSE)
write_csv(muestra_final, "data/muestra_eph_para_claude.csv")
message("✅ Guardada en data/muestra_eph_para_claude.csv")
