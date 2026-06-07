## ============================================================
## EXPLORACIÓN DE DATOS RAW - EPH
## Descarga UN trimestre de muestra y lo explora en detalle
## Cambiá ANO_MUESTRA y TRI_MUESTRA para ver otros períodos
## ============================================================

library(tidyverse)
library(eph)

ANO_MUESTRA <- 2023
TRI_MUESTRA <- 2

## -------------------------------------------------------
## 1. DESCARGA
## -------------------------------------------------------

cat("\n>>> Descargando EPH", ANO_MUESTRA, "- T", TRI_MUESTRA, "...\n")

df_raw <- get_microdata(year = ANO_MUESTRA, period = TRI_MUESTRA, type = "individual")
names(df_raw) <- toupper(names(df_raw))

cat("✅ Descarga OK\n")


## -------------------------------------------------------
## 2. ESTRUCTURA GENERAL
## -------------------------------------------------------

cat("\n========== DIMENSIONES ==========\n")
cat("Filas (personas encuestadas):", nrow(df_raw), "\n")
cat("Columnas:", ncol(df_raw), "\n")

cat("\n========== TODAS LAS COLUMNAS Y SUS TIPOS ==========\n")
glimpse(df_raw)

## Abre la tabla completa en el visor de RStudio
View(df_raw)


## -------------------------------------------------------
## 3. LAS VARIABLES QUE USAMOS — valores reales
## -------------------------------------------------------

cat("\n========== ESTADO (ocupado/desocupado/inactivo) ==========\n")
cat("1=Ocupado, 2=Desocupado, 3=Inactivo, 4=Menor de 10\n")
print(table(df_raw$ESTADO, useNA = "always"))

cat("\n========== CATEGORIA OCUPACIONAL ==========\n")
cat("1=Patrón, 2=Cuenta propia, 3=Asalariado, 4=Familiar sin remuneración\n")
col_cat <- if ("CAT_OCUP" %in% names(df_raw)) "CAT_OCUP" else "CATEGORIA"
print(table(df_raw[[col_cat]], useNA = "always"))

cat("\n========== CH04 — GÉNERO ==========\n")
cat("1=Varón, 2=Mujer\n")
print(table(df_raw$CH04, useNA = "always"))

cat("\n========== CH06 — EDAD (resumen) ==========\n")
print(summary(as.numeric(df_raw$CH06)))

cat("\n========== PP07H — DESCUENTO JUBILATORIO (proxy registro) ==========\n")
cat("1=Sí (registrado), 2=No (no registrado)\n")
print(table(df_raw$PP07H, useNA = "always"))

cat("\n========== PP04D_COD — CÓDIGO DE RAMA (CAES Rev.2) ==========\n")
col_rama <- if ("PP04D_COL" %in% names(df_raw)) "PP04D_COL" else "PP04D_COD"
cat("Top 30 códigos más frecuentes:\n")
print(sort(table(df_raw[[col_rama]]), decreasing = TRUE)[1:30])

cat("\n========== NIVEL_ED — NIVEL EDUCATIVO ==========\n")
cat("1=Primaria inc., 2=Primaria comp., 3=Secundaria inc., 4=Secundaria comp.\n")
cat("5=Superior inc., 6=Superior comp., 7=Sin instrucción\n")
print(table(df_raw$NIVEL_ED, useNA = "always"))

cat("\n========== REGION ==========\n")
cat("1=GBA, 40=NOA, 41=NEA, 42=Cuyo, 43=Pampeana, 44=Patagonia\n")
print(table(df_raw$REGION, useNA = "always"))

cat("\n========== P21 — INGRESO OCUPACIÓN PRINCIPAL ==========\n")
ingreso <- as.numeric(df_raw$P21)
print(summary(ingreso[ingreso > 0]))
cat("Ceros:", sum(ingreso == 0, na.rm = TRUE), "\n")
cat("NAs:  ", sum(is.na(ingreso)), "\n")

cat("\n========== PONDERA ==========\n")
print(summary(as.numeric(df_raw$PONDERA)))


## -------------------------------------------------------
## 4. IDENTIFICADORES DE PANEL — claves de matcheo
## -------------------------------------------------------

cat("\n========== IDENTIFICADORES DE PERSONA (para el panel) ==========\n")
cat("CODUSU: identificador de vivienda\n")
cat("NRO_HOGAR: número de hogar dentro de la vivienda\n")
cat("COMPONENTE: número de persona dentro del hogar\n")

cat("\nEjemplo — primeras 10 combinaciones únicas:\n")
df_raw %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  distinct() %>%
  slice_head(n = 10) %>%
  print()

cat("\n¿Hay duplicados en la clave CODUSU+NRO_HOGAR+COMPONENTE?\n")
dupes <- df_raw %>%
  count(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  filter(n > 1)
cat("Duplicados encontrados:", nrow(dupes), "\n")


## -------------------------------------------------------
## 5. SUBCONJUNTO — solo asalariados ocupados (los que usamos)
## -------------------------------------------------------

col_cat <- if ("CAT_OCUP" %in% names(df_raw)) "CAT_OCUP" else "CATEGORIA"

df_asalariados <- df_raw %>%
  mutate(across(any_of(c("ESTADO", col_cat)), as.character)) %>%
  filter(ESTADO == "1", .data[[col_cat]] == "3")

cat("\n========== ASALARIADOS OCUPADOS (ESTADO=1, CATEGORIA=3) ==========\n")
cat("N:", nrow(df_asalariados), "de", nrow(df_raw), "filas totales\n")
cat("Proporción:", round(nrow(df_asalariados)/nrow(df_raw)*100, 1), "%\n")

cat("\nPrimeras 20 filas del subconjunto que usamos:\n")
df_asalariados %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE,
         ANO4, TRIMESTRE,
         CH04, CH06,
         all_of(col_cat),
         PP07H,
         all_of(col_rama),
         NIVEL_ED, REGION,
         P21, PONDERA) %>%
  slice_head(n = 20) %>%
  print(width = Inf)

View(df_asalariados)
