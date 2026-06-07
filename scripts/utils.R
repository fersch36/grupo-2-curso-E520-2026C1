# utils.R
# Funciones auxiliares compartidas por todos los scripts
# -------------------------------------------------------

library(here)

# ── Períodos a analizar ────────────────────────────────────────────────────────
ANIOS      <- 2016:2025
TRIMESTRES <- 1:4

# ── Etiquetas de período para gráficos ────────────────────────────────────────
etiqueta_periodo <- function(anio, trimestre) {
  glue::glue("{anio}-T{trimestre}")
}

# ── Guardar gráficos ───────────────────────────────────────────────────────────
guardar_grafico <- function(plot, nombre, ancho = 12, alto = 7, dpi = 150) {
  path <- here("outputs", "graficos", paste0(nombre, ".png"))
  ggplot2::ggsave(path, plot = plot, width = ancho, height = alto,
                  dpi = dpi, bg = "white")
  message("Guardado: ", path)
  invisible(path)
}

# ── Guardar tablas ─────────────────────────────────────────────────────────────
guardar_tabla <- function(df, nombre) {
  path <- here("outputs", "tablas", paste0(nombre, ".csv"))
  readr::write_csv(df, path)
  message("Guardado: ", path)
  invisible(path)
}

# ── Clasificador sectorial (CAES Rev.2 — usar pp04b_cod o caes_division_cod) ──
# Recibe el código numérico de división CAES y devuelve el nombre del sector.
# Usar siempre esta función en todos los scripts para garantizar consistencia.
clasificar_sector <- function(cod) {
  c <- suppressWarnings(as.numeric(cod))
  dplyr::case_when(
    c >= 10 & c <= 33                           ~ "Industria",
    c %in% c(40, 41, 42, 43)                   ~ "Construcción",
    c %in% c(45, 46, 47, 48)                   ~ "Comercio",
    c == 97                                     ~ "Servicio Doméstico",
    c %in% c(85, 86)                            ~ "Educación y Salud",
    c == 84                                     ~ "Administración Pública",
    c >= 49 & c <= 53                           ~ "Transporte y Almacenamiento",
    c %in% c(1:9, 35:39)                        ~ "Minería, Energía y Agro",
    c %in% c(62, 63, 64, 65, 69, 70, 71, 72, 74) ~ "Servicios Profesionales e IT",
    TRUE                                        ~ "Otros Servicios / Actividades"
  )
}

# ── Paleta de colores ─────────────────────────────────────────────────────────

# Colores generales del proyecto
colores_proyecto <- c(
  primario   = "#2C3E7A",
  secundario = "#E84855",
  acento     = "#F4A261",
  neutro     = "#6C757D",
  fondo      = "#F8F9FA"
)

# Colores por sector (para gráficos sectoriales)
colores_sectores <- c(
  "Industria"                     = "#378ADD",
  "Construcción"                  = "#EF9F27",
  "Comercio"                      = "#9FE1CB",
  "Servicio Doméstico"            = "#D4537E",
  "Educación y Salud"             = "#1D9E75",
  "Administración Pública"        = "#534AB7",
  "Transporte y Almacenamiento"   = "#73726c",
  "Minería, Energía y Agro"       = "#3C3489",
  "Otros Servicios / Actividades" = "#B4B2A9"
)

# Colores por género
colores_genero <- c(
  "Varón" = "#378ADD",
  "Mujer" = "#D4537E"
)

# Colores por registro laboral
colores_registro <- c(
  "Registrado"     = "#1D9E75",
  "No Registrado"  = "#E24B4A"
)

# ── Tema base ggplot2 ─────────────────────────────────────────────────────────
tema_eph <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle    = ggplot2::element_text(color = "grey40", size = 11),
      plot.caption     = ggplot2::element_text(color = "grey55", size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(size = 10),
      legend.position  = "bottom"
    )
}
