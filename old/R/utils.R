# utils.R
# Funciones auxiliares compartidas por todos los scripts
# -------------------------------------------------------

library(here)

# ── Períodos a analizar ────────────────────────────────────────────────────────
ANIOS      <- 2017:2024
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

# ── Paleta de colores ─────────────────────────────────────────────────────────
colores_proyecto <- c(
  primario   = "#2C3E7A",
  secundario = "#E84855",
  acento     = "#F4A261",
  neutro     = "#6C757D",
  fondo      = "#F8F9FA"
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
