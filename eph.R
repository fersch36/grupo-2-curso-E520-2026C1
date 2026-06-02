
# Librerías ---------------------------------------------------------------

#install.packages("eph")
# Cargo la libreria
library(eph)

base_individual <- get_microdata(
  year = 2018:2019, # especifco el año
  trimester = 1, # el trimestre
  type = "individual", # y el tipo de base
  vars = c("PONDERA", "ESTADO", "CAT_OCUP")
) # opcionalmente, puedo especificar las variables que deseo utilizar.

base_individual


#' 
#' @software{carolina_pradier_2023_8352221,
#'   author       = {Carolina Pradier and
#'     Guido Weksler and
#'     Pablo Tiscornia and
#'     Natsumi Shokida and
#'     Germán Rosati and
#'     Diego Kozlowski},
#'   title        = {ropensci/eph V1.0.0},
#'   month        = sep,
#'   year         = 2023,
#'   publisher    = {Zenodo},
#'   version      = {1.0.0},
#'   doi          = {10.5281/zenodo.8352221},
#'   url          = {https://doi.org/10.5281/zenodo.8352221}
#' }
