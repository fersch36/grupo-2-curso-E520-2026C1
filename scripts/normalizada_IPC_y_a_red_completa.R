
### Normalización por ipc y extension de la red laboral
### Se busca normalizar por ipc para saber si los empleados a cambiar de sector mejoraron en terminos reales (siempre en contraposición a los que se quedaron)
### IPC: base 100 diciembre 2016, falta agregar de abril a noviembre del 2016


### Librerias 

library(tidyverse)
library(here)
library(ggraph)
library(igraph)
library(ggplot2)
library(lubridate)
library(dplyr)
library(purrr)

### Recostruccion del panel 


panel_crudo <- bind_rows(
  datos_limpios,
  .id = "periodo"
)


### Red sectorial laboral 
### Se busca armar un panel de ocupados, donde se discrimine por sector de actividad del empleador, Caes a 4 digitos para tener la red completa
### Se analiza solo a los ocupados, estado == 1, cat_ocup == 3 (obrero o empleado)



panel_ocupados <- panel_crudo |>
  filter(
    estado == 1 ,
    cat_ocup == 3 
  )

panel_ocupados <- panel_ocupados |>
  mutate(
    caes4 = trimws(pp04b_cod)
  ) |>
  filter(
    !caes4 %in% c("NA", "", "9999"),
    nchar(caes4) == 4
  )

### Hacemos un save

saveRDS(
  panel_crudo,
  "panel_crudo.rds",
  compress = FALSE
)

saveRDS(
  panel_ocupados,
  "panel_ocupados.rds",
  compress = FALSE
)

data.frame(
  objeto = ls(),
  GB = round(
    sapply(ls(), function(x)
      as.numeric(object.size(get(x))) / 1024^3
    ),
    3
  )
) |>
  arrange(desc(GB))

rm(datos_limpios)
gc()

save.image("workspace_red_laboral.RData")

### Proseguimos 
### verificación

nrow(panel_ocupados)

length(unique(panel_ocupados$caes4))

table(panel_ocupados$periodo) |> head()

names(panel_ocupados)[grepl(
  "codusu|componente|hogar|pondera|pp07h|pp07g",
  names(panel_ocupados),
  ignore.case = TRUE
)]

### Creador de un identificador unico de persona 

panel_ocupados <- panel_ocupados |>
  mutate(
    id_persona = paste(
      codusu,
      nro_hogar,
      componente,
      sep = "_"
    )
  )

n_distinct(panel_ocupados$id_persona)


### Convertir periodo a indice numerico

panel_ocupados <- panel_ocupados |>
  mutate(
    periodo_num = match(
      periodo,
      sort(unique(periodo))
    )
  )

unique(panel_ocupados[,c("periodo","periodo_num")]) |>
  arrange(periodo_num)

### otros

table(panel_ocupados$pp07h,
      useNA = "ifany")

table(table(panel_ocupados$id_persona))

panel_ocupados |>
  count(id_persona) |>
  count(n)

panel_ocupados |>
  count(id_persona, periodo_num) |>
  filter(n > 1)

panel_ocupados |>
  count(id_persona) |>
  summarise(
    personas = n(),
    promedio_obs = mean(n),
    max_obs = max(n)
  )

panel_ocupados |>
  count(id_persona) |>
  count(n)

panel_ocupados |>
  count(id_persona) |>
  filter(n >= 5)


### Panel de movilidad

panel_movilidad <- panel_ocupados |>
  arrange(id_persona, periodo_num) |>
  group_by(id_persona) |>
  mutate(
    formal = ifelse(pp07h == 1, 1, 0),
    
    periodo_sig = lead(periodo_num),
    caes4_sig   = lead(caes4),
    formal_sig  = lead(formal)
  ) |>
  ungroup()

panel_movilidad <- panel_movilidad |>
  filter(
    periodo_sig == periodo_num + 1
  )

nrow(panel_movilidad)

mean(panel_movilidad$caes4 != panel_movilidad$caes4_sig)

saveRDS(
  panel_movilidad,
  "panel_movilidad.rds",
  compress = FALSE
)

sum(panel_movilidad$caes4 != panel_movilidad$caes4_sig)
43984 / 209383
### es decir solo el 21% de los traspasos cambian de sector segun el codigo de caes
### es decir 43984 traspasos de sector, sobre un total de 209.383 traspasos entre periodos consecutivos
### Entonces la pregunta toma un camino distinto pero similar ¿Cambiar de actividad económica mejora el salario real respecto de un trabajador similar que permaneció?

### Hacemos un save con los paneles limpios

save.image("workspace_movilidad.RData")

### Siguiente paso, quienes formalizaron y quienes no, siempre en pos de si perciben aporte jubilatorio, pp07h 

panel_movilidad <- panel_movilidad |>
  mutate(
    cambia_sector = caes4 != caes4_sig,
    
    formaliza =
      formal == 0 &
      formal_sig == 1,
    
    informaliza =
      formal == 1 &
      formal_sig == 0
  )

panel_movilidad |>
  summarise(
    tasa_formalizacion = mean(formaliza),
    tasa_informalizacion = mean(informaliza)
  )

### De los traspaso un 3,91% formalizo y un 3,43 informalizo
### en numeros

sum(panel_movilidad$formaliza)
sum(panel_movilidad$informaliza)

### formalizaron 8.186 personas y 7.185 informalizaron, sobre un total de 209.383 traspasos entre periodos consecutivos


### ¿Los trabajadores que cambian de actividad tienen más probabilidad de formalizarse?

panel_movilidad |>
  group_by(cambia_sector) |>
  summarise(
    formalizacion = mean(formaliza),
    informalizacion = mean(informaliza),
    n = n()
  )

### Cambia sector	Formalización	Informalización	   N
### No	           3,30%          	2,83%	       165.399
### Sí	           6,21%	          5,68%	       43.984

6.21 / 3.30
### los trabajadores que cambian de sector tienen 1,88% de formalizarse que quienes permanecen en la misma actividad 

5.68 / 2.83
### los trabajadores que cambian de sector tienen 2,01% de informalizarse que quienes permanecen en la misma actividad

saveRDS(
  panel_movilidad,
  "panel_movilidad.rds",
  compress = FALSE
)

panel_movilidad |>
  count(cambia_sector, formal, formal_sig)

### Sin cambio de sector (165.399 casos)
###   Estado	          Casos	       %
### Informal → Informal	40.568	    24,5%
### Informal → Formal	   5.455	    3,3%
### Formal → Informal	   4.686	    2,8%
### Formal → Formal	   114.690	    69,3%

###   Con cambio de sector (43.984 casos)
###  Estado	             Casos	        %
### Informal → Informal	12.685	    28,8%
### Informal → Formal	   2.731	     6,2%
### Formal → Informal	   2.499	     5,7%
### Formal → Formal	    26.069	    59,3%


###La movilidad sectorial parece actuar como un mecanismo de reasignación laboral:
###  Los trabajadores que permanecen en el mismo sector tienen trayectorias más estables.
###  Los que cambian de actividad económica enfrentan más riesgo, pero también más oportunidades.
###  El cambio sectorial está asociado tanto a ascensos (formalización) como a descensos (informalización).

### Creamos una variable de transición laboral, porque cuando incorporemos salarios reales, el analisis puede ir a comparar:
### Cambio sectorial + Formalización
### vs
### Cambio sectorial + Permanencia formal
### vs
### Cambio sectorial + Informalización
### vs
### Sin cambio sectorial

### Ahora pasamos a ipc

IPC <- read.csv2("serie_ipc_divisiones.csv")

names(IPC)
head(IPC)
str(IPC)


### Nos falta de Abril a noviembre  del 2016, procedemos a deflactar dichos meses 

ipc2016 <- data.frame(
  periodo = c("201604","201605","201606","201607",
              "201608","201609","201610","201611","201612"),
  vm = c(3.4,4.2,3.1,2.0,0.2,1.1,2.4,1.6,NA)
)

ipc2016$indice <- NA
ipc2016$indice[9] <- 100

for(i in 8:1){
  ipc2016$indice[i] <- ipc2016$indice[i+1] /
    (1 + ipc2016$vm[i]/100)
}

ipc2016

names(panel_movilidad)[grepl(
  "p21|p47|ing|sal",
  names(panel_movilidad),
  ignore.case = TRUE
)]

IPC |>
  filter(
    Codigo == "0",
    Periodo %in% c(201612, 201701)
  ) |>
  count(Periodo, Region)


IPC_nacional <- IPC |>
  filter(
    Codigo == "0",
    Region == "Nacional"
  ) |>
  select(
    Periodo,
    Indice_IPC
  )


ipc2016 <- ipc2016 |>
  mutate(
    Periodo = as.integer(periodo),
    Indice_IPC = indice
  ) |>
  select(
    Periodo,
    Indice_IPC
  )

ipc_completo <- bind_rows(
  ipc2016,
  IPC_nacional
) |>
  distinct(Periodo, .keep_all = TRUE) |>
  arrange(Periodo)

saveRDS(
  ipc_completo,
  "ipc_completo.rds",
  compress = FALSE
)

### verificamos

head(ipc_completo, 12)

tail(ipc_completo, 12)

### IPC trimestral

ipc_trimestral <- ipc_completo |>
  mutate(
    anio = substr(Periodo, 1, 4),
    mes  = substr(Periodo, 5, 6),
    
    periodo = case_when(
      mes %in% c("01","02","03") ~ paste0(anio, "_T1"),
      mes %in% c("04","05","06") ~ paste0(anio, "_T2"),
      mes %in% c("07","08","09") ~ paste0(anio, "_T3"),
      mes %in% c("10","11","12") ~ paste0(anio, "_T4")
    )
  ) |>
  group_by(periodo) |>
  summarise(
    ipc_trim = mean(Indice_IPC),
    .groups = "drop"
  )

nrow(ipc_trimestral)

head(ipc_trimestral)

tail(ipc_trimestral)

ipc_trimestral <- ipc_trimestral |>
  filter(periodo <= "2025_T4")

nrow(ipc_trimestral)

head(ipc_trimestral)

tail(ipc_trimestral)

saveRDS(
  ipc_trimestral,
  "ipc_trimestral.rds",
  compress = FALSE
)

save.image("workspace_red_laboral.RData")
