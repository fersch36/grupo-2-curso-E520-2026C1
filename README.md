# Dinámica salarial intersectorial en Argentina
### Trabajo final — Curso E520-2026C1 · Grupo 2

---

## Descripción

Este proyecto analiza la **movilidad laboral entre sectores económicos** en Argentina y sus consecuencias sobre el poder adquisitivo de los trabajadores, usando los microdatos del panel rotante de la **Encuesta Permanente de Hogares (EPH)** del INDEC para el período 2T 2016 – 4T 2025.

La pregunta central es: cuando un asalariado migra de un sector a otro, ¿mejora o empeora su poder adquisitivo? ¿Varía eso según género, edad, nivel educativo, registro laboral o región?

---

## Estructura del repositorio

```
grupo-2-curso-E520-2026C1/
├── README.md
├── .gitignore
├── proyecto.Rproj
├── data/                                        ← excluido del repo (ver .gitignore)
│   ├── microdatos_raw/                          ← un .rds por trimestre descargado
│   ├── diccionarios_eph/                        ← caes, CNO, regiones, aglomerados
│   └── processed/                               ← outputs intermedios
│       ├── panel_cbt.rds                        ← panel con ingresos en CBT
│       ├── tabla_traspasos.rds                  ← traspasos detectados con Delta CBT
│       └── aristas_grafo.rds                    ← aristas del grafo (flujos agregados)
├── plots/                                       ← gráficos por script
│   ├── 03_muestra/
│   ├── 04_cbt/
│   ├── 05_traspasos/
│   └── 06_grafo/
└── scripts/
    ├── utils.R                                  ← funciones compartidas (clasificador, paletas, tema)
    ├── 01_descargar_completo.R                  ← descarga EPH 2016–2025 (39 trimestres)
    ├── 02_crear_muestra.R                       ← muestra estratificada para exploración rápida
    ├── 03_explorar_muestra.R                    ← análisis exploratorio descriptivo
    ├── 04_analisis_cbt.R                        ← ingresos en CBT, brechas, prima de registro
    ├── 05_traspasos.R                           ← detección de traspasos y cálculo de Delta CBT
    └── 06_grafo.R                               ← grafo dirigido de movilidad intersectorial
```

> La carpeta `data/` está excluida del repositorio por tamaño. Para reproducir, correr los scripts en orden (ver sección *Cómo reproducir*).

---

## Metodología

### Unidad de análisis
Asalariados (`CAT_OCUP == 3`) ocupados (`ESTADO == 1`) observados en al menos dos trimestres consecutivos del panel EPH.

### Identificación de individuos
El panel rotante de la EPH permite seguir a cada persona mediante la clave compuesta `CODUSU + NRO_HOGAR + COMPONENTE`. Un **traspaso** se detecta cuando el mismo individuo aparece en el trimestre *t* y el trimestre *t+1* en sectores económicos distintos, con validación de identidad por género y edad.

### Sectores económicos (CAES Rev. 2)
Clasificación centralizada en `scripts/utils.R → clasificar_sector()`, construida a partir del código de división CAES (`caes_division_cod`):

| Sector | Divisiones CAES |
|--------|----------------|
| Industria | 10–33 |
| Construcción | 40–43 |
| Comercio | 45–48 |
| Transporte y Almacenamiento | 49–53 |
| Administración Pública | 84 |
| Educación y Salud | 85–86 |
| Servicio Doméstico | 97 |
| Minería, Energía y Agro | 1–9, 35–39 |
| Servicios Profesionales e IT | 62–65, 69–72, 74 |
| Otros Servicios / Actividades | resto |

### Métrica de bienestar: canastas básicas totales (CBT)
El ingreso mensual (`P21`) se deflacta dividiendo por el valor de la **Canasta Básica Total (CBT)** del trimestre correspondiente, obtenida mediante `eph::get_poverty_lines()`. El indicador resultante —*CBT consumidas*— es comparable entre períodos de alta inflación.

El diferencial entre trimestres (`Delta_CBT = CBT_t1 − CBT_t`) determina si el traspaso implicó una **mejora** o un **deterioro** del poder adquisitivo.

### Variables de atributo
| Variable | Categorías |
|----------|------------|
| Género | Varón / Mujer |
| Tramo etario | 16–25 / 26–46 / 47+ |
| Registro laboral | Registrado / No Registrado |
| Nivel educativo | Hasta primaria incompleta / Primaria completa / Secundaria incompleta / Secundaria completa / Superior |
| Región | GBA / NOA / NEA / Cuyo / Pampeana / Patagonia |

---

## Visualización: grafo de traspasos intersectoriales

El resultado principal del proyecto es un **grafo dirigido** donde:
- Cada **nodo** representa un sector económico (tamaño = total de traspasos que lo involucran).
- Cada **arista** representa el flujo de trabajadores entre dos sectores (solo flujos con N ≥ 50).
- El **grosor** de la arista codifica la cantidad de traspasos detectados.
- El **color** de la arista codifica el Delta CBT medio del flujo (escala divergente: rojo = pérdida, blanco = neutro, verde = ganancia).

Se producen cuatro visualizaciones en `plots/06_grafo/`:
- `06_01_grafo_completo.png` — todos los flujos con N ≥ 50
- `06_02_grafo_ganancia.png` — solo flujos con Delta CBT > 0
- `06_03_grafo_perdida.png` — solo flujos con Delta CBT ≤ 0
- `06_04_heatmap_flujos.png` — heatmap de flujos (N ≥ 30), complemento tabular del grafo

---

## Fuentes de datos

- **EPH (INDEC)** — microdatos individuales trimestrales, accedidos vía el paquete [`eph`](https://github.com/holatam/eph) (rOpenSci).
- **Canasta Básica Total** — series mensuales del INDEC, consolidadas trimestralmente vía `eph::get_poverty_lines()`.

---

## Paquetes principales

`tidyverse` · `eph` · `ggraph` · `igraph` · `haven` · `here` · `glue` · `janitor` · `scales` · `Hmisc`

---

## Cómo reproducir

```r
# Los scripts deben correrse en orden. Cada uno lee el output del anterior.

source("scripts/01_descargar_completo.R")   # descarga 39 trimestres (~puede tardar)
source("scripts/02_crear_muestra.R")         # muestra para exploración rápida
source("scripts/03_explorar_muestra.R")      # diagnóstico descriptivo
source("scripts/04_analisis_cbt.R")          # panel con ingresos en CBT
source("scripts/05_traspasos.R")             # detección de traspasos
source("scripts/06_grafo.R")                 # grafo de movilidad intersectorial
```

> Los scripts son idempotentes: verifican si el output ya existe antes de regenerarlo.
> La descarga en `01_descargar_completo.R` puede interrumpirse y reanudarse — los trimestres ya descargados se saltan automáticamente.
