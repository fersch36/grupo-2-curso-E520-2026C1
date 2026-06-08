# Decisiones Metodológicas — Dinámica Salarial Intersectorial (EPH)
## Curso E520-2026C1 — Grupo 2

Este documento registra las decisiones metodológicas tomadas a lo largo del proyecto, con su justificación. Está pensado como respaldo para la presentación final y como guía para el grupo.

---

## 1. Fuente de datos

**Decisión:** usar la Encuesta Permanente de Hogares (EPH) del INDEC, cuestionario individual, accedida mediante el paquete `eph` de R (rOpenSci).

**Justificación:** la EPH es la única fuente de microdatos de mercado laboral en Argentina con cobertura nacional, periodicidad trimestral y estructura de panel rotante que permite seguir individuos a lo largo del tiempo. Esa estructura de panel es condición necesaria para detectar traspasos intersectoriales.

---

## 2. Período de análisis

**Decisión:** 2T2016 – 4T2025.

**Justificación:** en 2016 el INDEC reformó el cuestionario y la metodología de la EPH luego de la intervención del organismo. Los datos anteriores a 2016 tienen problemas de comparabilidad documentados. Se arranca en 2T2016 (y no 1T2016) porque el primer trimestre de ese año presenta inconsistencias en algunas variables de actividad económica.

**Advertencia sobre 2020:** el segundo trimestre de 2020 coincide con el ASPO (cuarentena estricta por COVID-19). La EPH de ese período tuvo cobertura reducida y se realizó por vía telefónica, lo que afecta la representatividad. Las observaciones de ese trimestre están incluidas pero deben interpretarse con cautela en los análisis de serie temporal.

---

## 3. Universo de análisis

**Decisión:** asalariados ocupados — `ESTADO == 1` (ocupado) y `CAT_OCUP == 3` (obrero o empleado).

**Justificación:** el proyecto analiza traspasos salariales *entre sectores de actividad económica*. Los trabajadores por cuenta propia y los patrones tienen una lógica de ingresos estructuralmente distinta (mezclan retribución al trabajo con retorno al capital) y no son comparables con los asalariados. Los trabajadores familiares sin remuneración no tienen ingreso declarado. La restricción a asalariados permite comparaciones más limpias entre sectores.

---

## 4. Variables retenidas y justificación

De las 252 variables del cuestionario individual de la EPH, se retienen 15 que responden directamente a la pregunta de investigación:

| Variable | Rol | Justificación |
|---|---|---|
| `CODUSU` | Identificación | Clave de vivienda para seguimiento longitudinal |
| `NRO_HOGAR` | Identificación | Número de hogar dentro de la vivienda |
| `COMPONENTE` | Identificación | Número de persona dentro del hogar |
| `Anio` / `Trimestre` | Temporal | Ubicación en el tiempo; necesario para join con CBT |
| `ESTADO` | Filtro universo | Seleccionar ocupados |
| `CAT_OCUP` | Filtro universo | Seleccionar asalariados |
| `caes_division_cod` | Sector | División CAES Rev.2 — define el nodo del grafo |
| `P21` | Ingreso | Ingreso de la ocupación principal |
| `PP07H` | Registro laboral | Descuento jubilatorio — proxy de formalidad laboral |
| `CH04` | Atributo | Género |
| `CH06` | Atributo | Edad (se agrupa en tramos) |
| `NIVEL_ED` | Atributo | Nivel educativo |
| `REGION` | Atributo | Región geográfica (6 categorías INDEC) |
| `PONDERA` | Ponderación | Factor de expansión — necesario para resultados representativos |

**Nota sobre `PP07H`:** esta variable pregunta si el empleador realiza descuento jubilatorio. Es el proxy estándar de informalidad/registro utilizado por INDEC en sus tabulados oficiales y por la literatura de mercado laboral argentino. Captura si el trabajador está "en blanco", no si existe un contrato formal escrito.

**Nota sobre `caes_division_cod`:** se usa la *división* CAES (2 dígitos) y no la *sección* (1 letra) para tener una clasificación más granular, ni el código completo de rama (4 dígitos) para evitar fragmentación excesiva. Se usa `pp04b_cod` (rama de actividad) y no `pp04d_cod` (código de ocupación CNO), ya que el objetivo es clasificar el *sector económico* y no la *tarea realizada*.

---

## 5. Clasificación sectorial

**Decisión:** 10 sectores definidos a partir de divisiones CAES Rev.2.

| Sector | Divisiones CAES |
|---|---|
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

**Justificación de las categorías:**

- *Construcción* incluye el código 40 (presente en versiones anteriores de CAES) además de 41–43.
- *Comercio* incluye el código 48 ("comercio excepto vehículos") además de 45–47.
- *Servicios Profesionales e IT* se separó de "Otros Servicios" porque concentra trabajadores de alta calificación (83.7% con nivel superior) e ingresos muy superiores al resto de la categoría residual, lo que distorsionaría el análisis si se los agrupara. Incluye IT (62), información (63), finanzas (64), seguros (65), legal/contabilidad (69), consultoría de gestión (70), arquitectura e ingeniería (71), I&D (72) y otras actividades profesionales (74).
- La categoría residual *Otros Servicios / Actividades* representa aproximadamente el 14% de los asalariados — un nivel aceptable para una categoría de este tipo.

**Implementación:** el clasificador está centralizado en `scripts/utils.R` como función `clasificar_sector()`. Todos los scripts del proyecto llaman a esa función para garantizar consistencia.

---

## 6. Métrica de ingreso

**Decisión:** ingreso expresado en canastas básicas totales (CBT) consumidas por mes — `CBT_consumidas = P21 / CBT_trimestral`.

**Justificación:** Argentina atravesó tasas de inflación muy elevadas durante el período 2016–2025. Comparar ingresos nominales entre años — o incluso entre trimestres — no tiene sentido económico. La CBT es publicada mensualmente por INDEC y refleja la evolución de precios de una canasta amplia de bienes y servicios. Al dividir el ingreso por la CBT, la unidad de medida pasa a ser "poder adquisitivo real" — cuántas canastas puede comprar el trabajador con su sueldo — lo que permite comparaciones a lo largo del tiempo y entre sectores.

**Fuente de la CBT:** `eph::get_poverty_lines()`, que descarga los valores oficiales de INDEC. Se guarda localmente en `data/cbt_historica.rds` para no depender de conexión a internet en cada ejecución.

**Nota sobre CBT regional vs nacional:** INDEC publica coeficientes regionales de la CBT. En esta versión del análisis se usa la CBT nacional promediada por trimestre (simplificación v1). En versiones futuras se puede incorporar la CBT regional para mejorar la comparabilidad entre trabajadores de distintas regiones.

---

## 7. Detección de traspasos

**Decisión:** un traspaso se define como un cambio de sector entre dos trimestres consecutivos para la misma persona, identificada por `CODUSU + NRO_HOGAR + COMPONENTE`.

**Justificación:** la EPH es un panel rotante de 4 ondas — cada hogar se entrevista 4 trimestres consecutivos. El solapamiento teórico entre trimestres consecutivos es del 75% de la muestra. Para seguir individuos se requiere la triple clave `CODUSU + NRO_HOGAR + COMPONENTE` porque ninguna de las tres es suficiente por sí sola.

**Validación de identidad:** se descarta un match si el género difiere entre los dos períodos (indica que la clave identifica a personas distintas, no a la misma persona). Opcionalmente se puede agregar la condición de que la edad varíe en 0 o 1 año.

---

## 8. Decisiones pendientes

- **CBT regional vs nacional:** incorporar coeficientes regionales para mejorar comparabilidad entre regiones.
- **Grupo de comparación (contrafactual):** definir contra qué se compara el Delta CBT del trabajador que se traspasó — la opción preferida es el trabajador de igual perfil (género, tramo etario, nivel educativo, región) que se mantuvo en el sector de origen.
- **Factores vs character:** evaluar si conviene convertir variables categóricas con orden natural (`Tramo_Edad`, `Nivel_Ed`) a factor ordenado.

---

## 9. Diseño del grafo intersectorial (`06_grafo.R`)

**Decisiones de representación:**

| Elemento visual | Variable codificada | Justificación |
|---|---|---|
| Dirección de la flecha | Sentido del traspaso (origen → destino) | Permite leer asimetría de los flujos |
| Grosor de arista | N de traspasos (rescalado 0.3–3.5) | Volumen = relevancia empírica del flujo |
| Color de arista | Delta CBT medio (divergente rojo–blanco–verde, centrado en 0) | Variable central del análisis |
| Tamaño de nodo | N total de traspasos que involucran al sector (entrada + salida) | Centralidad en la red de movilidad, no empleo total |
| Color de nodo | Paleta de proyecto (`colores_sectores` en `utils.R`) | Consistencia visual con el resto del proyecto |

**Umbral de flujo:** se incluyen solo flujos con N ≥ 50 en el grafo de red. Flujos con N < 50 tienen estimaciones de Delta CBT poco estables. El heatmap complementario usa N ≥ 30 para mostrar más flujos en formato tabular.

**Layout:** `"stress"` (Fruchterman-Reingold mejorado de `{graphlayouts}`) con `set.seed(42)` para reproducibilidad.

**`geom_edge_arc()`** con `strength = 0.25` para separar visualmente flujos bidireccionales (ej. Adm. Pública ↔ Otros Servicios, que son los flujos más voluminosos).

**Cuatro visualizaciones producidas:**
- `06_01_grafo_completo.png` — todos los flujos con N ≥ 50
- `06_02_grafo_ganancia.png` — solo flujos con Delta CBT > 0
- `06_03_grafo_perdida.png` — solo flujos con Delta CBT ≤ 0
- `06_04_heatmap_flujos.png` — heatmap de todos los flujos con N ≥ 30 (complemento tabular)
