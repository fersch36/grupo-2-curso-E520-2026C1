# Próximos Pasos — Dinámica Salarial Intersectorial (EPH)
## Curso E520-2026C1 — Grupo 2

---

## Prioridad alta — desbloquean el análisis central

### 1. Armar la presentación (15 minutos, director de investigación)
El producto final del proyecto es una presentación de 15 minutos frente al director del departamento de investigación. Hay que definir la estructura de slides, seleccionar los hallazgos y gráficos más relevantes, y ensayar el tiempo.

### 2. Definir la metodología del contrafactual
Para cada traspaso detectado, ¿contra qué comparamos el Delta CBT? La opción preferida es: trabajador de igual género, tramo etario, nivel educativo y región que se mantuvo en el sector de origen. Hay que definirlo operacionalmente y documentarlo en `decisiones_metodologicas.md`.

---

## Prioridad media — mejoran la calidad del análisis

### 3. Incorporar CBT regional
Actualmente se usa la CBT nacional promediada por trimestre. INDEC publica coeficientes regionales que permitirían comparar mejor entre trabajadores de distintas zonas. Ya existe `data/diccionarios_eph/canastas_reg_example.csv` con la estructura.

### 4. Evaluar factores ordenados para variables categóricas
`Tramo_Edad` y `Nivel_Ed` tienen orden natural. Convertirlas a factor ordenado garantiza que los gráficos y tablas siempre las muestren en el orden correcto sin depender de `fct_reorder()`. Decisión pendiente del grupo.

### 5. Agregar nota metodológica sobre brecha de género en sectores con baja participación femenina
El caso Construcción mostró que la brecha puede estar distorsionada por sesgo de selección cuando el N femenino es muy bajo. Agregar advertencia en `decisiones_metodologicas.md`.

---

## Prioridad baja — para después del análisis central

### 6. Análisis de robustez — excluir 2020-T2
Correr el análisis principal excluyendo el trimestre del ASPO para verificar que los resultados no están siendo afectados por ese período atípico.

### 7. Regionalizar el análisis del grafo
Producir versiones del grafo por región para identificar si los patrones de movilidad intersectorial difieren entre GBA, Pampeana, NOA, etc. (`06_grafo.R` admite filtrado por `Region` sin modificaciones mayores.)

---

## Decisiones tomadas — no reabrir sin consenso del grupo

- Período: 2T2016 – 4T2025
- Universo: asalariados ocupados (ESTADO=1, CAT_OCUP=3)
- Métrica: CBT consumidas (P21 / CBT trimestral)
- Clasificador sectorial: `clasificar_sector()` en `utils.R`
- Identificación de panel: `CODUSU + NRO_HOGAR + COMPONENTE`
- Tramos etarios: 16–25, 26–46, 47+
- Umbral mínimo de flujo para el grafo: N ≥ 50 (heatmap complementario: N ≥ 30)
- Layout del grafo: `"stress"` con `set.seed(42)`

---

## Completado ✅

- `00_explorar_raw.R` — exploración inicial
- `01_descargar_completo.R` — descarga EPH completa 2016–2025
- `02_crear_muestra.R` — muestra estratificada para exploración rápida
- `03_explorar_muestra.R` — análisis exploratorio descriptivo
- `04_analisis_cbt.R` — ingreso en CBT, prima de registro, brechas de género
- `05_traspasos.R` — detección de traspasos, Delta CBT, diagnóstico completo
- `06_grafo.R` — grafo dirigido (completo, ganancia, pérdida, heatmap)
