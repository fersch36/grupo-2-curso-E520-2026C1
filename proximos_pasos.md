# Próximos Pasos — Dinámica Salarial Intersectorial (EPH)
## Curso E520-2026C1 — Grupo 2

---

## Prioridad alta — desbloquean el análisis central

### 1. Validar categoría "Servicios Profesionales e IT" con el grupo
Decidimos separar IT/Finanzas/Servicios profesionales de "Otros Servicios" porque distorsionaban el ingreso medio de esa categoría residual. Hay que compartir la decisión con el grupo antes de fijarla definitivamente en `utils.R`.

### 2. Definir la metodología del contrafactual
Para cada traspaso detectado, ¿contra qué comparamos el Delta CBT? La opción preferida es: trabajador de igual género, tramo etario, nivel educativo y región que se mantuvo en el sector de origen. Hay que definirlo operacionalmente y documentarlo en `decisiones_metodologicas.md`.

### 3. Armar `05_traspasos.R`
Script que detecta traspasos entre trimestres consecutivos usando `CODUSU + NRO_HOGAR + COMPONENTE`, calcula Delta CBT para cada traspaso, e incorpora el grupo de comparación contrafactual. Output: `data/processed/tabla_traspasos.rds`.

---

## Prioridad media — mejoran la calidad del análisis

### 4. Incorporar CBT regional
Actualmente se usa la CBT nacional promediada por trimestre. INDEC publica coeficientes regionales que permitirían comparar mejor entre trabajadores de distintas zonas. Ya existe `data/diccionarios_eph/canastas_reg_example.csv` con la estructura.

### 5. Evaluar factores ordenados para variables categóricas
`Tramo_Edad` y `Nivel_Ed` tienen orden natural. Convertirlas a factor ordenado garantiza que los gráficos y tablas siempre las muestren en el orden correcto sin depender de `fct_reorder()`. Decisión pendiente del grupo.

### 6. Agregar nota metodológica sobre brecha de género en sectores con baja participación femenina
El caso Construcción mostró que la brecha puede estar distorsionada por sesgo de selección cuando el N femenino es muy bajo. Agregar advertencia en `decisiones_metodologicas.md`.

---

## Prioridad baja — para después del análisis central

### 7. Armar `06_grafo.R`
Visualización del grafo intersectorial con `ggraph`. Tres versiones: registrados, no registrados, total. Nodos = sectores, grosor de arista = cantidad de traspasos, color = Delta CBT promedio (verde = mejora, rojo = empeora).

### 8. Análisis de robustez — excluir 2020-T2
Correr el análisis principal excluyendo el trimestre del ASPO para verificar que los resultados no están siendo afectados por ese período atípico.

### 9. Regionalizar el análisis del grafo
Producir versiones del grafo por región para identificar si los patrones de movilidad intersectorial difieren entre GBA, Pampeana, NOA, etc.

---

## Decisiones tomadas — no reabrir sin consenso del grupo

- Período: 2T2016 – 4T2025
- Universo: asalariados ocupados (ESTADO=1, CAT_OCUP=3)
- Métrica: CBT consumidas (P21 / CBT trimestral)
- Clasificador sectorial: `clasificar_sector()` en `utils.R`
- Identificación de panel: `CODUSU + NRO_HOGAR + COMPONENTE`
- Tramos etarios: 16–25, 26–46, 47+
