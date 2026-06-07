# Hoja de Ruta — Dinámica Salarial Intersectorial (EPH)
## Curso E520-2026C1 — Grupo 2

---

## Pregunta de investigación

¿Cómo evoluciona el poder adquisitivo de los trabajadores asalariados que cambian de sector económico en Argentina (2016–2025)? ¿Esa evolución difiere según género, tramo etario, nivel educativo, región y registro laboral?

---

## Objetivo central

Construir un grafo de traspasos intersectoriales donde:
- Cada **nodo** es un sector de actividad económica
- Cada **arista** representa el flujo de trabajadores entre dos sectores
- El **grosor** de la arista codifica la cantidad de traspasos
- El **color** de la arista codifica el Delta CBT promedio (verde = mejora, rojo = empeora)

Producir al menos tres versiones del grafo: trabajadores registrados, no registrados, y total.

---

## Métrica principal

**Delta CBT** = CBT consumidas en el sector destino − CBT consumidas en el sector origen

Donde CBT consumidas = ingreso mensual (P21) / valor de la Canasta Básica Total del trimestre correspondiente.

---

## Variables de corte (atributos del análisis)

- Género (CH04)
- Tramo etario: 16–25, 26–46, 47+ (CH06)
- Nivel educativo agrupado (NIVEL_ED)
- Región (REGION)
- Registro laboral — registrado / no registrado (PP07H)

---

## Grupo de comparación (contrafactual)

Para cada trabajador que realizó un traspaso, comparar su Delta CBT contra el trabajador de igual perfil (género, tramo etario, nivel educativo, región) que se mantuvo en el sector de origen durante el mismo período. Decisión pendiente de definición operacional.

---

## Estructura de scripts

| Script | Descripción | Estado |
|---|---|---|
| `00_explorar_raw.R` | Exploración de un trimestre raw | ✅ Listo |
| `01_descargar_completo.R` | Descarga EPH completa 2016–2025 | ✅ Listo |
| `02_crear_muestra.R` | Genera muestra para exploración rápida | ✅ Listo |
| `03_explorar_muestra.R` | Análisis exploratorio descriptivo | ✅ Listo |
| `04_analisis_cbt.R` | Análisis en canastas básicas (CBT) | ✅ Listo |
| `05_traspasos.R` | Detección de traspasos y Delta CBT | 🔲 Pendiente |
| `06_grafo.R` | Visualización del grafo intersectorial | 🔲 Pendiente |
| `utils.R` | Funciones compartidas (clasificador, paletas, tema) | ✅ Listo |

---

## Documentos del proyecto

| Documento | Descripción | Estado |
|---|---|---|
| `decisiones_metodologicas.md` | Justificación de cada decisión de diseño | ✅ En curso |
| `hallazgos.md` | Resultados empíricos que van surgiendo | ✅ En curso |
| `hoja_de_ruta.md` | Este documento | ✅ En curso |
| `proximos_pasos.md` | Tareas pendientes priorizadas | ✅ En curso |

---

## Consideraciones importantes

- **2020-T2:** cobertura reducida por ASPO — interpretar con cautela en series temporales
- **CBT:** se usa la nacional promediada por trimestre (v1). Mejora futura: CBT regional
- **Clasificador sectorial:** centralizado en `utils.R → clasificar_sector()` — no duplicar
- **Seguimiento longitudinal:** requiere triple clave `CODUSU + NRO_HOGAR + COMPONENTE`
- **Categoría "Servicios Profesionales e IT":** pendiente validación con el grupo
