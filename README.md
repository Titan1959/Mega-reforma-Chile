# Política para no políticos

Portafolio de Maximiliano: diagnóstico con datos institucionales del Plan de Reconstrucción Nacional anunciado en marzo–abril de 2026.

Sitio: https://titan1959.github.io/Mega-reforma-Chile/

## Reproducir

1. Abrir Proyecto.Rproj. Instalar R y Quarto.
2. Ejecutar `source("scripts/instalar_paquetes.R")`.
3. Con los originales disponibles: `source("scripts/procesar_datos.R", encoding="UTF-8")`.
4. Ejecutar `quarto render` desde la raíz. El sitio se genera en `docs/`.

Los CSV depurados incluidos permiten renderizar sin ejecutar de nuevo el paso 3. Los originales voluminosos de Fonasa se conservan localmente y no se publican. El inventario `fuentes/manifest.csv` documenta los insumos. Los nuevos originales pequeños se incluyen en las carpetas temáticas. No se publican datos inventados ni se estiman efectos causales.

Las páginas y las limitaciones metodológicas están en español. `metodologia.qmd` distingue decisiones previas, hipótesis y análisis realizados. Las series previas a 2026 no son evidencia de efectos del plan.
