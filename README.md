# Dependencia en el mercado asegurador costarricense

<p>
  <img src="docs/assets/r.svg" alt="Hecho en R" height="25">
  <img src="docs/assets/curso.svg" alt="MA-0501 · Análisis Numérico I" height="25">
  <img src="docs/assets/ucr.svg" alt="Universidad de Costa Rica" height="25">
  <img src="docs/assets/estado.svg" alt="Proyecto en desarrollo" height="25">
</p>

Proyecto de Análisis Numérico I de la Universidad de Costa Rica, II ciclo de 2026. Estudiamos cómo se relacionan los *loss ratios* de distintos ramos y aseguradoras del mercado costarricense.

## Datos

El panel tiene 42 series de 12 aseguradoras y cubre 39 trimestres, de 2017-T1 a 2026-T3. Cada serie corresponde a una combinación de aseguradora y ramo.

Los montos vienen del reporte «Resultado Técnico-Financiero» de SUGESE. Para calcular el *loss ratio* de cada trimestre, dividimos la siniestralidad neta de reaseguro entre la prima imputada neta de reaseguro. El panel reúne las series con prima positiva y un cociente calculable en todos los trimestres del período. [Reporte de SUGESE](https://www.sugese.fi.cr/cr/es/estadisticas.html) · [Excel del proyecto](datos/datos_originales/Reporte_Cuentas_Tecnicas.xlsx) · [Panel trimestral](datos/datos_procesados/panel_trimestral.csv)

## Correlaciones y valores propios

Calculamos dos matrices de correlación. Pearson usa los valores del *loss ratio* y Spearman usa sus rangos dentro de cada serie. Las filas se centran y estandarizan antes de calcular la correlación entre pares de series. [Mapas de Pearson y Spearman](graficos/comparacion_pearson_spearman.pdf)

Comparamos los valores propios de ambas matrices con la banda de Marchenko–Pastur. Con `q = N/T = 42/39`, el borde superior de referencia es 4,1524. Solo un valor propio supera el borde en cada matriz. En Pearson es 5,9002 y en Spearman, 6,2003. [Comparación de los espectros](graficos/comparacion_pearson_spearman_eigen.pdf) · [MacMahon y Garlaschelli (2015), ecuación 12](https://arxiv.org/pdf/1311.1924#page=5)

## Limpieza de Spearman

Conservamos el primer valor propio de Spearman y reemplazamos los otros 41 por su promedio, 0,8732. Reconstruimos la matriz con los mismos vectores propios y la reescalamos para que la diagonal vuelva a valer uno. El mapa comparativo usa el mismo orden de series en ambos paneles para ver cómo cambian las correlaciones. [Matriz empírica y matriz limpia](graficos/comparacion_spearman_rmt.pdf)

## Ejecución en R

Abrí el [proyecto de RStudio](riesgo_mercado_asegurador_MA0501.Rproj) o la carpeta del repositorio en Positron. Corré los comandos desde la raíz del proyecto. Los paquetes se instalan una sola vez.

```r
install.packages(c("here", "ggplot2", "patchwork", "readxl", "tidyverse"))
```

El panel procesado ya está en el repo. Para reconstruirlo desde el Excel, corré también el primer comando.

```r
source("R/1_seleccionar_panel.R", encoding = "UTF-8")
source("R/4_analisis_empirica.R", encoding = "UTF-8")
source("R/5_RMT.R", encoding = "UTF-8")
source("R/6_limpiar_correlacion.R", encoding = "UTF-8")
```

El paso 4 guarda las matrices empíricas en `resultados/`. El paso 5 guarda los valores propios, sus vectores y el diagnóstico de Marchenko–Pastur. El paso 6 lee esas salidas y guarda `resultados/correlacion_spearman_limpia.csv` y el PDF con los mapas comparativos.

## Avance 1

El informe del primer avance reúne el análisis con la base de 2015 a 2022. [Avance 1 en PDF](informes/Avance%201.pdf) · [Fuente de Quarto](informes/Avance%201.qmd)
