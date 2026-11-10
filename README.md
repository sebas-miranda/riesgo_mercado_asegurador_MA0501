<picture>
  <source media="(prefers-reduced-motion: reduce)" srcset="docs/assets/portada.png">
  <img src="docs/assets/portada.gif" alt="Red de correlaciones del panel SUGESE: las conexiones cambian al aumentar y reducir el umbral h, mientras los nodos mantienen sus posiciones." width="100%">
</picture>

# Dependencia en el mercado asegurador costarricense

<p>
  <img src="docs/assets/r.svg" alt="Hecho en R" height="25">
  <img src="docs/assets/curso.svg" alt="MA-0501 · Análisis Numérico I" height="25">
  <img src="docs/assets/ucr.svg" alt="Universidad de Costa Rica" height="25">
  <img src="docs/assets/estado.svg" alt="Proyecto en desarrollo" height="25">
</p>

Proyecto de Análisis Numérico I de la Universidad de Costa Rica, II ciclo de 2026.

El proyecto estudia cómo se relacionan los ramos de distintas aseguradoras a partir de los datos públicos de primas y siniestros de SUGESE. El análisis parte de una matriz de correlación, representada como una red de conexiones entre series y estudiada a través de sus valores propios con herramientas de teoría de matrices aleatorias (RMT).

El panel actual contiene **50 series aseguradora–ramo y 32 trimestres, de 2015 a 2022**. El repositorio reúne el procesamiento de los datos, los scripts de R y las figuras del análisis.

## Los datos y la matriz

La fuente es el reporte **«Primas y siniestros — Por ramo y aseguradora»** de [SUGESE](https://www.sugese.fi.cr/cr/es/estadisticas.html). El Excel está en [`datos/datos_originales/`](datos/datos_originales/).

Los montos vienen acumulados dentro de cada año. Los flujos trimestrales se obtienen con el cierre de marzo y las diferencias entre cierres consecutivos. Después se calcula `loss_ratio = siniestros_brutos / primas_brutas` y se conservan las series completas en el período común del panel.

Cada fila de `L` es una combinación de aseguradora y ramo; cada columna es un trimestre. Cada fila se centra y estandariza para obtener `X`. La matriz empírica se calcula con:

```r
C <- (X %*% t(X)) / (T_serie - 1)
```

El divisor es `T_serie - 1` porque la estandarización usa la desviación estándar muestral.

<details>
<summary>Ver la matriz de correlación</summary>

![Matriz de correlación empírica, ordenada por similitud entre series](graficos/matriz_correlacion_empirica.png)

La diagonal vale uno. Los colores muestran el signo y la magnitud de la correlación.

</details>

## Las redes

Cada nodo es una serie aseguradora–ramo. El color identifica a la aseguradora; los enlaces azules son positivos y los naranjas discontinuos son negativos. El grosor depende de `|ρᵢⱼ|`.

### Elegir un umbral

Se conserva un enlace cuando `|ρᵢⱼ| ≥ h`. Al subir `h` quedan menos conexiones. Los nodos aislados permanecen en la figura para poder comparar el mismo conjunto de series.

| `h` | Enlaces | Positivos | Negativos |
| --- | ---: | ---: | ---: |
| 0.30 | 161 | 94 | 67 |
| 0.40 | 68 | 42 | 26 |
| 0.50 | 32 | 18 | 14 |

![Red signed de la matriz empírica con h igual a 0.40](graficos/red_empirica_h_0.40.png)

Las tres figuras del avance usan las mismas posiciones de los nodos.

### Conservar la planaridad: PMFG

El *Planar Maximally Filtered Graph* recorre los pares en orden y agrega cada enlace si la red sigue siendo planar. El script aplica dos criterios: `ρᵢⱼ` descendente y `|ρᵢⱼ|` descendente. En ambos se conserva la correlación original, incluido su signo.

<table width="100%">
  <tr>
    <th>Orden por ρᵢⱼ</th>
    <th>Orden por |ρᵢⱼ|</th>
  </tr>
  <tr>
    <td width="50%"><a href="graficos/pmfg_clasico.png"><img src="graficos/pmfg_clasico.png" alt="PMFG ordenado por rho: 139 enlaces positivos y 5 negativos" width="100%"></a></td>
    <td width="50%"><a href="graficos/pmfg_magnitud.png"><img src="graficos/pmfg_magnitud.png" alt="PMFG ordenado por valor absoluto de rho: 87 enlaces positivos y 57 negativos" width="100%"></a></td>
  </tr>
  <tr>
    <td>139 positivos · 5 negativos</td>
    <td>87 positivos · 57 negativos</td>
  </tr>
</table>

Los dos PMFG tienen `3N − 6 = 144` enlaces. El acomodo planar evita cruces; la forma triangular del dibujo viene del algoritmo de posiciones. Cada imagen se puede abrir para verla completa.

Ordenar por magnitud permite conservar relaciones negativas fuertes. Por ejemplo, la correlación entre MAPFRE–Bienes y MAPFRE–Vida es aproximadamente **−0.867** y aparece en el PMFG por `|ρᵢⱼ|`.

## El trabajo con RMT

[`R/RMT.R`](R/RMT.R) calcula el espectro de la matriz empírica, lo compara con los bordes de Marchenko–Pastur y separa componentes para explorar las modularidades de correlaciones `Q1` y `Q2`. La búsqueda de comunidades es una heurística propia con movimientos de nodos, fusiones y reinicios.

En el panel actual hay más series que trimestres (`N/T = 1.5625`). El análisis con la banda clásica de Marchenko–Pastur se mantiene **exploratorio**. Las figuras de umbral y PMFG de este README usan la matriz empírica, sin filtrado RMT. Las conexiones describen correlaciones entre las series; no permiten afirmar que una aseguradora contagia a otra.

Los resultados de esa parte se guardan en [`resultados/RMT_modularidad/`](resultados/RMT_modularidad/).

## Ejecución

Abrir [`riesgo_mercado_asegurador_MA0501.Rproj`](riesgo_mercado_asegurador_MA0501.Rproj) en RStudio o la carpeta del repositorio en Positron. Ejecutar los scripts desde la raíz del proyecto.

Instalar una vez los paquetes de CRAN:

```r
install.packages(c("here", "ggplot2", "patchwork", "readxl", "tidyverse"))
```

El panel procesado está en el repositorio.

<details>
<summary>Volver a procesar el Excel o ejecutar la parte espectral</summary>

```r
# reconstruir el panel a partir del excel
source("R/seleccionar_panel.R", encoding = "UTF-8")

# matriz de correlación y mapa de calor
source("R/analisis_emprica.R", encoding = "UTF-8")

# espectro, componentes y modularidades Q1 / Q2
source("R/RMT.R", encoding = "UTF-8")
```

</details>

## Archivos del proyecto

| Archivo o carpeta | Contenido |
| --- | --- |
| [`R/seleccionar_panel.R`](R/seleccionar_panel.R) | Lectura del Excel, flujos trimestrales y selección de series completas. |
| [`R/crear_L.R`](R/crear_L.R) y [`R/crear_X.R`](R/crear_X.R) | Matriz de loss ratios y estandarización por serie. |
| [`R/analisis_emprica.R`](R/analisis_emprica.R) | Correlación empírica y mapa de calor. |
| [`R/RMT.R`](R/RMT.R) | Análisis espectral y modularidades de correlaciones. |
| [`datos/`](datos/) | Excel original y panel procesado. |
| [`graficos/`](graficos/) | Imágenes generadas por los scripts. |
| [`resultados/RMT_modularidad/`](resultados/RMT_modularidad/) | Matrices, valores propios, comunidades y diagnóstico espectral. |

## Referencias de trabajo

- **SUGESE.** [Estadísticas del mercado de seguros](https://www.sugese.fi.cr/cr/es/estadisticas.html), reporte de primas y siniestros por ramo y aseguradora.
- **Tumminello, Aste, Di Matteo y Mantegna (2005).** [A tool for filtering information in complex systems](https://arxiv.org/abs/cond-mat/0501335). Construcción del PMFG.
- **MacMahon y Garlaschelli (2015).** [Community detection for correlation matrices](https://doi.org/10.1103/PhysRevX.5.021006). Modularidad para matrices de correlación.
