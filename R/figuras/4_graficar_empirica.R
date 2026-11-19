# Ejecutar después del paso 4 de R/analisis.
library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "crear_grafico_C.R"), encoding = "UTF-8")

C_pearson <- as.matrix(read.csv(here("resultados", "correlacion_pearson_empirica.csv"),
                               row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
C_spearman <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                                row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
datos_RMT <- read.csv(here("resultados", "comparacion.csv"))
T_serie <- datos_RMT$T[datos_RMT$metodo == "Spearman"]

# Construimos distancias, de modo que podamos agrupar las correlaciones mayores, es decir
# premiar a los grupos de productos que mas similar se comportan, utilizamos a spearman:
distancias <- as.dist(1 - C_spearman)
# Con hclust hacemos clustering, de modo que promediamos las distancias anteriormente calculadas.
# Las que tienen menor distancia, poseen mayor correlacion promedio entre ambos ramos.
agrupamiento <- hclust(distancias, method = "average")
# Extraemos el orden de los nodos (ramos) de izquierda a derecha calculados en agrupamiento
orden <- agrupamiento$order

# NOTA: Esto del clustering que realizamos justo arriba, es indispensable debido a que en 2_crear_matriz_lr.R
# se hace: nodos <- sort(unique(datos$nodo), esto ordena cada ramo por orden alfabetico sin repetirse
# el problema es que a la hora de graficar las correlaciones  se verian desordenadas; haciendo clustering
# con el as.dist acercamos las correlaciones mas altas (distancias mas bajas) a la diagonal.
# De este modo, se asemeja mucho mas a un mapa de calor.

# Generamos ambos graficos
grafico_C_pearson <- crear_grafico_C(C_pearson, "Pearson", orden, T_serie)
grafico_C_spearman <- crear_grafico_C(C_spearman, "Spearman", orden, T_serie)
comparacion <- grafico_C_pearson | grafico_C_spearman
comparacion
# Guardamos el grafico de comparacion entre ambos:
ggsave(
  here("graficos", "comparacion_pearson_spearman_corr.pdf"),
       plot = comparacion,
       width = 36, height = 15, bg = "white")
