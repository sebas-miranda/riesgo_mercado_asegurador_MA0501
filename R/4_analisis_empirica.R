# Matrices de correlación empírica de Pearson y Spearman

library(ggplot2)
library(patchwork)


# Cargamos las funciones de la construccion de la matriz de LR y la matriz de estandarización:
source("R/2_crear_L.R")
source("R/3_crear_X.R")
source("R/crear_grafico_C.R", encoding = "UTF-8")

# Construimos la matriz de LR y de estandarización, tanto para Pearson como para Spearman:
L<- crear_L("datos/datos_procesados/panel_trimestral.csv")

X_pearson <- crear_X(L, metodo = "pearson")
X_spearman <- crear_X(L, metodo = "spearman")

# Matriz de correlacion empirica 

# Acá definimos T como la cantidad de columnas (Periodos) de la matriz de LR. Es decir las series de tiempo 
# de los ramos.
T_serie <- ncol(L)


# Estas C corresponden a las matrices de correlación de Pearson y Spearman mediante la formula
# C = X*X^t/(T-1), note que %*% se utiliza para el producto de matrices:
C_pearson <- X_pearson %*% t(X_pearson) / (T_serie - 1)
C_spearman <- X_spearman %*% t(X_spearman) / (T_serie - 1)

write.csv(C_pearson, "resultados/correlacion_pearson_empirica.csv",
          fileEncoding = "UTF-8")
write.csv(C_spearman, "resultados/correlacion_spearman_empirica.csv",
          fileEncoding = "UTF-8")

# Construimos distancias, de modo que podamos agrupar las correlaciones mayores, es decir
# premiar a los grupos de productos que mas similar se comportan, utilizamos a spearman:
distancias <- as.dist(1 - C_spearman)
# Con hclust hacemos clustering, de modo que promediamos las distancias anteriormente calculadas.
# Las que tienen menor distancia, poseen mayor correlacion promedio entre ambos ramos.
agrupamiento <- hclust(distancias, method = "average")
# Extraemos el orden de los nodos (ramos) de izquierda a derecha calculados en agrupamiento
orden <- agrupamiento$order

# NOTA: Esto del clustering que realizamos justo arriba, es indispensable debido a que en 2_crear_L.R
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
  "graficos/comparacion_pearson_spearman_corr.pdf",
       plot = comparacion,
       width = 36, height = 15, bg = "white")