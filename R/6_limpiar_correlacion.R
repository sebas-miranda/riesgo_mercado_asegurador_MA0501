# ejecutar desde la raíz del proyecto después de los pasos 4 y 5
library(ggplot2)
library(patchwork)

source("R/crear_grafico_C.R", encoding = "UTF-8")

espectro <- read.csv("resultados/eigenvalues_spearman.csv")
V <- as.matrix(read.csv("resultados/eigenvectors_spearman.csv",
                        row.names = 1, fileEncoding = "UTF-8"))

lambda <- espectro$eigenvalue
# los valores propios negativos por redondeo cuentan como cero
lambda[lambda < 0] <- 0
#posiciones que quedaron por debajo de lambda max:
indices_ruido <- which(!espectro$sobre_MP)
promedio_ruido <- mean(lambda[indices_ruido])
lambda[indices_ruido] <- promedio_ruido

#conservamos los eigenvectors, cambiamos los eigenvalues en la diagonalización
C_reconstruida <- V %*% diag(lambda, nrow = nrow(V)) %*% t(V)

#reescala la matriz para que la diagonal vuelva a ser de unos
C_limpia <- cov2cor(C_reconstruida)
# quitar pequeñas asimetrías de redondeo y con ello deja una simetrica
C_limpia <- (C_limpia + t(C_limpia)) / 2

#pone los nombres en las 42 series en las filas y en las columnas
dimnames(C_limpia) <- list(rownames(V), rownames(V))

write.csv(C_limpia, "resultados/correlacion_spearman_limpia.csv",
          fileEncoding = "UTF-8")
cat("Se reemplazaron", length(indices_ruido), "valores propios por su promedio de",
    promedio_ruido, "\n")

C_empirica <- as.matrix(read.csv("resultados/correlacion_spearman_empirica.csv",
                                 row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))

diagnostico <- read.csv("resultados/diagnostico_RMT.csv")
T_serie <- diagnostico$T[diagnostico$metodo == "Spearman"]

# los dos mapas se ordenan con la matriz empírica, igual que en el paso 4
orden <- hclust(as.dist(1 - C_empirica), method = "average")$order
grafico_empirica <- crear_grafico_C(C_empirica, "Spearman", orden, T_serie)
grafico_limpia <- crear_grafico_C(C_limpia, "Spearman", orden, T_serie) +
    labs(title = "Matriz de correlación de Spearman limpia con RMT")

comparacion <- grafico_empirica | grafico_limpia
comparacion
ggsave("graficos/comparacion_spearman_rmt.pdf",
       plot = comparacion,
       width = 36, height = 15, bg = "white")