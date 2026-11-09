# ejecutar desde la raíz del proyecto después de R/5_RMT.R
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
