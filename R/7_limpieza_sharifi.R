#En este archivo se realizara la limpieza de la matriz de forma alternativa con el Sharifi - Krzanowski

# Lectura de espectro y autovectores

espectro <- read.csv("resultados/eigenvalues_spearman.csv")
V <- as.matrix(read.csv("resultados/eigenvectors_spearman.csv", row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
lambda <- espectro$eigenvalue

# Dejamos positivos
lambda<- pmax(lambda,0)


#Bloques de ruido 
idx_ruido <- which(!espectro$sobre_MP)
k <- length(idx_ruido)
# validar dimensiones
stopifnot(length(lambda) == nrow(V), ncol(V) == nrow(V))

# 3. Sustituir los k autovalores de ruido por una recta descendente.
# Parametrizacion propuesta a partir de Sharifi (2004), seccion 4.3.

# Promedio de los autovalores clasificados como ruido:
# lambda_media = (1/k) * sum(lambda_i), para i en el bloque de ruido.
media_ruido <- mean(lambda[idx_ruido])

# Elegimos una progresion aritmetica con separacion constante s.
# El menor valor sera: lambda_k = lambda_media - s*(k-1)/2.
# Imponiendo lambda_k >= 0, obtenemos s <= 2*lambda_media/(k-1).
# Tomamos la maxima separacion admisible, con lambda_k = 0.
pendiente <- if (k > 1) 2 * media_ruido / (k - 1) else 0

# Construimos los nuevos autovalores:
# lambda_i_nueva = lambda_media + s*((k+1)/2 - i), i = 1,...,k.
# Los desplazamientos suman cero, por lo que se conserva la suma original de los k autovalores de ruido.
lambda_limpia <- lambda
lambda_limpia[idx_ruido] <- media_ruido +
  pendiente * ((k + 1) / 2 - seq_len(k))



# Reconstruir matriz y normalizar diag 
#matriz
C_reconstruida <- V %*% diag(lambda_limpia, nrow = length(lambda_limpia)) %*% t(V)
#Cov
C_limpia_sharifi <- cov2cor(C_reconstruida)
# Promediar entre transpuesta y entrada para estabilidad numerica fuera de diag
C_limpia_sharifi <- (C_limpia_sharifi + t(C_limpia_sharifi)) / 2

dimnames(C_limpia_sharifi) <- list(rownames(V), rownames(V))
# Guardar matriz

write.csv(C_limpia_sharifi,
          "resultados/correlacion_spearman_sharifi.csv",
          fileEncoding = "UTF-8")