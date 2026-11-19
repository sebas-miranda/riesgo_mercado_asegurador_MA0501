# Ajusta la correlación de Spearman con distintos valores de alfa.
# Lee resultados/correlacion_spearman_empirica.csv,
# resultados/eigenvalues_spearman.csv y resultados/eigenvectors_spearman.csv.
# Guarda una matriz por alfa y espectro_alpha.csv en la carpeta resultados/contraccion_alpha/.
# La matriz elegida queda en resultados/correlacion_spearman_limpia.csv.

library(here)

alpha_elegido <- 0.5
codigos <- c("000", "010", "030", "050", "070", "100")

espectro <- read.csv(here("resultados", "eigenvalues_spearman.csv"))
V <- as.matrix(read.csv(here("resultados", "eigenvectors_spearman.csv"),
                        row.names = 1, fileEncoding = "UTF-8"))
C_empirica <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                                 row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))

stopifnot(alpha_elegido %in% (as.numeric(codigos) / 100))
V <- V[rownames(C_empirica), , drop = FALSE]
lambda <- pmax(espectro$eigenvalue, 0)
ruido <- !espectro$sobre_MP
promedio_ruido <- mean(lambda[ruido])

curvas <- data.frame()

for (codigo in codigos) {
  alpha <- as.numeric(codigo) / 100
  ajustados <- lambda
  # contraer solo los autovalores hasta el borde superior de MP
  ajustados[ruido] <- (1 - alpha) * lambda[ruido] + alpha * promedio_ruido
  A <- V %*% diag(ajustados, nrow = ncol(V)) %*% t(V)
  C <- cov2cor((A + t(A)) / 2)
  if (alpha == 0) C <- C_empirica

  archivo <- paste0("alpha_constante_", codigo, ".csv")
  write.csv(C, here("resultados", "contraccion_alpha", archivo), fileEncoding = "UTF-8")
  if (alpha == alpha_elegido) {
    write.csv(C, here("resultados", "correlacion_spearman_limpia.csv"), fileEncoding = "UTF-8")
  }

  curvas <- rbind(curvas, data.frame(
    posicion = espectro$posicion[ruido], autovalor = ajustados[ruido],
    alpha = alpha, promedio_ruido = promedio_ruido
  ))
}

write.csv(curvas, here("resultados", "contraccion_alpha", "espectro_alpha.csv"), row.names = FALSE)
