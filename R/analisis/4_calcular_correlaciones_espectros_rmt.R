# Calcula las correlaciones de Pearson y Spearman y sus espectros.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda resultados/correlacion_pearson_empirica.csv
# y resultados/correlacion_spearman_empirica.csv.
# Guarda resultados/eigenvalues_pearson.csv y resultados/eigenvalues_spearman.csv.
# Guarda resultados/eigenvectors_pearson.csv y resultados/eigenvectors_spearman.csv.
# Guarda resultados/comparacion.csv con una fila por método.

library(here)

source(here("R", "analisis", "2_crear_matriz_lr.R"), encoding = "UTF-8")
source(here("R", "analisis", "3_estandarizar_lr.R"), encoding = "UTF-8")

L <- crear_L(here("datos", "datos_procesados", "panel_trimestral.csv"))
N <- nrow(L)
T_serie <- ncol(L)
q <- N / T_serie
lambda_menos <- (1 - sqrt(q))^2
lambda_mas <- (1 + sqrt(q))^2

analizar_RMT <- function(tipo) {
  stopifnot(tipo %in% c("p", "s"))
  metodo <- if (tipo == "p") "Pearson" else "Spearman"
  sufijo <- tolower(metodo)

  X <- crear_X(L, metodo = sufijo)
  C <- X %*% t(X) / (T_serie - 1)
  write.csv(
    C, here("resultados", paste0("correlacion_", sufijo, "_empirica.csv")),
    fileEncoding = "UTF-8"
  )

  espectro <- eigen((C + t(C)) / 2, symmetric = TRUE)
  lambda <- espectro$values
  V <- espectro$vectors
  rownames(V) <- rownames(C)

  tabla_eigenvalues <- data.frame(
    posicion = seq_along(lambda),
    eigenvalue = lambda,
    sobre_MP = lambda > lambda_mas
  )
  write.csv(
    tabla_eigenvalues, here("resultados", paste0("eigenvalues_", sufijo, ".csv")),
    row.names = FALSE
  )
  write.csv(
    V, here("resultados", paste0("eigenvectors_", sufijo, ".csv")),
    fileEncoding = "UTF-8"
  )

  return(metodo)
}

metodos <- c(analizar_RMT("p"), analizar_RMT("s"))
comparacion <- data.frame(
  metodo = metodos,
  N = N,
  T = T_serie,
  lambda_menos = lambda_menos,
  lambda_mas = lambda_mas
)
write.csv(comparacion, here("resultados", "comparacion.csv"), row.names = FALSE)
