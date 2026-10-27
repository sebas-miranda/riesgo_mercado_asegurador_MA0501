

# 1. Construccion original de las series
source("R/crear_L.R")
source("R/crear_X.R")

L <- crear_L("datos/datos_procesados/panel_trimestral.csv")
X <- crear_X(L)

# 2. Validar X y garantizar una matriz de correlaciones genuina.
# Cada FILA = aseguradora/ramo; cada COLUMNA = trimestre.
X <- as.matrix(X)
if (!is.numeric(X) || any(!is.finite(X))) {
  stop("X debe ser una matriz numerica sin NA, NaN ni Inf.")
}
N <- nrow(X)
T_serie <- ncol(X)
if (N < 2L || T_serie < 3L) stop("Se requieren N >= 2 y T >= 3.")

nombres <- rownames(X)
if (is.null(nombres)) nombres <- paste0("Serie_", seq_len(N))
if (anyDuplicated(nombres)) nombres <- make.unique(nombres)
rownames(X) <- nombres

sd_por_serie <- apply(X, 1, sd)
if (any(!is.finite(sd_por_serie) | sd_por_serie <= 1e-12)) {
  stop("Hay series constantes (o casi constantes); revisar crear_L/crear_X.")
}

# crear_X.R ya centra y estandariza por fila con la desviacion
# muestral. No modificamos de nuevo las series originales del proyecto.
if (any(abs(rowMeans(X)) > 1e-8) ||
    any(abs(sd_por_serie - 1) > 1e-8)) {
  stop("crear_X no entrego las series centradas y estandarizadas.")
}
C <- tcrossprod(X) / (T_serie - 1)
C <- (C + t(C)) / 2
stopifnot(max(abs(diag(C) - 1)) < 1e-8)

# 3. Espectro de C
espectro <- eigen(C, symmetric = TRUE)
lambda <- espectro$values
V <- espectro$vectors

# 4. Marchenko-Pastur clasico: bandas exploratorias
q <- N / T_serie
lambda_menos <- (1 - sqrt(q))^2
lambda_mas   <- (1 + sqrt(q))^2

if (N >= T_serie) {
  warning(paste0(
    "N >= T: MacMahon (2015) requiere el regimen T > N para su ",
    "procedimiento clasico. Con q > 1 la ley MP incluye una masa ",
    "en cero. El filtrado de este script es EXPLORATORIO."
  ))
}
warning("La banda MP clasica no ajusta autocorrelacion temporal: interpretar con cautela.")

# MacMahon (2015), ec. (14): todos los eigenvalores <= lambda_mas
# se asignan al componente de ruido, incluso los inferiores a lambda_menos.
# Aqui se aplica de forma EXPLORATORIA por la limitacion N >= T.
indices_senal <- which(lambda > lambda_mas)
indices_ruido <- which(lambda <= lambda_mas)

componente_espectral <- function(indices) {
  if (length(indices) == 0L) {
    return(matrix(0, nrow = N, ncol = N, dimnames = dimnames(C)))
  }
  # Errores negativos de redondeo se truncan solo al reconstruir.
  pesos <- sqrt(pmax(lambda[indices], 0))
  B <- sweep(V[, indices, drop = FALSE], 2, pesos, `*`)
  resultado <- tcrossprod(B)
  dimnames(resultado) <- dimnames(C)
  resultado
}

C_ruido <- componente_espectral(indices_ruido)          # C^(r)
C_senal <- C - C_ruido                                 # C^(s) = C - C^(r)
C_senal <- (C_senal + t(C_senal)) / 2
C_modo_principal <- if (1L %in% indices_senal) {
  componente_espectral(1L)                             # C^(m): diagnostico solamente
} else componente_espectral(integer(0))
C_restante <- C_senal - C_modo_principal                # C^(g): NO se optimiza Q3

# 5. Matrices filtradas para modularidad de MacMahon
# Q1(g) = sum_ij [C_ij - delta_ij] 1{g_i = g_j} / Cnorm
# Q2(g) = sum_ij [C_ij - C_ruido_ij] 1{g_i = g_j} / Cnorm
# Cnorm = sum_ij C_ij. Se CONSERVA la diagonal de C_senal:
# contribuye una constante en Q2, pero no cambia el optimo.
W1 <- C - diag(N)
W2 <- C_senal
Cnorm <- sum(C)
if (!is.finite(Cnorm) || Cnorm < 1e-10) {
  stop("Cnorm = sum(C) es casi cero; no se puede normalizar como MacMahon.")
}

modularidad <- function(W, comunidades) {
  if (length(comunidades) != nrow(W)) stop("Longitud de comunidades invalida.")
  dentro <- outer(comunidades, comunidades, `==`)
  sum(W[dentro]) / Cnorm
}

# 6. Optimizacion heuristica de la modularidad DE CORRELACIONES.
# NO usar igraph::cluster_louvain(C): la modularidad convencional
# corresponde al modelo nulo de grados y no a Q1 / Q2.
# Algoritmo: mejoras por movimientos individuales + fusiones de grupos,
# con reinicios. Es una busqueda voraz propia inspirada en Louvain,
# NO es la implementacion exacta de Louvain y NO garantiza el maximo global.
renumerar_grupos <- function(g) match(g, unique(g))

optimizar_modularidad <- function(W, reinicios = 30L, semilla = 20261026L) {
  n <- nrow(W)
  A <- W
  diag(A) <- 0  # diagonal constante: no afecta las decisiones
  set.seed(semilla)
  mejor_puntaje <- -Inf
  mejor_grupo <- seq_len(n)

  puntaje_libre <- function(g) sum(A[outer(g, g, `==`)])

  mejorar <- function(g) {
    g <- renumerar_grupos(g)
    for (iter in seq_len(120L)) {
      cambio <- FALSE

      # Movimientos de un nodo entre comunidades o hacia un singleton
      for (i in sample.int(n)) {
        vieja <- g[i]
        indices_viejos <- which(g == vieja & seq_len(n) != i)
        suma_vieja <- sum(A[i, indices_viejos])
        mejor_delta <- 1e-10
        nuevo <- vieja

        for (destino in setdiff(unique(g), vieja)) {
          delta <- 2 * (sum(A[i, g == destino]) - suma_vieja)
          if (delta > mejor_delta) {
            mejor_delta <- delta
            nuevo <- destino
          }
        }
        if (length(indices_viejos) > 0L) {
          delta_singleton <- -2 * suma_vieja
          if (delta_singleton > mejor_delta) {
            mejor_delta <- delta_singleton
            nuevo <- max(g) + 1L
          }
        }
        if (nuevo != vieja) {
          g[i] <- nuevo
          cambio <- TRUE
        }
      }
      g <- renumerar_grupos(g)

      # Fusionar el par de grupos con mayor mejora positiva (si existe)
      grupos <- unique(g)
      delta_mejor <- 1e-10
      fusion <- NULL
      if (length(grupos) > 1L) {
        for (a in seq_len(length(grupos) - 1L)) {
          for (b in seq.int(a + 1L, length(grupos))) {
            ga <- grupos[a]
            gb <- grupos[b]
            delta <- 2 * sum(A[g == ga, g == gb, drop = FALSE])
            if (delta > delta_mejor) {
              delta_mejor <- delta
              fusion <- c(ga, gb)
            }
          }
        }
      }
      if (!is.null(fusion)) {
        g[g == fusion[2]] <- fusion[1]
        g <- renumerar_grupos(g)
        cambio <- TRUE
      }
      if (!cambio) break
    }
    renumerar_grupos(g)
  }

  for (r in seq_len(reinicios)) {
    inicio <- if (r == 1L) {
      seq_len(n)              # todos separados
    } else if (r == 2L) {
      rep(1L, n)              # todos juntos
    } else {
      k <- sample(seq.int(2L, min(n, 8L)), size = 1L)
      sample.int(k, n, replace = TRUE)
    }
    g <- mejorar(inicio)
    s <- puntaje_libre(g)
    if (s > mejor_puntaje + 1e-10) {
      mejor_puntaje <- s
      mejor_grupo <- g
    }
  }
  mejor_grupo
}

# Si no hay eigenvalores por encima de MP, Q2 no contiene senal.
# Se evita presentar una particion artificial como resultado RMT.
g_Q1 <- optimizar_modularidad(W1)
g_Q2 <- if (length(indices_senal) > 0L) {
  optimizar_modularidad(W2)
} else {
  rep(NA_integer_, N)
}

# 7. Comparacion Q1 vs Q2 sobre particiones comunes
una <- rep(1L, N)
separados <- seq_len(N)
particiones <- list(
  `Todos juntos` = una,
  `Todos separados` = separados,
  `Optimo heuristico Q1` = g_Q1
)
if (!all(is.na(g_Q2))) particiones$`Optimo heuristico Q2` <- g_Q2

resumen_modularidad <- do.call(rbind, lapply(names(particiones), function(nombre) {
  g <- particiones[[nombre]]
  data.frame(
    particion = nombre,
    n_comunidades = length(unique(g)),
    Q1 = modularidad(W1, g),
    Q2 = modularidad(W2, g)
  )
}))
rownames(resumen_modularidad) <- NULL

asignaciones <- data.frame(
  serie = nombres,
  comunidad_Q1 = g_Q1,
  comunidad_Q2 = g_Q2,
  stringsAsFactors = FALSE
)

diagnostico <- data.frame(
  N = N,
  T = T_serie,
  q = q,
  lambda_menos = lambda_menos,
  lambda_mas = lambda_mas,
  lambda_maxima = max(lambda),
  rango_empirico = sum(lambda > 1e-8),
  autovalores_casi_cero = sum(abs(lambda) < 1e-8),
  norma_F_C_grupos_Q3 = sqrt(sum(C_restante^2)),
  autovalores_sobre_MP = length(indices_senal),
  modos_secundarios_sobre_MP = max(0L, length(indices_senal) - 1L),
  masa_cero_teorica_q_mayor_1 = if (q > 1) 1 - 1/q else 0,
  porcentaje_mismo_signo_v1 = max(mean(V[, 1] > 0), mean(V[, 1] < 0)) * 100,
  Cnorm = Cnorm
)

# 8. Archivos de salida (en la raiz del proyecto; no dentro de R/)
outdir <- "resultados/RMT_modularidad"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(indice = seq_along(lambda), eigenvalor = lambda,
                     senal_MP = seq_along(lambda) %in% indices_senal),
          file.path(outdir, "eigenvalores.csv"), row.names = FALSE)
write.csv(C, file.path(outdir, "correlacion_empirica.csv"))
write.csv(C_ruido, file.path(outdir, "C_ruido.csv"))
write.csv(C_senal, file.path(outdir, "C_senal_Q2.csv"))
write.csv(resumen_modularidad, file.path(outdir, "comparacion_Q1_Q2.csv"), row.names = FALSE)
write.csv(asignaciones, file.path(outdir, "comunidades_Q1_Q2.csv"), row.names = FALSE)
write.csv(diagnostico, file.path(outdir, "diagnostico_RMT.csv"), row.names = FALSE)

png(file.path(outdir, "espectro_MP.png"), width = 1000, height = 600)
plot(lambda, type = "h", lwd = 2,
     main = "Espectro empirico y borde superior MP (exploratorio)",
     xlab = "Indice del valor propio", ylab = "Valor propio")
abline(h = lambda_mas, col = "red", lty = 2, lwd = 2)
abline(h = lambda_menos, col = "blue", lty = 3)
legend("topright", c("Limite superior MP", "Limite inferior MP"),
       col = c("red", "blue"), lty = c(2, 3), bty = "n")
dev.off()

cat("\n========== RMT ==========\n")
print(diagnostico, row.names = FALSE)
cat("\nEigenvalores que exceden lambda+ :\n")
print(lambda[indices_senal])
cat("\n========== MODULARIDAD (MacMahon, 2015) ==========\n")
print(resumen_modularidad, row.names = FALSE, digits = 5)
cat("\nArchivos guardados en: ", outdir, "\n", sep = "")

if (length(indices_senal) <= 1L) {
  cat("\nJUSTIFICACION Q3: Solo hay ", length(indices_senal),
      " modo(s) sobre lambda+. Si se sustrae tambien el modo principal,",
      " C^(g) queda nula (salvo redondeo): Q3 no distingue particiones.\n", sep = "")
} else {
  cat("\nJUSTIFICACION Q3: Hay modos secundarios, pero antes de restar el",
      " maximo debe justificarse empiricamente un factor GLOBAL,",
      " no asumir que el autovalor dominante es un modo mercado.\n")
}
cat("Advertencia: q >= 1 y/o autocorrelacion debilitan la calibracion MP clasica.\n")
