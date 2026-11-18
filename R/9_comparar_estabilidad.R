# R/9_comparar_estabilidad.R
# Comparacion de estabilidad de Spearman:
# Bouchaud-Potters (clipping uniforme) vs. Sharifi-Krzanowski (recta inclinada).
# Ejecutar desde la raiz del proyecto, despues de los scripts 1, 4, 5, 6 y 7.
# No modifica los resultados de las limpiezas existentes.

library(ggplot2)
source("R/2_crear_L.R")
source("R/3_crear_X.R")

# 1. Parametros del experimento
set.seed(2026)
B <- 300L             # Cantidad de remuestreos (se recomienda >= 300)
longitud_bloque <- 4L # Cuatro trimestres consecutivos: un ano
K_aristas <- 60L     # Numero fijo de conexiones mas fuertes por |correlacion|

# 2. Cargar los datos originales y las dos matrices finales de referencia
L <- crear_L("datos/datos_procesados/panel_trimestral.csv")
N <- nrow(L)
T_serie <- ncol(L)
stopifnot(N > 2L, T_serie > 2L, B >= 2L,
          longitud_bloque >= 1L, longitud_bloque <= T_serie,
          K_aristas >= 1L, K_aristas <= N * (N - 1L) / 2L)

leer_C <- function(ruta) {
  C <- as.matrix(read.csv(ruta, row.names = 1, check.names = FALSE,
                          fileEncoding = "UTF-8"))
  storage.mode(C) <- "double"
  if (!identical(rownames(C), colnames(C))) {
    stop("Los nombres de filas y columnas no coinciden en: ", ruta)
  }
  if (!setequal(rownames(C), rownames(L))) {
    stop("La matriz no contiene exactamente los nodos de L: ", ruta)
  }
  C <- C[rownames(L), rownames(L), drop = FALSE]
  if (any(!is.finite(C))) stop("Valores no finitos en: ", ruta)
  C
}

C_base_potters <- leer_C("resultados/correlacion_spearman_limpia.csv")
C_base_sharifi <- leer_C("resultados/correlacion_spearman_sharifi.csv")

# 3. Crear la correlacion de Spearman a partir de cualquier remuestreo de L
calcular_C <- function(L_muestra) {
  X <- crear_X(L_muestra, metodo = "spearman")
  C <- tcrossprod(X) / (ncol(X) - 1L)
  C <- (C + t(C)) / 2
  dimnames(C) <- list(rownames(L_muestra), rownames(L_muestra))
  C
}

# 4. Aplicar AMBAS limpiezas a una misma matriz empirica.
# Se replica la regla del script 5: ruido = lambda <= (1 + sqrt(N/T))^2.
# La parametrizacion de Sharifi corresponde a la del script 7 del proyecto.
limpiar_C <- function(C, metodo, T_observaciones) {
  descomp <- eigen(C, symmetric = TRUE)
  lambda <- pmax(descomp$values, 0)
  V <- descomp$vectors
  lambda_mas <- (1 + sqrt(nrow(C) / T_observaciones))^2
  idx_ruido <- which(lambda <= lambda_mas)
  k <- length(idx_ruido)
  nuevos <- lambda

  if (k > 0L) {
    media_ruido <- mean(lambda[idx_ruido])
    if (metodo == "Potters") {
      nuevos[idx_ruido] <- media_ruido
    } else if (metodo == "Sharifi") {
      pendiente <- if (k > 1L) 2 * media_ruido / (k - 1L) else 0
      nuevos[idx_ruido] <- media_ruido +
        pendiente * ((k + 1) / 2 - seq_len(k))
      nuevos[idx_ruido] <- pmax(nuevos[idx_ruido], 0)
    } else {
      stop("Metodo de limpieza desconocido: ", metodo)
    }
  }

  C_reconstruida <- V %*% diag(nuevos, nrow = length(nuevos)) %*% t(V)
  if (any(diag(C_reconstruida) <= 0)) {
    stop("La matriz reconstruida tiene diagonal no positiva.")
  }
  C_final <- cov2cor(C_reconstruida)
  C_final <- (C_final + t(C_final)) / 2
  dimnames(C_final) <- dimnames(C)
  C_final
}

# Comprobar que el experimento reproduce los CSV existentes.
# Si falla, los CSV pueden proceder de otra version del panel o de otra regla.
C_original <- calcular_C(L)
for (metodo in c("Potters", "Sharifi")) {
  C_calculada <- limpiar_C(C_original, metodo, T_serie)
  C_guardada <- if (metodo == "Potters") C_base_potters else C_base_sharifi
  diferencia <- max(abs(C_calculada - C_guardada))
  if (diferencia > 1e-6) {
    stop("Inconsistencia con el CSV de ", metodo,
         ": diferencia maxima = ", signif(diferencia, 5),
         ". Verifique el panel y los scripts 5, 6 y 7.")
  }
}

# 5. Remuestreo circular por bloques consecutivos, usando las mismas
# posiciones temporales para los dos metodos en cada repeticion.
# Preserva dependencia local; no prueba estacionariedad.
indices_bootstrap <- function(T_serie, longitud_bloque) {
  n_bloques <- ceiling(T_serie / longitud_bloque)
  comienzos <- sample.int(T_serie, n_bloques, replace = TRUE)
  indices <- unlist(lapply(comienzos, function(inicio) {
    ((inicio - 1L + seq_len(longitud_bloque) - 1L) %% T_serie) + 1L
  }), use.names = FALSE)
  indices[seq_len(T_serie)]
}

# 6. Medidas de estabilidad de la matriz y de sus conexiones.
pares <- which(upper.tri(C_base_potters))
seleccionar_aristas <- function(C, K) {
  valores <- C[pares]
  order(-abs(valores), seq_along(valores))[seq_len(K)]
}

referencias <- list(Potters = C_base_potters, Sharifi = C_base_sharifi)
aristas_base <- lapply(referencias, seleccionar_aristas, K = K_aristas)
conteo_aristas <- list(Potters = integer(length(pares)), Sharifi = integer(length(pares)))
resultados <- vector("list", 2L * B)
matrices_boot <- list(
  Potters = array(NA_real_, dim = c(N, N, B)),
  Sharifi = array(NA_real_, dim = c(N, N, B))
)

for (b in seq_len(B)) {
  posiciones <- indices_bootstrap(T_serie, longitud_bloque)
  # Se remuestrean simultaneamente las columnas de TODOS los nodos.
  L_b <- L[, posiciones, drop = FALSE]
  C_b <- calcular_C(L_b)

  for (metodo in names(referencias)) {
    C_limpia_b <- limpiar_C(C_b, metodo, T_serie)
    matrices_boot[[metodo]][, , b] <- C_limpia_b
    C_ref <- referencias[[metodo]]

    x_ref <- C_ref[pares]
    x_b <- C_limpia_b[pares]
    aristas_b <- seleccionar_aristas(C_limpia_b, K_aristas)
    conteo_aristas[[metodo]][aristas_b] <- conteo_aristas[[metodo]][aristas_b] + 1L
    retencion <- length(intersect(aristas_base[[metodo]], aristas_b)) / K_aristas

    resultados[[(b - 1L) * 2L + match(metodo, names(referencias))]] <- data.frame(
      replica = b,
      metodo = metodo,
      cambio_absoluto_medio = mean(abs(x_b - x_ref)),
      correlacion_matrices = if (sd(x_ref) > 0 && sd(x_b) > 0)
        cor(x_ref, x_b) else NA_real_,
      retencion_topK = retencion
    )
  }
}
metricas <- do.call(rbind, resultados)
dir.create("resultados", showWarnings = FALSE, recursive = TRUE)
dir.create("graficos", showWarnings = FALSE, recursive = TRUE)
write.csv(metricas, "resultados/estabilidad_spearman_replicas.csv", row.names = FALSE)

# 7. Resumen numerico de las distribuciones bootstrap
nombres_metricas <- c("cambio_absoluto_medio", "correlacion_matrices", "retencion_topK")
resumen <- do.call(rbind, lapply(names(referencias), function(metodo) {
  do.call(rbind, lapply(nombres_metricas, function(nombre) {
    x <- metricas[metricas$metodo == metodo, nombre]
    data.frame(
      metodo = metodo, metrica = nombre,
      media = mean(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE),
      p05 = unname(quantile(x, 0.05, na.rm = TRUE)),
      p95 = unname(quantile(x, 0.95, na.rm = TRUE))
    )
  }))
}))
write.csv(resumen, "resultados/estabilidad_spearman_resumen.csv", row.names = FALSE)

# Diferencias PAREADAS: positivo favorece a Sharifi en las tres columnas.
p <- metricas[metricas$metodo == "Potters", ]
s <- metricas[metricas$metodo == "Sharifi", ]
stopifnot(identical(p$replica, s$replica))
diferencias <- data.frame(
  replica = p$replica,
  mejora_sharifi_cambio = p$cambio_absoluto_medio - s$cambio_absoluto_medio,
  mejora_sharifi_correlacion = s$correlacion_matrices - p$correlacion_matrices,
  mejora_sharifi_aristas = s$retencion_topK - p$retencion_topK
)
write.csv(diferencias, "resultados/estabilidad_spearman_diferencias_pareadas.csv",
          row.names = FALSE)

# 8. Variabilidad de correlaciones y persistencia de las aristas en cada par
pares_matriz <- which(upper.tri(C_base_potters), arr.ind = TRUE)
variabilidad <- do.call(rbind, lapply(names(referencias), function(metodo) {
  A <- matrices_boot[[metodo]]
  C_ref <- referencias[[metodo]]
  datos <- do.call(rbind, lapply(seq_len(nrow(pares_matriz)), function(j) {
    i1 <- pares_matriz[j, 1]
    i2 <- pares_matriz[j, 2]
    valores <- A[i1, i2, ]
    data.frame(
      metodo = metodo,
      nodo1 = rownames(L)[i1],
      nodo2 = rownames(L)[i2],
      correlacion_base = C_ref[i1, i2],
      desviacion_bootstrap = sd(valores),
      cambio_absoluto_medio = mean(abs(valores - C_ref[i1, i2])),
      en_topK_base = j %in% aristas_base[[metodo]],
      frecuencia_topK = conteo_aristas[[metodo]][j] / B
    )
  }))
  datos
}))
write.csv(variabilidad, "resultados/estabilidad_spearman_pares.csv", row.names = FALSE)

# 9. Graficos: distribuciones de estabilidad y comparacion pareada
metricas_largas <- rbind(
  data.frame(replica = metricas$replica, metodo = metricas$metodo,
             indicador = "Cambio absoluto medio (menor = mejor)",
             valor = metricas$cambio_absoluto_medio),
  data.frame(replica = metricas$replica, metodo = metricas$metodo,
             indicador = "Correlacion de matrices (mayor = mejor)",
             valor = metricas$correlacion_matrices),
  data.frame(replica = metricas$replica, metodo = metricas$metodo,
             indicador = paste0("Retencion de las ", K_aristas, " aristas (mayor = mejor)"),
             valor = metricas$retencion_topK)
)

p_distribuciones <- ggplot(metricas_largas, aes(x = metodo, y = valor, fill = metodo)) +
  geom_boxplot(width = 0.55, outlier.alpha = 0.2) +
  facet_wrap(~ indicador, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c(Potters = "#3465A4", Sharifi = "#D95F02")) +
  labs(title = "Estabilidad de Spearman bajo bootstrap por bloques",
       subtitle = paste0(B, " remuestreos; bloque = ", longitud_bloque,
                         " trimestres; mismos remuestreos para ambos metodos"),
       x = NULL, y = "Valor") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
ggsave("graficos/estabilidad_spearman_distribuciones.png",
       p_distribuciones, width = 10, height = 11, bg = "white")

# Comparacion pareada por muestra: puntos por encima de la diagonal favorecen
# a Sharifi para las metricas donde se desea un valor MAYOR.
p_pareado <- ggplot(data.frame(
  potters = p$retencion_topK, sharifi = s$retencion_topK
), aes(x = potters, y = sharifi)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.35) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Persistencia de conexiones: comparacion pareada",
       subtitle = paste0("Fraccion de las ", K_aristas,
                         " conexiones originales que persisten en cada remuestreo"),
       x = "Bouchaud-Potters", y = "Sharifi-Krzanowski") +
  theme_minimal(base_size = 12)
ggsave("graficos/estabilidad_spearman_aristas.png",
       p_pareado, width = 8, height = 7, bg = "white")

# Grafico de dispersion (variabilidad) por pareja: los mismos nodos en ambos.
pares_wide <- merge(
  variabilidad[variabilidad$metodo == "Potters",
                c("nodo1", "nodo2", "desviacion_bootstrap")],
  variabilidad[variabilidad$metodo == "Sharifi",
                c("nodo1", "nodo2", "desviacion_bootstrap")],
  by = c("nodo1", "nodo2"), suffixes = c("_potters", "_sharifi")
)
p_pares <- ggplot(pares_wide, aes(x = desviacion_bootstrap_potters,
                                  y = desviacion_bootstrap_sharifi)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.4, size = 1.4) +
  coord_equal() +
  labs(title = "Variabilidad bootstrap de cada correlacion",
       subtitle = "Cada punto representa un par aseguradora-ramo",
       x = "Desviacion bootstrap: Potters",
       y = "Desviacion bootstrap: Sharifi") +
  theme_minimal(base_size = 12)
ggsave("graficos/estabilidad_spearman_correlaciones.png",
       p_pares, width = 8, height = 7, bg = "white")

print(resumen)
cat("\nResultados guardados en resultados/ y graficos/.\n")
cat("Interpretacion: menor cambio absoluto y menor desviacion por par, mejor.\n")
cat("Mayor correlacion entre matrices y retencion topK, mejor.\n")
cat("El bootstrap mide estabilidad, NO exactitud de la matriz poblacional.\n")
