library(ggplot2)

source("R/2_crear_L.R")
source("R/3_crear_X.R")

L <- crear_L("datos/datos_procesados/panel_trimestral.csv")

X_pearson <- crear_X(L, metodo = "pearson")
X_spearman <- crear_X(L, metodo = "spearman")


# Para cada matriz de correlacion, hacemos el analisis de RMT
analizar_RMT <- function(X, metodo) {
# Entradas:
# X:     Matriz estandarizada del LR
# metodo: Ya sea con spearman o pearson

X <- as.matrix(X)

N <- nrow(X)
T_serie <- ncol(X)
nombres <- rownames(X)

C <- X %*% t(X) / (T_serie - 1)
# Para eliminar errores de redondeo, promediamos a C con su misma transpuesta:
C <- (C + t(C)) / 2
  
dimnames(C) <- list(nombres, nombres)

# Calculamos los eigenvalues de mayor a menor

espectro <- eigen(C, symmetric = TRUE)

# Extraemos como tal los calores
lambda <- espectro$values
# Y ademas los eigenvectors
V <- espectro$vectors
# Conservamos aca tambien los nombres de los nodos en cada fila de un eigenvector
rownames(V) <- nombres

# 4. Calculamos las bandas Marchenko-Pastur
q <- N/(T_serie-1)
lambda_menos <- (1 - sqrt(q))^2
lambda_mas   <- (1 + sqrt(q))^2

# Si q > 1, existen eigenvalues en 0 (MP tiene masa puntual en 0), zeroizamos
# para evitar puntos negativos.
masa_cero_MP <- max(0, 1 - 1 / q)

# Los eigenvalues mayores a la banda de ruido
indices_senal <- which(lambda > lambda_mas)
# Los eigenvalues que se encuentren bajo la banda maxima de ruido
indices_ruido <- which(lambda <= lambda_mas)
# Los eigenvalues que se encuentran dentro de la banda de ruido
 indices_banda <- which(lambda >= lambda_menos & lambda <= lambda_mas)
# Eigenvalues bajo el extremo menor de la banda:
indices_bajo <- which(lambda < lambda_menos & lambda >= 1e-8)  
# Eigenvalues nulos, defina una tolerancia, eigenvalues menores a este implican cero
indices_cero <- which(abs(lambda) < 1e-8)


# Construimos la tabla de los eigenvalues
tabla_eigenvalues <- data.frame(
  posicion = seq_along(lambda),
  eigenvalue = lambda,
  sobre_MP = seq_along(lambda) %in% indices_senal,
  dentro_banda_MP = seq_along(lambda) %in% indices_banda,
  casi_cero = seq_along(lambda) %in% indices_cero
)
  
# Hacemos un breve resumen
  diagnostico <- data.frame(
    metodo = metodo,
    N = N,
    T = T_serie,
    q = q,
    lambda_menos = lambda_menos,
    lambda_mas = lambda_mas,
    masa_cero_MP = masa_cero_MP,
    lambda_maxima = lambda[1],
    peso_lambda_maximo = lambda[1] / sum(lambda),
    autovalores_sobre_MP = length(indices_senal),
    autovalores_dentro_banda = length(indices_banda),
    autovalores_bajo_banda = length(indices_bajo),
    autovalores_casi_cero = length(indices_cero)
  ) 
  
  
return(list(
    metodo = metodo,
    N = N,
    T = T_serie,
    C = C,
    lambda = lambda,
    V = V,
    q = q,
    lambda_menos = lambda_menos,
    lambda_mas = lambda_mas,
    indices_senal = indices_senal,
    indices_cero = indices_cero,
    tabla_eigenvalues = tabla_eigenvalues,
    diagnostico = diagnostico
  ))
}

rmt_pearson <- analizar_RMT(X_pearson, "Pearson")
rmt_spearman <- analizar_RMT(X_spearman, "Spearman")



graficar_MP <- function(resultado) {

  datos <- resultado$tabla_eigenvalues

  grafico <- ggplot(
    datos,
    aes(x = posicion, y = eigenvalue, fill = sobre_MP)
  ) +

    # Barras de los valores propios
    geom_col(width = 0.8) +

    # Limite superior de Marchenko-Pastur
    geom_hline(
      data = datos[1, , drop = FALSE],
      aes(yintercept = resultado$lambda_mas, color = "Superior"),
      linetype = "dashed",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +

    # Limite inferior de Marchenko-Pastur
    geom_hline(
      data = datos[1, , drop = FALSE],
      aes(yintercept = resultado$lambda_menos, color = "Inferior"),
      linetype = "dashed",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +

    # Leyenda con los valores numericos de ambas bandas
    scale_color_manual(
      name = NULL,
      values = c(
        "Superior" = "#FF0000",
        "Inferior" = "#0066CC"
      ),
      breaks = c("Superior", "Inferior"),
      labels = c(
        paste0("λ+ = ", sprintf("%.6f", resultado$lambda_mas)),
        paste0("λ- = ", sprintf("%.6f", resultado$lambda_menos))
      )
    ) +

    # Valores propios superiores a MP en rojo
    scale_fill_manual(
      values = c(
        "FALSE" = "#808080",
        "TRUE" = "#FF0000"
      ),
      guide = "none"
    ) +

    # Etiqueta sobre la barra del mayor valor propio
    # which.max() devuelve la posicion del valor mas grande.
    geom_text(
      data = datos[which.max(datos$eigenvalue), , drop = FALSE],
      aes(
        label = paste0(
          "λmax = ",
          round(eigenvalue, 3)
        )
      ),
      vjust = -0.6,
      size = 4.5,
      color = "black"
    ) +

    labs(
      title = "Valores propios, bandas Marchenko-Pastur",
      subtitle = paste0(
        "Bajo ", resultado$metodo, ". ",
        resultado$N, " ramos (Series) y ",
        resultado$T, " trimestres (Períodos)"
      ),
      x = "Índice",
      y = "Valor propio",
      caption = paste0(
        length(resultado$indices_cero),
        " valores propios prácticamente nulos"
      )
    ) +

    scale_x_continuous(
      breaks = seq(1, resultado$N, by = 5)
    ) +

    # Espacio para la etiqueta sobre el mayor valor propio
    expand_limits(
      y = max(resultado$lambda) * 1.15
    ) +

    theme_minimal(base_size = 13) +

    theme(
      # Leyenda en la esquina superior derecha
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1),

      legend.background = element_rect(
        fill = "white",
        color = "gray70",
        linewidth = 0.3
      ),

      # Aumentamos el tamaño del cuadro y sus elementos
      legend.key.width = grid::unit(1.4, "cm"),
      legend.key.height = grid::unit(0.6, "cm"),
      legend.text = element_text(size = 12),
      legend.margin = margin(9, 12, 9, 12),

      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "black")
    )

  return(grafico)
}


# Generamos ambos graficos
grafico_pearson <- graficar_MP(rmt_pearson)
grafico_spearman <- graficar_MP(rmt_spearman)

print(grafico_pearson)
print(grafico_spearman)


# Guardamos los resultados de Pearson, RDS es un objeto, en este caso contendra la matriz C,
# los eigenvalues, los eigenvectors, y el eigenvalue maximo.
saveRDS(
  list(
    C = rmt_pearson$C,
    lambda = rmt_pearson$lambda,
    V = rmt_pearson$V,
    lambda_mas = rmt_pearson$lambda_mas
  ),
  "resultados/RMT_pearson.rds"
)

# Analogamente, para Spearman
saveRDS(
  list(
    C = rmt_spearman$C,
    lambda = rmt_spearman$lambda,
    V = rmt_spearman$V,
    lambda_mas = rmt_spearman$lambda_mas
  ),
  "resultados/RMT_spearman.rds"
)

# Guardamos la tabla de eigenvalues de pearson
write.csv(
  rmt_pearson$tabla_eigenvalues,
  "resultados/eigenvalues_pearson.csv",
  row.names = FALSE
)
# Analogamente, para Spearman:
write.csv(
  rmt_spearman$tabla_eigenvalues,
  "resultados/eigenvalues_spearman.csv",
  row.names = FALSE
)

# Guardamos el resumen 
diagnostico <- rbind(
  rmt_pearson$diagnostico,
  rmt_spearman$diagnostico
)
write.csv(
  diagnostico,
  "resultados/diagnostico_RMT.csv",
  row.names = FALSE
)
# Finalmente, los graficos de los eigenvalues para Pearson y Spearman

ggsave(
  file.path("graficos", "rmt_pearson.png"),
  plot = grafico_pearson,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path("graficos", "rmt_spearman.png"),
  plot = grafico_spearman,
  width = 10,
  height = 6,
  dpi = 300
)