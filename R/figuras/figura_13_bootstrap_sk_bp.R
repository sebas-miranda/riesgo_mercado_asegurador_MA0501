# Grafica el bootstrap por bloques de los recortes BP y SK de la matriz de Spearman.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda resultados/bootstrap_sk_bp.csv y graficos/5_bootstrap/bootstrap_sk_bp.pdf
# (Figura 13, fig:bootstrap_sk_bp, de informes/paper_final.tex).
#
# Recorte de Bouchaud y Potters (BP) frente a la línea oblicua de Sharifi et al.
# (2004, sección 4.3), basada en Krzanowski (1984):
# - bootstrap por bloques móviles de 4 trimestres: se pegan 10 bloques con inicio
#   aleatorio y se recorta a 39 trimestres; los mismos 300 remuestreos para los dos métodos.
# - los autovalores bajo el borde de MP, (1 + sqrt(N/T))^2, se cambian:
#   BP: todos por su promedio (como alpha = 1 del paso 5).
#   SK: por una recta decreciente con la misma suma cuyo último valor es 0.
# - cada matriz remuestreada se compara con la del panel completo limpiada con el mismo
#   método: cambio absoluto medio, correlación entre matrices y cuántas de las 60 aristas
#   más fuertes se mantienen.

library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")
source(here("R", "analisis", "2_crear_matriz_lr.R"), encoding = "UTF-8")

L <- crear_L(here("datos", "datos_procesados", "panel_trimestral.csv"))
N <- nrow(L)
T_serie <- ncol(L)
lambda_mas <- (1 + sqrt(N / T_serie))^2

n_remuestreos <- 300
largo_bloque <- 4
n_aristas <- 60

limpiar <- function(C, metodo) {
  e <- eigen(C, symmetric = TRUE)
  lambda <- pmax(e$values, 0)
  ruido <- which(e$values < lambda_mas)
  if (metodo == "BP") {
    lambda[ruido] <- mean(lambda[ruido])
  } else {
    k <- length(ruido)
    paso <- 2 * sum(lambda[ruido]) / (k * (k - 1))
    # ruido va de mayor a menor: el primero recibe (k - 1) * paso y el último 0
    lambda[ruido] <- paso * ((k - 1):0)
  }
  # se reconstruye V D V' y se lleva la diagonal a 1
  A <- e$vectors %*% diag(lambda) %*% t(e$vectors)
  cov2cor((A + t(A)) / 2)
}

superior <- upper.tri(diag(N))
aristas_fuertes <- function(C) {
  order(abs(C[superior]), decreasing = TRUE)[1:n_aristas]
}

C_completa <- cor(t(L), method = "spearman")
referencia <- list(BP = limpiar(C_completa, "BP"), SK = limpiar(C_completa, "SK"))

set.seed(20261120)
n_bloques <- ceiling(T_serie / largo_bloque)
resultados <- data.frame()
for (b in 1:n_remuestreos) {
  # se repite el sorteo si alguna serie queda constante
  repeat {
    inicios <- sample.int(T_serie - largo_bloque + 1, n_bloques, replace = TRUE)
    columnas <- c()
    for (i in inicios) {
      columnas <- c(columnas, i:(i + largo_bloque - 1))
    }
    Lb <- L[, columnas[1:T_serie]]
    if (all(apply(Lb, 1, function(x) length(unique(x)) > 1))) break
  }
  Cb <- cor(t(Lb), method = "spearman")

  for (metodo in c("BP", "SK")) {
    limpia <- limpiar(Cb, metodo)
    ref <- referencia[[metodo]]
    resultados <- rbind(resultados, data.frame(
      remuestreo = b, metodo = metodo,
      cambio = mean(abs(limpia[superior] - ref[superior])),
      correlacion = cor(limpia[superior], ref[superior]),
      retencion = length(intersect(aristas_fuertes(limpia), aristas_fuertes(ref))) / n_aristas
    ))
  }
}

write.csv(resultados, here("resultados", "bootstrap_sk_bp.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(aggregate(cbind(cambio, correlacion, retencion) ~ metodo, resultados, median))

# figura: cajas horizontales como las del LR del INS (figura 4), con la mediana escrita
# a la derecha; los tres paneles usan la misma escala de 0 a 1
numero <- function(x, digitos = 2) {
  sub("^-", "−", formatC(x, format = "f", digits = digitos, decimal.mark = ","))
}
nombres_metodo <- c(BP = "Bouchaud–Potters", SK = "Sharifi–Krzanowski")
contorno <- c(oscurecer(pal$primas, 0.35), oscurecer(pal$ciruela, 0.35))
relleno <- c(aclarar(pal$primas, 0.5), aclarar(pal$ciruela, 0.5))
names(contorno) <- nombres_metodo
names(relleno) <- nombres_metodo
x_mediana <- 1.2 # columna con el valor de la mediana

tema <- theme_classic(base_size = 9, base_family = "sans") +
  theme(
    text = element_text(colour = pal$tinta),
    axis.text = element_text(colour = pal$tinta, size = 7.5),
    axis.text.y = element_text(size = 7.5, margin = margin(r = 2)),
    axis.ticks.x = element_line(colour = pal$tinta, linewidth = 0.25),
    axis.ticks.length = grid::unit(1.8, "pt"),
    axis.line.x = element_line(colour = pal$tinta, linewidth = 0.25),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    panel.grid.major.x = element_line(colour = aclarar(pal$borde, 0.45), linewidth = 0.25),
    plot.title = element_text(size = 8, face = "bold", hjust = 0, margin = margin(b = 1)),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(2, 6, 2, 2)
  )

panel_metrica <- function(columna, titulo, digitos = 2, con_eje = FALSE) {
  d <- data.frame(metodo = factor(nombres_metodo[resultados$metodo], rev(nombres_metodo)),
                  valor = resultados[[columna]])
  medianas <- aggregate(valor ~ metodo, d, median)
  atipicos <- data.frame()
  for (m in levels(d$metodo)) {
    x <- d[d$metodo == m, ]
    atipicos <- rbind(atipicos, x[x$valor %in% boxplot.stats(x$valor)$out, ])
  }

  p <- ggplot(d, aes(valor, metodo)) +
    geom_boxplot(aes(fill = metodo, colour = metodo), width = 0.6, linewidth = 0.4,
                 outlier.shape = NA, staplewidth = 0) +
    geom_point(data = atipicos, aes(fill = metodo), shape = 21, colour = "white",
               stroke = 0.3, size = 1.3) +
    geom_text(data = medianas, aes(x = x_mediana, y = metodo, label = numero(valor, digitos)),
              hjust = 1, size = 6.8, size.unit = "pt", colour = pal$tinta) +
    annotate("text", x = x_mediana, y = 2.8, label = "Mediana", hjust = 1,
             size = 6.8, size.unit = "pt", fontface = "bold", colour = pal$tinta) +
    scale_fill_manual(values = relleno) +
    scale_colour_manual(values = contorno) +
    scale_x_continuous(breaks = seq(0, 1, 0.25), labels = numero) +
    coord_cartesian(xlim = c(-0.05, x_mediana), ylim = c(0.45, 2.9), expand = FALSE, clip = "off") +
    labs(title = titulo, x = NULL, y = NULL) +
    tema
  # los números del eje van solo en el panel de abajo
  if (!con_eje) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  p
}

figura <- panel_metrica("cambio", "(a) Cambio absoluto medio (menor es mejor)", digitos = 3) /
  panel_metrica("correlacion", "(b) Correlación entre matrices (mayor es mejor)") /
  panel_metrica("retencion", "(c) Retención de las 60 aristas (mayor es mejor)", con_eje = TRUE)

ggsave(here("graficos", "5_bootstrap", "bootstrap_sk_bp.pdf"), figura,
       width = 84.95 / 25.4, height = 2.8, device = cairo_pdf, bg = "white")
