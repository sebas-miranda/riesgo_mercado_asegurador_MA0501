# Grafica la matriz triangular con Pearson abajo y Spearman arriba, a una columna del
# artículo y con solo los índices de la Tabla 1 en los ejes.
# Lee resultados/correlacion_pearson_empirica.csv y resultados/correlacion_spearman_empirica.csv.
# Guarda resultados/orden_carteras_matriz.csv con el índice de cada cartera e
# graficos/2_correlacion/matriz_triangular_una_columna.pdf
# (Figura 6, fig:matriz_triangular_pearson_spearman, de informes/paper_final.tex).

library(here)
library(ggplot2)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

C_pearson <- as.matrix(read.csv(here("resultados", "correlacion_pearson_empirica.csv"),
                                row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
C_spearman <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                                 row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))

# orden jerárquico de Spearman: deja juntas las carteras más correlacionadas
orden <- hclust(as.dist(1 - C_spearman), method = "average")$order
nodos <- rownames(C_spearman)[orden]
N <- length(nodos)

# el índice de cada cartera se guarda para la Tabla 1 del artículo
write.csv(data.frame(indice = 1:N, cartera = nodos),
          here("resultados", "orden_carteras_matriz.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Pearson va en el triángulo inferior y Spearman en el superior
C_triangular <- C_pearson[nodos, nodos]
superior <- upper.tri(C_triangular)
C_triangular[superior] <- C_spearman[nodos, nodos][superior]
# la diagonal se deja vacía porque encima va una celda gris
diag(C_triangular) <- NA

# una fila por celda: fila i desde arriba y columna j desde la izquierda
celdas <- expand.grid(fila = 1:N, columna = 1:N)
celdas$valor <- C_triangular[cbind(celdas$fila, celdas$columna)]
celdas$y <- N + 1 - celdas$fila
diagonal <- data.frame(x = 1:N, y = N + 1 - (1:N))

# los tres pares de la tabla de correlaciones se marcan en los dos triángulos
serie_1 <- c("DAVIVIENDA-Pérdidas pecuniarias", "MNK-Automóvil", "INS-Incendio y líneas aliadas")
serie_2 <- c("ASSA-Incendio y líneas aliadas", "ISCR-Salud", "ISCR-Otros daños a los bienes")
fila_par <- pmin(match(serie_1, nodos), match(serie_2, nodos))
columna_par <- pmax(match(serie_1, nodos), match(serie_2, nodos))
celdas_pares <- data.frame(numero = c(1:3, 1:3),
                           x = c(columna_par, fila_par),
                           y = N + 1 - c(fila_par, columna_par))
celdas_pares$x_rotulo <- celdas_pares$x + 1.9
celdas_pares$y_rotulo <- celdas_pares$y - 1.9

# cuadrito con la mitad del triángulo de cada método pintada
icono <- function(x0, y0, lado, arriba) {
  caja <- data.frame(x = x0 + c(0, lado, lado, 0), y = y0 + c(0, 0, lado, lado))
  if (arriba) {
    mitad <- data.frame(x = x0 + c(0, lado, lado), y = y0 + c(lado, lado, 0))
  } else {
    mitad <- data.frame(x = x0 + c(0, 0, lado), y = y0 + c(lado, 0, 0))
  }
  list(
    annotate("polygon", x = caja$x, y = caja$y, fill = "white",
             colour = pal$tinta, linewidth = 0.25),
    annotate("polygon", x = mitad$x, y = mitad$y, fill = pal$tinta, colour = NA)
  )
}
lado <- 1.25
separacion <- 1.15

figura <- ggplot(celdas, aes(columna, y)) +
  geom_tile(aes(fill = valor), colour = "white", linewidth = 0.08) +
  # la diagonal va en gris y separa los dos triángulos
  geom_tile(data = diagonal, aes(x, y), inherit.aes = FALSE,
            fill = pal$borde, colour = "white", linewidth = 0.08) +
  # recta de esquina a esquina que marca el límite entre Pearson y Spearman
  annotate("segment", x = 0.5, y = N + 0.5, xend = N + 0.5, yend = 0.5,
           colour = pal$tinta, linewidth = 0.3) +
  annotate("rect", xmin = 0.5, xmax = N + 0.5, ymin = 0.5, ymax = N + 0.5,
           fill = NA, colour = pal$tinta, linewidth = 0.3) +
  # Spearman arriba a la derecha y Pearson abajo a la izquierda
  icono(N + 0.5 - lado, N + 0.5 + separacion, lado, TRUE) +
  annotate("text", x = N + 0.5 - lado - 0.7, y = N + 0.5 + separacion + lado / 2,
           label = 'bold("Spearman")~"(triángulo superior)"', parse = TRUE, hjust = 1, vjust = 0.5,
           size = 7.5, size.unit = "pt", family = "Arial", colour = pal$tinta) +
  icono(0.5 - separacion - lado, 0.5, lado, FALSE) +
  annotate("text", x = 0.5 - separacion - lado / 2, y = 0.5 + lado + 0.7,
           label = 'bold("Pearson")~"(triángulo inferior)"', parse = TRUE, angle = 90, hjust = 0, vjust = 0.5,
           size = 7.5, size.unit = "pt", family = "Arial", colour = pal$tinta) +
  scale_fill_gradientn(colours = pal$rampa, limits = c(-1, 1), breaks = seq(-1, 1, 0.5),
                       labels = c("−1", "−0,5", "0", "0,5", "1"), oob = scales::squish,
                       na.value = "white", name = "Correlación") +
  geom_tile(data = celdas_pares, aes(x, y), inherit.aes = FALSE,
            width = 1, height = 1, fill = NA, colour = pal$tinta, linewidth = 0.45) +
  geom_segment(data = celdas_pares, aes(x = x + 0.5, y = y - 0.5, xend = x_rotulo, yend = y_rotulo),
               inherit.aes = FALSE, colour = pal$tinta, linewidth = 0.3) +
  geom_label(data = celdas_pares, aes(x_rotulo, y_rotulo, label = numero),
             inherit.aes = FALSE, size = 6, size.unit = "pt", fontface = "bold",
             family = "Arial", colour = "white", fill = pal$tinta, linewidth = 0,
             label.padding = grid::unit(0.9, "pt"), label.r = grid::unit(0.8, "pt")) +
  # los 42 índices; en el eje x van verticales para que quepan
  scale_x_continuous(breaks = 1:N, expand = c(0, 0)) +
  scale_y_continuous(breaks = N + 1 - (1:N), labels = 1:N, expand = c(0, 0), position = "right") +
  coord_fixed(xlim = c(0.5, N + 0.5), ylim = c(0.5, N + 0.5), clip = "off") +
  guides(fill = guide_colourbar(
    title.position = "left", direction = "horizontal",
    theme = theme(
      legend.key.width = grid::unit(38, "mm"),
      legend.key.height = grid::unit(2.2, "mm"),
      legend.ticks = element_line(colour = pal$tinta, linewidth = 0.25),
      legend.ticks.length = grid::unit(c(0, 0.6), "mm"),
      legend.frame = element_rect(colour = pal$tinta, linewidth = 0.25),
      legend.title = element_text(size = 7.5, colour = pal$tinta, vjust = 1, margin = margin(r = 6)),
      legend.text = element_text(size = 7, colour = pal$tinta, margin = margin(t = 1.5))
    )
  )) +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 6, colour = pal$tinta, angle = 90,
                               hjust = 1, vjust = 0.5, margin = margin(t = 1.2)),
    axis.text.y.right = element_text(size = 6, colour = pal$tinta, hjust = 0, margin = margin(l = 1.2)),
    axis.ticks = element_line(colour = pal$tinta_suave, linewidth = 0.2),
    axis.ticks.length = grid::unit(1.6, "pt"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(7, "pt"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(15, 2, 2, 15)
  )

# ancho de una columna del artículo: 84,95 mm
ggsave(here("graficos", "2_correlacion", "matriz_triangular_una_columna.pdf"), figura,
       width = 84.95 / 25.4, height = 3.62, device = cairo_pdf, bg = "white")
