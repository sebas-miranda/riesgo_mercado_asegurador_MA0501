# Grafica la matriz de Spearman empírica junto a la contracción α = 0,5.
# Lee resultados/correlacion_spearman_empirica.csv y resultados/contraccion_alpha/alpha_constante_050.csv.
# Guarda graficos/3_contraccion/mapa_spearman_alpha050.pdf (Figura 9, fig:43_alpha050, de informes/paper_final.tex).

library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

C_spearman <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                                 row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
C_050 <- as.matrix(read.csv(here("resultados", "contraccion_alpha", "alpha_constante_050.csv"),
                            row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))

# orden jerárquico de la empírica, el mismo de la Tabla 1
orden <- hclust(as.dist(1 - C_spearman), method = "average")$order
nodos <- rownames(C_spearman)[orden]
N <- length(nodos)
diagonal <- data.frame(x = 1:N, y = N + 1 - (1:N))

# un panel por matriz, con los índices de la Tabla 1 en los dos ejes
panel <- function(C, titulo) {
  # celda (i, j): fila i desde arriba y columna j desde la izquierda
  M <- C[nodos, nodos]
  diag(M) <- NA
  celdas <- expand.grid(fila = 1:N, columna = 1:N)
  celdas$valor <- M[cbind(celdas$fila, celdas$columna)]
  celdas$y <- N + 1 - celdas$fila

  ggplot(celdas, aes(columna, y)) +
    geom_tile(aes(fill = valor), colour = "white", linewidth = 0.08) +
    # la diagonal vale 1 y no aporta: va en gris
    geom_tile(data = diagonal, aes(x, y), inherit.aes = FALSE,
              fill = pal$borde, colour = "white", linewidth = 0.08) +
    # la escala se satura en ±0,5: casi todas las |ρ| son menores que 0,5
    scale_fill_gradientn(colours = pal$rampa, limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25),
                         labels = c("−0,5", "−0,25", "0", "0,25", "0,5"),
                         oob = scales::squish, na.value = "white", name = "Correlación") +
    scale_x_continuous(breaks = 1:N, expand = c(0, 0)) +
    scale_y_continuous(breaks = N + 1 - (1:N), labels = 1:N, expand = c(0, 0)) +
    coord_fixed(xlim = c(0.5, N + 0.5), ylim = c(0.5, N + 0.5), clip = "off") +
    labs(title = titulo, x = NULL, y = NULL) +
    theme_void(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 8, face = "bold", colour = pal$tinta,
                                hjust = 0.5, margin = margin(b = 3)),
      axis.text.x = element_text(size = 6, colour = pal$tinta, angle = 90,
                                 hjust = 1, vjust = 0.5, margin = margin(t = 1)),
      axis.text.y = element_text(size = 6, colour = pal$tinta, hjust = 1, margin = margin(r = 2)),
      axis.ticks = element_line(colour = pal$tinta_suave, linewidth = 0.2),
      axis.ticks.length = grid::unit(1.4, "pt"),
      plot.margin = margin(2, 3, 2, 1)
    )
}

figura <- (panel(C_spearman, "Spearman empírica") | panel(C_050, "Contracción α = 0,5")) +
  plot_layout(guides = "collect") &
  guides(fill = guide_colourbar(
    title = "Correlación", title.position = "left", direction = "vertical",
    theme = theme(
      legend.key.width = grid::unit(2.4, "mm"),
      legend.key.height = grid::unit(48, "mm"),
      legend.ticks = element_line(colour = pal$tinta, linewidth = 0.25),
      legend.ticks.length = grid::unit(c(0, 0.6), "mm"),
      legend.frame = element_rect(colour = pal$tinta, linewidth = 0.25),
      legend.title = element_text(size = 7.2, face = "bold", colour = pal$tinta, angle = 90,
                                  hjust = 0.5, margin = margin(r = 2)),
      legend.text = element_text(size = 6.6, colour = pal$tinta, margin = margin(l = 1.5))
    )
  )) &
  theme(legend.position = "right", legend.margin = margin(0, 0, 0, 2),
        plot.background = element_rect(fill = "white", colour = NA))

# 188 mm: un poco más que el ancho de texto, para que las filas midan 2 mm
ggsave(here("graficos", "3_contraccion", "mapa_spearman_alpha050.pdf"), figura,
       width = 188 / 25.4, height = 96 / 25.4, device = cairo_pdf, bg = "white")
