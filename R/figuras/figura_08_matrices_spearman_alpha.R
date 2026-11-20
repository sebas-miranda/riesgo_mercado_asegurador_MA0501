# Grafica las matrices de Spearman con α = 0; 0,5; 0,7 y 1 en una cuadrícula 2 x 2, a una
# columna del artículo y con los 14 recuadros marcados.
# Lee resultados/correlacion_spearman_empirica.csv y cuatro matrices de resultados/contraccion_alpha/.
# Guarda graficos/3_contraccion/matrices_spearman_alpha.pdf
# (Figura 8, fig:matrices-alpha-columna, de informes/paper_final.tex).

library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

empirica <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                               row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
codigos <- c("000", "050", "070", "100")
rotulos <- c("α = 0", "α = 0,5", "α = 0,7", "α = 1")
N <- nrow(empirica)

# orden jerárquico de la empírica, el mismo de la Tabla 1
orden <- hclust(as.dist(1 - empirica), method = "average")$order
nodos <- rownames(empirica)[orden]

# los 14 recuadros en ese orden (filas y columnas desde la esquina superior izquierda)
bloques <- data.frame(
  fila_desde = c(12, 2, 2, 20, 24, 24, 24, 12, 24, 12, 20, 31, 31, 12),
  fila_hasta = c(17, 7, 7, 22, 29, 29, 29, 17, 29, 17, 21, 34, 34, 17),
  col_desde = c(24, 12, 24, 24, 24, 31, 41, 12, 36, 31, 31, 31, 41, 42),
  col_hasta = c(29, 18, 29, 29, 29, 34, 42, 17, 38, 34, 34, 34, 42, 42)
)
# cada recuadro fuera de la diagonal se dibuja también en el triángulo de abajo
reflejos <- bloques[bloques$fila_desde != bloques$col_desde, ]
reflejos <- data.frame(fila_desde = reflejos$col_desde, fila_hasta = reflejos$col_hasta,
                       col_desde = reflejos$fila_desde, col_hasta = reflejos$fila_hasta)
bloques <- rbind(bloques, reflejos)

# marcas cada 5 carteras; en el eje x solo se numera cada 10 para que no se encimen
marcas <- c(1, seq(5, 40, by = 5))
numeros_x <- ifelse(marcas %in% c(1, 10, 20, 30, 40), marcas, "")

# la diagonal va en gris, como en las demás matrices del artículo
diagonal <- data.frame(Nodo1 = factor(nodos, levels = rev(nodos)),
                       Nodo2 = factor(nodos, levels = nodos))

mapas <- list()
for (i in 1:4) {
  C <- as.matrix(read.csv(here("resultados", "contraccion_alpha", paste0("alpha_constante_", codigos[i], ".csv")),
                          row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))

  # una fila por celda de la matriz ordenada
  datos <- as.data.frame(as.table(C[orden, orden]))
  colnames(datos) <- c("Nodo1", "Nodo2", "Correlacion")
  datos$Nodo1 <- factor(datos$Nodo1, levels = rev(nodos))
  datos$Nodo2 <- factor(datos$Nodo2, levels = nodos)

  # los números van solo en el borde de afuera de la cuadrícula 2 x 2
  if (i > 2) {
    etiquetas_x <- numeros_x
  } else {
    etiquetas_x <- NULL
  }
  if (i %% 2 == 1) {
    etiquetas_y <- marcas
  } else {
    etiquetas_y <- NULL
  }

  mapas[[i]] <- ggplot(datos, aes(x = Nodo2, y = Nodo1, fill = Correlacion)) +
    geom_tile(color = "white", linewidth = 0.12) +
    geom_tile(data = diagonal, aes(x = Nodo2, y = Nodo1), inherit.aes = FALSE,
              fill = pal$borde, colour = "white", linewidth = 0.12) +
    scale_fill_gradientn(colours = pal$rampa, limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25),
                         labels = c("−0,50", "−0,25", "0", "0,25", "0,50"),
                         oob = scales::squish, na.value = "white", name = "Correlación") +
    scale_x_discrete(position = "bottom", expand = c(0, 0), breaks = nodos[marcas], labels = etiquetas_x) +
    scale_y_discrete(expand = c(0, 0), breaks = nodos[marcas], labels = etiquetas_y) +
    coord_fixed() +
    geom_rect(data = bloques, inherit.aes = FALSE,
              aes(xmin = col_desde - 0.5, xmax = col_hasta + 0.5,
                  ymin = N + 0.5 - fila_hasta, ymax = N + 1.5 - fila_desde),
              fill = NA, colour = pal$tinta, linewidth = 0.19) +
    labs(title = rotulos[i], x = NULL, y = NULL) +
    # leyenda vertical a la derecha: deja más ancho para las matrices
    guides(fill = guide_colorbar(title = "Correlación", title.position = "left",
                                 barwidth = grid::unit(2.6, "mm"), barheight = grid::unit(48, "mm"))) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 6.5, colour = pal$tinta, angle = 0, hjust = 0.5,
                                     vjust = 1, margin = margin(t = 1)),
          axis.text.y = element_text(size = 6.5, colour = pal$tinta, hjust = 1, margin = margin(r = 1)),
          axis.ticks = element_line(colour = pal$tinta_suave, linewidth = 0.2),
          axis.ticks.length = grid::unit(1.5, "pt"),
          text = element_text(family = "Arial"),
          plot.title = element_text(size = 8.5, face = "bold", colour = pal$tinta, margin = margin(b = 1.5)),
          plot.margin = margin(1.5, 2, 1.5, 2),
          legend.title = element_text(size = 7.5, face = "plain", angle = 90, hjust = 0.5, vjust = 0.5),
          legend.text = element_text(size = 6.5),
          legend.margin = margin(0, 0, 0, 0), legend.box.spacing = grid::unit(3, "pt"))
}

figura <- wrap_plots(mapas, ncol = 2, guides = "collect") & theme(legend.position = "right")

# ancho de una columna del artículo (carta, márgenes de 1,95 cm y separación de 0,7 cm)
ancho <- (8.5 - (2 * 1.95 + 0.7) / 2.54) / 2
ggsave(here("graficos", "3_contraccion", "matrices_spearman_alpha.pdf"), figura,
       width = ancho, height = 2.85, device = cairo_pdf, bg = "white")
