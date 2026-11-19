library(igraph)
library(ggraph)

graficar_red <- function(red, xy, titulo, subtitulo) {
  colores <- c(Positiva = "#28798A", Negativa = "#C45030", Cero = "#808080")
  signos <- intersect(names(colores), E(red)$signo)
  ggraph(red, layout = "manual", x = xy[, 1], y = xy[, 2]) +
    geom_edge_link0(aes(colour = signo, linetype = signo, width = magnitud),
                    alpha = 0.45) +
    geom_node_point(shape = 21, fill = "white", colour = "#253840", size = 2.1) +
    geom_node_text(aes(label = etiqueta), repel = TRUE, seed = 50,
                   size = 2.6, max.overlaps = Inf, segment.colour = "grey75") +
    scale_edge_colour_manual(name = NULL, values = colores, limits = signos) +
    scale_edge_linetype_manual(name = NULL, limits = signos,
                               values = c(Positiva = "solid", Negativa = "dashed", Cero = "dotted")) +
    scale_edge_width_continuous(limits = c(0, 1), range = c(0.13, 1.05), guide = "none") +
    coord_equal(clip = "off") +
    labs(title = titulo, subtitle = subtitulo) +
    theme_graph(base_family = "sans") +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
          plot.margin = margin(12, 24, 12, 24))
}
