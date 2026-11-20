# Grafica el subgrafo de las aristas con |ρ| ≥ 0,40 en la matriz con α = 0,5, centrado en
# MAPFRE-Automóvil.
# Lee resultados/contraccion_alpha/alpha_constante_050.csv.
# Guarda graficos/4_redes/red_spearman_mapfre.pdf (Figura 12, fig:red_mapfre, de informes/paper_final.tex).

library(here)
library(ggplot2)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

C <- as.matrix(read.csv(here("resultados", "contraccion_alpha", "alpha_constante_050.csv"),
                        row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))

# coma decimal y signo menos, como en el texto del artículo
coma <- function(x, digitos) {
  sub("^-", "−", formatC(x, format = "f", digits = digitos, decimal.mark = ","))
}

# las seis carteras del subgrafo, con su índice de la Tabla 1; las coordenadas
# solo acomodan los rótulos
nodos <- data.frame(
  serie = c("ASSA-Pérdidas pecuniarias", "SAGICOR-Vida", "MAPFRE-Automóvil",
            "MAGIST-Accidentes", "ISCR-Salud", "ASSA-Salud"),
  etiqueta = c("ASSA\nPérd. pecun.", "SAGICOR\nVida", "MAPFRE\nAutomóvil",
               "MAGIST\nAccidentes", "ISCR\nSalud", "ASSA\nSalud"),
  indice = c(20, 17, 28, 27, 25, 24),
  x = c(0.22, 0.16, 0.48, 0.81, 0.56, 0.86),
  y = c(0.62, 0.24, 0.43, 0.25, 0.82, 0.82),
  texto_y = c(0.77, 0.085, 0.255, 0.095, 0.955, 0.955),
  foco = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
)

# aristas: los pares con |rho| >= 0,40 (cada par una vez)
indices <- which(upper.tri(C) & abs(C) >= 0.40, arr.ind = TRUE)
origen <- match(rownames(C)[indices[, 1]], nodos$serie)
destino <- match(colnames(C)[indices[, 2]], nodos$serie)
aristas <- data.frame(x = nodos$x[origen], y = nodos$y[origen],
                      xend = nodos$x[destino], yend = nodos$y[destino], rho = C[indices])
aristas$signo <- ifelse(aristas$rho > 0, "Positiva", "Negativa")
aristas$etiqueta <- paste0(ifelse(aristas$rho > 0, "+", ""), coma(aristas$rho, 3))

figura <- ggplot() +
  geom_segment(data = aristas, aes(x, y, xend = xend, yend = yend, colour = signo, linetype = signo),
               linewidth = 0.7) +
  geom_label(data = aristas, aes((x + xend) / 2, (y + yend) / 2, label = etiqueta, colour = signo),
             size = 7, size.unit = "pt", family = "Arial", fontface = "bold",
             fill = "white", linewidth = 0, label.padding = grid::unit(1, "pt")) +
  geom_point(data = nodos, aes(x, y, fill = foco), size = 4.6, shape = 21, colour = pal$tinta, stroke = 0.45) +
  geom_text(data = nodos, aes(x, y, label = indice, colour = foco), size = 6, size.unit = "pt", family = "Arial") +
  geom_text(data = nodos, aes(x, texto_y, label = etiqueta, fontface = ifelse(foco, "bold", "plain")),
            size = 7.5, size.unit = "pt", family = "Arial", lineheight = 0.92, colour = pal$tinta) +
  scale_colour_manual(values = c(pal$arista, "FALSE" = pal$tinta, "TRUE" = "white")) +
  scale_linetype_manual(values = c(Positiva = "solid", Negativa = "dashed")) +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = pal$tinta)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.05), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(1, 2, 1, 2))

ggsave(here("graficos", "4_redes", "red_spearman_mapfre.pdf"), figura,
       width = 84.95 / 25.4, height = 1.92, device = cairo_pdf, bg = "white")
