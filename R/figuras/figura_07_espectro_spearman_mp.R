# Grafica el espectro de la matriz de Spearman con las bandas de Marchenko-Pastur y un
# recuadro con los autovalores cercanos a cero.
# Lee resultados/eigenvalues_spearman.csv y resultados/comparacion.csv.
# Guarda graficos/2_correlacion/espectro_spearman_mp.pdf
# (Figura 7, fig:espectro_spearman_mp, de informes/paper_final.tex).

library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

espectro <- read.csv(here("resultados", "eigenvalues_spearman.csv"))
limites <- read.csv(here("resultados", "comparacion.csv"))
limites <- limites[limites$metodo == "Spearman", ]

# los autovalores que son cero salen como 1e-16 o -1e-16 por redondeo
espectro$eigenvalue[abs(espectro$eigenvalue) < 1e-12] <- 0
mayor <- espectro[which.max(espectro$eigenvalue), ]
menores <- tail(espectro, 6)

# coma decimal y signo menos, como en el texto del artículo
coma <- function(x, digitos) {
  sub("^-", "−", formatC(x, format = "f", digits = digitos, decimal.mark = ","))
}

tema_espectro <- theme_classic(base_size = 9, base_family = "sans") +
  theme(
    axis.text = element_text(colour = pal$tinta, size = 8),
    axis.ticks.length = grid::unit(1.8, "pt"),
    axis.ticks = element_line(colour = pal$tinta, linewidth = 0.25),
    axis.line = element_line(colour = pal$tinta, linewidth = 0.25)
  )

# recuadro con los seis autovalores más pequeños
detalle <- ggplot(menores, aes(posicion, eigenvalue)) +
  geom_hline(yintercept = limites$lambda_menos, colour = pal$referencia,
             linewidth = 0.45, linetype = "dotted") +
  geom_point(colour = pal$tinta, size = 1.35) +
  annotate("text", x = max(menores$posicion) + 0.3, y = 0.0088,
           label = sprintf("lambda['-'] %%~~%% '%s'", coma(limites$lambda_menos, 6)),
           parse = TRUE, hjust = 1, vjust = 0.5, size = 8.6, size.unit = "pt",
           colour = pal$referencia) +
  scale_x_continuous(breaks = menores$posicion, expand = expansion(add = 0.48)) +
  scale_y_continuous(breaks = c(0, 0.004, 0.008), labels = function(x) coma(x, 3),
                     expand = expansion(add = c(0.0009, 0.0016))) +
  coord_cartesian(clip = "off") +
  labs(title = "Detalle cerca de cero", x = NULL, y = NULL) +
  tema_espectro +
  theme(
    axis.text = element_text(size = 7),
    plot.title.position = "plot",
    plot.title = element_text(size = 8, face = "bold", colour = pal$tinta, margin = margin(b = 2)),
    plot.background = element_rect(fill = "white", colour = pal$borde, linewidth = 0.3),
    plot.margin = margin(4, 5, 3, 4)
  )

figura <- ggplot(espectro, aes(posicion, eigenvalue)) +
  geom_hline(yintercept = limites$lambda_mas, colour = pal$referencia,
             linewidth = 0.5, linetype = "dashed") +
  geom_segment(aes(xend = posicion, y = 0, yend = eigenvalue, colour = sobre_MP), linewidth = 0.36) +
  geom_point(aes(colour = sobre_MP), size = 1.45) +
  annotate("text", x = mayor$posicion + 1.4, y = mayor$eigenvalue,
           label = sprintf("lambda[1] %%~~%% '%s'", coma(mayor$eigenvalue, 4)),
           parse = TRUE, hjust = 0, size = 9, size.unit = "pt", colour = pal$pos) +
  annotate("text", x = limites$N, y = limites$lambda_mas,
           label = sprintf("lambda['+'] %%~~%% '%s'", coma(limites$lambda_mas, 4)),
           parse = TRUE, hjust = 1, vjust = -0.8, size = 8.6, size.unit = "pt",
           colour = pal$referencia) +
  scale_colour_manual(values = c("FALSE" = pal$tinta, "TRUE" = pal$pos), guide = "none") +
  scale_x_continuous(breaks = c(1, 10, 20, 30, limites$N), expand = expansion(add = 0.8)) +
  scale_y_continuous(breaks = seq(0, 6, 2), expand = expansion(add = c(0.15, 0.40))) +
  labs(x = "Índice del autovalor", y = "Autovalor") +
  tema_espectro +
  theme(plot.margin = margin(3, 4, 2, 1)) +
  inset_element(detalle, left = 0.45, bottom = 0.16, right = 0.99, top = 0.6)

ggsave(here("graficos", "2_correlacion", "espectro_spearman_mp.pdf"), figura,
       width = 84.95 / 25.4, height = 2.3, device = cairo_pdf, bg = "white")
