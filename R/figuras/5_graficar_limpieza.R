# Ejecutar después del paso 5 de R/analisis.
library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "crear_grafico_C.R"), encoding = "UTF-8")


curvas <- read.csv(here("resultados", "contraccion_alpha", "espectro_alpha.csv"))
codigos <- c("000", "010", "030", "050", "070", "100")
alphas <- as.numeric(codigos) / 100
promedio_ruido <- curvas$promedio_ruido[1]
C_empirica <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                                 row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
datos_RMT <- read.csv(here("resultados", "comparacion.csv"))
T_serie <- datos_RMT$T[datos_RMT$metodo == "Spearman"]

N <- nrow(C_empirica)
orden <- hclust(as.dist(1 - C_empirica), method = "average")$order
etiquetas <- format(alphas, trim = TRUE, drop0trailing = TRUE, decimal.mark = ",")

# posiciones de los grupos marcados en el panel de 42 series
bordes <- NULL
if (N == 42 && T_serie == 39 &&
    identical(rownames(C_empirica)[orden[c(1, 5, 19, 24, 42)]],
              c("SAGICOR-Salud", "INS-Seguro obligatorio de automóviles (SOA)",
                "INS-Seguro obligatorio de riesgos del trabajo (SRT)", "ASSA-Salud", "PANAM-Salud"))) {
  grupos <- data.frame(
    fila_desde = c(12, 2, 2, 20, 24, 24, 24, 12, 24, 12, 20, 31, 31, 12),
    fila_hasta = c(17, 7, 7, 22, 29, 29, 29, 17, 29, 17, 21, 34, 34, 17),
    col_desde = c(24, 12, 24, 24, 24, 31, 41, 12, 36, 31, 31, 31, 41, 42),
    col_hasta = c(29, 18, 29, 29, 29, 34, 42, 17, 38, 34, 34, 34, 42, 42)
  )
  reflejos <- subset(grupos, fila_desde != col_desde)
  grupos <- rbind(grupos, transform(reflejos, fila_desde = col_desde, fila_hasta = col_hasta,
                                  col_desde = fila_desde, col_hasta = fila_hasta))
  bordes <- geom_rect(data = grupos, inherit.aes = FALSE,
                      aes(xmin = col_desde - 0.5, xmax = col_hasta + 0.5,
                          ymin = N + 0.5 - fila_hasta, ymax = N + 1.5 - fila_desde),
                      fill = NA, colour = "#303A48", linewidth = 0.25)
}

mapas <- list()
for (i in seq_along(codigos)) {
  alpha <- alphas[i]
  archivo <- here("resultados", "contraccion_alpha", paste0("alpha_constante_", codigos[i], ".csv"))
  C <- as.matrix(read.csv(archivo, row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
  if (alpha == 1) C_recorte <- C

  mapas[[i]] <- crear_grafico_C(C, "Spearman", orden, T_serie, limite = 0.5) +
    bordes +
    labs(title = paste("α =", etiquetas[i]), subtitle = NULL) +
    guides(fill = guide_colorbar(barwidth = grid::unit(5, "cm"))) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          plot.title = element_text(size = 12), plot.margin = margin(4, 6, 4, 6))
}

mapa <- wrap_plots(mapas, ncol = 3, guides = "collect") +
  plot_annotation(title = "Matrices de Spearman según α",
                  subtitle = paste(N, "series ·", T_serie, "trimestres"),
                  caption = "Mismo orden y escala en los seis mapas. Los colores se saturan en ±0,50.",
                  theme = theme(plot.title = element_text(size = 18, face = "bold"),
                                plot.caption = element_text(hjust = 0),
                                plot.margin = margin(12, 16, 12, 16))) &
  theme(legend.position = "bottom")

curvas$alpha <- factor(curvas$alpha, levels = alphas, labels = etiquetas)
curva <- ggplot(curvas, aes(posicion, autovalor, colour = alpha, linetype = alpha, shape = alpha)) +
  geom_hline(yintercept = promedio_ruido, colour = "grey45", linetype = "dotted") +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.7) +
  scale_colour_manual(values = c("#242424", "#6585A2", "#0072B2", "#00486D", "#D55E00", "#834111")) +
  scale_shape_manual(values = c(16, 1, 17, 2, 15, 0)) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Espectro de ruido para distintos valores de α",
       subtitle = paste0("Antes de normalizar la diagonal. Promedio = ",
                         format(round(promedio_ruido, 5), decimal.mark = ","), "."),
       x = "Posición en el espectro original", y = "Autovalor",
       colour = "α", linetype = "α", shape = "α") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom", plot.margin = margin(12, 16, 12, 16))

ggsave(here("graficos", "contraccion_alpha", "matrices_alpha.pdf"), mapa,
       width = 13, height = 9.5, device = cairo_pdf, bg = "white")
ggsave(here("graficos", "contraccion_alpha", "espectro_alpha.pdf"), curva,
       width = 11, height = 5.5, device = cairo_pdf, bg = "white")

# se conserva el gráfico del recorte clásico que se usa en el borrador
grafico_empirica <- crear_grafico_C(C_empirica, "Spearman", orden, T_serie)
grafico_recorte <- crear_grafico_C(C_recorte, "Spearman", orden, T_serie) +
  labs(title = "Matriz de Spearman con recorte por promedio (α = 1)")
comparacion <- grafico_empirica | grafico_recorte
ggsave(here("graficos", "comparacion_spearman_rmt.png"), comparacion, width = 36, height = 15, bg = "white")
ggsave(here("graficos", "comparacion_spearman_rmt.pdf"), comparacion,
       width = 36, height = 15, device = cairo_pdf, bg = "white")
