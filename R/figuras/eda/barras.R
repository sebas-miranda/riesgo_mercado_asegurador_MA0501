library(ggplot2)

tabla <- read.csv(here::here("resultados", "eda", "medianas_LR_barras.csv"))
indice <- read.csv(here::here("resultados", "eda", "indice_barras.csv"))

salida <- here::here("graficos", "eda")
dir.create(file.path(salida, "barras", "por_aseguradora"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(salida, "barras", "por_ramo"), recursive = TRUE, showWarnings = FALSE)

# Los nombres largos ocupan dos líneas en el eje.
partir_etiquetas <- function(nombres) {
  for (i in seq_along(nombres)) {
    lineas <- strwrap(nombres[i], width = 39)
    nombres[i] <- paste(lineas, collapse = "\n")
  }
  nombres
}
dibujar_barras <- function(filas, titulo, comparacion, color) {
  porcentaje <- formatC(filas$mediana_pct, format = "f", digits = 1, decimal.mark = ",")
  filas$texto_barra <- paste0(porcentaje, " %")

  ggplot(filas, aes(mediana_pct, etiqueta)) +
    geom_vline(xintercept = 100, color = "grey55", linetype = "dashed", linewidth = 0.3) +
    geom_col(fill = color, color = color, width = 0.65, linewidth = 0.2) +
    geom_label(aes(label = texto_barra), hjust = -0.12, size = 3.6,
      color = "grey15", fill = "white", linewidth = 0,
      label.padding = grid::unit(0.1, "lines"), label.r = grid::unit(0, "lines")) +
    scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, " %"), expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(labels = partir_etiquetas, expand = expansion(add = 0.6)) +
    labs(title = titulo,
      subtitle = paste("Mediana del LR", comparacion, "| 2017-T1 a 2026-T3 | 39 trimestres"),
      x = "Mediana del LR", y = NULL) +
    theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "grey15"),
      plot.subtitle = element_text(size = 10, color = "grey35", margin = margin(b = 12)),
      plot.title.position = "plot",
      axis.text = element_text(color = "grey20"),
      axis.text.y = element_text(size = 10.5, margin = margin(r = 8)),
      axis.title.x = element_text(size = 10.5, margin = margin(t = 8)),
      axis.line.x = element_line(color = "grey60", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      legend.position = "none", plot.margin = margin(12, 22, 10, 12)
    )
}

# Poner los gráficos en el PDF y guardar cada uno como PNG.
alturas <- 2.2 + 0.42 * indice$barras
alto_pagina <- max(alturas)
pdf(file.path(salida, "BARRAS_LR_42.pdf"), width = 11, height = alto_pagina,
    family = "Helvetica", useDingbats = FALSE, useKerning = FALSE)
for (i in seq_len(nrow(indice))) {
  grupo <- indice$grupo[i]
  if (indice$agrupar[i] == "aseguradora") {
    filas <- tabla[tabla$aseguradora == grupo, ]
    filas <- filas[order(filas$orden_en_aseguradora), ]
    nombres <- filas$ramo
    comparacion <- "por ramo"
    color <- "#2F6B9A"
  } else {
    filas <- tabla[tabla$ramo == grupo, ]
    filas <- filas[order(filas$orden_en_ramo), ]
    nombres <- filas$aseguradora
    comparacion <- "por aseguradora"
    color <- "#788536"
  }
  # El eje vertical empieza abajo. Invertir el orden pone el LR mayor arriba.
  filas$etiqueta <- factor(nombres, levels = rev(nombres))
  grafico <- dibujar_barras(filas, grupo, comparacion, color)

  grid::grid.newpage()
  espacio <- grid::viewport(y = 1, height = alturas[i] / alto_pagina, just = c("center", "top"))
  print(grafico, newpage = FALSE, vp = espacio)
  ggsave(indice$png[i], grafico, path = salida, width = 11, height = alturas[i], dpi = 160,
         device = ragg::agg_png)
}
dev.off()
