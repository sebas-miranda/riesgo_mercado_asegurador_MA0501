library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

datos <- read.csv(here::here("resultados", "eda", "datos.csv"))
resumen <- read.csv(here::here("resultados", "eda", "resumen_cajas.csv"))
ventanas <- read.csv(here::here("resultados", "eda", "ventanas_cajas.csv"))
fuera_ventana <- read.csv(here::here("resultados", "eda", "conteo_fuera_detalle.csv"))

salida <- here::here("graficos", "eda")
dir.create(file.path(salida, "cajas"), recursive = TRUE, showWarnings = FALSE)

colores <- c(primas = "#2F6B9A", siniestros = "#C47431", lr = "#788536")
nombres <- c(primas = "Primas", siniestros = "Siniestros", lr = "LR")
unidades <- c(primas = "Millones de CRC", siniestros = "Millones de CRC", lr = "LR (%)")
ejes <- list(
  primas = scales::transform_log10(),
  siniestros = scales::transform_pseudo_log(sigma = 1, base = 10),
  lr = scales::transform_pseudo_log(sigma = 1, base = 10)
)

cajas <- datos |>
  mutate(lr = 100 * lr) |>
  select(nodo, serie, anio, trimestre, primas, siniestros, lr) |>
  pivot_longer(c(primas, siniestros, lr), names_to = "variable", values_to = "valor")

numero <- function(valores) {
  etiquetas <- character(length(valores))
  for (i in seq_along(valores)) {
    etiquetas[i] <- format(valores[i], trim = TRUE, scientific = FALSE,
                           big.mark = " ", decimal.mark = ",")
  }
  etiquetas
}

graficar_cajas <- function(medida) {
  orden <- resumen |>
    filter(variable == medida) |>
    arrange(mediana, serie) |>
    pull(serie)
  serie <- cajas |>
    filter(variable == medida) |>
    mutate(serie = factor(serie, levels = orden))
  ventana <- filter(ventanas, variable == medida)
  recuento <- filter(fuera_ventana, variable == medida)
  cantidad_fuera <- sum(recuento$n_fuera)
  color <- colores[[medida]]
  eje <- ejes[[medida]]

  base <- ggplot(serie, aes(valor, serie)) +
    labs(x = unidades[[medida]], y = NULL) +
    theme_bw(base_size = 9, base_family = "sans") +
    theme(
      plot.title = element_text(size = 10),
      axis.text.y = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey94", linewidth = 0.2),
      panel.border = element_rect(color = "grey75")
    )

  referencia_cero <- geom_vline(
    xintercept = 0, color = "grey70", linewidth = 0.25, linetype = "dashed"
  )
  caja <- geom_boxplot(
    orientation = "y", width = 0.55, linewidth = 0.4,
    color = color, fill = adjustcolor(color, alpha.f = 0.15),
    outlier.shape = 1, outlier.size = 0.9, outlier.stroke = 0.4
  )

  completo <- base + referencia_cero + caja +
    scale_x_continuous(labels = numero) +
    labs(title = "Escala completa")

  # Escoger los números que se muestran en el eje del acercamiento.
  detalle <- base
  if (medida == "primas") {
    tipo_escala <- "log10"
    cortes <- 10^seq(floor(log10(ventana$desde)), ceiling(log10(ventana$hasta)))
    nota_escala <- " Eje x log10."
  } else {
    # La escala simétrica conserva el cero y los valores negativos.
    tipo_escala <- "log simétrica"
    cortes <- c(-10^seq(6, 0), 0, 10^seq(0, 6))
    nota_escala <- " Eje x log simétrica: admite ceros y negativos."
    detalle <- detalle + referencia_cero
  }
  cortes <- cortes[cortes >= ventana$desde & cortes <= ventana$hasta]

  # Calcular cuartiles y bigotes antes de transformar el eje.
  detalle <- detalle + caja +
    scale_x_continuous(labels = numero, breaks = cortes, expand = expansion(mult = 0)) +
    coord_transform(x = eje, xlim = c(ventana$desde, ventana$hasta)) +
    labs(title = paste("Cajas y bigotes completos -", tipo_escala)) +
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      plot.margin = margin(t = 5.5, r = 16, b = 5.5, l = 5.5)
    )

  # La página tiene un encabezado, dos gráficos y un pie.
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    3, 2, widths = unit(c(8, 6), "inches"),
    heights = unit(c(0.7, 10.4, 0.9), "inches")
  )))
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.text(paste("Cajas y bigotes:", nombres[[medida]]),
    x = 0.01, y = 0.7, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))
  grid.text("42 nodos | 2017-T1 a 2026-T3 | 39 trimestres por nodo | Orden por mediana",
    x = 0.01, y = 0.25, just = "left", gp = gpar(fontsize = 9, col = "grey30"))
  popViewport()
  print(completo, vp = viewport(layout.pos.row = 2, layout.pos.col = 1), newpage = FALSE)
  print(detalle, vp = viewport(layout.pos.row = 2, layout.pos.col = 2), newpage = FALSE)

  pie <- c(
    "Caja: percentiles 25-75; línea: mediana. Bigotes: extremos dentro de 1,5 IQR. Puntos: valores fuera de los bigotes.",
    paste0("Detalle: límites de los bigotes con margen (", numero(round(ventana$desde, 2)),
      " a ", numero(round(ventana$hasta, 2)), ").", nota_escala),
    if (cantidad_fuera == 0) {
      paste0("El detalle incluye los ", nrow(serie), " valores; todas las cajas y sus bigotes se muestran completos.")
    } else {
      paste0("Todas las cajas y bigotes están completos. Los ", cantidad_fuera,
        " puntos fuera del detalle se muestran en la escala completa.")
    }
  )
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
  grid.text(paste(pie, collapse = "\n"), x = 0.01, y = 0.95, just = c("left", "top"),
    gp = gpar(fontsize = 8.5, col = "grey25", lineheight = 1.25))
  popViewport(2)
}

pdf(file.path(salida, "CAJAS_PANEL_42.pdf"), width = 14, height = 12,
  family = "Helvetica", useDingbats = FALSE)
for (medida in names(nombres)) {
  graficar_cajas(medida)
}
dev.off()

for (medida in names(nombres)) {
  ragg::agg_png(file.path(salida, paste0("cajas/cajas_", medida, ".png")),
    width = 14, height = 12, units = "in", res = 160)
  graficar_cajas(medida)
  dev.off()
}
