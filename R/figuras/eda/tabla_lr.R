library(grid)

tabla <- read.csv(here::here("resultados", "eda", "tabla_resumen_LR.csv"))

salida <- here::here("graficos", "eda")
dir.create(file.path(salida, "tablas"), recursive = TRUE, showWarnings = FALSE)

porcentaje <- function(x) {
  paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = ","), " %")
}

# Las posiciones de la tabla están en milímetros desde la esquina superior izquierda.
alto_pagina <- 260
texto <- function(etiqueta, x, y, tamano = 10, alineacion = "left", negrita = FALSE) {
  grid.text(etiqueta, x = unit(x, "mm"), y = unit(alto_pagina - y, "mm"),
    just = alineacion,
    gp = gpar(fontfamily = "serif", fontsize = tamano,
      fontface = if (negrita) "bold" else "plain", col = "grey15"))
}
regla <- function(y, grosor = 0.3, color = "grey80") {
  grid.lines(x = unit(c(15, 195), "mm"), y = unit(rep(alto_pagina - y, 2), "mm"),
    gp = gpar(col = color, lwd = grosor))
}

dibujar_tabla <- function() {
  grid.newpage()
  texto("Resumen del LR de las 42 series", 15, 16, tamano = 14, negrita = TRUE)
  texto("2017-T1 a 2026-T3. 39 trimestres por serie.", 15, 23, tamano = 9.5)

  posicion_columnas <- c(128, 146, 164, 182, 193)
  cabeceras <- c("Media", "Mediana", "Mínimo", "Máximo", "Neg.")
  columnas <- c("media", "mediana", "minimo", "maximo", "negativos")
  posicion_filas <- 44.5 + (seq_len(nrow(tabla)) - 1) * 4.8

  regla(32, grosor = 0.8, color = "grey20")
  texto("Aseguradora y ramo", 15, 36.5, tamano = 9.5, negrita = TRUE)
  for (j in seq_along(cabeceras)) {
    texto(cabeceras[j], posicion_columnas[j], 36.5, tamano = 9.5,
      alineacion = "right", negrita = TRUE)
  }
  regla(41, grosor = 0.5, color = "grey35")

  # Dibujar una línea entre aseguradoras.
  for (i in seq_len(nrow(tabla) - 1)) {
    if (tabla$aseguradora[i] != tabla$aseguradora[i + 1]) {
      regla(mean(posicion_filas[c(i, i + 1)]))
    }
  }
  texto(tabla$serie, 15, posicion_filas)
  # Mostrar el LR como porcentaje y los negativos como un conteo.
  for (j in seq_along(columnas)) {
    valores <- tabla[[columnas[j]]]
    if (columnas[j] == "negativos") {
      etiquetas <- as.character(valores)
    } else {
      etiquetas <- porcentaje(valores)
    }
    texto(etiquetas, posicion_columnas[j], posicion_filas, alineacion = "right")
  }
  regla(tail(posicion_filas, 1) + 3.2, grosor = 0.8, color = "grey20")
}

pdf(file.path(salida, "RESUMEN_LR_42.pdf"), width = 210 / 25.4, height = alto_pagina / 25.4,
  family = "Times", useDingbats = FALSE, useKerning = FALSE)
dibujar_tabla()
dev.off()

ragg::agg_png(file.path(salida, "tablas/resumen_LR_42.png"), width = 210, height = alto_pagina,
  units = "mm", res = 180, background = "white")
dibujar_tabla()
dev.off()
