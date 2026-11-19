# Ejecutar después del paso 6 de R/analisis.
library(here)
library(patchwork)
source(here("R", "figuras", "graficar_red.R"), encoding = "UTF-8")

datos_redes <- readRDS(here("resultados", "redes_umbral.rds"))
resumen <- datos_redes$resumen
alphas <- unique(resumen$alpha)
umbrales <- unique(resumen$h)
referencia <- datos_redes$referencia

set.seed(2026)
xy <- graphlayouts::layout_with_stress(referencia, weights = NA)

for (h in umbrales) {
  dibujos <- list()
  for (i in seq_along(alphas)) {
    alpha <- alphas[i]
    titulo <- paste("α =", format(alpha, decimal.mark = ","))
    fila <- which(resumen$h == h & resumen$alpha == alpha)
    red <- datos_redes$redes[[fila]]
    subtitulo <- paste(resumen$enlaces[fila], "enlaces ·", resumen$aislados[fila], "nodos aislados")
    dibujos[[i]] <- graficar_red(red, xy, titulo, subtitulo)
  }

  figura <- wrap_plots(dibujos, ncol = 2, guides = "collect") +
    plot_annotation(title = paste("Redes de Spearman · h =", format(h, nsmall = 2, decimal.mark = ",")),
                    caption = "El grosor indica la magnitud de la correlación.") &
    theme(legend.position = "bottom")
  umbral <- format(round(h, 2), nsmall = 2, decimal.mark = ".")
  archivo <- here("graficos", "redes", paste0("umbral_h_", umbral))
  ggsave(paste0(archivo, ".png"), figura, width = 16, height = 12, dpi = 200, bg = "white")
  ggsave(paste0(archivo, ".pdf"), figura, width = 16, height = 12, device = cairo_pdf, bg = "white")
}
