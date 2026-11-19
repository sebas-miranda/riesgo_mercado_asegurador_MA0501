# Ejecutar desde la raíz del proyecto después del paso 5.
library(here)
library(ggplot2)

C <- as.matrix(read.csv(
  here("resultados", "contraccion_alpha", "alpha_constante_050.csv"),
  row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"
))
stopifnot(identical(rownames(C), colnames(C)))
indices <- which(upper.tri(C), arr.ind = TRUE)
rho <- C[indices]
pares <- data.frame(from = indices[, 1], to = indices[, 2], rho, magnitud = abs(rho))
foco <- "MAPFRE-Automóvil"

# Acomodo de las series activas con h = 0,30 y posición de cada rótulo.
posiciones <- read.table(sep = "|", header = TRUE, text = "serie|x|y|texto_x|texto_y
ASSA-Accidentes|2.00|-3.22|2.000|-3.552
ASSA-Automóvil|-0.40|-2.42|-0.799|-2.420
ASSA-Salud|2.88|2.48|3.279|2.480
MAPFRE-Automóvil|0.00|0.00|0.980|0.000
PANAM-Salud|-4.73|-2.50|-4.730|-2.832
ASSA-Caución|3.21|-3.69|3.210|-3.358
BMI-Salud|5.45|-3.69|5.450|-3.358
ASSA-Incendio y líneas aliadas|3.54|0.52|3.540|0.948
INS-Otros daños a los bienes|5.32|-0.45|5.320|-0.782
MAPFRE-Vida|1.14|1.73|1.140|2.062
ASSA-Pérdidas pecuniarias|-3.97|2.44|-4.567|2.440
INS-Salud|-1.62|5.03|-1.620|5.363
ISCR-Accidentes|-5.42|0.63|-5.420|0.298
ISCR-Salud|0.52|3.20|0.969|2.751
MAGIST-Accidentes|-2.31|0.28|-1.861|-0.169
SAGICOR-Vida|-1.68|2.21|-1.970|2.500
ISCR-Vida|3.66|4.59|3.660|4.922
DAVIVIENDA-Pérdidas pecuniarias|-1.76|-1.43|-1.760|-1.762
INS-Caución|1.42|-1.55|1.420|-0.812
SAGICOR-Otros daños a los bienes|-3.40|-1.36|-3.650|-1.550")

# Fracción de la arista donde se mueve el coeficiente para evitar cruces.
posiciones_aristas <- read.table(sep = "|", header = TRUE, text = "origen|destino|fraccion
ASSA-Pérdidas pecuniarias|MAPFRE-Automóvil|0.57
DAVIVIENDA-Pérdidas pecuniarias|MAPFRE-Automóvil|0.44
INS-Caución|MAPFRE-Automóvil|0.15
MAPFRE-Automóvil|MAPFRE-Vida|0.52
MAPFRE-Automóvil|SAGICOR-Otros daños a los bienes|0.63
ASSA-Salud|SAGICOR-Vida|0.47
MAGIST-Accidentes|SAGICOR-Vida|0.30")

red_detalle <- function(h) {
  aristas <- pares[pares$magnitud >= h, ]
  series <- rownames(C)
  activas <- sort(unique(c(aristas$from, aristas$to)))
  nodos <- posiciones[match(series[activas], posiciones$serie), ]
  stopifnot(nrow(aristas) > 0, !anyNA(nodos))

  aseguradora <- sub("-.*$", "", nodos$serie)
  ramo <- sub("^[^-]+-", "", nodos$serie)
  ramo[ramo == "Automóvil"] <- "Auto"
  ramo[ramo == "Incendio y líneas aliadas"] <- "Incendio"
  ramo[ramo == "Pérdidas pecuniarias"] <- "Pérdidas"
  ramo[ramo == "Otros daños a los bienes"] <- "Bienes"
  nodos$etiqueta <- paste(aseguradora, ramo, sep = "\n")
  nodos$etiqueta[nodos$serie == foco] <- "MAPFRE Auto"

  origen <- match(series[aristas$from], nodos$serie)
  destino <- match(series[aristas$to], nodos$serie)
  aristas$x <- nodos$x[origen]
  aristas$y <- nodos$y[origen]
  aristas$xend <- nodos$x[destino]
  aristas$yend <- nodos$y[destino]
  aristas$foco <- nodos$serie[origen] == foco | nodos$serie[destino] == foco
  aristas$destacada <- h < 0.40 & aristas$magnitud >= 0.40

  rotulos <- match(paste(nodos$serie[origen], nodos$serie[destino]),
                   paste(posiciones_aristas$origen, posiciones_aristas$destino))
  fraccion <- posiciones_aristas$fraccion[rotulos]
  fraccion[is.na(fraccion)] <- 0.50
  aristas$texto_x <- aristas$x + fraccion * (aristas$xend - aristas$x)
  aristas$texto_y <- aristas$y + fraccion * (aristas$yend - aristas$y)
  aristas$etiqueta <- paste0(ifelse(aristas$rho > 0, "+", "−"),
                            formatC(aristas$magnitud, digits = 3, format = "f", decimal.mark = ","))
  positiva <- aristas$rho > 0
  enfasis <- aristas$foco | aristas$destacada
  aristas$color <- ifelse(positiva, "#136A7B", "#A94225")
  aristas$trazo <- ifelse(enfasis, aristas$color, ifelse(positiva, "#6F9EA7", "#BD8779"))
  aristas$fondo <- ifelse(aristas$destacada, ifelse(positiva, "#E0F1F3", "#F9E7DF"), "white")
  aristas$grosor <- ifelse(aristas$foco, 0.94, ifelse(aristas$destacada, 0.64, 0.39))
  aristas$linea <- ifelse(positiva, "solid", "42")
  aristas$peso_texto <- ifelse(enfasis, "bold", "plain")

  xlim <- range(c(nodos$x, nodos$texto_x)) + c(-0.54, 0.54)
  ylim <- range(c(nodos$y, nodos$texto_y)) + c(-0.34, 0.39)
  umbral <- formatC(h, digits = 2, format = "f", decimal.mark = ",")
  nota <- if (h < 0.40) {
    "Trazo grueso para las aristas de MAPFRE Auto. Fondo de color para |ρ| ≥ 0,40."
  } else {
    "Trazo grueso para las aristas de MAPFRE Auto."
  }
  subtitulo <- paste0("Contracción α = 0,5   ·   h = ", umbral, "   ·   ",
                     nrow(nodos), " nodos   ·   ", nrow(aristas), " aristas")

  ggplot() +
    geom_segment(data = aristas[order(aristas$grosor), ],
                 aes(x, y, xend = xend, yend = yend, colour = trazo,
                     linewidth = grosor, linetype = linea), lineend = "round") +
    geom_label(data = aristas,
               aes(texto_x, texto_y, label = etiqueta, colour = color,
                   fill = fondo, fontface = peso_texto),
               family = "Arial", size = 3.6, linewidth = 0,
               label.padding = grid::unit(0.12, "lines"), label.r = grid::unit(0.21, "lines")) +
    geom_point(data = nodos[nodos$serie != foco, ], aes(x, y), shape = 21,
               size = 2.3, fill = "white", colour = "#172C39", stroke = 0.56) +
    geom_point(data = nodos[nodos$serie == foco, ], aes(x, y), size = 4.2, colour = "#172C39") +
    geom_text(data = nodos[nodos$serie != foco, ], aes(texto_x, texto_y, label = etiqueta),
              family = "Arial", size = 4.1, lineheight = 0.95, colour = "#172C39") +
    geom_text(data = nodos[nodos$serie == foco, ], aes(texto_x, texto_y, label = etiqueta),
              family = "Arial", size = 4.7, fontface = "bold", colour = "#172C39") +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_linewidth_identity() +
    scale_linetype_identity() +
    coord_equal(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = "Red de correlaciones de Spearman", subtitle = subtitulo, caption = nota) +
    theme_void(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 22, face = "bold", colour = "#172C39",
                                margin = margin(b = 6)),
      plot.subtitle = element_text(size = 12, colour = "#425865", margin = margin(b = 12)),
      plot.caption = element_text(size = 10.5, colour = "#425865", hjust = 0,
                                  margin = margin(t = 14)),
      plot.margin = margin(22, 22, 19, 22),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

for (h in c(0.30, 0.40)) {
  figura <- red_detalle(h)
  umbral <- format(round(h, 2), nsmall = 2, decimal.mark = ".")
  archivo <- here("graficos", "redes", paste0("mapfre_auto_alpha_0.50_h_", umbral))
  ancho <- if (h == 0.30) 12.8 else 8.1
  alto <- if (h == 0.30) 10.1 else 5.1
  ggsave(paste0(archivo, ".png"), figura, width = ancho, height = alto, dpi = 300, bg = "white")
  ggsave(paste0(archivo, ".pdf"), figura, width = ancho, height = alto,
         device = cairo_pdf, bg = "white")
}
