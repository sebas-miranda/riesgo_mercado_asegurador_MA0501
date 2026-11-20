# Grafica la cuadrícula de redes α × h con el acomodo libre (estrés de la red empírica
# con h = 0,20).
# Lee cuatro matrices de resultados/contraccion_alpha/ (α = 0; 0,5; 0,7 y 1).
# Guarda graficos/4_redes/redes_alpha_umbral_libre.pdf y su versión .png
# (Figura 10, fig:redes-alpha-columna, de informes/paper_final.tex).
#
# Cada fila es un α y cada columna un umbral h: cada panel es el grafo G_h de la sección 5.1,
# con las aristas de |rho| >= h. El color y el guion indican el signo. Los nodos llevan el
# color de su familia y el índice de la Tabla 1.

library(here)
library(grid)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

codigos <- c("000", "050", "070", "100")
umbrales <- c(0.20, 0.30, 0.40)
matrices <- list()
for (i in 1:4) {
  matrices[[i]] <- as.matrix(read.csv(here("resultados", "contraccion_alpha", paste0("alpha_constante_", codigos[i], ".csv")),
                                      row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
}
empirica <- matrices[[1]] # alpha = 0 es la matriz empírica
series <- rownames(empirica)
N <- length(series)

# número de cada nodo: su índice de la Tabla 1 (orden jerárquico de la empírica)
orden <- hclust(as.dist(1 - empirica), method = "average")$order
indice <- match(1:N, orden)

# familia y color de cada cartera según su ramo
ramo <- sub("^[^-]+-", "", series)
fam <- familia_de[ramo]
color_nodo <- color_familia[fam]

# aristas de cada alfa y umbral, de la más débil a la más fuerte
pares <- which(upper.tri(empirica), arr.ind = TRUE)
redes <- list()
conteos <- matrix(0, nrow = 3, ncol = 4)
for (i in 1:4) {
  rho <- matrices[[i]][pares]
  redes[[i]] <- list()
  for (k in 1:3) {
    seleccion <- which(abs(rho) >= umbrales[k])
    seleccion <- seleccion[order(abs(rho[seleccion]))]
    redes[[i]][[k]] <- data.frame(desde = pares[seleccion, 1], hasta = pares[seleccion, 2],
                                  signo = ifelse(rho[seleccion] > 0, "Positiva", "Negativa"))
    conteos[k, i] <- length(seleccion)
  }
}

# mezcla en RGB hacia la tinta (oscurecer() de la paleta mezcla en Lab y da otro tono)
oscurecer_rgb <- function(color, p) {
  a <- col2rgb(color)
  b <- col2rgb(pal$tinta)[, rep(1, ncol(a)), drop = FALSE]
  m <- round(a + (b - a) * p)
  rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

# separa los pares de nodos más cercanos que `minimo` para que los números no se encimen;
# ningún nodo sale de x en [-1, 1]
separar <- function(xy, minimo, iteraciones) {
  for (k in 1:iteraciones) {
    d <- as.matrix(dist(xy))
    diag(d) <- Inf
    cerca <- which(d < minimo, arr.ind = TRUE)
    cerca <- cerca[cerca[, 1] < cerca[, 2], , drop = FALSE]
    if (nrow(cerca) == 0) break
    for (f in 1:nrow(cerca)) {
      v <- xy[cerca[f, 1], ] - xy[cerca[f, 2], ]
      largo <- sqrt(sum(v^2))
      if (largo == 0) {
        v <- c(1, 0)
      } else {
        v <- v / largo
      }
      paso <- (minimo - largo) / 2
      xy[cerca[f, 1], ] <- xy[cerca[f, 1], ] + paso * v
      xy[cerca[f, 2], ] <- xy[cerca[f, 2], ] - paso * v
    }
    xy[, 1] <- pmin(pmax(xy[, 1], -1), 1)
  }
  xy
}

# acomodo de estrés sobre la red empírica con h = 0,20; las aristas auxiliares entre
# carteras de una misma familia (largo 0,6) solo sirven para agruparlas y no se dibujan.
# el acomodo se estira 1,35 veces en vertical para que los números de 6 pt quepan
A <- (abs(empirica) >= 0.20) * 1
diag(A) <- 0
misma_familia <- outer(fam, fam, "==") & upper.tri(A)
corr <- which(A == 1 & upper.tri(A), arr.ind = TRUE)
aux <- which(misma_familia & A == 0, arr.ind = TRUE)
red_ref <- igraph::graph_from_edgelist(rbind(corr, aux), directed = FALSE)
largos <- c(rep(1, nrow(corr)), rep(0.6, nrow(aux)))
set.seed(2026)
xy <- graphlayouts::layout_with_stress(red_ref, weights = largos)
xy <- sweep(xy, 2, colMeans(apply(xy, 2, range)))
xy <- cbind(xy[, 1] / max(abs(xy[, 1])), 1.35 * xy[, 2] / max(abs(xy[, 2])))
xy <- separar(xy, 0.31, 3000)

# centro de cada familia: por ahí pasan las aristas entre familias
centros <- do.call(rbind, lapply(split(1:N, fam), function(v) colMeans(xy[v, , drop = FALSE])))

grosor <- 0.5           # aristas entre familias distintas
grosor_familia <- 1.0   # aristas dentro de una familia
opacidad_entre <- 0.38
opacidad_familia <- 1
trazo_neg <- "22"
radio_nodo <- 3.7       # pt; cabe un número de dos cifras en 6 pt

# las de una misma familia son rectas; las de familias distintas pasan por el centro de
# cada familia y forman haces (beta acerca la curva a la recta)
dibujar_arista <- function(i, j, misma, gp, beta = 0.8) {
  P <- xy[i, ]
  Q <- xy[j, ]
  if (misma) {
    grid.segments(P[1], P[2], Q[1], Q[2], default.units = "native", gp = gp)
  } else {
    C <- rbind(P, centros[fam[i], ], centros[fam[j], ], Q)
    t <- seq(0, 1, length.out = nrow(C))
    recta <- cbind(P[1] + t * (Q[1] - P[1]), P[2] + t * (Q[2] - P[2]))
    C <- beta * C + (1 - beta) * recta
    grid.xspline(C[, 1], C[, 2], default.units = "native", shape = 1, open = TRUE,
                 repEnds = TRUE, gp = gp)
  }
}

# zona de una familia: envolvente convexa de círculos de radio `pad` alrededor de sus nodos
contorno_familia <- function(v, pad) {
  ang <- seq(0, 2 * pi, length.out = 41)[-41]
  P <- c()
  for (i in v) {
    P <- rbind(P, cbind(xy[i, 1] + pad * cos(ang), xy[i, 2] + pad * sin(ang)))
  }
  P[rev(chull(P)), , drop = FALSE]
}

# nombres de las familias en la clave, anclados a nodos de cada familia y justo afuera
# de su zona; Laboral va en vertical bajo el nodo 19 porque a sus lados no cabe
rotulos_clave <- function(pad, sep) {
  p <- function(i) xy[match(i, indice), , drop = FALSE]
  arriba <- function(i) max(p(i)[, 2]) + pad + sep
  abajo <- function(i) min(p(i)[, 2]) - pad - sep
  data.frame(
    familia = c("Financieros", "Autos", "Personas", "Daños", "Laboral"),
    x = c(-1.2, mean(p(c(36, 7))[, 1]), mean(p(c(16, 38))[, 1]), p(40)[, 1] - pad - sep, p(19)[, 1]),
    y = c(arriba(2), arriba(36), abajo(c(16, 38)), abajo(40), abajo(19)),
    hjust = c(0, 0.5, 0.5, 1, 1),
    vjust = c(0, 0, 1, 1, 0.5),
    rot = c(0, 0, 0, 0, 90)
  )
}

dibujar_panel <- function(E, clave) {
  # el panel tiene la proporción de escala_y sobre escala_x: misma escala en los dos ejes
  pushViewport(viewport(xscale = escala_x, yscale = escala_y))
  # las aristas de una misma familia van opacas y encima; las demás, tenues y debajo
  misma <- fam[E$desde] == fam[E$hasta]
  for (k in order(misma)) {
    if (misma[k]) {
      ancho_linea <- grosor_familia
      opacidad <- opacidad_familia
    } else {
      ancho_linea <- grosor
      opacidad <- opacidad_entre
    }
    if (E$signo[k] == "Negativa") {
      linea <- trazo_neg
    } else {
      linea <- "solid"
    }
    dibujar_arista(E$desde[k], E$hasta[k], misma[k],
                   gpar(col = pal$arista[E$signo[k]], lwd = ancho_linea, alpha = opacidad,
                        lty = linea, lineend = "butt", fill = NA))
  }

  # clave de familias en los paneles sin aristas (h = 0,40 con alpha = 0,7 y 1), para no
  # depender del color: con protanopía Personas y Financieros se confunden
  if (clave) {
    pt <- convertWidth(unit(1, "pt"), "native", valueOnly = TRUE)
    pad <- (radio_nodo + 0.9) * pt
    grupos <- split(1:N, fam)[unique(fam)]
    for (f in names(grupos)) {
      P <- contorno_familia(grupos[[f]], pad)
      grid.polygon(P[, 1], P[, 2], default.units = "native",
                   gp = gpar(fill = NA, col = pal$tinta_tenue, lty = "22", lwd = 0.4))
    }
    r <- rotulos_clave(pad, sep = 1.4 * pt)
    for (k in 1:nrow(r)) {
      grid.text(r$familia[k], r$x[k], r$y[k], default.units = "native", hjust = r$hjust[k],
                vjust = r$vjust[k], rot = r$rot[k],
                gp = gpar(fontfamily = "Arial", fontsize = 6, fontface = "italic", col = pal$tinta_suave))
    }
  }

  # el número va dentro de un círculo claro del color de su familia; en las aisladas va en gris
  activos <- (1:N) %in% c(E$desde, E$hasta)
  relleno <- ifelse(activos, sapply(color_nodo, aclarar, p = 0.55), "white")
  borde <- ifelse(activos, oscurecer_rgb(color_nodo, 0.2), pal$borde)
  grid.circle(xy[, 1], xy[, 2], r = unit(radio_nodo, "pt"), default.units = "native",
              gp = gpar(fill = relleno, col = borde, lwd = 0.35))
  grid.text(indice, xy[, 1], xy[, 2], default.units = "native",
            gp = gpar(fontfamily = "Arial", fontsize = 6,
                      col = ifelse(activos, pal$tinta, aclarar(pal$tinta, 0.4))))
  popViewport()
}

texto <- function(etiqueta, ..., tam = 6.4, cara = "plain", color = pal$tinta) {
  grid.text(etiqueta, ..., gp = gpar(fontfamily = "Arial", fontsize = tam, fontface = cara, col = color))
}

dibujar_leyenda <- function() {
  # fila 1: signo y cartera aislada
  y1 <- unit(0.83, "npc")
  grid.segments(unit(0.00, "npc"), y1, unit(0.00, "npc") + unit(13, "pt"), y1,
                gp = gpar(col = pal$arista["Positiva"], lwd = 0.9, lineend = "butt"))
  texto("Positiva", unit(0.00, "npc") + unit(16, "pt"), y1, just = "left")
  grid.segments(unit(0.25, "npc"), y1, unit(0.25, "npc") + unit(13, "pt"), y1,
                gp = gpar(col = pal$arista["Negativa"], lwd = 0.9, lty = trazo_neg, lineend = "butt"))
  texto("Negativa", unit(0.25, "npc") + unit(16, "pt"), y1, just = "left")
  grid.circle(unit(0.50, "npc") + unit(6.5, "pt"), y1, r = unit(radio_nodo, "pt"),
              gp = gpar(fill = "white", col = pal$borde, lwd = 0.35))
  texto("Cartera aislada", unit(0.50, "npc") + unit(16, "pt"), y1, just = "left")

  # fila 2: aristas dentro y entre familias
  ym <- unit(0.50, "npc")
  grid.segments(unit(0.00, "npc"), ym, unit(0.00, "npc") + unit(13, "pt"), ym,
                gp = gpar(col = pal$tinta_suave, lwd = grosor_familia + 0.2,
                          alpha = opacidad_familia, lineend = "butt"))
  texto("Dentro de una familia", unit(0.00, "npc") + unit(16, "pt"), ym, just = "left")
  grid.segments(unit(0.50, "npc"), ym, unit(0.50, "npc") + unit(13, "pt"), ym,
                gp = gpar(col = pal$tinta_suave, lwd = grosor + 0.2, alpha = opacidad_entre,
                          lineend = "butt"))
  texto("Entre familias", unit(0.50, "npc") + unit(16, "pt"), ym, just = "left")

  # fila 3: las cinco familias
  x <- c(0.00, 0.20, 0.37, 0.56, 0.74)
  y2 <- unit(0.17, "npc")
  for (k in 1:5) {
    grid.circle(unit(x[k], "npc") + unit(5, "pt"), y2, r = unit(radio_nodo, "pt"),
                gp = gpar(fill = aclarar(color_familia[k], 0.55),
                          col = oscurecer_rgb(color_familia[k], 0.2), lwd = 0.35))
    texto(names(color_familia)[k], unit(x[k], "npc") + unit(13, "pt"), y2, just = "left")
  }
}

# medidas en pulgadas: una columna de carta (márgenes de 1,95 cm y separación de 0,7 cm)
# más 10 mm que se salen hacia el margen, para separar más los grupos
ancho <- (8.5 - (2 * 1.95 + 0.7) / 2.54) / 2 + 10 / 25.4
margen_izq <- 0.24
lado <- (ancho - margen_izq) / 3
escala_x <- c(-1.15, 1.15)
escala_y <- range(xy[, 2]) + c(-0.15, 0.15)
aspecto <- diff(escala_y) / diff(escala_x)
encabezado <- 0.17
alto_leyenda <- 0.5
# franjas de 7 pt entre las filas alpha = 0,7 y alpha = 1 (para los nombres de la clave)
# y antes de la leyenda (para el nombre de Daños)
franja_media <- 7 / 72
franja_clave <- 7 / 72
alto <- encabezado + 4 * lado * aspecto + franja_media + franja_clave + alto_leyenda
fila_alfa <- c(2, 3, 4, 6) # fila del layout de cada alpha
rotulos_alfa <- c("α = 0", "α = 0,5", "α = 0,7", "α = 1")
rotulos_h <- c("h = 0,20", "h = 0,30", "h = 0,40")

for (formato in c("pdf", "png")) {
  archivo <- here("graficos", "4_redes", paste0("redes_alpha_umbral_libre.", formato))
  if (formato == "pdf") {
    cairo_pdf(archivo, ancho, alto, bg = "white")
  } else {
    png(archivo, ancho, alto, units = "in", res = 600, bg = "white", type = "cairo")
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    8, 4, widths = unit(c(margen_izq, rep(lado, 3)), "in"),
    heights = unit(c(encabezado, rep(lado * aspecto, 3), franja_media, lado * aspecto,
                     franja_clave, alto_leyenda), "in"))))
  for (k in 1:3) {
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = k + 1))
    texto(rotulos_h[k], 0.5, 0.4, tam = 7, cara = "bold")
    popViewport()
  }
  for (i in 1:4) {
    pushViewport(viewport(layout.pos.row = fila_alfa[i], layout.pos.col = 1))
    texto(rotulos_alfa[i], 0.42, 0.5, rot = 90, tam = 7, cara = "bold")
    popViewport()
    for (k in 1:3) {
      pushViewport(viewport(layout.pos.row = fila_alfa[i], layout.pos.col = k + 1))
      pushViewport(viewport(width = unit(1, "npc") - unit(5, "pt"),
                            height = unit(1, "npc") - unit(5, "pt")))
      dibujar_panel(redes[[i]][[k]], clave = (k == 3 & i >= 3))
      popViewport()
      # número de aristas arriba a la derecha
      texto(conteos[k, i], unit(1, "npc") - unit(3, "pt"), unit(1, "npc") - unit(3, "pt"),
            just = c("right", "top"), tam = 6.4, color = pal$tinta_suave)
      popViewport()
    }
  }
  pushViewport(viewport(layout.pos.row = 8, layout.pos.col = 2:4))
  dibujar_leyenda()
  popViewport(2)
  dev.off()
}
