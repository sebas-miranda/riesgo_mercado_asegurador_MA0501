# Grafica la red de Spearman con α = 0,5 y h = 0,20, con las carteras agrupadas por ramo.
# Lee resultados/contraccion_alpha/alpha_constante_050.csv.
# Guarda graficos/4_redes/red_por_ramo_C.pdf
# (Figura 11, fig:red_por_ramo, de informes/paper_final.tex).

library(here)
library(grid)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

C <- as.matrix(read.csv(here("resultados", "contraccion_alpha", "alpha_constante_050.csv"),
                        row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
series <- rownames(C)
N <- length(series)
entidad <- sub("-.*$", "", series)
ramo <- sub("^[^-]+-", "", series)

# aristas con |rho| >= 0,20, cada par una vez (fila por fila)
pares <- which(upper.tri(C) & abs(C) >= 0.20, arr.ind = TRUE)
pares <- pares[order(pares[, 1], pares[, 2]), ]
aristas <- data.frame(desde = pares[, 1], hasta = pares[, 2], peso = C[pares])
aislado <- !((1:N) %in% c(aristas$desde, aristas$hasta))

# mezcla en RGB: p = 0 deja el color y p = 1 da el fondo
mezclar <- function(color, fondo, p) {
  a <- col2rgb(color)
  b <- col2rgb(fondo)
  m <- round(a + (b - a) * p)
  rgb(m[1], m[2], m[3], maxColorValue = 255)
}

# nombres cortos de los ramos
nombre_ramo <- names(familia_de)
names(nombre_ramo) <- names(familia_de)
nombre_ramo["Seguro obligatorio de automóviles (SOA)"] <- "SOA"
nombre_ramo["Seguro obligatorio de riesgos del trabajo (SRT)"] <- "SRT"
nombre_ramo["Responsabilidad civil"] <- "Resp. civil"
nombre_ramo["Incendio y líneas aliadas"] <- "Incendio"
nombre_ramo["Vehículos marítimos"] <- "Marítimo"
nombre_ramo["Otros daños a los bienes"] <- "Daños a bienes"
nombre_ramo["Pérdidas pecuniarias"] <- "Pérdidas pec."

# radios del anillo: nodos conectados, aislados y banda de color
r_nodo <- 5.7
r_aislado <- 8.32
r_banda <- 8.0

# un sector por ramo; el ancho depende de cuántas carteras tiene
ramos <- names(familia_de)
cantidad <- c()
for (r in ramos) {
  cantidad[r] <- sum(ramo == r)
}
amplitud <- 360 * (5 + 7 * cantidad) / sum(5 + 7 * cantidad)

# ángulo de cada cartera y datos de cada sector
angulo <- rep(0, N)
sectores <- data.frame()
inicio <- -amplitud[1] / 2
for (k in 1:12) {
  r <- ramos[k]
  familia <- familia_de[r]
  fin <- inicio + amplitud[k]
  # los ramos de una misma familia quedan casi pegados
  anterior <- ramos[ifelse(k == 1, 12, k - 1)]
  siguiente <- ramos[ifelse(k == 12, 1, k + 1)]
  hueco_ini <- ifelse(familia_de[anterior] == familia, 0.15, 0.6)
  hueco_fin <- ifelse(familia_de[siguiente] == familia, 0.15, 0.6)
  sectores <- rbind(sectores, data.frame(ramo = r, a0 = inicio + hueco_ini, a1 = fin - hueco_fin,
                                         medio = inicio + amplitud[k] / 2,
                                         color = color_familia[familia]))

  for (es_aislado in c(FALSE, TRUE)) {
    capa <- which(ramo == r & aislado == es_aislado)
    capa <- capa[order(entidad[capa])]
    # con un número impar de carteras, la del centro queda bajo el nombre del ramo;
    # ahí va la de nombre más corto para que no lo tape
    if (length(capa) %% 2 == 1 & length(capa) > 1) {
      corto <- capa[which.min(nchar(entidad[capa]))]
      capa <- capa[capa != corto]
      mitad <- length(capa) %/% 2
      capa <- c(capa[seq_len(mitad)], corto, capa[-seq_len(mitad)])
    }
    for (j in seq_along(capa)) {
      angulo[capa[j]] <- inicio + amplitud[k] * (j - 0.5) / length(capa)
    }
  }
  inicio <- fin
}
radio <- ifelse(aislado, r_aislado, r_nodo)
x <- radio * cos(angulo * pi / 180)
y <- radio * sin(angulo * pi / 180)
color_nodo <- color_familia[familia_de[ramo]]
oscuro_nodo <- sapply(color_nodo, mezclar, fondo = pal$tinta, p = 0.45)

# sector del anillo entre los radios r1 y r2
dibujar_sector <- function(r1, r2, a0, a1, color) {
  a <- seq(a0, a1, length.out = 60) * pi / 180
  grid.polygon(c(r2 * cos(a), rev(r1 * cos(a))), c(r2 * sin(a), rev(r1 * sin(a))),
               default.units = "native", gp = gpar(fill = color, col = NA))
}

# texto en coordenadas polares, siempre legible de izquierda a derecha.
# tangente: sigue el anillo. junto_a: ángulo de un rombo; el texto empieza a su lado
texto_polar <- function(radio, angulo, texto, tam, color, cara = "plain",
                        tangente = FALSE, junto_a = NULL) {
  if (is.null(junto_a)) {
    giro <- angulo
  } else {
    giro <- junto_a
  }
  if (tangente) giro <- giro + 90
  giro <- giro %% 360
  invertido <- giro > 90 & giro < 270
  if (invertido) giro <- giro + 180
  if (!is.null(junto_a)) {
    angulo <- junto_a + ifelse(invertido, -1, 1) * angulo
  }
  if (!is.null(junto_a)) {
    ajuste <- 0
  } else if (tangente) {
    ajuste <- 0.5
  } else if (invertido) {
    ajuste <- 1
  } else {
    ajuste <- 0
  }
  grid.text(texto, radio * cos(angulo * pi / 180), radio * sin(angulo * pi / 180),
            default.units = "native", rot = giro, hjust = ajuste, vjust = 0.5,
            gp = gpar(fontfamily = "Arial", fontsize = tam, fontface = cara, col = color))
}

# rombo de lado `lado` (en pt) centrado en (x, y)
dibujar_rombo <- function(x, y, lado, relleno, borde, grosor) {
  d <- unit(lado * sqrt(2) / 2, "pt")
  grid.polygon(unit(x, "native") + unit.c(d * 0, d, d * 0, -d),
               unit(y, "native") + unit.c(d, d * 0, -d, d * 0),
               gp = gpar(fill = relleno, col = borde, lwd = grosor / 0.75))
}

# medidas en puntos: 0,8 del ancho de texto del artículo
ancho <- 0.8 * 17.69 / 2.54 * 72
alto <- ancho * 1.02
escala <- min(ancho / 17.5, 0.9 * alto / 17.4) # puntos por unidad, igual en los dos ejes

cairo_pdf(here("graficos", "4_redes", "red_por_ramo_C.pdf"), ancho / 72, alto / 72, bg = "white")
grid.newpage()
# los ejes ocupan el 90 % de arriba; abajo va la leyenda
pushViewport(viewport(y = unit(0.1 * alto, "pt"), height = unit(0.9 * alto, "pt"), just = "bottom"))
pushViewport(viewport(width = unit(17.5 * escala, "pt"), height = unit(17.4 * escala, "pt"),
                      xscale = c(-8.75, 8.75), yscale = c(-8.7, 8.7)))

# bandas de los sectores: primero la clara y encima la línea de color
for (k in 1:12) {
  dibujar_sector(r_nodo - 0.3, r_banda, sectores$a0[k], sectores$a1[k],
                 mezclar(sectores$color[k], "#FFFFFF", 0.86))
}
for (k in 1:12) {
  dibujar_sector(r_banda - 0.12, r_banda, sectores$a0[k], sectores$a1[k], sectores$color[k])
}

# aristas: las de un mismo ramo van más gruesas, opacas y encima
mismo <- ramo[aristas$desde] == ramo[aristas$hasta]
for (capa in c(FALSE, TRUE)) {
  for (k in which(mismo == capa)) {
    peso <- aristas$peso[k]
    if (capa) {
      grosor <- 0.45 + 1.5 * abs(peso)
      opacidad <- 0.92
    } else {
      grosor <- 0.2 + 0.7 * abs(peso)
      opacidad <- 0.34
    }
    if (peso > 0) {
      color <- pal$arista_pos
      linea <- "solid"
      punta <- "round"
    } else {
      color <- pal$arista_neg
      linea <- "32"
      punta <- "butt"
    }
    grid.segments(x[aristas$desde[k]], y[aristas$desde[k]], x[aristas$hasta[k]], y[aristas$hasta[k]],
                  default.units = "native",
                  gp = gpar(col = color, lwd = grosor / 0.75, alpha = opacidad, lty = linea, lineend = punta))
  }
}

# nodos: conectados en el color de su familia y aislados como rombos
for (i in 1:N) {
  if (aislado[i]) {
    dibujar_rombo(x[i], y[i], sqrt(13), "white", oscuro_nodo[i], 0.8)
  } else {
    grid.circle(x[i], y[i], r = unit(sqrt(14) / 2, "pt"), default.units = "native",
                gp = gpar(fill = color_nodo[i], col = "white", lwd = 0.5 / 0.75))
  }
}

# nombre de cada ramo sobre su banda
for (k in 1:12) {
  texto_polar(r_banda - 0.36, sectores$medio[k], nombre_ramo[sectores$ramo[k]], 6.6,
              mezclar(sectores$color[k], pal$tinta, 0.45), cara = "bold", tangente = TRUE)
}

# nombre de cada aseguradora: al lado del nodo o, si está aislada, al lado del rombo
for (i in 1:N) {
  if (aislado[i]) {
    texto_polar(r_aislado, (0.25 / r_aislado) * 180 / pi, entidad[i], 6.3, oscuro_nodo[i],
                cara = "bold", tangente = TRUE, junto_a = angulo[i])
  } else {
    texto_polar(r_nodo + 0.22, angulo[i], entidad[i], 6.3, pal$tinta)
  }
}
popViewport(2)

# leyenda en dos filas centradas, como la de matplotlib
tinta_suave <- mezclar(pal$tinta, "#FFFFFF", 0.30)
fila_leyenda <- function(y, etiquetas, largo, separacion, dibujar) {
  textos <- c()
  for (e in etiquetas) {
    texto <- textGrob(e, gp = gpar(fontfamily = "Arial", fontsize = 6.8))
    textos <- c(textos, convertWidth(grobWidth(texto), "pt", valueOnly = TRUE))
  }
  total <- sum(largo + separacion + textos) + 10.88 * (length(etiquetas) - 1)
  x <- (ancho - total) / 2
  for (k in seq_along(etiquetas)) {
    dibujar(k, x, x + largo)
    grid.text(etiquetas[k], unit(x + largo + separacion, "pt"), unit(y, "pt"), just = "left",
              gp = gpar(fontfamily = "Arial", fontsize = 6.8, col = pal$tinta))
    x <- x + largo + separacion + textos[k] + 10.88
  }
}

# fila de arriba: el color de cada familia
fila_leyenda(31, names(color_familia), 13.6, 2.04, function(k, x0, x1) {
  grid.rect(unit((x0 + x1) / 2, "pt"), unit(31, "pt"), unit(5.5, "pt"), unit(5.5, "pt"),
            gp = gpar(fill = color_familia[k], col = NA))
})

# fila de abajo: nodos y signo de las aristas
fila_leyenda(14.8, c("Nodo conectado", "Nodo aislado", "Correlación positiva", "Correlación negativa"),
             14.96, 5.44, function(k, x0, x1) {
  centro <- unit((x0 + x1) / 2, "pt")
  if (k == 1) {
    grid.circle(centro, unit(14.8, "pt"), r = unit(2.1, "pt"),
                gp = gpar(fill = tinta_suave, col = "white", lwd = 1 / 0.75))
  } else if (k == 2) {
    d <- unit(3.6 * sqrt(2) / 2, "pt")
    grid.polygon(centro + unit.c(d * 0, d, d * 0, -d), unit(14.8, "pt") + unit.c(d, d * 0, -d, d * 0),
                 gp = gpar(fill = "white", col = tinta_suave, lwd = 1 / 0.75))
  } else if (k == 3) {
    grid.segments(unit(x0, "pt"), unit(14.8, "pt"), unit(x1, "pt"), unit(14.8, "pt"),
                  gp = gpar(col = pal$arista_pos, lwd = 1.2 / 0.75))
  } else {
    grid.segments(unit(x0, "pt"), unit(14.8, "pt"), unit(x1, "pt"), unit(14.8, "pt"),
                  gp = gpar(col = pal$arista_neg, lwd = 1.2 / 0.75, lty = "32", lineend = "butt"))
  }
})
dev.off()
