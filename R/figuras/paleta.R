# Colores de las figuras del artículo: zafiro y rubí (decisión del 2026-11-19).
# Se carga con source(here("R", "figuras", "paleta.R")) y deja la lista pal,
# las funciones aclarar() y oscurecer() y los colores de las familias de ramos.

pal <- list(
  neg = "#194CB1", mid = "#F9F6F2", pos = "#9E122F",
  arista_neg = "#5279C1", arista_pos = "#B65A5F",
  tinta = "#171F2E", borde = "#CDD1D9", referencia = "#758196",
  primas = "#008674", siniestros = "#BF7B1F", ciruela = "#7F3E7A"
)

# mezcla un color con blanco: p = 0 deja el color y p = 1 da blanco
aclarar <- function(color, p) {
  colorRampPalette(c(color, "white"), space = "Lab")(101)[round(100 * p) + 1]
}

# mezcla un color con la tinta: p = 0 deja el color y p = 1 da la tinta
oscurecer <- function(color, p) {
  colorRampPalette(c(color, pal$tinta), space = "Lab")(101)[round(100 * p) + 1]
}

pal$tinta_suave <- aclarar(pal$tinta, 0.30)
pal$tinta_tenue <- aclarar(pal$tinta, 0.55)
pal$arista <- c(Positiva = pal$arista_pos, Negativa = pal$arista_neg)

# escala de las matrices: de neg a mid y de mid a pos en línea recta en el espacio
# OKLab (Ottosson, 2020); en RGB el azul se volvía lila
hex_a_oklab <- function(hex) {
  colores <- col2rgb(hex) / 255
  colores <- ifelse(colores <= 0.04045, colores / 12.92, ((colores + 0.055) / 1.055)^2.4)
  M1 <- matrix(c(0.4122214708, 0.5363325363, 0.0514459929,
                 0.2119034982, 0.6806995451, 0.1073969566,
                 0.0883024619, 0.2817188376, 0.6299787005), 3, byrow = TRUE)
  M2 <- matrix(c(0.2104542553, 0.7936177850, -0.0040720468,
                 1.9779984951, -2.4285922050, 0.4505937099,
                 0.0259040371, 0.7827717662, -0.8086757660), 3, byrow = TRUE)
  lms <- M1 %*% colores
  t(M2 %*% (sign(lms) * abs(lms)^(1 / 3)))
}

oklab_a_hex <- function(lab) {
  lms <- cbind(lab[, 1] + 0.3963377774 * lab[, 2] + 0.2158037573 * lab[, 3],
               lab[, 1] - 0.1055613458 * lab[, 2] - 0.0638541728 * lab[, 3],
               lab[, 1] - 0.0894841775 * lab[, 2] - 1.2914855480 * lab[, 3])^3
  MI <- matrix(c(4.0767416621, -3.3077115913, 0.2309699292,
                 -1.2684380046, 2.6097574011, -0.3413193965,
                 -0.0041960863, -0.7034186147, 1.7076147010), 3, byrow = TRUE)
  colores <- lms %*% t(MI)
  colores <- pmin(pmax(colores, 0), 1)
  colores <- ifelse(colores <= 0.0031308, 12.92 * colores, 1.055 * colores^(1 / 2.4) - 0.055)
  rgb(colores[, 1], colores[, 2], colores[, 3])
}

crear_rampa <- function(neg, mid, pos) {
  extremos <- hex_a_oklab(c(neg, mid, pos))
  valores <- seq(-1, 1, length.out = 257)
  lab <- matrix(0, nrow = 257, ncol = 3)
  for (i in 1:257) {
    if (valores[i] < 0) {
      destino <- extremos[1, ]
    } else {
      destino <- extremos[3, ]
    }
    lab[i, ] <- extremos[2, ] + (destino - extremos[2, ]) * abs(valores[i])
  }
  oklab_a_hex(lab)
}

pal$rampa <- crear_rampa(pal$neg, pal$mid, pal$pos)

# familia de cada ramo y color de cada familia, en el orden de los sectores de la figura 11
familia_de <- c(
  "Salud" = "Personas", "Vida" = "Personas", "Accidentes" = "Personas",
  "Automóvil" = "Autos", "Seguro obligatorio de automóviles (SOA)" = "Autos",
  "Seguro obligatorio de riesgos del trabajo (SRT)" = "Laboral",
  "Incendio y líneas aliadas" = "Daños", "Vehículos marítimos" = "Daños",
  "Otros daños a los bienes" = "Daños",
  "Caución" = "Financieros", "Responsabilidad civil" = "Financieros",
  "Pérdidas pecuniarias" = "Financieros"
)
color_familia <- c(Personas = "#41A083", Autos = "#D79F4C", Laboral = "#96A362",
                   Daños = "#BC78AD", Financieros = "#B09A84")
