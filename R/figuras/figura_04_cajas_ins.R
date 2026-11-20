# Grafica las cajas del LR trimestral de los 11 ramos del INS, ordenadas por la mediana
# y con los colores de familia.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda graficos/1_exploratorio/cajas_ins.pdf (Figura 4, fig:eda_cajas, de informes/paper_final.tex).

library(here)
library(ggplot2)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

panel <- read.csv(here("datos", "datos_procesados", "panel_trimestral.csv"), fileEncoding = "UTF-8")
ins <- panel[panel$aseguradora == "INS", ]

lr_ins <- data.frame(ramo = ins$ramo, porcentaje = 100 * ins$loss_ratio)
lr_ins$nombre <- lr_ins$ramo
lr_ins$nombre[lr_ins$ramo == "Seguro obligatorio de riesgos del trabajo (SRT)"] <- "SRT"
lr_ins$nombre[lr_ins$ramo == "Seguro obligatorio de automóviles (SOA)"] <- "SOA"
lr_ins$familia <- factor(familia_de[lr_ins$ramo], names(color_familia))

# los ramos se ordenan por la mediana del LR
medianas <- aggregate(porcentaje ~ nombre, lr_ins, median)
medianas <- medianas[order(medianas$porcentaje), ]
medianas$nombre <- factor(medianas$nombre, medianas$nombre)
lr_ins$nombre <- factor(lr_ins$nombre, levels(medianas$nombre))

# el eje se corta en -300 %: solo el mínimo de Caución queda fuera y se marca con una flecha
limite_x <- c(-300, 270)
minimo <- min(lr_ins$porcentaje)
atipicos <- data.frame()
for (n in levels(lr_ins$nombre)) {
  d <- lr_ins[lr_ins$nombre == n, ]
  atipicos <- rbind(atipicos, d[d$porcentaje %in% boxplot.stats(d$porcentaje)$out, ])
}
atipicos <- atipicos[atipicos$porcentaje >= limite_x[1], ]
contorno_familia <- sapply(color_familia, oscurecer, p = 0.35)
relleno_familia <- sapply(color_familia, aclarar, p = 0.5)
x_mediana <- 335 # columna con el valor de la mediana, a la derecha del panel

# números con coma decimal, espacio fino de miles y signo menos
numero <- function(x, digitos = 0) {
  texto <- formatC(x, format = "f", digits = digitos, big.mark = " ", decimal.mark = ",")
  sub("^-", "−", texto)
}

tema_eda <- theme_classic(base_size = 9, base_family = "sans") +
  theme(
    text = element_text(colour = pal$tinta),
    axis.text = element_text(colour = pal$tinta, size = 7.5),
    axis.title = element_text(size = 8),
    axis.ticks = element_line(colour = pal$tinta, linewidth = 0.25),
    axis.ticks.length = grid::unit(1.8, "pt"),
    axis.line = element_line(colour = pal$tinta, linewidth = 0.25),
    plot.title = element_text(size = 8, face = "bold", hjust = 0, margin = margin(b = 3)),
    plot.title.position = "plot",
    strip.background = element_blank(),
    strip.text = element_text(size = 8, face = "bold", hjust = 0, colour = pal$tinta,
                              margin = margin(t = 1, b = 3)),
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(2, "pt"),
    panel.spacing = grid::unit(7, "pt"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = aclarar(pal$borde, 0.45), linewidth = 0.25),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    axis.text.y = element_text(size = 7.5, margin = margin(r = 2)),
    legend.position = "bottom", legend.justification = "center", legend.location = "plot",
    legend.text = element_text(size = 7, margin = margin(r = 4, l = 2)),
    legend.key.width = grid::unit(7, "pt"), legend.key.height = grid::unit(5, "pt"),
    plot.margin = margin(4, 6, 2, 2)
  )

figura <- ggplot(lr_ins, aes(porcentaje, nombre)) +
  geom_vline(xintercept = 100, colour = pal$referencia, linewidth = 0.35, linetype = "dotted") +
  geom_boxplot(aes(fill = familia, colour = familia), width = 0.62, linewidth = 0.4,
               outlier.shape = NA, staplewidth = 0, key_glyph = "rect") +
  geom_point(data = atipicos, aes(fill = familia), shape = 21, colour = "white",
             stroke = 0.3, size = 1.3, show.legend = FALSE) +
  annotate("segment", x = limite_x[1] + 38, xend = limite_x[1] + 3,
           y = "Caución", yend = "Caución", colour = pal$tinta_suave, linewidth = 0.35,
           arrow = grid::arrow(length = grid::unit(3, "pt"), type = "closed")) +
  annotate("text", x = limite_x[1] + 42, y = "Caución", vjust = -0.55, hjust = 0.2,
           label = paste0(numero(minimo, 1), " %"),
           size = 6.2, size.unit = "pt", colour = pal$tinta_suave) +
  geom_text(data = medianas, aes(x = x_mediana, y = nombre, label = numero(porcentaje, 1)),
            hjust = 1, size = 6.8, size.unit = "pt", colour = pal$tinta) +
  annotate("text", x = x_mediana, y = 11.85, label = "Mediana", hjust = 1,
           size = 6.8, size.unit = "pt", fontface = "bold", colour = pal$tinta) +
  scale_fill_manual(values = relleno_familia, drop = FALSE) +
  scale_colour_manual(values = contorno_familia, drop = FALSE) +
  scale_x_continuous(breaks = seq(-200, 200, 100), labels = numero) +
  coord_cartesian(xlim = c(limite_x[1], x_mediana), ylim = c(0.6, 11.9),
                  expand = FALSE, clip = "off") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1)) +
  labs(x = "LR trimestral (%)", y = NULL, fill = NULL, colour = NULL) +
  tema_eda

ggsave(here("graficos", "1_exploratorio", "cajas_ins.pdf"), figura,
       width = 84.95 / 25.4, height = 2.75, device = cairo_pdf, bg = "white")
