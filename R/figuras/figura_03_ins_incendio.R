# Grafica los cambios trimestrales de las primas de INS-Incendio, con los primeros y
# segundos trimestres marcados.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda graficos/1_exploratorio/ins_incendio.pdf (Figura 3, fig:eda_incendio, de informes/paper_final.tex).

library(here)
library(ggplot2)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

panel <- read.csv(here("datos", "datos_procesados", "panel_trimestral.csv"), fileEncoding = "UTF-8")
panel$fecha <- as.Date(panel$fecha)
incendio <- panel[panel$aseguradora == "INS" & panel$ramo == "Incendio y líneas aliadas", ]

# cambio de las primas respecto al trimestre anterior (el primero no tiene)
incendio$cambio <- c(NA, diff(incendio$primas_netas))
incendio <- incendio[-1, ]
marcas <- incendio[incendio$trimestre == 1 | incendio$trimestre == 2, ]
marcas$marca <- ifelse(marcas$trimestre == 1, "Primer trimestre (T1)", "Segundo trimestre (T2)")

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
    panel.grid.major.y = element_line(colour = aclarar(pal$borde, 0.45), linewidth = 0.25),
    plot.title = element_text(size = 8, face = "bold", hjust = 0, margin = margin(b = 3)),
    plot.title.position = "plot",
    strip.background = element_blank(),
    strip.text = element_text(size = 8, face = "bold", hjust = 0, colour = pal$tinta,
                              margin = margin(t = 1, b = 3)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 7.5, margin = margin(r = 8, l = 2)),
    legend.key.width = grid::unit(16, "pt"),
    legend.key.height = grid::unit(8, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(2, "pt"),
    panel.spacing = grid::unit(7, "pt"),
    plot.margin = margin(2, 6, 2, 2)
  )

figura <- ggplot(incendio, aes(fecha, cambio)) +
  geom_hline(yintercept = 0, colour = pal$referencia, linewidth = 0.35, linetype = "dashed") +
  geom_line(colour = pal$tinta_tenue, linewidth = 0.45) +
  geom_point(data = marcas, aes(shape = marca, fill = marca),
             colour = "white", stroke = 0.4, size = 2.1) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c(pal$primas, pal$ciruela)) +
  scale_y_continuous(labels = numero, breaks = seq(-8000, 8000, 4000),
                     expand = expansion(mult = 0.06)) +
  scale_x_date(breaks = as.Date(paste0(seq(2017, 2025, 2), "-01-01")), date_labels = "%Y",
               limits = as.Date(c("2017-01-01", "2026-07-01")), expand = expansion(add = 45)) +
  labs(x = NULL, y = "Cambio (millones de colones)") +
  tema_eda

ggsave(here("graficos", "1_exploratorio", "ins_incendio.pdf"), figura,
       width = 84.95 / 25.4, height = 2.05, device = cairo_pdf, bg = "white")
