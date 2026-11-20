# Grafica las primas y la siniestralidad agregadas del panel y del INS, con 2019-T4 marcado.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda graficos/1_exploratorio/evolucion_panel_ins.pdf
# (Figura 1, fig:eda_evolucion, de informes/paper_final.tex).

library(here)
library(ggplot2)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

panel <- read.csv(here("datos", "datos_procesados", "panel_trimestral.csv"), fileEncoding = "UTF-8")
panel$fecha <- as.Date(panel$fecha)
panel$periodo <- paste0(panel$anio, "-T", panel$trimestre)
ins <- panel[panel$aseguradora == "INS", ]

# sumar primas y siniestros por trimestre
total_panel <- aggregate(cbind(primas_netas, siniestros_netos) ~ fecha + periodo, panel, sum)
total_ins <- aggregate(cbind(primas_netas, siniestros_netos) ~ fecha + periodo, ins, sum)
grupos <- c("(a) Panel de 42 carteras", "(b) INS, 11 ramos")
series <- c("Primas imputadas netas", "Siniestralidad neta")

# una fila por trimestre, serie y grupo
evolucion <- rbind(
  data.frame(fecha = total_panel$fecha, periodo = total_panel$periodo, serie = series[1],
             valor = total_panel$primas_netas, grupo = grupos[1]),
  data.frame(fecha = total_panel$fecha, periodo = total_panel$periodo, serie = series[2],
             valor = total_panel$siniestros_netos, grupo = grupos[1]),
  data.frame(fecha = total_ins$fecha, periodo = total_ins$periodo, serie = series[1],
             valor = total_ins$primas_netas, grupo = grupos[2]),
  data.frame(fecha = total_ins$fecha, periodo = total_ins$periodo, serie = series[2],
             valor = total_ins$siniestros_netos, grupo = grupos[2])
)
evolucion$serie <- factor(evolucion$serie, series)
evolucion$grupo <- factor(evolucion$grupo, grupos)
pico <- evolucion[evolucion$periodo == "2019-T4", ]

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

figura <- ggplot(evolucion, aes(fecha, valor / 1000, colour = serie, linetype = serie)) +
  geom_vline(xintercept = pico$fecha[1], colour = pal$referencia, linewidth = 0.4,
             linetype = "dotted") +
  geom_line(linewidth = 0.55) +
  geom_point(data = pico, aes(fill = serie),
             shape = 21, colour = "white", stroke = 0.45, size = 1.9, show.legend = FALSE) +
  facet_wrap(~grupo, ncol = 1) +
  scale_colour_manual(values = c(pal$primas, pal$siniestros)) +
  scale_fill_manual(values = c(pal$primas, pal$siniestros)) +
  scale_linetype_manual(values = c("solid", "42")) +
  scale_y_continuous(labels = numero, breaks = seq(50, 250, 50), limits = c(45, 255)) +
  scale_x_date(breaks = as.Date(paste0(seq(2017, 2025, 2), "-01-01")), date_labels = "%Y",
               limits = as.Date(c("2017-01-01", "2026-07-01")), expand = expansion(add = 45)) +
  labs(x = NULL, y = "Miles de millones de colones") +
  tema_eda

ggsave(here("graficos", "1_exploratorio", "evolucion_panel_ins.pdf"), figura,
       width = 84.95 / 25.4, height = 3.0, device = cairo_pdf, bg = "white")
