# Grafica las primas, la siniestralidad y el LR trimestral de MNK-Automóvil, con el
# evento de 2018-T4 marcado.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda graficos/1_exploratorio/mnk_automovil.pdf (Figura 2, fig:eda_mnk, de informes/paper_final.tex).

library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

panel <- read.csv(here("datos", "datos_procesados", "panel_trimestral.csv"), fileEncoding = "UTF-8")
panel$fecha <- as.Date(panel$fecha)
panel$periodo <- paste0(panel$anio, "-T", panel$trimestre)
mnk <- panel[panel$aseguradora == "MNK" & panel$ramo == "Automóvil", ]

# en 2018-T4 las primas caen y el LR se dispara
evento <- mnk[mnk$periodo == "2018-T4", ]
anterior <- mnk[mnk$periodo == "2018-T3", ]
caida_primas <- 1 - evento$primas_netas / anterior$primas_netas

series <- c("Primas imputadas netas", "Siniestralidad neta")
montos <- rbind(
  data.frame(fecha = mnk$fecha, periodo = mnk$periodo, serie = series[1], valor = mnk$primas_netas),
  data.frame(fecha = mnk$fecha, periodo = mnk$periodo, serie = series[2], valor = mnk$siniestros_netos)
)
montos$serie <- factor(montos$serie, series)

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

eje_tiempo <- scale_x_date(
  breaks = as.Date(paste0(seq(2017, 2025, 2), "-01-01")), date_labels = "%Y",
  limits = as.Date(c("2017-01-01", "2026-07-01")), expand = expansion(add = 45)
)

figura_montos <- ggplot(montos, aes(fecha, valor, colour = serie, linetype = serie)) +
  geom_hline(yintercept = 0, colour = pal$borde, linewidth = 0.3) +
  geom_vline(xintercept = evento$fecha, colour = pal$referencia, linewidth = 0.4, linetype = "dotted") +
  geom_line(linewidth = 0.55) +
  geom_point(data = montos[montos$periodo == "2018-T4", ], aes(fill = serie),
             shape = 21, colour = "white", stroke = 0.45, size = 1.9, show.legend = FALSE) +
  annotate("text", x = evento$fecha + 50, y = evento$primas_netas,
           label = paste0(numero(-100 * caida_primas, 1), " %"),
           hjust = 0, vjust = 1.1, size = 6.8, size.unit = "pt", colour = pal$tinta_suave) +
  scale_colour_manual(values = c(pal$primas, pal$siniestros)) +
  scale_fill_manual(values = c(pal$primas, pal$siniestros)) +
  scale_linetype_manual(values = c("solid", "42")) +
  scale_y_continuous(labels = numero, breaks = seq(-500, 1500, 500)) +
  eje_tiempo +
  labs(x = NULL, y = "Millones de colones") +
  tema_eda +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

figura_lr <- ggplot(mnk, aes(fecha, 100 * loss_ratio)) +
  geom_hline(yintercept = 100, colour = pal$referencia, linewidth = 0.3, linetype = "dashed") +
  geom_vline(xintercept = evento$fecha, colour = pal$referencia, linewidth = 0.4, linetype = "dotted") +
  geom_line(colour = pal$tinta, linewidth = 0.5) +
  geom_point(data = evento, shape = 21, fill = pal$pos, colour = "white", stroke = 0.45,
             size = 2.1) +
  annotate("text", x = evento$fecha + 60, y = 100 * evento$loss_ratio,
           label = paste0(numero(100 * evento$loss_ratio, 1), " % en 2018-T4"),
           hjust = 0, vjust = 0.5, size = 7, size.unit = "pt", colour = pal$tinta) +
  scale_y_continuous(labels = numero, breaks = seq(-500, 1000, 500),
                     expand = expansion(mult = c(0.06, 0.1))) +
  eje_tiempo +
  labs(x = NULL, y = "LR (%)") +
  tema_eda

figura <- figura_montos / figura_lr +
  plot_layout(heights = c(1.15, 1), guides = "collect") &
  theme(legend.position = "top")

ggsave(here("graficos", "1_exploratorio", "mnk_automovil.pdf"), figura,
       width = 84.95 / 25.4, height = 2.8, device = cairo_pdf, bg = "white")
