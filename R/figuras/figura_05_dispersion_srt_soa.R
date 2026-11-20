# Grafica el LR trimestral del SRT y del SOA en dos paneles, con los trimestres sobre
# 100 % en el tono oscuro de su familia.
# Lee datos/datos_procesados/panel_trimestral.csv.
# Guarda graficos/1_exploratorio/dispersion_srt_soa.pdf
# (Figura 5, fig:eda_srt_soa, de informes/paper_final.tex).

library(here)
library(ggplot2)
source(here("R", "figuras", "paleta.R"), encoding = "UTF-8")

panel <- read.csv(here("datos", "datos_procesados", "panel_trimestral.csv"), fileEncoding = "UTF-8")
panel$fecha <- as.Date(panel$fecha)
panel$periodo <- paste0(panel$anio, "-T", panel$trimestre)

srt <- panel[panel$aseguradora == "INS" & panel$ramo == "Seguro obligatorio de riesgos del trabajo (SRT)", ]
soa <- panel[panel$aseguradora == "INS" & panel$ramo == "Seguro obligatorio de automóviles (SOA)", ]
srt$nombre <- "SRT"
soa$nombre <- "SOA"
obligatorios <- rbind(soa, srt)
obligatorios$panel <- ifelse(obligatorios$nombre == "SRT", "(a) SRT", "(b) SOA")
obligatorios$porcentaje <- 100 * obligatorios$loss_ratio
obligatorios$encima <- obligatorios$porcentaje > 100
obligatorios$clave <- paste(obligatorios$nombre, obligatorios$encima)

# los trimestres sobre 100 % van en el tono oscuro de la familia y los demás en el claro
relleno_punto <- c("SRT TRUE" = oscurecer(color_familia["Laboral"], 0.35),
                   "SRT FALSE" = aclarar(color_familia["Laboral"], 0.45),
                   "SOA TRUE" = oscurecer(color_familia["Autos"], 0.35),
                   "SOA FALSE" = aclarar(color_familia["Autos"], 0.45))

conteo <- data.frame(panel = c("(b) SOA", "(a) SRT"),
                     encima = c(sum(soa$loss_ratio > 1), sum(srt$loss_ratio > 1)))
conteo$etiqueta <- paste(conteo$encima, "de 39 sobre 100 %")

# el máximo del SRT y el mínimo del SOA se rotulan a la derecha del punto
extremos <- rbind(obligatorios[obligatorios$nombre == "SRT" & obligatorios$periodo == "2019-T4", ],
                  obligatorios[obligatorios$nombre == "SOA" & obligatorios$periodo == "2020-T2", ])
extremos$x_texto <- extremos$fecha + 110
extremos$y_texto <- extremos$porcentaje + c(-12, 0)

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
    panel.spacing = grid::unit(10, "pt"),
    plot.margin = margin(2, 6, 2, 2)
  )

figura <- ggplot(obligatorios, aes(fecha, porcentaje)) +
  geom_hline(yintercept = 100, colour = pal$referencia, linewidth = 0.35, linetype = "dotted") +
  geom_point(aes(fill = clave), shape = 21, colour = "white", stroke = 0.3, size = 1.7) +
  geom_text(data = conteo, aes(x = as.Date("2026-07-01"), y = 258, label = etiqueta),
            hjust = 1, vjust = 1, size = 6.6, size.unit = "pt", colour = pal$tinta_suave) +
  geom_text(data = extremos, aes(x = x_texto, y = y_texto,
                                 label = paste0(numero(porcentaje, 1), " %\n", periodo)),
            hjust = 0, lineheight = 0.9, size = 6.4, size.unit = "pt", colour = pal$tinta) +
  facet_wrap(~panel, nrow = 1) +
  scale_fill_manual(values = relleno_punto, guide = "none") +
  scale_y_continuous(breaks = seq(50, 200, 50), labels = numero, limits = c(30, 258)) +
  scale_x_date(breaks = as.Date(paste0(c(2017, 2021, 2025), "-01-01")), date_labels = "%Y",
               limits = as.Date(c("2017-01-01", "2026-07-01")), expand = expansion(add = 60)) +
  labs(x = NULL, y = "LR trimestral (%)") +
  tema_eda

ggsave(here("graficos", "1_exploratorio", "dispersion_srt_soa.pdf"), figura,
       width = 84.95 / 25.4, height = 2.15, device = cairo_pdf, bg = "white")
