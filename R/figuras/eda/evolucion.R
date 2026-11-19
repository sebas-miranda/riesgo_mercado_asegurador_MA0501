library(dplyr)
library(tidyr)
library(ggplot2)

panel <- read.csv(here::here("resultados", "eda", "evolucion_panel.csv"))
por_aseguradora <- read.csv(here::here("resultados", "eda", "evolucion_aseguradoras.csv"))

panel$fecha <- as.Date(panel$fecha)
por_aseguradora$fecha <- as.Date(por_aseguradora$fecha)
texto_ramos <- ifelse(por_aseguradora$ramos == 1, " ramo)", " ramos)")
por_aseguradora$titulo <- paste0(por_aseguradora$aseguradora, " (", por_aseguradora$ramos, texto_ramos)

salida <- here::here("graficos", "eda")
dir.create(file.path(salida, "evolucion"), recursive = TRUE, showWarnings = FALSE)

variables <- c("primas", "siniestros", "lr")
nombres <- c(primas = "Primas", siniestros = "Siniestros", lr = "LR")
colores <- c(primas = "#2F6B9A", siniestros = "#C47431", lr = "#788536")
tipos_linea <- c(primas = "solid", siniestros = "dashed", lr = "solid")
rotulos <- c(
  primas = "Primas (miles de millones de CRC)",
  siniestros = "Siniestros (miles de millones de CRC)", lr = "LR (%)"
)

# Dividir los montos entre mil para mostrarlos en miles de millones.
montos <- mutate(por_aseguradora, primas = primas / 1000, siniestros = siniestros / 1000)
montos <- montos |>
  pivot_longer(c(primas, siniestros), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, levels = variables[1:2]))
total <- mutate(panel, primas = primas / 1000, siniestros = siniestros / 1000, lr = 100 * lr)
total <- total |>
  pivot_longer(all_of(variables), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, levels = variables))

formato_eje <- function(valores) {
  etiquetas <- character(length(valores))
  for (i in seq_along(valores)) {
    etiquetas[i] <- format(valores[i], trim = TRUE, scientific = FALSE,
                           big.mark = " ", decimal.mark = ",")
  }
  etiquetas
}
inicios_anio <- data.frame(inicio = as.Date(paste0(sort(unique(panel$anio)), "-01-01")))
marcas_ano <- geom_vline(
  data = inicios_anio, aes(xintercept = inicio), color = "grey82", linewidth = 0.25,
  linetype = "dotted", show.legend = FALSE
)
calendario <- scale_x_date(
  limits = range(panel$fecha), breaks = inicios_anio$inicio, date_labels = "%Y",
  expand = expansion(mult = c(0.025, 0.045))
)
tema_evolucion <- theme_bw(base_size = 9, base_family = "sans") +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "grey35"),
    legend.position = "top", legend.justification = "left",
    legend.key.width = grid::unit(1.8, "lines"),
    strip.background = element_rect(fill = "grey97", color = "grey80"),
    strip.text = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.2),
    panel.spacing = grid::unit(1, "lines"),
    plot.margin = margin(t = 7, r = 12, b = 7, l = 7)
  )

referencias <- data.frame(
  variable = factor(variables, levels = variables), nivel = c(0, 0, 100)
)
grafico_panel <- ggplot(total, aes(fecha, valor, color = variable, linetype = variable)) +
  marcas_ano +
  geom_hline(data = referencias, aes(yintercept = nivel), color = "grey65",
    linewidth = 0.25, linetype = "dashed") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.7, show.legend = FALSE) +
  facet_wrap(~variable, ncol = 1, scales = "free_y", axes = "all_x", axis.labels = "all_x",
    labeller = as_labeller(rotulos)) +
  scale_color_manual(values = colores, labels = nombres, name = NULL) +
  scale_linetype_manual(values = tipos_linea, labels = nombres, name = NULL) +
  calendario + scale_y_continuous(labels = formato_eje) +
  labs(
    title = "Evolución trimestral del panel",
    subtitle = "42 nodos de 12 aseguradoras | 2017-T1 a 2026-T3",
    x = NULL, y = NULL
  ) + tema_evolucion

grafico_montos_aseguradora <- ggplot(montos,
  aes(fecha, valor, color = variable, linetype = variable)) +
  marcas_ano +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.25) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.4, show.legend = FALSE) +
  facet_wrap(~titulo, ncol = 3, scales = "free_y", axes = "all_x", axis.labels = "all_x") +
  scale_color_manual(values = colores[1:2], labels = nombres[1:2], name = NULL) +
  scale_linetype_manual(values = tipos_linea[1:2], labels = nombres[1:2], name = NULL) +
  calendario + scale_y_continuous(labels = formato_eje) +
  labs(
    title = "Primas y Siniestros por aseguradora",
    subtitle = "Ramos incluidos en el panel de 42 nodos | 2017-T1 a 2026-T3",
    x = NULL, y = "Miles de millones de CRC"
  ) + tema_evolucion

grafico_lr_aseguradora <- ggplot(por_aseguradora, aes(fecha, 100 * lr)) +
  marcas_ano +
  geom_hline(yintercept = 100, color = "grey65", linewidth = 0.25, linetype = "dashed") +
  geom_line(color = colores[["lr"]], linewidth = 0.5) +
  geom_point(color = colores[["lr"]], size = 0.6) +
  facet_wrap(~titulo, ncol = 3, scales = "free_y", axes = "all_x", axis.labels = "all_x") +
  calendario + scale_y_continuous(labels = formato_eje) +
  labs(
    title = "LR trimestral por aseguradora",
    subtitle = "Ramos incluidos en el panel de 42 nodos | 2017-T1 a 2026-T3",
    x = NULL, y = "LR (%)"
  ) + tema_evolucion

graficos <- list(
  panel = grafico_panel,
  primas_siniestros_aseguradora = grafico_montos_aseguradora,
  lr_aseguradora = grafico_lr_aseguradora
)
pdf(file.path(salida, "EVOLUCION_PANEL_42.pdf"), width = 12, height = 10.5,
  family = "Helvetica", useDingbats = FALSE, useKerning = FALSE)
for (grafico in graficos) {
  print(grafico)
}
dev.off()
for (nombre in names(graficos)) {
  ggsave(paste0("evolucion/", nombre, ".png"), graficos[[nombre]], path = salida,
    width = 12, height = 10.5, dpi = 160, device = ragg::agg_png)
}
