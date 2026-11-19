library(dplyr)
library(tidyr)
library(ggplot2)

datos <- read.csv(here::here("resultados", "eda", "datos.csv"))
datos$fecha <- as.Date(datos$fecha)
indice <- read.csv(here::here("resultados", "eda", "indice_lineas.csv"))
notas <- read.csv(here::here("resultados", "eda", "anotaciones_graficos.csv"))
ejemplo_vida <- read.csv(here::here("resultados", "eda", "ejemplo_ISCR_2024.csv"))

salida <- here::here("graficos", "eda")
dir.create(file.path(salida, "lineas"), recursive = TRUE, showWarnings = FALSE)

etiquetas <- c(
  primas = "Primas (millones de CRC)",
  siniestros = "Siniestros (millones de CRC)",
  lr = "LR (%)",
  cambio_primas = "Cambio de primas (millones de CRC)",
  cambio_siniestros = "Cambio de siniestros (millones de CRC)",
  cambio_lr = "Cambio del LR (pp)"
)
variables <- names(etiquetas)
colores <- c(Primas = "#2F6B9A", Siniestros = "#C47431", LR = "#788536")
inicios_anio <- as.Date(paste0(sort(unique(datos$anio)), "-01-01"))
leyenda_comparacion <- c(
  "Arriba: valores trimestrales. Abajo: cambio = actual - anterior; > 0 aumento, < 0 disminución, 0 sin cambio.",
  "El cambio del LR se mide en puntos porcentuales (pp). Cada panel tiene su propia escala."
)

lineas <- datos |>
  mutate(lr = 100 * lr) |>
  select(nodo, serie, fecha, trimestre, all_of(variables)) |>
  pivot_longer(all_of(variables), names_to = "variable", values_to = "valor") |>
  mutate(
    variable = factor(variable, levels = variables),
    medida = case_when(
      variable %in% c("primas", "cambio_primas") ~ "Primas",
      variable %in% c("siniestros", "cambio_siniestros") ~ "Siniestros",
      TRUE ~ "LR"
    )
  ) |>
  filter(is.finite(valor))

trimestres_marcados <- select(notas, serie, variable, trimestre)
notas <- notas |>
  group_by(serie, variable) |>
  summarise(texto = paste(texto, collapse = "\n"), .groups = "drop")

numero <- function(x, digitos = 1) {
  formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
}
cambio <- function(x, digitos = 1) {
  paste0(ifelse(x > 0, "+", ""), numero(x, digitos))
}

# Marcar el pico de MNK y el trimestre siguiente.
mnk <- datos |>
  filter(serie == "MNK - Automóvil", fecha %in% as.Date(c("2018-10-01", "2019-01-01"))) |>
  arrange(fecha)
periodo_mnk <- paste0(mnk$anio, "-T", mnk$trimestre, ": ")
notas_mnk <- data.frame(
  serie = "MNK - Automóvil",
  variable = c("lr", "cambio_lr"),
  texto = c(paste0(periodo_mnk, numero(100 * mnk$lr), "%", collapse = "\n"),
            paste0(periodo_mnk, cambio(mnk$cambio_lr), " pp", collapse = "\n"))
)

# Buscar el valor más alto de cada línea para poner la nota encima.
rangos <- lineas |>
  group_by(nodo, serie, variable) |>
  summarise(
    maximo = max(valor), amplitud = max(max(valor) - min(valor), 0.01),
    .groups = "drop"
  )
notas <- bind_rows(notas, notas_mnk) |>
  left_join(rangos, by = c("serie", "variable")) |>
  mutate(
    variable = factor(variable, levels = variables),
    fecha = as.Date("2017-01-01"),
    altura = maximo + 0.18 * amplitud,
    techo = maximo + 0.45 * amplitud
  )
marcas <- lineas |>
  inner_join(trimestres_marcados, by = c("serie", "variable", "trimestre")) |>
  mutate(
    variable = factor(variable, levels = variables),
    trimestre_marcado = paste0("T", trimestre)
  )
eventos_mnk <- lineas |>
  filter(
    serie == "MNK - Automóvil", variable %in% c("lr", "cambio_lr"),
    fecha %in% mnk$fecha
  )
pie_vida <- paste0(
  "2024-T2: primas ", cambio(ejemplo_vida$porcentaje_primas),
  "%; siniestros ", cambio(ejemplo_vida$porcentaje_siniestros, 2),
  "%; LR ", cambio(ejemplo_vida$cambio_lr), " pp."
)

graficar_nodo <- function(id_nodo) {
  serie <- filter(lineas, nodo == id_nodo)
  nombre <- serie$serie[1]
  notas_nodo <- filter(notas, nodo == id_nodo)
  marcas_nodo <- filter(marcas, nodo == id_nodo)
  puntos_mnk <- filter(eventos_mnk, nodo == id_nodo)

  pie <- leyenda_comparacion
  if (nombre == "ISCR - Vida") {
    pie <- c(pie, pie_vida)
  }
  if (nombre == "MNK - Automóvil") {
    pie <- c(pie, "Un pico del LR genera dos cambios consecutivos de signo contrario.")
  }
  pie <- paste(pie, collapse = "\n")

  ggplot(serie, aes(fecha, valor, color = medida)) +
    geom_vline(
      data = data.frame(inicio = inicios_anio),
      aes(xintercept = inicio, linetype = "Inicio de año (T1)"),
      color = "grey65", linewidth = 0.3
    ) +
    geom_hline(
      data = data.frame(variable = factor(variables[4:6], levels = variables)),
      aes(yintercept = 0), color = "grey60", linewidth = 0.25, linetype = "dashed"
    ) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.55, show.legend = FALSE) +
    geom_point(
      data = marcas_nodo, aes(shape = trimestre_marcado),
      fill = "white", size = 1.8, stroke = 0.5
    ) +
    geom_point(
      data = puntos_mnk,
      shape = 21, fill = "white", size = 2, stroke = 0.6, show.legend = FALSE
    ) +
    geom_blank(data = notas_nodo, aes(fecha, techo), inherit.aes = FALSE) +
    geom_text(
      data = notas_nodo, aes(fecha, altura, label = texto), inherit.aes = FALSE,
      hjust = 0, vjust = 0, size = 2.8, lineheight = 1.1, color = "grey20"
    ) +
    facet_wrap(
      ~variable, ncol = 3, scales = "free_y", drop = FALSE,
      axes = "all_x", axis.labels = "all_x",
      labeller = as_labeller(etiquetas)
    ) +
    scale_color_manual(values = colores, limits = names(colores), name = NULL) +
    scale_linetype_manual(values = c("Inicio de año (T1)" = "dotted"), name = NULL) +
    scale_shape_manual(
      values = c(T1 = 21, T2 = 24),
      limits = sort(unique(marcas_nodo$trimestre_marcado)), name = NULL
    ) +
    scale_x_date(
      limits = range(datos$fecha),
      breaks = inicios_anio,
      date_labels = "%Y"
    ) +
    scale_y_continuous(labels = function(x) format(x, digits = 3, trim = TRUE)) +
    guides(
      color = guide_legend(order = 1, override.aes = list(shape = NA)),
      linetype = guide_legend(order = 2),
      shape = guide_legend(order = 3, override.aes = list(color = "grey25", fill = "white"))
    ) +
    labs(title = nombre, subtitle = "2017-T1 a 2026-T3", caption = pie, x = NULL, y = NULL) +
    theme_bw(base_size = 9, base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey35"),
      plot.caption = element_text(size = 8.5, hjust = 0, margin = margin(t = 7)),
      legend.position = "top", legend.justification = "left",
      legend.margin = margin(0, 0, 1, 0), legend.key.width = grid::unit(1.4, "lines"),
      strip.background = element_rect(fill = "grey95", color = "grey75"),
      strip.text = element_text(size = 8.5),
      axis.text.x = element_text(size = 7.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey93", linewidth = 0.2),
      panel.grid.major.x = element_blank(),
      panel.spacing = grid::unit(1, "lines")
    )
}

pdf(file.path(salida, "LINEAS_PANEL_42.pdf"), width = 12, height = 8,
  family = "Helvetica", useDingbats = FALSE)
for (i in seq_len(nrow(indice))) {
  id_nodo <- indice$nodo[i]
  archivo_png <- indice$png[i]
  grafico <- graficar_nodo(id_nodo)
  print(grafico)
  ggsave(archivo_png, grafico, path = salida,
    width = 12, height = 8, dpi = 160, device = ragg::agg_png)
}
dev.off()
