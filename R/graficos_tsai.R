library(ggplot2)
library(dplyr)
library(tidyr)

carpeta_proyecto <- path.expand("~/Documents/Ciencias-Actuariales/CA-3er-año/Segundo-Semestre/Análisis númerico/Proyecto/riesgo_mercado_asegurador_MA0501")
datos <- read.csv(file.path(carpeta_proyecto, "datos/datos_procesados/panel_trimestral.csv"))
datos$fecha <- as.Date(datos$fecha)

resumen_global <- datos |>
  summarise(media = mean(loss_ratio), mediana = median(loss_ratio),
            desviacion = sd(loss_ratio))

resumen_series <- datos |>
  group_by(aseguradora, categoria, ramo) |>
  summarise(n = n(), media = mean(loss_ratio), mediana = median(loss_ratio),
            desviacion = sd(loss_ratio), minimo = min(loss_ratio),
            maximo = max(loss_ratio), .groups = "drop")

# Para trabajar por ramo, primero sumamos los montos y luego dividimos.
ramos <- datos |>
  group_by(categoria, ramo, anio, trimestre, fecha) |>
  summarise(primas = sum(primas_brutas), siniestros = sum(siniestros_brutos),
            .groups = "drop") |>
  group_by(categoria, ramo) |>
  arrange(fecha, .by_group = TRUE) |>
  mutate(lr = siniestros / primas, cambio = lr - lag(lr)) |>
  ungroup()

stopifnot(all(ramos$primas != 0), all(is.finite(ramos$lr)))

resumen_ramos <- ramos |>
  group_by(categoria, ramo) |>
  summarise(media = mean(lr), mediana = median(lr), desviacion = sd(lr),
            media_cambio = mean(cambio, na.rm = TRUE),
            mediana_cambio = median(cambio, na.rm = TRUE), .groups = "drop")

theme_set(theme_bw(base_size = 11))
azul <- "#285E87"
naranja <- "#BC641F"

# Cambios trimestrales. Cada ramo tiene su propia escala.
g_cambios <- ggplot(filter(ramos, !is.na(cambio)), aes(fecha, cambio)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = azul) +
  geom_point(colour = azul, size = 1) +
  facet_wrap(~ ramo, ncol = 3, scales = "free_y", labeller = label_wrap_gen(28)) +
  labs(title = "Cambios trimestrales del loss ratio", x = NULL, y = "Cambio del LR")

anuales <- datos |>
  group_by(categoria, ramo, anio) |>
  summarise(primas = sum(primas_brutas), siniestros = sum(siniestros_brutos),
            .groups = "drop") |>
  group_by(categoria, ramo) |>
  arrange(anio, .by_group = TRUE) |>
  mutate(lr = siniestros / primas, cambio = lr - lag(lr)) |>
  ungroup()

g_anuales <- ggplot(filter(anuales, !is.na(cambio)), aes(anio, cambio)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(colour = azul) +
  geom_point(colour = azul) +
  facet_wrap(~ ramo, ncol = 3, scales = "free_y", labeller = label_wrap_gen(28)) +
  labs(title = "Cambios anuales del loss ratio", x = "Año", y = "Cambio del LR")

# Comparacion media y mediana
medias <- resumen_ramos |>
  pivot_longer(c(media, mediana), names_to = "medida", values_to = "valor")

g_medias <- ggplot(medias, aes(valor, reorder(ramo, valor), colour = medida, shape = medida)) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(media = azul, mediana = naranja)) +
  labs(title = "Media y mediana del LR por ramo", x = "Loss ratio", y = NULL,
       colour = NULL, shape = NULL) +
  theme(legend.position = "bottom")

# Para los residuos usamos un AR(1) con trimestre estacional.
grupos <- group_split(ramos, categoria, ramo)
residuos <- list()

for (i in seq_along(grupos)) {
  d <- grupos[[i]] |>
    mutate(cambio_anterior = lag(cambio)) |>
    filter(!is.na(cambio), !is.na(cambio_anterior))
  modelo <- lm(cambio ~ cambio_anterior + factor(trimestre), data = d)
  d$residuo <- residuals(modelo)
  residuos[[i]] <- d
}
residuos <- bind_rows(residuos)

g_densidades <- ggplot(residuos, aes(residuo)) +
  geom_density(colour = azul, fill = "#DFE9F1") +
  geom_rug(alpha = .4) +
  facet_wrap(~ ramo, ncol = 3, scales = "free", labeller = label_wrap_gen(28)) +
  labs(title = "Densidad de los residuos", x = "Residuo", y = "Densidad",
       caption = "30 residuos por ramo; las curvas dependen del suavizado.")

# Para que la matriz de dispersión se pueda leer, se usan seis ramos generales.
principales <- ramos |>
  filter(categoria == "Generales") |>
  group_by(ramo) |>
  summarise(primas = sum(primas), .groups = "drop") |>
  slice_max(primas, n = 6, with_ties = FALSE) |>
  pull(ramo)

pares <- residuos |>
  filter(categoria == "Generales", ramo %in% principales) |>
  select(fecha, ramo, residuo) |>
  pivot_wider(names_from = ramo, values_from = residuo) |>
  arrange(fecha)
matriz <- as.data.frame(select(pares, all_of(principales)))

contornos <- function(x, y, ...) {
  densidad <- MASS::kde2d(x, y, n = 40)
  contour(densidad$x, densidad$y, densidad$z, add = TRUE,
          drawlabels = FALSE, col = azul, nlevels = 4)
}

# Guardar los gráficos en la carpeta del proyecto.
carpeta_graficos <- file.path(carpeta_proyecto, "graficos")

ggsave(file.path(carpeta_graficos, "cambios_trimestrales.png"), g_cambios,
       width = 14, height = 11, dpi = 300, bg = "white")
ggsave(file.path(carpeta_graficos, "cambios_anuales.png"), g_anuales,
       width = 14, height = 11, dpi = 300, bg = "white")
ggsave(file.path(carpeta_graficos, "media_mediana.png"), g_medias,
       width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(carpeta_graficos, "densidad_residuos.png"), g_densidades,
       width = 14, height = 11, dpi = 300, bg = "white")

png(file.path(carpeta_graficos, "dispersion_residuos.png"),
    width = 2400, height = 1800, res = 200)
pairs(matriz, upper.panel = points, lower.panel = contornos,
      labels = vapply(principales, function(x) paste(strwrap(x, 18), collapse = "\n"), character(1)),
      pch = 16, cex = .5, col = azul, cex.labels = .7,
      main = "Residuos: dispersión y contornos")
dev.off()

print(resumen_global)
print(resumen_series)
print(resumen_ramos)
