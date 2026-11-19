library(dplyr)
library(tidyr)

datos <- read.csv(here::here("resultados", "eda", "datos.csv"))

# Poner los distintos cambios en una sola columna para resumirlos juntos.
cambios <- select(datos, serie, trimestre, cambio_primas, cambio_siniestros,
                  cambio_lr, porcentaje_primas, porcentaje_siniestros)
cambios <- pivot_longer(cambios, -c(serie, trimestre), names_to = "variable",
                       values_to = "cambio")

# Contar subidas, bajadas y ceros por serie y trimestre.
resumen <- cambios |>
  group_by(serie, trimestre, variable) |>
  summarise(
    n = sum(is.finite(cambio)),
    subidas = sum(cambio > 0, na.rm = TRUE),
    bajadas = sum(cambio < 0, na.rm = TRUE),
    ceros = sum(cambio == 0, na.rm = TRUE),
    media = mean(cambio, na.rm = TRUE),
    mediana = median(cambio, na.rm = TRUE),
    desviacion = sd(cambio, na.rm = TRUE),
    iqr = IQR(cambio, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(resumen, here::here("resultados", "eda", "cambios_por_trimestre.csv"), row.names = FALSE, na = "")

# Escoger los trimestres de estas series.
casos_elegidos <- data.frame(
  serie = c(
    "ISCR - Vida",
    "ISCR - Accidentes",
    "MAGIST - Accidentes",
    "INS - Incendio y líneas aliadas",
    "INS - Incendio y líneas aliadas",
    "LAFISE - Automóvil",
    "QUALITAS - Automóvil",
    "SAGICOR - Pérdidas pecuniarias"
  ),
  trimestre = c(2, 2, 2, 1, 2, 1, 1, 4)
)
casos <- semi_join(resumen, casos_elegidos, by = c("serie", "trimestre"))
write.csv(casos, here::here("resultados", "eda", "casos_hallazgos.csv"), row.names = FALSE, na = "")

ejemplo <- datos |>
  filter(serie == "ISCR - Vida", anio == 2024, trimestre == 2) |>
  select(serie, anio, trimestre, porcentaje_primas, porcentaje_siniestros, cambio_lr)
write.csv(ejemplo, here::here("resultados", "eda", "ejemplo_ISCR_2024.csv"), row.names = FALSE)

# Escoger los 12 cambios de LR más grandes, sean subidas o bajadas.
extremos <- datos |>
  filter(is.finite(cambio_lr)) |>
  arrange(desc(abs(cambio_lr))) |>
  select(serie, anio, trimestre, primas, siniestros, lr, cambio_lr) |>
  slice_head(n = 12)
write.csv(extremos, here::here("resultados", "eda", "mayores_cambios_LR.csv"), row.names = FALSE)

perfil <- datos |>
  group_by(serie) |>
  summarise(
    media_lr = mean(lr), mediana_lr = median(lr),
    desviacion_lr = sd(lr), iqr_lr = IQR(lr), mad_lr = mad(lr),
    prima_minima = min(primas), prima_mediana = median(primas),
    iqr_primas = IQR(primas),
    iqr_sobre_mediana_primas = 100 * IQR(primas) / median(primas),
    trimestres_lr_sobre_uno = sum(lr > 1),
    trimestres_lr_negativo = sum(lr < 0),
    lr_minimo = min(lr), lr_maximo = max(lr), .groups = "drop"
  )
write.csv(perfil, here::here("resultados", "eda", "resumen_nodos.csv"), row.names = FALSE)

seleccion <- read.csv(here::here("datos", "eda", "anotaciones_trimestrales.csv"))
notas <- left_join(seleccion, resumen, by = c("serie", "variable", "trimestre"))
if (nrow(notas) != nrow(seleccion) || anyNA(notas$n)) {
  stop("Cada anotación necesita una fila del resumen de cambios.")
}
notas$veces <- ifelse(notas$direccion == "sube", notas$subidas, notas$bajadas)
notas$texto <- paste0("T", notas$trimestre, ": ", notas$direccion,
                     " ", notas$veces, " de ", notas$n)
write.csv(notas, here::here("resultados", "eda", "anotaciones_graficos.csv"), row.names = FALSE)
