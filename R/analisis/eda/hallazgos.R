library(dplyr)
library(tidyr)

datos <- read.csv(here::here("resultados", "eda", "datos.csv"))
por_aseguradora <- read.csv(here::here("resultados", "eda", "evolucion_aseguradoras.csv"))
panel <- read.csv(here::here("resultados", "eda", "evolucion_panel.csv"))
panel$aseguradora <- "Panel"

# Sumar primas y siniestros de enero a septiembre de cada año.
montos <- bind_rows(por_aseguradora, panel)
montos <- filter(montos, anio %in% c(2025, 2026), trimestre <= 3)
montos <- montos |>
  group_by(aseguradora, anio) |>
  summarise(primas = sum(primas), siniestros = sum(siniestros),
    trimestres = n(), .groups = "drop")
montos$lr_pct <- 100 * montos$siniestros / montos$primas

# Poner los dos años lado a lado y calcular los cambios.
comparacion <- pivot_wider(montos, names_from = anio,
                           values_from = c(primas, siniestros, lr_pct, trimestres))
comparacion$cambio_primas_pct <- 100 * (comparacion$primas_2026 / comparacion$primas_2025 - 1)
comparacion$cambio_siniestros_pct <- 100 * (comparacion$siniestros_2026 / comparacion$siniestros_2025 - 1)
comparacion$cambio_lr_pp <- comparacion$lr_pct_2026 - comparacion$lr_pct_2025
comparacion$cambio_siniestros <- comparacion$siniestros_2026 - comparacion$siniestros_2025

es_panel <- comparacion$aseguradora == "Panel"
total_primas <- sum(comparacion$primas_2026[!es_panel])
comparacion$cuota_primas_2026_pct <- 100 * comparacion$primas_2026 / total_primas
total_cambio <- comparacion$cambio_siniestros[es_panel]
comparacion$aporte_cambio_siniestros_pct <- 100 * comparacion$cambio_siniestros / total_cambio
comparacion$aporte_cambio_siniestros_pct[es_panel] <- NA_real_
comparacion <- arrange(comparacion, aseguradora)
write.csv(comparacion, here::here("resultados", "eda", "comparacion_enero_septiembre_2025_2026.csv"), row.names = FALSE)

# Buscar la mediana de LR más baja y la más alta de cada ramo.
medianas <- read.csv(here::here("resultados", "eda", "medianas_LR_barras.csv"))
brechas <- medianas |>
  group_by(ramo) |>
  summarise(
    aseguradoras = n(),
    menor_aseguradora = aseguradora[which.min(mediana_pct)],
    menor_mediana_pct = min(mediana_pct),
    mayor_aseguradora = aseguradora[which.max(mediana_pct)],
    mayor_mediana_pct = max(mediana_pct),
    .groups = "drop"
  ) |>
  mutate(brecha_pp = if_else(aseguradoras > 1, mayor_mediana_pct - menor_mediana_pct, NA_real_)) |>
  arrange(desc(brecha_pp))
write.csv(brechas, here::here("resultados", "eda", "brechas_medianas_por_ramo.csv"), row.names = FALSE)

# Contar cuándo el LR supera el 100 % en estos dos ramos del INS.
persistencia <- datos |>
  filter(aseguradora == "INS", ramo %in% c(
    "Seguro obligatorio de riesgos del trabajo (SRT)", "Seguro obligatorio de automóviles (SOA)"
  )) |>
  group_by(aseguradora, ramo) |>
  summarise(trimestres = n(), mediana_pct = 100 * median(lr),
    sobre_100 = sum(lr > 1), .groups = "drop")
write.csv(persistencia, here::here("resultados", "eda", "persistencia_INS_SRT_SOA.csv"), row.names = FALSE)

# Guardar el trimestre de mayor LR y el de menor LR de MNK en Automóvil.
mnk <- datos |> filter(aseguradora == "MNK", ramo == "Automóvil")
maximo_mnk <- mnk[which.max(mnk$lr), ]
minimo_mnk <- mnk[which.min(mnk$lr), ]
maximo_mnk$episodio <- "Máximo"
minimo_mnk$episodio <- "Mínimo"

extremos_mnk <- bind_rows(maximo_mnk, minimo_mnk) |>
  mutate(lr_pct = 100 * lr, mediana_historica_pct = 100 * median(mnk$lr)) |>
  select(episodio, anio, trimestre, primas, siniestros, lr_pct, mediana_historica_pct)
write.csv(extremos_mnk, here::here("resultados", "eda", "extremos_MNK_LR.csv"), row.names = FALSE)

# Comparar el LR de ASSA y SAGICOR en dos bloques de nueve trimestres.
periodos <- por_aseguradora |>
  filter(aseguradora %in% c("ASSA", "SAGICOR"),
         trimestre <= 3, anio %in% c(2017:2019, 2024:2026))
periodos$bloque <- ifelse(periodos$anio <= 2019, "2017-2019, T1-T3", "2024-2026, T1-T3")
periodos <- periodos |>
  group_by(aseguradora, bloque) |>
  summarise(trimestres = n(), mediana_lr_pct = 100 * median(lr), .groups = "drop")

write.csv(periodos, here::here("resultados", "eda", "cambios_nivel_ASSA_SAGICOR.csv"), row.names = FALSE)
