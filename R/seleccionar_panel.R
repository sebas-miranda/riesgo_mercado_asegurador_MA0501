library(readxl)
library(dplyr)
library(tidyr)

datos <- read_excel(
  "datos/datos_originales/Reporte_Primas_Y_Siniestros_Por_Ramo_Y_Aseguradora.xlsx",
  sheet = "Por ramo-aseguradora"
)

meses <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"
)

datos <- datos |>
  rename(
    anio = Año,
    mes = Mes,
    aseguradora = Aseguradora,
    tipo = Clasificación,
    categoria = Categoría,
    ramo = Ramo,
    monto_acumulado = Monto
  ) |>
  mutate(
    mes = match(mes, meses),
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  )

# Marzo da el primer trimestre; para los demás restamos el cierre anterior.
trimestres <- datos |>
  filter(mes %in% c(3, 6, 9, 12)) |>
  group_by(aseguradora, categoria, ramo, anio, tipo) |>
  arrange(mes, .by_group = TRUE) |>
  mutate(
    monto_trimestral = case_when(
      mes == 3 ~ monto_acumulado,
      mes - lag(mes) == 3 ~ monto_acumulado - lag(monto_acumulado),
      TRUE ~ NA_real_
    )
  ) |>
  ungroup() |>
  select(anio, mes, fecha, aseguradora, categoria, ramo, tipo, monto_trimestral) |>
  pivot_wider(names_from = tipo, values_from = monto_trimestral) |>
  rename(primas_brutas = Primas, siniestros_brutos = Siniestros) |>
  mutate(
    trimestre = mes / 3,
    loss_ratio = if_else(primas_brutas != 0,
                         siniestros_brutos / primas_brutas, NA_real_)
  )

# Buscamos desde cuándo y hasta cuándo hay registros de cada aseguradora.
periodos <- trimestres |>
  group_by(aseguradora) |>
  summarise(inicio = min(fecha), fin = max(fecha))

inicio <- max(periodos$inicio)
fin <- min(periodos$fin)
numero_trimestres <- length(seq(inicio, fin, by = "3 months"))

# Dentro del período común, conservamos las series completas.
panel_final <- trimestres |>
  filter(fecha >= inicio, fecha <= fin) |>
  group_by(aseguradora, categoria, ramo) |>
  filter(n() == numero_trimestres, all(!is.na(loss_ratio))) |>
  ungroup() |>
  select(anio, trimestre, fecha, aseguradora, categoria, ramo,
         primas_brutas, siniestros_brutos, loss_ratio) |>
  arrange(aseguradora, ramo, fecha)

series <- panel_final |>
  distinct(aseguradora, categoria, ramo)

# Ejecutar desde la raíz del proyecto (abrir el archivo .Rproj).
dir.create("datos/datos_procesados", recursive = TRUE, showWarnings = FALSE)
write.csv(panel_final, "datos/datos_procesados/panel_trimestral.csv", row.names = FALSE)
write.csv(series, "datos/datos_procesados/series_seleccionadas.csv", row.names = FALSE)

print(count(series, aseguradora))
head(panel_final)
