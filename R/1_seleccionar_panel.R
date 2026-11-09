library(readxl)
library(tidyverse)

# LEER DATOS
datos <- read_excel("datos/datos_originales/Reporte_Cuentas_Tecnicas.xlsx")

# Defina las columnas:
primas <- "I. Primas Imputadas, Netas de Reaseguro"
siniestros <- "II. Siniestralidad, Neta de Reaseguro"

# FILTRAR LAS ENTRADAS DE PRIMAS Y SINIESTROS, AGREGAR TRIMESTRE Y TIPO
datos <- datos %>%
  rename(
    anio = `Año`,
    trimestre_texto = `Trimestre`,
    aseguradora = `Aseguradora`,
    categoria = `Categoría`,
    ramo = `Ramo`,
    cuenta_madre = `Cuenta madre`,
    cuenta = `Cuenta`,
    monto = `Monto`
  ) %>%
  # Aqui la idea es filtrar sobre las entradas que representan unicamente
  # primas netas y siniestros netos
  filter(
    cuenta_madre %in% c(primas, siniestros),
    str_detect(cuenta, "^(I\\.|II\\.)")
  ) %>%
 
  mutate(
       # Agregamos una columna para identificar trimestre
      trimestre = case_when(
      trimestre_texto == "Ene-Mar" ~ 1,
      trimestre_texto == "Abr-Jun" ~ 2,
      trimestre_texto == "Jul-Set" ~ 3,
      trimestre_texto == "Oct-Dic" ~ 4,
      TRUE ~ NA_real_
),
    # Para facilitar que representa cada entrada, agregamos otra columna para
    # comprender si es una prima neta / siniestro neto
    anio = as.integer(anio),
    tipo = if_else(
      cuenta_madre == primas,
      "primas_netas",
      "siniestros_netos"
    )
  )

# ORDENAR LOS DATOS TRIMESTRALES EN ORDEN ASCENDENTE Y ORDENAR COLUMNAS
# DE PRIMAS Y SINIESTROS
trimestres <- datos %>%
  # Para construir la matriz nos interesan nada mas las siguientes columnas:
  select(anio, trimestre, aseguradora, categoria, ramo, tipo, monto) %>%

  # Como las entradas difieren las primas y los siniestros, la idea 
  # Es trasnformar las filas, de forma que en una misma entrada 
  # Haya una columna para primas, y otra para siniestros:
  pivot_wider(
    names_from = tipo,
    values_from = monto
  ) %>%
  # Transformamos las fechas en tipo Date:
  mutate(
    fecha = as.Date(paste(anio, trimestre * 3 - 2, 1, sep = "-"))
  ) %>%
  arrange(aseguradora, ramo, fecha)


# CALCULAR LR
datos_LR <- trimestres %>%
  group_by(aseguradora, categoria, ramo) %>%
  filter(
    # Verificamos que si hayan 39 trimestres: 2017-1 a 2026-3:
    n_distinct(anio, trimestre) == 39,
    # FIltramos por primas positivas, y primas y siniestros sin registros nulos:
    all(!is.na(primas_netas)),
    all(primas_netas > 0),
    all(!is.na(siniestros_netos))
  ) %>%
  ungroup() %>%
  mutate(
    # Nueva columna, calculando el LR = siniestros/primas:
    loss_ratio = siniestros_netos / primas_netas
  ) %>%
  select(
    anio, trimestre, fecha, aseguradora,
    categoria, ramo, primas_netas,
    siniestros_netos, loss_ratio
  )

# Las series representan la cantidad de ramos-aseguradora disponibles
# en la base de datos. Como se pueden observar, son 42.
series <- datos_LR %>%
  distinct(aseguradora, categoria, ramo)

write_csv(
  datos_LR,
  "datos/datos_procesados/panel_trimestral.csv"
)

write_csv(
  series,
  "datos/datos_procesados/series_seleccionadas.csv"
)