# Escoge la ventana con más observaciones de series completas y con LR variable.
# Lee datos/datos_originales/Reporte_Cuentas_Tecnicas.xlsx.
# Guarda datos/datos_procesados/panel_trimestral.csv con los LR trimestrales.

library(readxl)
library(tidyverse)
library(here)

# LEER DATOS
datos <- read_excel(
  here("datos", "datos_originales", "Reporte_Cuentas_Tecnicas.xlsx")
)
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
    fecha = as.Date(paste(anio, trimestre * 3 - 2, 1, sep = "-")),
    periodo = 4 * anio + trimestre
  ) %>%
  arrange(aseguradora, ramo, fecha)


datos_validos <- trimestres %>%
  filter(primas_netas > 0, !is.na(siniestros_netos)) %>%
  mutate(loss_ratio = siniestros_netos / primas_netas)

primero <- min(trimestres$periodo, na.rm = TRUE)
ultimo <- max(trimestres$periodo, na.rm = TRUE)
n_trimestres <- ultimo - primero + 1
if (n_trimestres < 2) {
  stop("Se necesitan al menos dos trimestres.")
}

mejor_panel <- datos_validos[0, ]

# probar primero las ventanas largas para que ganen los empates
for (duracion in n_trimestres:2) {
  for (inicio in primero:(ultimo - duracion + 1)) {
    fin <- inicio + duracion - 1
    panel <- datos_validos %>%
      filter(periodo >= inicio, periodo <= fin) %>%
      group_by(aseguradora, categoria, ramo) %>%
      filter(n_distinct(periodo) == duracion, n_distinct(loss_ratio) > 1) %>%
      ungroup()

    # cada serie aporta una fila por trimestre
    if (nrow(panel) > nrow(mejor_panel)) {
      mejor_panel <- panel
    }
  }
}


datos_LR <- mejor_panel %>%
  select(
    anio, trimestre, fecha, aseguradora,
    categoria, ramo, primas_netas,
    siniestros_netos, loss_ratio
  )


write_csv(
  datos_LR,
  here("datos", "datos_procesados", "panel_trimestral.csv")
)


