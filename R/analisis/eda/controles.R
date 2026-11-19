library(dplyr)

datos <- read.csv(here::here("resultados", "eda", "datos.csv"))

# Contar filas, huecos y valores fuera de lo esperado por trimestre.
por_trimestre <- datos |>
  group_by(anio, trimestre) |>
  summarise(
    observados = n(),
    huecos = 42 - observados,
    primas_cero = sum(primas == 0),
    primas_negativas = sum(primas < 0),
    siniestros_negativos = sum(siniestros < 0),
    lr_no_calculables = sum(!is.finite(lr)),
    .groups = "drop"
  )
write.csv(por_trimestre, here::here("resultados", "eda", "calidad_por_trimestre.csv"), row.names = FALSE)

ruta_excel <- here::here("datos", "datos_originales", "Reporte_Cuentas_Tecnicas.xlsx")
if (!file.exists(ruta_excel)) {
  ruta_excel <- here::here("fuentes", "cuenta_tecnica.xlsx")
}
# Basta leer la primera columna para contar las filas del Excel.
primera_columna <- readxl::read_excel(ruta_excel, col_types = c("numeric", rep("skip", 7)))

# Resumir los conteos de todo el panel.
conteos <- c(
  "Filas contables de la fuente" = nrow(primera_columna),
  "Observaciones analizadas" = nrow(datos),
  "Nodos analizados" = n_distinct(datos$nodo),
  "Trimestres por nodo" = n_distinct(datos$fecha),
  "Huecos" = sum(por_trimestre$huecos),
  "Primas no positivas" = sum(datos$primas <= 0),
  "Siniestros negativos" = sum(datos$siniestros < 0),
  "LR no calculables" = sum(!is.finite(datos$lr))
)
calidad <- data.frame(control = names(conteos), cantidad = unname(conteos))
write.csv(calidad, here::here("resultados", "eda", "calidad.csv"), row.names = FALSE)

# Volver a calcular cada caja con boxplot.stats() y compararla con el CSV.
resumen <- read.csv(here::here("resultados", "eda", "resumen_cajas.csv"))
ventanas <- read.csv(here::here("resultados", "eda", "ventanas_cajas.csv"))
atipicos <- read.csv(here::here("resultados", "eda", "valores_fuera_bigotes.csv"))

# Los CSV de las cajas usan el LR en porcentaje.
datos$lr <- 100 * datos$lr
revision <- data.frame(
  variable = c("primas", "siniestros", "lr"),
  cajas = 0,
  estadisticas_iguales = TRUE,
  atipicos_iguales = TRUE,
  cajas_completas = TRUE,
  bigotes_completos = TRUE
)

for (i in seq_len(nrow(revision))) {
  medida <- revision$variable[i]
  cajas <- filter(resumen, variable == medida)
  ventana <- filter(ventanas, variable == medida)
  valores_fuera <- filter(atipicos, variable == medida)

  # Revisar que las cajas y los bigotes quepan en los límites del eje.
  revision$cajas[i] <- nrow(cajas)
  revision$cajas_completas[i] <- all(cajas$q1 >= ventana$desde & cajas$q3 <= ventana$hasta)
  revision$bigotes_completos[i] <- all(
    cajas$bigote_inferior >= ventana$desde & cajas$bigote_superior <= ventana$hasta
  )

  for (j in seq_len(nrow(cajas))) {
    fila <- cajas[j, ]
    valores <- datos[datos$nodo == fila$nodo, medida]
    caja <- boxplot.stats(valores)
    guardados <- c(fila$bigote_inferior, fila$q1, fila$mediana, fila$q3, fila$bigote_superior)
    atipicos_nodo <- valores_fuera$valor[valores_fuera$nodo == fila$nodo]

    if (!isTRUE(all.equal(caja$stats, guardados))) {
      revision$estadisticas_iguales[i] <- FALSE
    }
    if (!isTRUE(all.equal(sort(caja$out), sort(atipicos_nodo)))) {
      revision$atipicos_iguales[i] <- FALSE
    }
  }
}

write.csv(revision, here::here("resultados", "eda", "verificacion_ejes_log.csv"), row.names = FALSE)
