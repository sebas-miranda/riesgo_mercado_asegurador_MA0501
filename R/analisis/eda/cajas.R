library(dplyr)
library(tidyr)

datos <- read.csv(here::here("resultados", "eda", "datos.csv"))

# Pasar el LR a porcentaje antes de calcular los cuartiles.
datos$lr <- 100 * datos$lr
cajas <- select(datos, nodo, serie, anio, trimestre, primas, siniestros, lr)
cajas <- pivot_longer(cajas, c(primas, siniestros, lr), names_to = "variable",
                     values_to = "valor")

# Calcular mediana, cuartiles y bigotes para cada caja.
resumen <- cajas |>
  group_by(nodo, serie, variable) |>
  summarise(
    n = n(),
    minimo = min(valor),
    q1 = quantile(valor, 0.25),
    mediana = median(valor),
    q3 = quantile(valor, 0.75),
    maximo = max(valor),
    iqr = q3 - q1,
    media = mean(valor),
    desviacion = sd(valor),
    mad = mad(valor),
    bigote_inferior = min(valor[valor >= q1 - 1.5 * iqr]),
    bigote_superior = max(valor[valor <= q3 + 1.5 * iqr]),
    fuera_bigotes = sum(valor < q1 - 1.5 * iqr | valor > q3 + 1.5 * iqr),
    .groups = "drop"
  )
write.csv(resumen, here::here("resultados", "eda", "resumen_cajas.csv"), row.names = FALSE)

# Guardar los puntos que quedan fuera de los bigotes.
bigotes <- select(resumen, nodo, variable, bigote_inferior, bigote_superior)
atipicos <- left_join(cajas, bigotes, by = c("nodo", "variable")) |>
  filter(valor < bigote_inferior | valor > bigote_superior)
write.csv(atipicos, here::here("resultados", "eda", "valores_fuera_bigotes.csv"), row.names = FALSE)

# Buscar los bigotes más alejados para cada variable.
ventanas <- resumen |>
  group_by(variable) |>
  summarise(
    menor_q1 = min(q1), mayor_q3 = max(q3),
    menor_bigote = min(bigote_inferior), mayor_bigote = max(bigote_superior),
    .groups = "drop"
  )
ventanas$desde <- NA_real_
ventanas$hasta <- NA_real_

# Añadir un 2 % de espacio a cada lado en la escala del gráfico.
for (i in seq_len(nrow(ventanas))) {
  if (ventanas$variable[i] == "primas") {
    eje <- scales::transform_log10()
  } else {
    eje <- scales::transform_pseudo_log(sigma = 1, base = 10)
  }
  bigotes <- c(ventanas$menor_bigote[i], ventanas$mayor_bigote[i])
  bigotes_en_el_eje <- eje$transform(bigotes)
  margen <- 0.02 * (bigotes_en_el_eje[2] - bigotes_en_el_eje[1])
  limite_izquierdo <- bigotes_en_el_eje[1] - margen
  limite_derecho <- bigotes_en_el_eje[2] + margen
  limites <- eje$inverse(c(limite_izquierdo, limite_derecho))
  ventanas$desde[i] <- limites[1]
  ventanas$hasta[i] <- limites[2]
}
write.csv(ventanas, here::here("resultados", "eda", "ventanas_cajas.csv"), row.names = FALSE)

# Contar los puntos que quedan fuera de ese acercamiento.
fuera_ventana <- cajas |>
  left_join(ventanas, by = "variable") |>
  group_by(nodo, serie, variable) |>
  summarise(n_fuera = sum(valor < desde | valor > hasta), .groups = "drop")
write.csv(fuera_ventana, here::here("resultados", "eda", "conteo_fuera_detalle.csv"), row.names = FALSE)
