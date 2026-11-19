library(dplyr)

ruta_panel <- here::here("datos", "datos_procesados", "panel_trimestral.csv")
if (!file.exists(ruta_panel)) {
  ruta_panel <- here::here("fuentes", "panel_repositorio.csv")
}

panel <- read.csv(ruta_panel)
catalogo <- read.csv(here::here("datos", "eda", "catalogo_nodos.csv"))

# Un nodo es una combinación de aseguradora, categoría y ramo.
clave_nodo <- c("aseguradora", "categoria", "ramo")
if (nrow(catalogo) != 42 || anyDuplicated(catalogo$nodo) || anyDuplicated(catalogo[clave_nodo])) {
  stop("El catálogo debe tener 42 nodos únicos.")
}
if (anyDuplicated(panel[c(clave_nodo, "fecha")])) {
  stop("El panel tiene trimestres repetidos por nodo.")
}

# Poner el número de nodo del catálogo en cada fila del panel.
datos <- inner_join(panel, catalogo, by = clave_nodo,
                    relationship = "many-to-one", unmatched = "error")
datos <- rename(datos, primas = primas_netas, siniestros = siniestros_netos, lr = loss_ratio)
datos$fecha <- as.Date(datos$fecha)
datos$periodo <- 4 * (datos$anio - 2017) + datos$trimestre
datos$serie <- paste(datos$aseguradora, datos$ramo, sep = " - ")
datos <- arrange(datos, nodo, fecha)

# Revisar que cada nodo tenga los 39 trimestres completos.
calendario <- seq(as.Date("2017-01-01"), as.Date("2026-07-01"), by = "quarter")
trimestres_por_nodo <- table(datos$nodo)
if (length(trimestres_por_nodo) != 42 || any(trimestres_por_nodo != 39)) {
  stop("Cada nodo debe tener los 39 trimestres del panel.")
}
if (any(!datos$periodo %in% 1:39)) {
  stop("Los períodos deben ir del 1 al 39.")
}
fecha_esperada <- calendario[datos$periodo]
if (!isTRUE(all(datos$fecha == fecha_esperada))) {
  stop("Las fechas deben coincidir con los trimestres del panel.")
}
if (!isTRUE(all(datos$primas > 0))) {
  stop("Las primas deben ser positivas.")
}
lr_calculado <- datos$siniestros / datos$primas
if (!isTRUE(all(abs(datos$lr - lr_calculado) < 1e-10))) {
  stop("El LR debe coincidir con siniestros / primas.")
}

# lag() toma el trimestre anterior, por separado para cada nodo.
datos <- datos |>
  group_by(nodo) |>
  mutate(
    primas_anteriores = lag(primas),
    siniestros_anteriores = lag(siniestros),
    lr_anterior = lag(lr)
  ) |>
  ungroup()

datos$cambio_primas <- datos$primas - datos$primas_anteriores
datos$cambio_siniestros <- datos$siniestros - datos$siniestros_anteriores
datos$cambio_lr <- 100 * (datos$lr - datos$lr_anterior)

# El cambio porcentual queda vacío si el valor anterior es cero o negativo.
datos$porcentaje_primas <- ifelse(
  datos$primas_anteriores > 0, 100 * (datos$primas / datos$primas_anteriores - 1), NA
)
datos$porcentaje_siniestros <- ifelse(
  datos$siniestros_anteriores > 0, 100 * (datos$siniestros / datos$siniestros_anteriores - 1), NA
)

datos$observado <- TRUE
datos$es_panel <- TRUE
datos <- select(datos, anio, trimestre, aseguradora, categoria, ramo, primas, siniestros,
                periodo, observado, nodo, archivo, es_panel, fecha, serie, lr,
                cambio_primas, cambio_siniestros, cambio_lr, porcentaje_primas,
                porcentaje_siniestros)

dir.create(here::here("resultados", "eda"), recursive = TRUE, showWarnings = FALSE)
write.csv(datos, here::here("resultados", "eda", "datos.csv"), row.names = FALSE, na = "")
catalogo$es_panel <- TRUE
write.csv(catalogo, here::here("resultados", "eda", "catalogo.csv"), row.names = FALSE)

# Resumir los 39 valores de LR de cada nodo.
resumen_lr <- datos |>
  group_by(nodo, aseguradora, ramo) |>
  summarise(trimestres = n(), media = mean(lr), mediana = median(lr),
            minimo = min(lr), maximo = max(lr), negativos = sum(lr < 0),
            .groups = "drop") |>
  mutate(serie = paste(aseguradora, ramo, sep = " - ")) |>
  arrange(aseguradora, desc(mediana), ramo)
write.csv(resumen_lr, here::here("resultados", "eda", "tabla_resumen_LR.csv"), row.names = FALSE)

barras <- resumen_lr |>
  select(nodo, aseguradora, ramo, trimestres, mediana, media, minimo, maximo, negativos) |>
  mutate(mediana_pct = 100 * mediana) |>
  group_by(aseguradora) |>
  mutate(orden_en_aseguradora = row_number()) |>
  ungroup()

# Volver a ordenar para comparar aseguradoras dentro de cada ramo.
barras <- barras |>
  arrange(ramo, desc(mediana), aseguradora) |>
  group_by(ramo) |>
  mutate(orden_en_ramo = row_number()) |>
  ungroup() |>
  arrange(aseguradora, orden_en_aseguradora)
write.csv(barras, here::here("resultados", "eda", "medianas_LR_barras.csv"), row.names = FALSE)

# LR trimestral = siniestros totales / primas totales.
por_aseguradora <- datos |>
  group_by(aseguradora, anio, trimestre, fecha) |>
  summarise(primas = sum(primas), siniestros = sum(siniestros), ramos = n(),
            .groups = "drop")
por_aseguradora$lr <- por_aseguradora$siniestros / por_aseguradora$primas

por_trimestre <- por_aseguradora |>
  group_by(anio, trimestre, fecha) |>
  summarise(primas = sum(primas), siniestros = sum(siniestros),
            nodos = sum(ramos), aseguradoras = n(), .groups = "drop")
por_trimestre$lr <- por_trimestre$siniestros / por_trimestre$primas
write.csv(por_aseguradora, here::here("resultados", "eda", "evolucion_aseguradoras.csv"), row.names = FALSE)
write.csv(por_trimestre, here::here("resultados", "eda", "evolucion_panel.csv"), row.names = FALSE)

# Los índices fijan el orden del PDF y el nombre de cada imagen.
indice_lineas <- catalogo |>
  arrange(nodo) |>
  transmute(nodo, aseguradora, ramo, pagina = row_number(),
            png = paste0("lineas/", archivo, ".png"))
write.csv(indice_lineas, here::here("resultados", "eda", "indice_lineas.csv"), row.names = FALSE)

# Nombre de cada PNG por ramo.
nombres_ramos <- c(
  "Accidentes" = "accidentes",
  "Automóvil" = "automovil",
  "Caución" = "caucion",
  "Incendio y líneas aliadas" = "incendio_y_lineas_aliadas",
  "Otros daños a los bienes" = "otros_danos_a_los_bienes",
  "Pérdidas pecuniarias" = "perdidas_pecuniarias",
  "Responsabilidad civil" = "responsabilidad_civil",
  "Salud" = "salud",
  "Seguro obligatorio de automóviles (SOA)" = "seguro_obligatorio_de_automoviles_soa",
  "Seguro obligatorio de riesgos del trabajo (SRT)" = "seguro_obligatorio_de_riesgos_del_trabajo_srt",
  "Vehículos marítimos" = "vehiculos_maritimos",
  "Vida" = "vida"
)
stopifnot("Falta el nombre del PNG de algún ramo" = all(catalogo$ramo %in% names(nombres_ramos)))

barras_por_aseguradora <- catalogo |>
  count(grupo = aseguradora, name = "barras") |>
  mutate(
    agrupar = "aseguradora",
    png = paste0("barras/por_aseguradora/", tolower(grupo), ".png")
  )

barras_por_ramo <- catalogo |>
  count(grupo = ramo, name = "barras") |>
  mutate(
    agrupar = "ramo",
    png = paste0("barras/por_ramo/", nombres_ramos[grupo], ".png")
  )

indice_barras <- bind_rows(barras_por_aseguradora, barras_por_ramo) |>
  arrange(agrupar, grupo, .locale = "es") |>
  transmute(agrupar, grupo, barras, pagina = row_number(), png)
write.csv(indice_barras, here::here("resultados", "eda", "indice_barras.csv"), row.names = FALSE)
