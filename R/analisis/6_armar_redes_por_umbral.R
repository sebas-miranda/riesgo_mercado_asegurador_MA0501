# Arma una red por cada combinación de alfa y umbral de correlación.
# Lee resultados/correlacion_spearman_empirica.csv y cuatro matrices
# de la carpeta resultados/contraccion_alpha/ para la comparación.
# Guarda una fila de conteos por alfa y umbral en resultados/resumen_redes_umbral.csv.
# Guarda todas las redes y la red de referencia en resultados/redes_umbral.rds.

library(here)
library(igraph)

pares_correlacion <- function(C) {
  # el triángulo superior toma cada par una sola vez
  indices <- which(upper.tri(C), arr.ind = TRUE)
  rho <- C[indices]
  signo <- rep("Cero", length(rho))
  signo[rho > 0] <- "Positiva"
  signo[rho < 0] <- "Negativa"
  data.frame(from = indices[, 1], to = indices[, 2], rho,
             magnitud = abs(rho), signo)
}

codigos <- c("100", "070", "050", "000")
alphas <- as.numeric(codigos) / 100
umbrales <- c(0.20, 0.30, 0.40)

C_empirica <- as.matrix(read.csv(
  here("resultados", "correlacion_spearman_empirica.csv"),
  row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"
))
series <- rownames(C_empirica)
stopifnot(identical(series, colnames(C_empirica)))

pares <- list()
for (i in seq_along(codigos)) {
  archivo <- here("resultados", "contraccion_alpha", paste0("alpha_constante_", codigos[i], ".csv"))
  C <- as.matrix(read.csv(archivo, row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
  stopifnot(identical(rownames(C), series), identical(colnames(C), series))
  pares[[i]] <- pares_correlacion(C)
}

abreviaturas <- c(
  "Accidentes" = "Accid.", "Automóvil" = "Auto",
  "Incendio y líneas aliadas" = "Incendio", "Pérdidas pecuniarias" = "Pérdidas",
  "Responsabilidad civil" = "R. civil", "Vehículos marítimos" = "Marítimo",
  "Otros daños a los bienes" = "Bienes", "Mercancías transportadas" = "Carga",
  "Seguro obligatorio de automóviles (SOA)" = "SOA",
  "Seguro obligatorio de riesgos del trabajo (SRT)" = "SRT"
)

#uso de expresiones regulares para reemplazar por vacío lo que esté antes o después del guión, incluido
aseguradora <- sub("-.*$", "", series)
ramo <- sub("^[^-]+-", "", series)
corto <- abreviaturas[ramo]
abreviar <- !is.na(corto)
ramo[abreviar] <- corto[abreviar]
nodos <- data.frame(id = as.character(seq_along(series)),
                    etiqueta = paste(aseguradora, ramo, sep = "\n"))

pares_empiricos <- pares_correlacion(C_empirica)
aristas_empiricas <- pares_empiricos[pares_empiricos$magnitud >= 0.30, ]
referencia <- graph_from_data_frame(aristas_empiricas, directed = FALSE, vertices = nodos)

redes <- list()
resumen <- data.frame()
for (h in umbrales) {
  for (i in seq_along(alphas)) {
    alpha <- alphas[i]
    tipo <- if (alpha == 0) "empirica" else "limpia"
    aristas <- pares[[i]]
    aristas <- aristas[aristas$magnitud >= h, ]
    red <- graph_from_data_frame(aristas, directed = FALSE, vertices = nodos)
    aislados <- sum(degree(red) == 0)
    redes[[nrow(resumen) + 1]] <- red

    resumen <- rbind(resumen, data.frame(matriz = tipo, alpha, h, enlaces = nrow(aristas),
      positivas = sum(aristas$rho > 0), negativas = sum(aristas$rho < 0),
      aislados))
  }

}

write.csv(resumen, here("resultados", "resumen_redes_umbral.csv"), row.names = FALSE)
print(resumen, row.names = FALSE)
saveRDS(list(referencia = referencia, redes = redes, resumen = resumen),
        here("resultados", "redes_umbral.rds"))
