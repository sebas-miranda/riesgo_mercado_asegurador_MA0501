# Graficar matriz de correlacion con sharifi-krzanowski


# Comparacion visual entre Spearman empirica y Sharifi-Krzanowski

library(ggplot2)
library(patchwork)

# 1. Cargar la funcion de visualizacion existente
source("R/crear_grafico_C.R", encoding = "UTF-8")

# 2. Leer ambas matrices de correlacion
C_empirica <- as.matrix(read.csv(
  "resultados/correlacion_spearman_empirica.csv",
  row.names = 1,
  check.names = FALSE,
  fileEncoding = "UTF-8"
))

C_sharifi <- as.matrix(read.csv(
  "resultados/correlacion_spearman_sharifi.csv",
  row.names = 1,
  check.names = FALSE,
  fileEncoding = "UTF-8"
))

# 3. Obtener T del diagnostico RMT
diagnostico <- read.csv("resultados/diagnostico_RMT.csv")

T_serie <- diagnostico$T[
  diagnostico$metodo == "Spearman"
]

# 4. Verificar que ambas matrices contienen los mismos nodos
stopifnot(
  identical(rownames(C_empirica), rownames(C_sharifi)),
  identical(colnames(C_empirica), colnames(C_sharifi))
)

# 5. Ordenamiento jerarquico usando solamente la matriz empirica.
# La distancia d_ij = 1 - C_ij permite agrupar correlaciones altas.
# Usamos average linkage, igual que en el script original.
distancias <- as.dist(1 - C_empirica)
agrupamiento <- hclust(distancias, method = "average")
orden <- agrupamiento$order

# 6. Construir los dos mapas de calor con el mismo orden
grafico_empirica <- crear_grafico_C(
  C_empirica, "Spearman", orden, T_serie
)

grafico_sharifi <- crear_grafico_C(
  C_sharifi, "Spearman", orden, T_serie
) +
  labs(
    title = "Matriz de Spearman limpia con Sharifi-Krzanowski"
  )

# 7. Comparacion lado a lado
comparacion <- grafico_empirica | grafico_sharifi

comparacion

# 8. Guardar PDF
ggsave(
  "graficos/comparacion_spearman_sharifi.png",
  plot = comparacion,
  width = 36,
  height = 15,
  bg = "white"
)
