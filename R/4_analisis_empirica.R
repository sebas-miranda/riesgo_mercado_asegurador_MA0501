# Matrices de correlación empírica de Pearson y Spearman

library(ggplot2)

# Cargamos las funciones de la construccion de la matriz de LR y la matriz de estandarización:
source("R/2_crear_L.R")
source("R/3_crear_X.R")

# Construimos la matriz de LR y de estandarización, tanto para Pearson como para Spearman:
L<- crear_L("datos/datos_procesados/panel_trimestral.csv")

X_pearson <- crear_X(L, metodo = "pearson")
X_spearman <- crear_X(L, metodo = "spearman")

# Matriz de correlacion empirica 

# Acá definimos T como la cantidad de columnas (Periodos) de la matriz de LR. Es decir las series de tiempo 
# de los ramos.
T_serie <- ncol(L)


# Estas C corresponden a las matrices de correlación de Pearson y Spearman mediante la formula
# C = X*X^t/(T-1), note que %*% se utiliza para el producto de matrices:
C_pearson <- X_pearson %*% t(X_pearson) / (T_serie - 1)
C_spearman <- X_spearman %*% t(X_spearman) / (T_serie - 1)

write.csv(C_pearson, "resultados/correlacion_pearson_empirica.csv",
          fileEncoding = "UTF-8")
write.csv(C_spearman, "resultados/correlacion_spearman_empirica.csv",
          fileEncoding = "UTF-8")

# Construimos distancias, de modo que podamos agrupar las correlaciones mayores, es decir
# premiar a los grupos de productos que mas similar se comportan, utilizamos a spearman:
distancias <- as.dist(1 - C_spearman)
# Con hclust hacemos clustering, de modo que promediamos las distancias anteriormente calculadas.
# Las que tienen menor distancia, poseen mayor correlacion promedio entre ambos ramos.
agrupamiento <- hclust(distancias, method = "average")
# Extraemos el orden de los nodos (ramos) de izquierda a derecha calculados en agrupamiento
orden <- agrupamiento$order

# NOTA: Esto del clustering que realizamos justo arriba, es indispensable debido a que en 2_crear_L.R
# se hace: nodos <- sort(unique(datos$nodo), esto ordena cada ramo por orden alfabetico sin repetirse
# el problema es que a la hora de graficar las correlaciones  se verian desordenadas; haciendo clustering
# con el as.dist acercamos las correlaciones mas altas (distancias mas bajas) a la diagonal.
# De este modo, se asemeja mucho mas a un mapa de calor.

# Funcion para crear el grafico de las correlaciones, parametrizando el metodo de correlacion
# (Pearson, o Spearman)
crear_grafico_C <- function(C, metodo) {
# Entradas: 
# C:         matriz de correlaciones que queremos graficar.
# metodo:    ya sea pearson o spearman
#
# Salida:
#  Grafico de ggplot. La idea es guardarlo en el repo

# Ordenamos la matriz C, de modo que cada entrada contenga la correlacion ya clusterizada. 
C_ordenada <- C[orden, orden]

# Convertirmos la matriz de modo que por cada fila, se muestre la correlacion entre 2 ramos:
datos_grafico <- as.data.frame(as.table(C_ordenada))
# nota: de ahora en adelante, defina cada rama como nodo, con el objetivo de asemejarse a la representacion de los grafos:
colnames(datos_grafico) <- c("Nodo1", "Nodo2", "Correlacion") # nombramos las columnas del df aca

  
# Aqui, extraemos los nombres de los nodos que ya ordenamos en la matriz C_ordenada.
nodos <- rownames(C_ordenada)

# Reordenamos el orden de las categorias (nodos) en los ejes. con rev se invierte
# el eje verital, de modo que el primer nodo aparezca en la parte superior.
datos_grafico$Nodo1 <- factor(
  datos_grafico$Nodo1,
  levels = rev(nodos)
)
# analogamente reordenamos el nodo 2.
datos_grafico$Nodo2 <- factor(
  datos_grafico$Nodo2,
  levels = nodos
)

  
# Realizamos el grafico de ggplot:

grafico <- ggplot(
  datos_grafico,
  aes(x = Nodo2, y = Nodo1, fill = Correlacion)
) +
  # Dibujamos una celda (Cuadro) por cada entrada, inicialmente blanco
  geom_tile(color = "white", linewidth = 0.12) +
  # Con esta funcion se pintan las celdas recien dibujadas
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    oob = scales::squish,
    name = paste("Correlación", metodo, sep = "\n")
  ) +
  
    # Colocamos igual, el eje horizontal x arriba
    scale_x_discrete(
      position = "top",
      expand = c(0, 0)
    ) +
  
  scale_y_discrete(
    expand = c(0, 0)
  ) +
  
  coord_fixed() +
  
  labs(
    title = paste("Matriz de correlación empírica de", metodo),
    subtitle = paste0("Correlacion entre ramos de aseguradoras. ", "N = ", nrow(C)," nodos | T = ", T_serie ),
    x = NULL,
    y = NULL,
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    # Negrita al titulo
    plot.title = element_text(
      size = 19,
      face = "bold",
      color = "#000000"
    ),
    # Subitulo
    plot.subtitle = element_text(
      face = "bold",
      size = 15,
      color = "#000000"
    ),
    
    axis.text.x = element_text(
      size = 7,
      color = "#000000",
      angle = 90,
      vjust = 0.5,
      hjust = 0
    ),
    axis.ticks.x = element_line(),
    
    # Mostrar nombrs de los nodos en el eje vertical
    axis.text.y = element_text(
      size = 7,
      color = "#000000"
    ),
      
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.position = "right",
    
    plot.caption = element_text(
      size = 9,
      color = "#64748B"
    ),
    
    plot.margin = margin(15, 20, 15, 15)
  )
  return(grafico)
}
# Generamos ambos graficos
grafico_C_pearson <- crear_grafico_C(C_pearson, "Pearson")
grafico_C_spearman <- crear_grafico_C(C_spearman, "Spearman")

# Guardamos el grafico de correlacion empirica de Pearson:
ggsave(
  "graficos/matriz_correlacion_pearson.pdf",
  plot = grafico_C_pearson,
  width = 18,
  height = 15,
  dpi = 300,
  bg = "white"
)
# Analogamente con el grafico de Spearman:
ggsave(
  "graficos/matriz_correlacion_spearman.pdf",
  plot = grafico_C_spearman,
  width = 18,
  height = 15,
  dpi = 300,
  bg = "white"
)
