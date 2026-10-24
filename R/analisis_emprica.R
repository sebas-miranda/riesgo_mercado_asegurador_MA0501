# Implementacion de matriz de correlacion empirica sin regularizacion de rmt 
#Cargar funciones

source('R/crear_L.R'); source('R/crear_X.R')
# Construye matriz de loss ratios

L<- crear_L("datos/datos_procesados/panel_trimestral.csv")
# Estandarizada 
X <- crear_X(L)

# Matriz de correlacion empirica 

T_serie <- ncol(X)

C <- (X%*%t(X))/(T_serie -1 )

# mapa de calor descriptivo 


# Paleta de colores:
# Azul = correlación negativa
# Blanco = correlación cercana a cero
# Rojo = correlación positiva




library(ggplot2)

# 1. Ordenar los nodos según su similitud
# Agrupamiento jerárquico para facilitar la lectura

distancias <- as.dist(1 - C)
agrupamiento <- hclust(distancias, method = "average")

orden <- agrupamiento$order
C_ordenada <- C[orden, orden]

# 2. Convertir la matriz a formato largo
# Una fila por cada correlación entre dos nodos

datos_grafico <- as.data.frame(as.table(C_ordenada))

colnames(datos_grafico) <- c("Nodo1", "Nodo2", "Correlacion")

# 3. Crear identificadores para el eje horizontal
# Se utilizan para ordenar, pero NO se mostrarán

nodos <- rownames(C_ordenada)
ids <- sprintf("%02d", seq_along(nodos))

datos_grafico$ID <- ids[
  match(datos_grafico$Nodo2, nodos)
]

# Ordenar ambos ejes de forma consistente

datos_grafico$ID <- factor(
  datos_grafico$ID,
  levels = ids
)

datos_grafico$Nodo1 <- factor(
  datos_grafico$Nodo1,
  levels = rev(nodos)
)

# 4. Conservar la diagonal principal
# Corr(Xi, Xi) = 1
# No se reemplaza ningún valor por NA

# 5. Crear el mapa de calor

grafico_C <- ggplot(
  datos_grafico,
  aes(x = ID, y = Nodo1, fill = Correlacion)
) +
  
  geom_tile(color = "white", linewidth = 0.12) +
  
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    name = "Correlación\nPearson"
  ) +
  
  scale_x_discrete(
    position = "top",
    labels = function(x) rep("", length(x)),
    expand = c(0, 0)
  ) +
  
  scale_y_discrete(
    expand = c(0, 0)
  ) +
  
  coord_fixed() +
  
  labs(
    title = "Matriz de correlación empírica",
    subtitle = "Dependencia entre aseguradoras y ramos de seguros",
    x = NULL,
    y = NULL,
    caption = paste0(
      "N = ", nrow(C),
      " nodos | Matriz empírica sin regularización RMT\n",
      "Nodos agrupados por similitud | Diagonal principal = 1"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    plot.title = element_text(
      size = 19,
      face = "bold",
      color = "#172033"
    ),
    
    plot.subtitle = element_text(
      size = 12,
      color = "#64748B"
    ),
    
    # Ocultar completamente los números del eje superior
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_text(
      size = 7,
      color = "#334155"
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

# 6. Mostrar el gráfico
print(grafico_C)

# 7. Guardar con buena resolución
dir.create("graficos", showWarnings = FALSE)

ggsave(
  "graficos/matriz_correlacion_empirica.png",
  plot = grafico_C,
  width = 18,
  height = 15,
  dpi = 300,
  bg = "white"
)


