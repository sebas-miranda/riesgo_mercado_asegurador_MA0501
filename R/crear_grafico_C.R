# Funcion para crear el grafico de las correlaciones, parametrizando el metodo de correlacion
# (Pearson, o Spearman)
crear_grafico_C <- function(C, metodo, orden, T_serie) {
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
