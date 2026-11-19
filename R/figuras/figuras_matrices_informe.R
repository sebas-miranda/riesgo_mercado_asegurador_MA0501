# Ejecutar desde la raíz del proyecto después de los pasos 4 y 5.
library(here)
library(ggplot2)
library(patchwork)
source(here("R", "figuras", "crear_grafico_C.R"), encoding = "UTF-8")

C_pearson <- as.matrix(read.csv(here("resultados", "correlacion_pearson_empirica.csv"),
                               row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
C_spearman <- as.matrix(read.csv(here("resultados", "correlacion_spearman_empirica.csv"),
                                row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
C_recorte <- as.matrix(read.csv(here("resultados", "contraccion_alpha", "alpha_constante_100.csv"),
                               row.names = 1, check.names = FALSE, fileEncoding = "UTF-8"))
datos_RMT <- read.csv(here("resultados", "comparacion.csv"))
T_serie <- datos_RMT$T[datos_RMT$metodo == "Spearman"]
orden <- hclust(as.dist(1 - C_spearman), method = "average")$order
nodos <- rownames(C_spearman)[orden]

# SOA y SRT se definen en el texto del informe.
nombre_serie <- sub("Seguro obligatorio de automóviles (SOA)", "SOA", nodos, fixed = TRUE)
nombre_serie <- sub("Seguro obligatorio de riesgos del trabajo (SRT)", "SRT",
                    nombre_serie, fixed = TRUE)
etiquetas <- setNames(nombre_serie, nodos)

# Las columnas alternan arriba y abajo para dar espacio a cada nombre.
etiquetas_columna <- etiquetas
abreviaturas <- c(
  "Pérdidas pecuniarias" = "Pérd. pecun.",
  "Otros daños a los bienes" = "Otros daños",
  "Incendio y líneas aliadas" = "Incendio",
  "Vehículos marítimos" = "Marítimos",
  "Responsabilidad civil" = "Resp. civil"
)
for (ramo in names(abreviaturas)) {
  etiquetas_columna <- sub(ramo, abreviaturas[[ramo]], etiquetas_columna, fixed = TRUE)
}
impares <- seq(1, length(nodos), by = 2)

# Los tres pares numerados en la tabla del informe.
pares <- data.frame(
  numero = 1:3,
  serie_1 = c("DAVIVIENDA-Pérdidas pecuniarias", "MNK-Automóvil", "INS-Incendio y líneas aliadas"),
  serie_2 = c("ASSA-Incendio y líneas aliadas", "ISCR-Salud", "ISCR-Otros daños a los bienes")
)
indice_1 <- match(pares$serie_1, nodos)
indice_2 <- match(pares$serie_2, nodos)
stopifnot(!anyNA(indice_1), !anyNA(indice_2))
pares$columna <- pmax(indice_1, indice_2)
pares$fila <- length(nodos) + 1 - pmin(indice_1, indice_2)
pares$x_etiqueta <- pares$columna + 2
pares$y_etiqueta <- pares$fila - 2

marcas <- list(
  geom_tile(data = pares, aes(x = columna, y = fila), inherit.aes = FALSE,
            width = 1, height = 1, fill = NA, colour = "#172435", linewidth = 0.45),
  geom_segment(data = pares, aes(x = columna + 0.5, y = fila - 0.5,
                                 xend = x_etiqueta, yend = y_etiqueta),
               inherit.aes = FALSE, colour = "#172435", linewidth = 0.35),
  geom_label(data = pares, aes(x = x_etiqueta, y = y_etiqueta, label = numero),
             inherit.aes = FALSE, size = 2.8, fontface = "bold",
             colour = "white", fill = "#172435", linewidth = 0,
             label.padding = grid::unit(1, "pt"))
)

estilo_matriz <- list(
  labs(subtitle = NULL),
  scale_x_discrete(
    position = "top", expand = c(0, 0),
    breaks = nodos[impares], labels = etiquetas_columna[nodos[impares]],
    sec.axis = dup_axis(name = NULL,
                        breaks = nodos[-impares], labels = etiquetas_columna[nodos[-impares]])
  ),
  scale_y_discrete(expand = c(0, 0), labels = etiquetas),
  coord_cartesian(expand = FALSE),
  guides(fill = guide_colorbar(title = "Correlación", barwidth = grid::unit(1.36, "in"),
                               barheight = grid::unit(7, "pt"))),
  theme(
    plot.title = element_text(size = 11, hjust = 0.5, margin = margin(b = 6)),
    plot.title.position = "panel",
    axis.text.x.top = element_text(size = 7.4, hjust = 0, margin = margin(b = 1)),
    axis.text.x.bottom = element_text(size = 7.4, hjust = 1, margin = margin(t = 1)),
    axis.text.y = element_text(size = 7.2, margin = margin(r = 3)),
    axis.ticks.x = element_line(colour = "grey45", linewidth = 0.2),
    axis.ticks.length.x = grid::unit(2, "pt"),
    legend.title.position = "top",
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(3, 4, 3, 4)
  )
)

mapa_pearson <- crear_grafico_C(C_pearson, "Pearson", orden, T_serie) +
  estilo_matriz + labs(title = "Pearson") + marcas
mapa_spearman <- crear_grafico_C(C_spearman, "Spearman", orden, T_serie) +
  estilo_matriz + labs(title = "Spearman") + marcas
mapa_empirica <- crear_grafico_C(C_spearman, "Spearman", orden, T_serie) +
  estilo_matriz + labs(title = "Spearman empírica")
mapa_recorte <- crear_grafico_C(C_recorte, "Spearman", orden, T_serie) +
  estilo_matriz + labs(title = "Recorte por promedio")

guardar_mapas <- function(archivo, izquierda, derecha) {
  cairo_pdf(archivo, width = 7.91, height = 6.75, bg = "white")
  on.exit(dev.off())

  # La escala común ocupa el espacio libre sobre las etiquetas de las filas.
  tabla <- ggplotGrob(izquierda + theme(legend.position = "bottom"))
  leyenda <- tabla$grobs[[match("guide-box-bottom", tabla$layout$name)]]
  leyenda <- grid::editGrob(leyenda, vp = grid::viewport(
    x = 0.12, y = 0.875, width = 0.19, height = 0.15
  ))

  derecha <- derecha + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  mapas <- (izquierda | derecha) & theme(legend.position = "none")
  grid::grid.draw(grid::grobTree(patchworkGrob(mapas), leyenda))
}

guardar_mapas(here("informes", "recursos", "matrices_pearson_spearman.pdf"), mapa_pearson, mapa_spearman)
guardar_mapas(here("informes", "recursos", "matrices_spearman_recorte.pdf"), mapa_empirica, mapa_recorte)
