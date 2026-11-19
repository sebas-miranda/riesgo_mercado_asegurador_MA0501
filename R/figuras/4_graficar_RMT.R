# Ejecutar después del paso 4 de R/analisis.
library(here)
library(ggplot2)
library(patchwork)

datos_RMT <- read.csv(here("resultados", "comparacion.csv"))

leer_RMT <- function(metodo, archivo) {
  resultado <- as.list(datos_RMT[datos_RMT$metodo == metodo, ])
  resultado$tabla_eigenvalues <- read.csv(archivo)
  resultado$lambda <- resultado$tabla_eigenvalues$eigenvalue
  resultado
}

rmt_pearson <- leer_RMT("Pearson", here("resultados", "eigenvalues_pearson.csv"))
rmt_spearman <- leer_RMT("Spearman", here("resultados", "eigenvalues_spearman.csv"))

limite_y <- max(
  rmt_pearson$lambda,
  rmt_spearman$lambda
) * 1.25


graficar_MP <- function(resultado) {

  datos <- resultado$tabla_eigenvalues

  grafico <- ggplot(
    datos,
    aes(x = posicion, y = eigenvalue, fill = sobre_MP)
  ) +

    # Barras de los valores propios
    geom_col(width = 0.8) +

    # Limite superior de Marchenko-Pastur
    geom_hline(
      data = datos[1, , drop = FALSE],
      aes(yintercept = resultado$lambda_mas, color = "Superior"),
      linetype = "dashed",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +

    # Limite inferior de Marchenko-Pastur
    geom_hline(
      data = datos[1, , drop = FALSE],
      aes(yintercept = resultado$lambda_menos, color = "Inferior"),
      linetype = "dashed",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +

    # Leyenda con los valores numericos de ambas bandas
    scale_color_manual(
      name = NULL,
      values = c(
        "Superior" = "#FF0000",
        "Inferior" = "#0066CC"
      ),
      breaks = c("Superior", "Inferior"),
      labels = as.expression(list(
        bquote(lambda["+"] == .(formatC(resultado$lambda_mas, digits = 6, format = "f", decimal.mark = ","))),
        bquote(lambda["-"] == .(formatC(resultado$lambda_menos, digits = 6, format = "f", decimal.mark = ",")))
      ))
    ) +

    # Valores propios superiores a MP en rojo
    scale_fill_manual(
      values = c(
        "FALSE" = "#808080",
        "TRUE" = "#FF0000"
      ),
      guide = "none"
    ) +

    # Etiqueta sobre la barra del mayor valor propio
    # which.max() devuelve la posicion del valor mas grande.
    geom_text(
      data = datos[which.max(datos$eigenvalue), , drop = FALSE],
      aes(label = paste0("lambda[max] == '",
                         formatC(eigenvalue, digits = 4, format = "f", decimal.mark = ","), "'")),
      parse = TRUE,
      vjust = -0.4,
      hjust = 0,
      nudge_x = 0.5,
      size = 3.9,
      color = "black"
    ) +

    labs(
      title = resultado$metodo,
      x = "Índice",
      y = "Valor propio",

    ) +

    scale_x_continuous(
      breaks = seq(1, resultado$N, by = 5),
      expand = expansion(add = 0.6)
    ) +

    # Espacio para la etiqueta sobre el mayor valor propio
  coord_cartesian(
    ylim = c(0, limite_y),
    expand = c(bottom = FALSE, top = FALSE)
  ) + 

    theme_minimal(base_size = 10) +

    theme(
      legend.key.width = grid::unit(1, "cm"),
      legend.text = element_text(size = 9),
      legend.margin = margin(2, 0, 0, 0),
      plot.title = element_text(face = "bold", size = 11),
      plot.margin = margin(3, 6, 2, 6),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "black")
    )

  return(grafico)
}


# Generamos ambos graficos
grafico_pearson <- graficar_MP(rmt_pearson)
grafico_spearman <- graficar_MP(rmt_spearman)
comparacion <- (grafico_pearson | grafico_spearman) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")
comparacion

# Finalmente, guardamos los graficos

ggsave(
  here("graficos", "comparacion_pearson_spearman_eigen.pdf"),
       plot = comparacion,
       width = 6.96, height = 2.4, device = cairo_pdf, bg = "white")
ggsave(
  here("graficos", "comparacion_pearson_spearman_eigen.png"),
       plot = comparacion,
       width = 6.96, height = 2.4, dpi = 300, bg = "white")

