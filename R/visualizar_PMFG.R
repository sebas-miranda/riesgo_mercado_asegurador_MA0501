library(here)
library(graph)
library(ggplot2)
library(ggrepel)

# abrir el .Rproj del proyecto antes de ejecutar este archivo
# RBGL se instala desde Bioconductor, no desde CRAN

# matriz empirica del proyecto
source(here("R", "crear_L.R"), encoding="UTF-8")
source(here("R", "crear_X.R"), encoding="UTF-8")
ruta <- here("datos", "datos_procesados", "panel_trimestral.csv")
L <- crear_L(ruta)
X <- crear_X(L)
T_serie <- ncol(X)
C <- (X %*% t(X))/(T_serie - 1)

panel <- read.csv(ruta, stringsAsFactors=FALSE, fileEncoding="UTF-8")
# ordenar los nombres igual que las filas de la matriz C
meta <- unique(panel[c("aseguradora", "ramo")])
nombres <- paste(meta$aseguradora, meta$ramo, sep="-")
orden <- match(rownames(C), nombres)
meta <- meta[orden, ]

# abreviar los ramos para que las etiquetas quepan en la imagen
ramo_corto <- c("Accid.", "Agro", "Auto", "Aviación", "Caución", "Incendio",
  "Carga", "Bienes", "Pérdidas", "R. civil", "Salud", "Vida", "SOA", "SRT", "Marítimo")
names(ramo_corto) <- c("Accidentes", "Agrícolas y pecuarios", "Automóvil", "Aviación",
  "Caución", "Incendio y líneas aliadas", "Mercancías transportadas",
  "Otros daños a los bienes", "Pérdidas pecuniarias", "Responsabilidad civil", "Salud", "Vida",
  "Seguro obligatorio de automóviles (SOA)", "Seguro obligatorio de riesgos del trabajo (SRT)",
  "Vehículos marítimos")
nodos <- data.frame(id=seq_len(nrow(C)), aseguradora=meta$aseguradora,
  etiqueta=paste(meta$aseguradora, ramo_corto[meta$ramo], sep="\n"))
companias <- sort(unique(nodos$aseguradora))
paleta <- setNames(c("#438D8D", "#B66C86", "#C18D3A", "#648AB5", "#8E78AD",
  "#A5AA4B", "#CC7D5A", "#5D9D7C", "#A76D64", "#7B98B1", "#BAA34A", "#8B8C9B", "#61A2AE"), companias)

# tomar solo la mitad superior de C: cada enlace aparece una vez
# la diagonal se excluye porque relaciona cada nodo consigo mismo
pares <- which(upper.tri(C), arr.ind=TRUE)
enlaces <- data.frame(from=pares[,1], to=pares[,2], rho=C[pares])
enlaces$signo <- ifelse(enlaces$rho>0, "Positiva", "Negativa")

# probar los enlaces en orden y aceptar solo los que mantienen la planaridad
crear_pmfg <- function(enlaces, n, magnitud=FALSE) {
  if(magnitud) orden <- order(-abs(enlaces$rho)) else orden <- order(-enlaces$rho)
  candidatos <- enlaces[orden, ]
  red <- new("graphNEL", nodes=as.character(1:n), edgemode="undirected")
  elegidos <- integer(0)

  for(j in 1:nrow(candidatos)) {
    desde <- as.character(candidatos$from[j])
    hasta <- as.character(candidatos$to[j])
    prueba <- graph::addEdge(desde, hasta, red, candidatos$rho[j])
    if(RBGL::boyerMyrvoldPlanarityTest(prueba)) {
      red <- prueba
      elegidos <- c(elegidos, j)
    }
    if(length(elegidos)==3*n-6) break
  }
  list(grafo=red, enlaces=candidatos[elegidos, ])
}

resultados <- list(
  clasico=crear_pmfg(enlaces, nrow(C)),
  magnitud=crear_pmfg(enlaces, nrow(C), magnitud=TRUE)
)
dir.create(here("graficos"), showWarnings=FALSE)
for(tipo in names(resultados)) {
  red <- resultados[[tipo]]$grafo
  aristas <- resultados[[tipo]]$enlaces

  # este acomodo dibuja el grafo planar con segmentos sin cruces
  xy <- RBGL::chrobakPayneStraightLineDrawing(red)
  nodos$x <- xy["x", as.character(nodos$id)]
  nodos$y <- xy["y", as.character(nodos$id)]
  aristas$x <- nodos$x[aristas$from]
  aristas$y <- nodos$y[aristas$from]
  aristas$xend <- nodos$x[aristas$to]
  aristas$yend <- nodos$y[aristas$to]
  titulo <- if(tipo=="clasico") expression(PMFG:~rho[ij]) else expression(PMFG:~abs(rho[ij]))
  grafico <- ggplot(nodos, aes(x, y)) +
    geom_segment(data=aristas,
      aes(x=x, y=y, xend=xend, yend=yend, colour=signo, linetype=signo, linewidth=abs(rho)),
      inherit.aes=FALSE, alpha=.45,
      show.legend=c(colour=FALSE, linetype=TRUE, linewidth=FALSE)) +
    geom_point(aes(colour=aseguradora), shape=21, fill="#FAF9F5", size=3.8, stroke=1.1) +
    geom_text_repel(aes(label=etiqueta), size=3, lineheight=.9, colour="#263B43",
      seed=2026, max.overlaps=Inf, box.padding=.3, point.padding=.3, segment.colour="#CBD4D1") +
    scale_colour_manual(values=c(paleta, Positiva="#507A8D", Negativa="#BE684B"),
      breaks=companias, name="Aseguradora") +
    scale_linetype_manual(values=c(Positiva="solid", Negativa="dashed"),
      limits=c("Positiva", "Negativa"), name="Correlación") +
    scale_linewidth_continuous(limits=c(0,1), range=c(.2,.9), guide="none") +
    guides(colour=guide_legend(order=1),
      linetype=guide_legend(order=2, override.aes=list(colour=c("#507A8D", "#BE684B")))) +
    coord_equal(clip="off") +
    labs(title=titulo,
      subtitle=sprintf("%s nodos | %s enlaces (%s positivos, %s negativos)",
        nrow(nodos), nrow(aristas), sum(aristas$rho>0), sum(aristas$rho<0)),
      caption="Color del nodo: aseguradora. Grosor del enlace: magnitud de correlación.") +
    theme_void(base_size=12) +
    theme(plot.background=element_rect(fill="#FAF9F5", colour=NA),
      plot.title=element_text(family="serif", size=24, colour="#263B43"),
      plot.subtitle=element_text(colour="#5D7077", margin=margin(b=15)),
      plot.caption=element_text(hjust=0, colour="#5D7077"),
      legend.key=element_rect(fill="#FAF9F5", colour=NA),
      plot.margin=margin(15,20,15,20))

  print(grafico)
  ggsave(here("graficos", paste0("pmfg_", tipo, ".png")), plot=grafico,
    width=16, height=9, units="in", dpi=300, bg="#FAF9F5")
}
