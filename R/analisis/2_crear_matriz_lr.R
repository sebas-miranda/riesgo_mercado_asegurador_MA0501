# Ordena los LR por serie y trimestre.
# Recibe la ruta del CSV del panel (datos/datos_procesados/panel_trimestral.csv).
# Devuelve L con una fila por aseguradora y ramo, y una columna por trimestre.

library(tidyverse)

crear_L <- function(ruta_csv) {
  # procesar los datos del csv
  datos <- read_csv(ruta_csv, show_col_types = FALSE)
  # identificacion del nodo concatenar aseguradora-ramo 
  datos$nodo <- paste(datos$aseguradora, datos$ramo, sep = '-' )
  datos$periodo <- paste0(datos$anio, 'T',datos$trimestre)
  # ordenamos los nodos y periodos sin repetidos 
  nodos <- sort(unique(datos$nodo))
  periodos <- sort(unique(datos$periodo))
  
  # validacion de duplicados 
  # aca directamente eliminar uno de los duplicados 
  if(anyDuplicated(datos[c('nodo','periodo')])){
    stop('Hay duplicacion de datos por nodo y trimestre')
  }
  # crear matriz vacia
  L<- matrix (NA_real_, nrow= length(nodos), ncol =length(periodos), dimnames = list(nodos,periodos))
  # hacemos match de fila x col para cada dato ordenado 
  
  filas <-match(datos$nodo, nodos)
  columnas <-match(datos$periodo, periodos)
  
  # agregar el dato donde hay match 
  
  L[cbind(filas,columnas)] <- datos$loss_ratio
  
  # 7. verificar llenado completo
  
  # aca hacer control para imputar atipicos y controlar los outliers e
  if (any(!is.finite(L))) {
    stop("La matriz L contiene valores faltantes o no finitos.")
  }
  
  return(L) # output matriz de loss-ratios en serie matriz n*t
}