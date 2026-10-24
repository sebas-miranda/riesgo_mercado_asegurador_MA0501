# Creacion de la matriz de loss ratios 

crear_L <- function(ruta_csv) {
  # Procesar los datos del csv
  datos <- read.csv(ruta_csv, stringsAsFactors = FALSE, fileEncoding = 'UTF-8')
  # Identificacion del nodo concatenar Aseguradora-Ramo 
  datos$nodo <- paste(datos$aseguradora, datos$ramo, sep = '-' )
  datos$periodo <- paste0(datos$anio, 'T',datos$trimestre)
  # Ordenamos los nodos y periodos sin repetidos 
  nodos <- sort(unique(datos$nodo))
  periodos <- sort(unique(datos$periodo))
  
  # Validacion de duplicados 
  # ACA DIRECTAMENTE ELIMINAR UNO DE LOS DUPLICADOS 
  if(anyDuplicated(datos[c('nodo','periodo')])){
    stop('Hay duplicacion de datos por nodo y trimestre')
  }
  # Crear matriz vacia
  L<- matrix (NA_real_, nrow= length(nodos), ncol =length(periodos), dimnames = list(nodos,periodos))
  # Hacemos match de fila x col para cada dato ordenado 
  
  filas <-match(datos$nodo, nodos)
  columnas <-match(datos$periodo, periodos)
  
  # Agregar el dato donde hay match 
  
  L[cbind(filas,columnas)] <- datos$loss_ratio
  
  # 7. Verificar llenado completo
  
  # ACA HACER CONTROL PARA IMPUTAR ATIPICOS Y CONTROLAR LOS OUTLIERS E
  if (any(!is.finite(L))) {
    stop("La matriz L contiene valores faltantes o no finitos.")
  }
  
  return(L) # Output matriz de loss-ratios en serie matriz N*T
}