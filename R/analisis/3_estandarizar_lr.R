# Centra y estandariza cada serie. Para Spearman usa los rangos de los LR.
# Recibe la matriz L y el método (Pearson o Spearman).
# Devuelve la matriz estandarizada X.

crear_X <- function(L, metodo = "pearson") {
  #dimension extraction
  N <- nrow(L)
  t <- ncol(L)
   # inicializar matriz dim n*t 
  X<-matrix(NA_real_, nrow= N, ncol = t, dimnames = dimnames(L))
  
  # revisar seccion para control de atipicos
  if(any(!is.finite(L))){
    stop(" Implementar control para estrategia de control de alores extremos u outliers")
  }
  
  # estandariza cada nodo 
  for (i in 1:N) {
    # extraemos la serie del lr
    serie <- L[i, ]

    # para la correlacion de spearman, utilizamos los rangos
    if(metodo == "spearman"){
      serie <- rank(serie, ties.method = "average")
    }

    # media temporal en la fila i-esima 
    media_fil_i <- mean(serie)
    # desviacion estandar muestral 
    sd_fila_i <- sd(serie)
    
    # seccion a revisar para control de outliers en la  estandarizacion 
    if(sd_fila_i==0){
      stop(paste('Nodo',rownames(L)[i], 'tiene sd 0'))
    }
    
    # estandarizar
    X[i,] <- (serie-media_fil_i)/sd_fila_i
    
  }
  #devolver x estandarizado
  
  return(X)
}