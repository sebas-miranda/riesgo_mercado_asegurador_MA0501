# Estandarizamos L los loss ratio con la desviacion estandar muestral insesgada 

crear_X <- function(L, metodo = "pearson") {
  #Dimension extraction
  N <- nrow(L)
  t <- ncol(L)
   # Inicializar matriz dim N*t 
  X<-matrix(NA_real_, nrow= N, ncol = t, dimnames = dimnames(L))
  
  # REVISAR SECCION para control de atipicos
  if(any(!is.finite(L))){
    stop(" Implementar control para estrategia de control de alores extremos u outliers")
  }
  
  # Estandariza cada nodo 
  for (i in 1:N) {
    # Extraemos la serie del LR
    serie <- L[i, ]

    # Para la correlacion de Spearman, utilizamos los rangos
    if(metodo == "spearman"){
      serie <- rank(serie, ties.method = "average")
    }

    # media temporal en la fila i-esima 
    media_fil_i <- mean(serie)
    # Desviacion estandar muestral 
    sd_fila_i <- sd(serie)
    
    # SECCION A REVISAR PARA CONTROL DE OUTLIERS EN LA  ESTANDARIZACION 
    if(sd_fila_i==0){
      stop(paste('Nodo',rownames(L)[i], 'tiene sd 0'))
    }
    
    # Estandarizar
    X[i,] <- (serie-media_fil_i)/sd_fila_i
    
  }
  #Devolver X estandarizado
  
  return(X)
}