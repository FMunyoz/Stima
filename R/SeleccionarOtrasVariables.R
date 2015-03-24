SeleccionarOtrasVariables <- function(Muestra){
  
  #Si es de tipologia 2 se usa el numero de Dormitorios
  Datos <- as.matrix(subset(Muestra, select = (c("NumeroDeAscensores", 
                                                 "ExteriorInterior", 
                                                 "NumeroDePlanta", 
                                                 "DatosDeLaPlanta"))))
  #Actualiza Datos Auxiliares
  Datos[is.na(Datos[,"NumeroDeAscensores"]),"NumeroDeAscensores"] <- 1
  Datos[is.na(Datos[,"ExteriorInterior"]),"ExteriorInterior"] <- 1
  Datos[is.na(Datos[,"DatosDeLaPlanta"]),"DatosDeLaPlanta"] <- 7
  
  Datos
  
}