LeerArchivos <- function(Directorio) {
  #Caso a estimar
  Estimacion <- read.csv2(paste(Directorio, "CasoEstimacion.csv", sep = "/"))
  
  #Ley qendo Muestra
  Muestra <- read.csv2(paste(Directorio, "DatosMuestra.csv", sep = "/"))
  #Leyendo Parametros de Calculo
  Parametros <- read.csv2(paste(Directorio, "ParametrosCalculo.csv", sep = "/"))
  list("Estimacion" = Estimacion, "Muestra" = Muestra, "Parametros" = Parametros)
}

SeleccionarVariables <- function(Muestra){
  
  #Si es de tipologia 2 se usa el numero de Dormitorios
  Datos <- as.matrix(subset(Muestra, select = (c("ValorUnitarioDeMercado",
                                        "NumeroDeBanos",
                                        "NumeroDeDormitorios",
                                        "SuperficieConstruidaDeLaVivienda",
                                        "CosteDeConstruccionBrutoUnitarioVivienda",
                                        "PorcentajeDepreciacionVivienda"))))
  
  #Si es de tipologia 1 se usa la superficie de la parcela
  Datos <- as.matrix(subset(Muestra, select = (c("ValorUnitarioDeMercado",
                                        "NumeroDeBanos",
                                        "SuperficieDeLaParcela",
                                        "SuperficieConstruidaDeLaVivienda",
                                        "CosteDeConstruccionBrutoUnitarioVivienda",
                                        "PorcentajeDepreciacionVivienda"))))
  
}

TransformarVariables <- function(Muestra){
  Datos <- Muestra + t(matrix(c(0, 2, 2, 0, 0, 2), nrow=6, ncol=nrow(Muestra)))
  log(Datos)
}

SeleccionarOtrasVariables <- function(Muestra){
  
  #Si es de tipologia 2 se usa el numero de Dormitorios
  Datos <- as.matrix(subset(Muestra, select = (c("NumeroDeAscensores", 
                                            "ExteriorInterior", 
                                            "NumeroDePlanta", 
                                            "DatosDeLaPlanta"))))
  #Actualiza Datos Auxiliares
  Datos[,"NumeroDeAscensores"][is.na(Datos[,"NumeroDeAscensores"])] <- 1
  Datos[,"ExteriorInterior"][is.na(Datos[,"ExteriorInterior"])] <- 1
  Datos[,"DatosDeLaPlanta"][is.na(Datos[,"DatosDeLaPlanta"])] <- 7
  Datos[,"DatosDeLaPlanta"] <- ifelse(Datos[,"DatosDeLaPlanta"] == 0, 5, 7)
  
  Datos
  
}
GrabarTasacion <- function(Directorio, Muestra){
  N <- nrow(Muestra)
  Pesos <- sum(Muestra[,"Pesos"])
  PW <- sum(Muestra[,"PesosXwCorregido"])
  PW2 <- sum(Muestra[,"PesosXwCorregidoCuadrado"])
  MediaPW <- PW/Pesos
  MediaPW2 <- PW2/Pesos
  DesviacionTipica <- sqrt(Pesos* (MediaPW2 - MediaPW^2)/(Pesos - 1))
  md <- round(Pesos - 1)
  t <- round(qt(.975, 1:30)[round(Pesos - 1)],3)
  LimiteInferiorLog <- MediaPW - DesviacionTipica * t
  LimiteSuperiorLog <- MediaPW + DesviacionTipica * t
  PorcentajeDesviacionLimiteSuperior <- (LimiteSuperiorLog - MediaPW)/MediaPW
  Estimacion <- exp(MediaPW)
  LimiteInferior <- exp(LimiteInferiorLog)
  LimiteSuperior <- exp(LimiteSuperiorLog)
  PorcentajeDesviacionLimiteInferior <- (Estimacion - LimiteInferior)/Estimacion
  PorcentajeDesviacionLimiteSuperior <- (LimiteSuperior - Estimacion)/Estimacion
  write.table(Estimacion, paste(Directorio,  "Tasacion.csv", sep = "/"), dec = ",", row.names = FALSE, col.names = FALSE)
  Estimacion
}