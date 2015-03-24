LeerArchivos <- function(Directorio) {
  #Caso a estimar
  Estimacion <- read.csv2(paste(Directorio, "CasoEstimacion.csv", sep = "/"))
  
  #Leyendo Muestra
  Muestra <- read.csv2(paste(Directorio, "DatosMuestra.csv", sep = "/"))
  #Leyendo Parametros de Calculo
  Parametros <- read.csv2(paste(Directorio, "ParametrosCalculo.csv", sep = "/"))
  list("Estimacion" = Estimacion, "Muestra" = Muestra, "Parametros" = Parametros)
}

SeleccionarVariables <- function(Muestra, Tipologia){
  
  #Si es de tipologia 2 se usa el numero de Dormitorios
  if (Tipologia == 1){
    as.matrix(subset(Muestra, select = (c("ValorUnitarioDeMercado",
                                                   "NumeroDeBanos",
                                                   "SuperficieDeLaParcela",
                                                   "SuperficieConstruidaDeLaVivienda",
                                                   "CosteDeConstruccionBrutoUnitarioVivienda",
                                                   "PorcentajeDepreciacionVivienda"))))
  } 
  else {
    as.matrix(subset(Muestra, select = (c("ValorUnitarioDeMercado",
                                                   "NumeroDeBanos",
                                                   "NumeroDeDormitorios",
                                                   "SuperficieConstruidaDeLaVivienda",
                                                   "CosteDeConstruccionBrutoUnitarioVivienda",
                                                   "PorcentajeDepreciacionVivienda"))))
    
  }
  
  #Si es de tipologia 1 se usa la superficie de la parcela

  
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
  Datos[is.na(Datos[,"NumeroDeAscensores"]),"NumeroDeAscensores"] <- 1
  Datos[is.na(Datos[,"ExteriorInterior"]),"ExteriorInterior"] <- 1
  Datos[is.na(Datos[,"DatosDeLaPlanta"]),"DatosDeLaPlanta"] <- 7

  Datos
  
}

ObtieneDatosDelModelo <- function(ParametrosDelModelo, Tipologia){
  #Modelo AJ
  #Tipologia: 1 = Unifamiliar, 2 = Plurifamiliar
  #Calculo de valores minimos y maximos. Opcion de usar range
  TipoDeTipologia = ifelse(Tipologia == 1, "Unifamiliar", "Plurifamiliar")
  Beta <- abs(subset(ParametrosDelModelo, Tipologia==TipoDeTipologia & Tipo=="beta", select = c(4:9)))
  B <- subset(ParametrosDelModelo, Tipologia==TipoDeTipologia & Tipo=="b", select = c(4:9))
  Beta$cte <- sum(Beta)
  B_OtrasVariables <- subset(ParametrosDelModelo, Tipologia==TipoDeTipologia & Tipo=="b", select = c(11:16))
  
  CoeficienteBase <- Beta$cte
  Beta/CoeficienteBase
}
ObtieneOtrasVariables <- function(MatrizOtrasVariables){
  #OtrasVariablesCaso
  SinAscensor <- ifelse(MatrizOtrasVariables[,"NumeroDeAscensores"] == 0, 1, 0)
  EsPlantaBaja <- ifelse(MatrizOtrasVariables[,"DatosDeLaPlanta"] == 5, 1, 0)
  EsSemiSotano <- ifelse(MatrizOtrasVariables[,"DatosDeLaPlanta"] == 3, 1, 0)
  EsInterior <- MatrizOtrasVariables[,"ExteriorInterior"] - 1
  EsAtico <- ifelse(MatrizOtrasVariables[,"DatosDeLaPlanta"] > 1, 0, ifelse(MatrizOtrasVariables[,"NumeroDePlanta"] < 3, 0, 1))
  NDePlanta <- ifelse(MatrizOtrasVariables[,"NumeroDePlanta"] < 1, 0, MatrizOtrasVariables[,"NumeroDePlanta"])
  
  cbind(MatrizOtrasVariables, SinAscensor, EsPlantaBaja, EsSemiSotano, EsInterior, EsAtico, NDePlanta)
}

CorreccionPorOtrasVariables <- function(VariablesDeLaMuestra, CasoEstimacion, Parametros){
  MatrizCasoEstimacion <- CasoEstimacion[rep(row.names(CasoEstimacion), nrow(VariablesDeLaMuestra)), 1:10]
  Beta <- subset(Parametros, Tipologia=="Plurifamiliar" & Tipo=="beta", select = c(11:16))
  B <- subset(Parametros, Tipologia=="Plurifamiliar" & Tipo=="b", select = c(11:16))
  SinAscensor <- exp((-VariablesDeLaMuestra[,"SinAscensor"]+MatrizCasoEstimacion[,"SinAscensor"])*Beta$SinAscensor)
  EsPlantaBaja <- exp((-VariablesDeLaMuestra[,"EsPlantaBaja"]+MatrizCasoEstimacion[,"EsPlantaBaja"])*Beta$EsPlantaBaja)
  EsSemiSotano <- exp((-VariablesDeLaMuestra[,"EsSemiSotano"]+MatrizCasoEstimacion[,"EsSemiSotano"])*Beta$EsSemiSotano)
  EsInterior <- exp((-VariablesDeLaMuestra[,"EsInterior"]+MatrizCasoEstimacion[,"EsInterior"])*Beta$EsInterior)
  EsAtico <- 0
  #Calculo de si es atico
  for (Muestra in 1:nrow(VariablesDeLaMuestra)){
    if (CasoEstimacion[,"SinAscensor"] == FALSE){
      if(VariablesDeLaMuestra[Muestra,"SinAscensor"] == FALSE){
        EsAtico[Muestra] <- exp((-VariablesDeLaMuestra[Muestra,"EsAtico"]+CasoEstimacion[,"EsAtico"])*Beta$EsAtico)      
      }
      else {
        EsAtico[Muestra] <- exp(+CasoEstimacion[,"EsAtico"]*Beta$EsAtico)          
      }
    }
    else {
      if(VariablesDeLaMuestra[Muestra,"SinAscensor"] == FALSE){
        EsAtico[Muestra] <- exp((-VariablesDeLaMuestra[Muestra,"EsAtico"])*Beta$EsAtico)
      }
      else {
        EsAtico[Muestra] <- 1      
      }
    }
  }
  
  lPlanta <- 0
  #Calculo del Logaritmo de la planta
  for (Muestra in 1:nrow(VariablesDeLaMuestra)){
    if (CasoEstimacion[,"SinAscensor"] == FALSE){
      lPlanta[Muestra] <- ((CasoEstimacion[,"NDePlanta"]+1)/(VariablesDeLaMuestra[Muestra,"NDePlanta"]+1))^B$NDePlanta     
    }
    else {
      lPlanta[Muestra] <- exp(Beta$NDePlanta)^(CasoEstimacion[,"NDePlanta"]^2 - VariablesDeLaMuestra[Muestra,"NDePlanta"]^2)
    }
  }
  
  SinAscensor * EsPlantaBaja * EsSemiSotano * EsInterior * EsAtico * lPlanta
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
  Estimacion <- c(Estimacion, LimiteInferior, LimiteSuperior, PorcentajeDesviacionLimiteInferior, PorcentajeDesviacionLimiteSuperior)
  write.table(Estimacion, paste(Directorio,  "Tasacion.csv", sep = "/"), dec = ",", row.names = FALSE, col.names = FALSE)
  Estimacion
}

