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