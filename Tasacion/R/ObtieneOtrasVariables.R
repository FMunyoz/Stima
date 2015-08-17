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