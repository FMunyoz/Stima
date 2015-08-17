TransformarCaso <- function(Caso, MuestraTransformada){
  Datos <- TransformarVariables(Caso)
  if (is.na(Caso[,"SuperficieConstruidaDeLaVivienda"]) || Caso[,"SuperficieConstruidaDeLaVivienda"] == 0) 
      Datos[,"SuperficieConstruidaDeLaVivienda"] <- mean(MuestraTransformada[,"SuperficieConstruidaDeLaVivienda"])
  if (is.na(Caso[,"CosteDeConstruccionBrutoUnitarioVivienda"]) || Caso[,"CosteDeConstruccionBrutoUnitarioVivienda"] == 0) 
      Datos[,"CosteDeConstruccionBrutoUnitarioVivienda"] <- mean(MuestraTransformada[,"CosteDeConstruccionBrutoUnitarioVivienda"])
  if (is.na(Caso[,"PorcentajeDepreciacionVivienda"]) || Caso[,"PorcentajeDepreciacionVivienda"] == 0) 
      Datos[,"PorcentajeDepreciacionVivienda"] <- mean(MuestraTransformada[,"PorcentajeDepreciacionVivienda"])
  Datos  
}