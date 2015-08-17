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
}
