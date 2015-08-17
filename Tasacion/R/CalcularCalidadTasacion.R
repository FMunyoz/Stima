CalcularCalidadTasacion <- function(CasoA_Estimar, Muestra, MuestraSeleccionada, Coeficientes, CasoTransformado){
  
  ColumnaDependienteDeTipologia <-  ifelse("NumeroDeDormitorios" %in% colnames(CasoTransformado),
                                           "NumeroDeDormitorios",
                                           "SuperficieDeLaParcela")
  CasoTransformado <- CasoTransformado[,c("NumeroDeBanos",
                                         ColumnaDependienteDeTipologia,
                                         "SuperficieConstruidaDeLaVivienda",
                                         "CosteDeConstruccionBrutoUnitarioVivienda", 
                                         "PorcentajeDepreciacionVivienda")]
  MuestraCalidad <- Muestra[,c("NumeroDeBanos",
                               ColumnaDependienteDeTipologia,
                               "SuperficieConstruidaDeLaVivienda",
                               "CosteDeConstruccionBrutoUnitarioVivienda", 
                               "PorcentajeDepreciacionVivienda")]
  MuestraCalidad <- rbind(CasoTransformado, MuestraCalidad)
  MuestraCalidadEstandarizada <- scale(MuestraCalidad, center = TRUE, scale = TRUE)[,c(1:5)]
  MuestraCalidadEstandarizada <- cbind(MuestraCalidadEstandarizada, Muestra[,"Pesos"][match(rownames(MuestraCalidadEstandarizada),rownames(Muestra))])
  colnames(MuestraCalidadEstandarizada)[6] <- "Pesos"
  CalculoCentroDeGravedad <- MuestraCalidadEstandarizada[,1:5] * MuestraCalidadEstandarizada[,"Pesos"]
  CalculoNivelConfianza <- CalculoCentroDeGravedad
  PesoTotal <- sum(Muestra[,"Pesos"][match(rownames(Muestra), rownames(MuestraSeleccionada))], na.rm = TRUE)
  CentroDeGravedad <- colSums(subset(CalculoCentroDeGravedad, rownames(CalculoCentroDeGravedad) %in% rownames(MuestraSeleccionada)))/PesoTotal
  if(!is.na(CasoA_Estimar$NumeroDeBanos))
  {
    CalculoNivelConfianza[,"NumeroDeBanos"] <- (CentroDeGravedad["NumeroDeBanos"] - MuestraCalidadEstandarizada[,"NumeroDeBanos"])^2
  } else {
    CalculoNivelConfianza[,"NumeroDeBanos"] <- 0
  }
  
  if(is.na(CasoA_Estimar[,ColumnaDependienteDeTipologia]))
  {
    CalculoNivelConfianza[,ColumnaDependienteDeTipologia] <- 0
    
  } else {
    CalculoNivelConfianza[,ColumnaDependienteDeTipologia] <- (CentroDeGravedad[ColumnaDependienteDeTipologia] - MuestraCalidadEstandarizada[,ColumnaDependienteDeTipologia])^2
  }
  
  if(!is.na(CasoA_Estimar$SuperficieConstruidaDeLaVivienda))
  {
    CalculoNivelConfianza[,"SuperficieConstruidaDeLaVivienda"] <- (CentroDeGravedad["SuperficieConstruidaDeLaVivienda"] - MuestraCalidadEstandarizada[,"SuperficieConstruidaDeLaVivienda"])^2
  } else {
    CalculoNivelConfianza[,"SuperficieConstruidaDeLaVivienda"] <- 0
  }
  
  if(!is.na(CasoA_Estimar$CosteDeConstruccionBrutoUnitarioVivienda))
  {
    CalculoNivelConfianza[,"CosteDeConstruccionBrutoUnitarioVivienda"] <- (CentroDeGravedad["CosteDeConstruccionBrutoUnitarioVivienda"] - MuestraCalidadEstandarizada[,"CosteDeConstruccionBrutoUnitarioVivienda"])^2
  } else {
    CalculoNivelConfianza[,"CosteDeConstruccionBrutoUnitarioVivienda"] <- 0
  }
  
  if(!is.na(CasoA_Estimar$PorcentajeDepreciacionVivienda))
  {
    CalculoNivelConfianza[,"PorcentajeDepreciacionVivienda"] <- (CentroDeGravedad["PorcentajeDepreciacionVivienda"] - MuestraCalidadEstandarizada[,"PorcentajeDepreciacionVivienda"])^2
  } else {
    CalculoNivelConfianza[,"PorcentajeDepreciacionVivienda"] <- 0
  }
  
  d <- sqrt(rowSums(CalculoNivelConfianza %*% diag(Coeficientes[1:5])))
  dxPesos <- d * MuestraCalidadEstandarizada[,"Pesos"]
  dCuadradoxPesos <- d^2 * MuestraCalidadEstandarizada[,"Pesos"]
  SumaDePesosxW <- sum(subset(dxPesos, names(dxPesos) %in% rownames(MuestraSeleccionada)))
  SumaDePesosCuadradoxW <- sum(subset(dCuadradoxPesos, names(dxPesos) %in% rownames(MuestraSeleccionada)))
  MediaPesos <- SumaDePesosxW/PesoTotal
  MediaCuadradoPesos <- SumaDePesosCuadradoxW/PesoTotal
  DesviacionTipica <- sqrt(PesoTotal*(MediaCuadradoPesos-MediaPesos^2)/(PesoTotal-1))
  CalidadEstudio <- (d["CasoTransformado"]-MediaPesos)/DesviacionTipica
  names(CalidadEstudio) <- "CalidadEstudio"
  CalidadEstudioAlfa <- ifelse (CalidadEstudio < 0, "Muy bueno", 
          ifelse (CalidadEstudio >= 0 && CalidadEstudio <= 1, "Bueno", 
                  ifelse (CalidadEstudio > 1 && CalidadEstudio <= 2, "Regular","Malo")))
  names(CalidadEstudioAlfa) <- "CalidadEstudioAlfa"
  c(CalidadEstudio, CalidadEstudioAlfa)
}