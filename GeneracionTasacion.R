Tasacion <- function(Directorio){
    Directorio <- "C:/Users/federico.munyoz/Documents/Stima/Datos"
    ListaDeDatos <- LeerArchivos(Directorio)
        
    Variables <- SeleccionarVariables(ListaDeDatos$Muestra)
    VariablesCaso <- SeleccionarVariables(ListaDeDatos$Estimacion)
    
    #Datos Auxiliares
    OtrasVariables <- SeleccionarOtrasVariables(ListaDeDatos$Muestra)
    OtrasVariablesCaso <- SeleccionarOtrasVariables(ListaDeDatos$Estimacion)
    
    #Hay que poner este valor si es 1, 2 ó 7
    OtrasVariablesCaso[,"NumeroDePlanta"][is.na(OtrasVariablesCaso[,"NumeroDePlanta"])] <- 0.411764706
    
    LogaritmoDatosCaso <- TransformarVariables(VariablesCaso)
    LogaritmoVariables <- TransformarVariables(Variables)
    
    #Modelo AJ
    #Tipologia: 1 = Unifamiliar, 2 = Multifamiliar
    #Calculo de valores minimos y maximos. Opcion de usar range
    BetaUnifamiliar <- abs(subset(ListaDeDatos$Parametros, Tipologia=="Unifamiliar" & Tipo=="beta", select = c(4:9)))
    B_Unifamiliar <- subset(ListaDeDatos$Parametros, Tipologia=="Unifamiliar" & Tipo=="b", select = c(4:9))
    BetaUnifamiliar$cte <- sum(BetaUnifamiliar)
    BetaOtrasVariables <- subset(ListaDeDatos$Parametros, Tipologia=="Plurifamiliar" & Tipo=="beta", select = c(11:16))
    B_OtrasVariables <- subset(ListaDeDatos$Parametros, Tipologia=="Plurifamiliar" & Tipo=="b", select = c(11:16))
    
    CoeficienteBase <- BetaUnifamiliar$cte
    Coeficientes <- BetaUnifamiliar/CoeficienteBase
    ValoresMinimos <- t(as.matrix(apply(LogaritmoVariables, 2, min)))
    ValoresMaximos <- t(as.matrix(apply(LogaritmoVariables, 2, max)))
    Rangos <- ValoresMaximos - ValoresMinimos
    rownames(Rangos) <- "1"
    Medias <- t(as.matrix(apply(LogaritmoVariables, 2, mean)))
    DesviacionTipica <- apply(LogaritmoVariables, 2, sd)
    
    # Se asigna valor a CosteDeConstruccionBrutoUnitarioVivienda porque no tiene datos
    
    LogaritmoDatosCaso[1,"CosteDeConstruccionBrutoUnitarioVivienda"] = Medias[1,"CosteDeConstruccionBrutoUnitarioVivienda"]
    
    CotaDistancia <- (LogaritmoDatosCaso / (1000 * Rangos))^2
    # 
    DatoCoeficiente <- sum(CotaDistancia[,2:6] * Coeficientes[,1:5])
    
    #Distancia
    MatrizDatosCaso <- LogaritmoDatosCaso[rep(row.names(LogaritmoDatosCaso), nrow(Variables)), 1:6]
    MatrizRangos <- Rangos[rep(row.names(Rangos), nrow(Variables)), 1:6]
    MatrizCoeficientes <- Coeficientes[rep(row.names(Coeficientes), nrow(Variables)), 1:6]
    Matriz_b <- B_Unifamiliar[rep(row.names(B_Unifamiliar), nrow(Variables)), 1:6]
    Distancia <- as.matrix(rowSums(((LogaritmoVariables[,2:6] - MatrizDatosCaso[,2:6])/MatrizRangos[,2:6])^2 * MatrizCoeficientes[,1:5]),na.rm = TRUE)
    colnames(Distancia) <- "Distancia"
    
    Ly_Transformada <- as.matrix(LogaritmoVariables[,1] + rowSums((MatrizDatosCaso[,2:6] - LogaritmoVariables[,2:6])*Matriz_b[,1:5]),na.rm = TRUE)
    colnames(Ly_Transformada) <- "Ly_Transformada"
    
    jerarquia <- as.matrix(order(Distancia))
    DistanciaOrdenada <- as.matrix(Distancia[order(Distancia)])
    rownames(DistanciaOrdenada) <- jerarquia
    
    p_original <- as.matrix(abs(log(Distancia))/log(1/DatoCoeficiente))
    colnames(p_original) <- "p_original"

    Tipologia <- as.matrix(ListaDeDatos$Muestra[,"Tipologia"])
    colnames(Tipologia) <- "Tipologia"
    LogaritmoVariables <- cbind(LogaritmoVariables, Distancia, Ly_Transformada, p_original, Tipologia)
    
    MuestraOrdenada <- head(LogaritmoVariables[order(LogaritmoVariables[,"Distancia"]),], 30)
    Total_p_original <- sum(MuestraOrdenada[,"p_original"])
    
    Pesos <- as.matrix(MuestraOrdenada[,"p_original"]*30/Total_p_original)
    colnames(Pesos) <- "Pesos"
    
    MuestraOrdenada <- cbind(MuestraOrdenada, Pesos)
    
    
    #OtrasVariablesCaso
    SinAscensor <- ifelse(OtrasVariablesCaso[,"NumeroDeAscensores"] == 0, 1, 0)
    EsPlantaBaja <- ifelse(OtrasVariablesCaso[,"DatosDeLaPlanta"] == 5, 1, 0)
    EsSemiSotano <- ifelse(OtrasVariablesCaso[,"DatosDeLaPlanta"] == 3, 1, 0)
    EsInterior <- OtrasVariablesCaso[,"ExteriorInterior"] - 1
    EsAtico <- ifelse(OtrasVariablesCaso[,"DatosDeLaPlanta"] > 1, 0, ifelse(OtrasVariablesCaso[,"NumeroDePlanta"] < 3, 0, 1))
    NDePlanta <- ifelse(OtrasVariablesCaso[,"NumeroDePlanta"] < 1, 0, OtrasVariablesCaso[,"NumeroDePlanta"])

    OtrasVariablesCaso <- cbind(OtrasVariablesCaso, SinAscensor, EsPlantaBaja, EsSemiSotano, EsInterior, EsAtico, NDePlanta)
    
    #OtrasVariables
    SinAscensor <- ifelse(OtrasVariables[,"NumeroDeAscensores"] == 0, 1, 0)
    EsPlantaBaja <- ifelse(OtrasVariables[,"DatosDeLaPlanta"] == 5, 1, 0)
    EsSemiSotano <- ifelse(OtrasVariables[,"DatosDeLaPlanta"] == 3, 1, 0)
    EsInterior <- OtrasVariables[,"ExteriorInterior"] - 1
    EsAtico <- ifelse(OtrasVariables[,"DatosDeLaPlanta"] > 1, 0, ifelse(OtrasVariables[,"NumeroDePlanta"] < 3, 0, 1))
    NDePlanta <- ifelse(OtrasVariables[,"NumeroDePlanta"] < 1, 0, OtrasVariables[,"NumeroDePlanta"])

    OtrasVariables <- cbind(OtrasVariables, SinAscensor, EsPlantaBaja, EsSemiSotano, EsInterior, EsAtico, NDePlanta)
    
    
    MatrizDatosCasoAuxiliares <- OtrasVariablesCaso[rep(row.names(OtrasVariablesCaso), nrow(OtrasVariables)), 1:10]
    
    SinAscensor <- exp((-OtrasVariables[,"SinAscensor"]+MatrizDatosCasoAuxiliares[,"SinAscensor"])*BetaOtrasVariables$SinAscensor)
    EsPlantaBaja <- exp((-OtrasVariables[,"EsPlantaBaja"]+MatrizDatosCasoAuxiliares[,"EsPlantaBaja"])*BetaOtrasVariables$pbaja)
    EsSemiSotano <- exp((-OtrasVariables[,"EsSemiSotano"]+MatrizDatosCasoAuxiliares[,"EsSemiSotano"])*BetaOtrasVariables$pbaja)
    EsInterior <- exp((-OtrasVariables[,"EsInterior"]+MatrizDatosCasoAuxiliares[,"EsInterior"])*BetaOtrasVariables$interior)
    EsAtico <- exp((-OtrasVariables[,"EsAtico"]+MatrizDatosCasoAuxiliares[,"EsAtico"])*BetaOtrasVariables$Atico)
    lPlanta <- exp((-OtrasVariables[,"NDePlanta"]+MatrizDatosCasoAuxiliares[,"NDePlanta"])*BetaOtrasVariables$lplanta)
    
    
    CorreccionOtrasVariables <- SinAscensor * EsPlantaBaja * EsSemiSotano * EsInterior * EsAtico * lPlanta
    Correccion <- CorreccionOtrasVariables[rownames(MuestraOrdenada)]
    wCorregido <- ifelse(MuestraOrdenada[,"Tipologia"]==1, MuestraOrdenada[,"Ly_Transformada"],log(MuestraOrdenada[,"Ly_Transformada"]*exp(Correccion)))
    PesosXwCorregido <- MuestraOrdenada[,"Pesos"]*wCorregido
    PesosXwCorregidoCuadrado <-  MuestraOrdenada[,"Pesos"]*wCorregido^2
    SumaPesos <- cumsum(MuestraOrdenada[,"Pesos"])
    SumaPesosXwCorregido <- cumsum(PesosXwCorregido)
    Estimacion <- SumaPesosXwCorregido/SumaPesos
    SumaPesosXwCorregidoCuadrado <- cumsum(PesosXwCorregidoCuadrado)
    SumaPesosXwCorregidoCuadradoEntreSumaPesos <- SumaPesosXwCorregidoCuadrado/SumaPesos
    s <- sqrt(SumaPesos*round(SumaPesosXwCorregidoCuadradoEntreSumaPesos-Estimacion^2,8)/(SumaPesos-1))
    rndSumaPesosMenos1 <- round(SumaPesos-1)
    t <- ifelse(rndSumaPesosMenos1>30, rndSumaPesosMenos1,qt(.975, 1:30)[rndSumaPesosMenos1])
    sxt <- s*t
    MuestraOrdenada <- cbind(MuestraOrdenada, Correccion, wCorregido, PesosXwCorregido, PesosXwCorregidoCuadrado, SumaPesos, SumaPesosXwCorregido, 
                             Estimacion, SumaPesosXwCorregidoCuadrado, SumaPesosXwCorregidoCuadradoEntreSumaPesos, s, rndSumaPesosMenos1, t, sxt)
    

    MuestraOrdenadaFiltrada <- tail(MuestraOrdenada, 25)
    ValorDistribucionT <- ifelse(min(MuestraOrdenadaFiltrada[,"sxt"])>=log(1.05), min(MuestraOrdenadaFiltrada[,"sxt"]), MuestraOrdenadaFiltrada[,"sxt"][25])
    
    Seleccionados <- head(MuestraOrdenada, 5)
    Adicionales <- subset(MuestraOrdenadaFiltrada, MuestraOrdenadaFiltrada[,"sxt"] == ValorDistribucionT)
    Seleccionados <- rbind(Seleccionados, Adicionales)
    
    GrabarTasacion(Directorio, Seleccionados)

}

Tasacion("C:/Users/federico.munyoz/Documents/Stima/Datos")
