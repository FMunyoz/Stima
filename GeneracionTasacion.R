Tasacion <- function(Directorio){
    ListaDeDatos <- LeerArchivos(Directorio)

    TipologiaEstimacion <- ListaDeDatos$Estimacion[,"Tipologia"]
        
    Variables <- SeleccionarVariables(ListaDeDatos$Muestra, TipologiaEstimacion)
    VariablesCaso <- SeleccionarVariables(ListaDeDatos$Estimacion, TipologiaEstimacion)
    
    #Datos Auxiliares
    OtrasVariablesCaso <- SeleccionarOtrasVariables(ListaDeDatos$Estimacion)
    OtrasVariables <- SeleccionarOtrasVariables(ListaDeDatos$Muestra)
    
    #Hay que poner este valor si es 1, 2 ó 7
    ElementosConValor7 <- sum(OtrasVariables[,"DatosDeLaPlanta"] == 7)
    DatosASumar <- OtrasVariables[,"DatosDeLaPlanta"] == 7
    TotalDePlantaParaElementosConValor7 <- sum(OtrasVariables[DatosASumar,"NumeroDePlanta"])
    
    OtrasVariablesCaso[is.na(OtrasVariablesCaso[,"NumeroDePlanta"]), "NumeroDePlanta"] <- TotalDePlantaParaElementosConValor7/ElementosConValor7
    if(OtrasVariablesCaso[,"DatosDeLaPlanta"] == 0) OtrasVariablesCaso[,"DatosDeLaPlanta"] <-  5
    
    LogaritmoVariablesCaso <- TransformarVariables(VariablesCaso)
    LogaritmoVariables <- TransformarVariables(Variables)
    
    Coeficientes <- ObtieneDatosDelModelo(ListaDeDatos$Parametros, TipologiaEstimacion)
    Medias <- t(as.matrix(apply(LogaritmoVariables, 2, mean)))

# Se asigna valor a CosteDeConstruccionBrutoUnitarioVivienda porque no tiene datos
    
    LogaritmoVariablesCaso[1,"CosteDeConstruccionBrutoUnitarioVivienda"] = Medias[1,"CosteDeConstruccionBrutoUnitarioVivienda"]
    
    
    ValoresMinimos <- t(as.matrix(apply(rbind(LogaritmoVariables, LogaritmoVariablesCaso), 2, min)))
    ValoresMaximos <- t(as.matrix(apply(rbind(LogaritmoVariables, LogaritmoVariablesCaso), 2, max)))
    Rangos <- ValoresMaximos - ValoresMinimos
    rownames(Rangos) <- "1"
    B_Unifamiliar <- subset(ListaDeDatos$Parametros, Tipologia=="Unifamiliar" & Tipo=="b", select = c(4:9))

    
    CotaDistancia <- (LogaritmoVariablesCaso / (1000 * Rangos))^2
    # 
    DatoCoeficiente <- sum(CotaDistancia[,2:6] * Coeficientes[,1:5])
    
    #Distancia
    MatrizDatosCaso <- LogaritmoVariablesCaso[rep(row.names(LogaritmoVariablesCaso), nrow(Variables)), 1:6]
    MatrizRangos <- Rangos[rep(row.names(Rangos), nrow(Variables)), 1:6]
    MatrizCoeficientes <- Coeficientes[rep(row.names(Coeficientes), nrow(Variables)), 1:6]
    if (TipologiaEstimacion==1){
      Matriz_b <- subset(ListaDeDatos$Parametros, Tipologia=="Unifamiliar" & Tipo=="b", select = c(4:9))
    } else {
      Matriz_b <- subset(ListaDeDatos$Parametros, Tipologia=="Plurifamiliar" & Tipo=="b", select = c(4:9))      
    }
    Matriz_b <- Matriz_b[rep(row.names(Matriz_b), nrow(Variables)), 1:6]
                       
    Distancia <- as.matrix(rowSums(((LogaritmoVariables[,2:6] - MatrizDatosCaso[,2:6])/MatrizRangos[,2:6])^2 * MatrizCoeficientes[,1:5]),na.rm = TRUE)
    colnames(Distancia) <- "Distancia"
    
    Ly_Transformada <- as.matrix(LogaritmoVariables[,"ValorUnitarioDeMercado"] + rowSums((MatrizDatosCaso[,2:6] - LogaritmoVariables[,2:6])*Matriz_b[,1:5]),na.rm = TRUE)
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
    
    OtrasVariablesCaso <- ObtieneOtrasVariables(OtrasVariablesCaso)
    
    OtrasVariables <- ObtieneOtrasVariables(OtrasVariables)
    
    CorreccionOtrasVariables <- CorreccionPorOtrasVariables(OtrasVariables, OtrasVariablesCaso, ListaDeDatos$Parametros)

    Correccion <- CorreccionOtrasVariables[rownames(MuestraOrdenada)]
    wCorregido <- ifelse(MuestraOrdenada[,"Tipologia"]==1, MuestraOrdenada[,"Ly_Transformada"],log(Correccion*exp(MuestraOrdenada[,"Ly_Transformada"])))
    PesosXwCorregido <- MuestraOrdenada[,"Pesos"]*wCorregido
    PesosXwCorregidoCuadrado <-  MuestraOrdenada[,"Pesos"]*wCorregido^2
    SumaPesos <- cumsum(MuestraOrdenada[,"Pesos"])
    SumaPesosXwCorregido <- cumsum(PesosXwCorregido)
    Estimacion <- SumaPesosXwCorregido/SumaPesos
    SumaPesosXwCorregidoCuadrado <- cumsum(PesosXwCorregidoCuadrado)
    SumaPesosXwCorregidoCuadradoEntreSumaPesos <- SumaPesosXwCorregidoCuadrado/SumaPesos
    s <- sqrt(SumaPesos*round(SumaPesosXwCorregidoCuadradoEntreSumaPesos-Estimacion^2,8)/(SumaPesos-1))
    rndSumaPesosMenos1 <- round(SumaPesos-1)
    rndSumaPesosMenos1 <- ifelse(rndSumaPesosMenos1 == 0, 1, rndSumaPesosMenos1)
    t <- ifelse(rndSumaPesosMenos1>30, rndSumaPesosMenos1,qt(.975, 1:30)[rndSumaPesosMenos1])
    sxt <- s*t
    MuestraOrdenada <- cbind(MuestraOrdenada, Correccion, wCorregido, PesosXwCorregido, PesosXwCorregidoCuadrado, SumaPesos, SumaPesosXwCorregido, 
                             Estimacion, SumaPesosXwCorregidoCuadrado, SumaPesosXwCorregidoCuadradoEntreSumaPesos, s, rndSumaPesosMenos1, t, sxt)
    

    MuestraOrdenadaFiltrada <- MuestraOrdenada[6:30,]
    ValorDistribucionT <- ifelse(min(MuestraOrdenadaFiltrada[,"sxt"])>=log(1.05), min(MuestraOrdenadaFiltrada[,"sxt"]), MuestraOrdenadaFiltrada[,"sxt"][25])
    
    Seleccionados <- head(MuestraOrdenada, 5)
    MuestraDeCorte <- subset(MuestraOrdenadaFiltrada, MuestraOrdenadaFiltrada[,"sxt"] == ValorDistribucionT)
    MuestrasAdicionales <- subset(MuestraOrdenadaFiltrada, MuestraOrdenadaFiltrada[,"Distancia"] <= MuestraDeCorte[,"Distancia"])
    Seleccionados <- rbind(Seleccionados, MuestrasAdicionales)
    
    GrabarTasacion(Directorio, Seleccionados)
}

#Tasacion( "C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1314917")
#Tasacion( "C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1314921")
#Tasacion( "C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1314922")
#Tasacion( "C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315155")
#Tasacion( "C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315178")
#Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315296")
#Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315298")
#Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315299")
#Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315301")
#Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315307")
#Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315311")
Tasacion("C:/Users/federico.munyoz/Documents/TINSA/Documentos/WEB_1315323")
