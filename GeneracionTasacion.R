#Caso a estimar
CasoA_Estimar <- read.csv("C:/Users/federico.munyoz/Documents/Stima/Datos/CasoA_Estimar.csv", sep = ";", dec = ",")

#Leyendo Muestra
DatosMuestra <- read.csv("C:/Users/federico.munyoz/Documents/Stima/Datos/DatosMuestra.csv", sep = ";", dec = ",")
#Leyendo Parametros de Calculo
ParametrosCalculo <- read.csv("C:/Users/federico.munyoz/Documents/Stima/Datos/ParametrosCalculo.csv", sep = ";", dec = ",")
DatosCaso <- as.matrix(subset(CasoA_Estimar, select = c(3,5,6,7,8,9)))
Variables <- as.matrix(subset(DatosMuestra, select = c(3,4,5,6,7,8)))
# Si es de tipologia 1 se usa la Superficie de parcela 
DatosCaso <- as.matrix(subset(CasoA_Estimar, select = c(3,5,12,7,8,9)))
Variables <- as.matrix(subset(DatosMuestra, select = c(3,4,11,6,7,8)))

#Datos Auxiliares
VariablesAuxiliares <- as.matrix(subset(DatosMuestra, select = c(9, 19, 13, 12)))
DatosCasoAuxiliares <- as.matrix(subset(CasoA_Estimar, select = c(10,15,14,13)))
#Actualiza Datos Auxiliares
DatosCasoAuxiliares[,"NumeroDeAscensores"][is.na(DatosCasoAuxiliares[,"NumeroDeAscensores"])] <- 1
DatosCasoAuxiliares[,"ExteriorInterior"][is.na(DatosCasoAuxiliares[,"ExteriorInterior"])] <- 1
DatosCasoAuxiliares[,"DatosDeLaPlanta"][is.na(DatosCasoAuxiliares[,"DatosDeLaPlanta"])] <- 7
DatosCasoAuxiliares[,"DatosDeLaPlanta"] <- ifelse(DatosCasoAuxiliares[,"DatosDeLaPlanta"] == 0, 5, 7)
#Hay que poner este valor si es 1, 2 ó 7
DatosCasoAuxiliares[,"NumeroDePlanta"][is.na(DatosCasoAuxiliares[,"NumeroDePlanta"])] <- 0.411764706

VariablesNormalizada <- Variables + t(matrix(c(0, 2, 2, 0, 0, 2), nrow=6, ncol=nrow(DatosMuestra)))

DatosCasoNormalizado <- DatosCaso + t(matrix(c(0, 2, 2, 0, 0, 2), nrow=6, ncol=1))
LogaritmoDatosCaso <- log(DatosCasoNormalizado)
LogaritmoVariables <- log(VariablesNormalizada)

#Obtengo la media del ValorUnitario
MediaY <- mean(LogaritmoVariables[,"ValorUnitarioDeMercado"])

#Modelo AJ
#Tipologia: 1 = Unifamiliar, 2 = Multifamiliar
#Calculo de valores minimos y maximos. Opcion de usar range
ParametrosSeleccionados <- abs(subset(ParametrosCalculo, Tipologia=="Unifamiliar" & Tipo=="beta", select = c(4:9)))
ParametrosSeleccionados_b <- subset(ParametrosCalculo, Tipologia=="Unifamiliar" & Tipo=="b", select = c(4:9))
ParametrosSeleccionados$cte <- sum(ParametrosSeleccionados)
ParametrosValoresAuxiliares_b <- subset(ParametrosCalculo, Tipologia=="Plurifamiliar" & Tipo=="b", select = c(11:16))
ParametrosValoresAuxiliares_beta <- subset(ParametrosCalculo, Tipologia=="Plurifamiliar" & Tipo=="beta", select = c(11:16))

CoeficienteBase <- ParametrosSeleccionados$cte
Coeficientes <- ParametrosSeleccionados/CoeficienteBase
ValoresMinimos <- t(as.matrix(apply(LogaritmoVariables, 2, min)))
ValoresMaximos <- t(as.matrix(apply(LogaritmoVariables, 2, max)))
Rangos <- ValoresMaximos - ValoresMinimos
rownames(Rangos) <- "1"
Medias <- t(as.matrix(apply(LogaritmoVariables, 2, mean)))
DesviacionTipica <- apply(LogaritmoVariables, 2, sd)
MediaMenos3DesviacionesTipicas <- Medias - 3 * DesviacionTipica
MediaMas3DesviacionesTipicas <- Medias + 3 * DesviacionTipica
# Se asigna valor a CosteDeConstruccionBrutoUnitarioVivienda porque no tiene datos

LogaritmoDatosCaso[1,"CosteDeConstruccionBrutoUnitarioVivienda"] = Medias[1,"CosteDeConstruccionBrutoUnitarioVivienda"]

CotaDistancia <- (LogaritmoDatosCaso / (1000 * Rangos))^2
# 
DatoCoeficiente <- sum(CotaDistancia[,2:6] * Coeficientes[,1:5])

#Distancia
MatrizDatosCaso <- LogaritmoDatosCaso[rep(row.names(LogaritmoDatosCaso), nrow(DatosMuestra)), 1:6]
MatrizRangos <- Rangos[rep(row.names(Rangos), nrow(DatosMuestra)), 1:6]
MatrizCoeficientes <- Coeficientes[rep(row.names(Coeficientes), nrow(DatosMuestra)), 1:6]
Matriz_b <- ParametrosSeleccionados_b[rep(row.names(ParametrosSeleccionados_b), nrow(DatosMuestra)), 1:6]
Distancia <- as.matrix(rowSums(((LogaritmoVariables[,2:6] - MatrizDatosCaso[,2:6])/MatrizRangos[,2:6])^2 * MatrizCoeficientes[,1:5]),na.rm = TRUE)
colnames(Distancia) <- "Distancia"
LogaritmoVariables <- cbind(LogaritmoVariables, Distancia)

Ly_Transformada <- as.matrix(LogaritmoVariables[,1] + rowSums((MatrizDatosCaso[,2:6] - LogaritmoVariables[,2:6])*Matriz_b[,1:5]),na.rm = TRUE)
colnames(Ly_Transformada) <- "Ly_Transformada"
LogaritmoVariables <- cbind(LogaritmoVariables, Ly_Transformada)

jerarquia <- as.matrix(order(Distancia))
DistanciaOrdenada <- as.matrix(Distancia[order(Distancia)])
rownames(DistanciaOrdenada) <- jerarquia

p_original <- as.matrix(abs(log(Distancia))/log(1/DatoCoeficiente))
colnames(p_original) <- "p_original"
LogaritmoVariables <- cbind(LogaritmoVariables, p_original)


#Tipologia
Tipologia <- as.matrix(DatosMuestra[,"Tipologia"])
colnames(Tipologia) <- "Tipologia"
LogaritmoVariables <- cbind(LogaritmoVariables, Tipologia)

MuestraOrdenada <- head(LogaritmoVariables[order(LogaritmoVariables[,"Distancia"]),], 30)
Total_p_original <- sum(MuestraOrdenada[,"p_original"])

Pesos <- as.matrix(MuestraOrdenada[,"p_original"]*30/Total_p_original)
colnames(Pesos) <- "Pesos"

MuestraOrdenada <- cbind(MuestraOrdenada, Pesos)


#DatosCasoAuxiliares
SinAscensor <- ifelse(DatosCasoAuxiliares[,"NumeroDeAscensores"] == 0, 1, 0)
DatosCasoAuxiliares <- cbind(DatosCasoAuxiliares, SinAscensor)

EsPlantaBaja <- ifelse(DatosCasoAuxiliares[,"DatosDeLaPlanta"] == 5, 1, 0)
DatosCasoAuxiliares <- cbind(DatosCasoAuxiliares, EsPlantaBaja)

EsSemiSotano <- ifelse(DatosCasoAuxiliares[,"DatosDeLaPlanta"] == 3, 1, 0)
DatosCasoAuxiliares <- cbind(DatosCasoAuxiliares, EsSemiSotano)

EsInterior <- DatosCasoAuxiliares[,"ExteriorInterior"] - 1
DatosCasoAuxiliares <- cbind(DatosCasoAuxiliares, EsInterior)

EsAtico <- ifelse(DatosCasoAuxiliares[,"DatosDeLaPlanta"] > 1, 0, ifelse(DatosCasoAuxiliares[,"NumeroDePlanta"] < 3, 0, 1))
DatosCasoAuxiliares <- cbind(DatosCasoAuxiliares, EsAtico)

NDePlanta <- ifelse(DatosCasoAuxiliares[,"NumeroDePlanta"] < 1, 0, DatosCasoAuxiliares[,"NumeroDePlanta"])
DatosCasoAuxiliares <- cbind(DatosCasoAuxiliares, NDePlanta)


#VariablesAuxiliares
SinAscensor <- ifelse(VariablesAuxiliares[,"NumeroDeAscensores"] == 0, 1, 0)
VariablesAuxiliares <- cbind(VariablesAuxiliares, SinAscensor)

EsPlantaBaja <- ifelse(VariablesAuxiliares[,"DatosDeLaPlanta"] == 5, 1, 0)
VariablesAuxiliares <- cbind(VariablesAuxiliares, EsPlantaBaja)

EsSemiSotano <- ifelse(VariablesAuxiliares[,"DatosDeLaPlanta"] == 3, 1, 0)
VariablesAuxiliares <- cbind(VariablesAuxiliares, EsSemiSotano)

EsInterior <- VariablesAuxiliares[,"ExteriorInterior"] - 1
VariablesAuxiliares <- cbind(VariablesAuxiliares, EsInterior)

EsAtico <- ifelse(VariablesAuxiliares[,"DatosDeLaPlanta"] > 1, 0, ifelse(VariablesAuxiliares[,"NumeroDePlanta"] < 3, 0, 1))
VariablesAuxiliares <- cbind(VariablesAuxiliares, EsAtico)

NDePlanta <- ifelse(VariablesAuxiliares[,"NumeroDePlanta"] < 1, 0, VariablesAuxiliares[,"NumeroDePlanta"])
VariablesAuxiliares <- cbind(VariablesAuxiliares, NDePlanta)


MatrizDatosCasoAuxiliares <- DatosCasoAuxiliares[rep(row.names(DatosCasoAuxiliares), nrow(DatosMuestra)), 1:10]

SinAscensor <- exp((-VariablesAuxiliares[,"SinAscensor"]+MatrizDatosCasoAuxiliares[,"SinAscensor"])*ParametrosValoresAuxiliares_beta$SinAscensor)
EsPlantaBaja <- exp((-VariablesAuxiliares[,"EsPlantaBaja"]+MatrizDatosCasoAuxiliares[,"EsPlantaBaja"])*ParametrosValoresAuxiliares_beta$pbaja)
EsSemiSotano <- exp((-VariablesAuxiliares[,"EsSemiSotano"]+MatrizDatosCasoAuxiliares[,"EsSemiSotano"])*ParametrosValoresAuxiliares_beta$pbaja)
EsInterior <- exp((-VariablesAuxiliares[,"EsInterior"]+MatrizDatosCasoAuxiliares[,"EsInterior"])*ParametrosValoresAuxiliares_beta$interior)
EsAtico <- exp((-VariablesAuxiliares[,"EsAtico"]+MatrizDatosCasoAuxiliares[,"EsAtico"])*ParametrosValoresAuxiliares_beta$Atico)
lPlanta <- exp((-VariablesAuxiliares[,"NDePlanta"]+MatrizDatosCasoAuxiliares[,"NDePlanta"])*ParametrosValoresAuxiliares_beta$lplanta)


CorreccionOtrasVariables <- SinAscensor * EsPlantaBaja * EsSemiSotano * EsInterior * EsAtico * lPlanta
Correccion <- CorreccionOtrasVariables[rownames(MuestraOrdenada)]
MuestraOrdenada <- cbind(MuestraOrdenada, Correccion)
wCorregido <- ifelse(MuestraOrdenada[,"Tipologia"]==1, MuestraOrdenada[,"Ly_Transformada"],log(MuestraOrdenada[,"Ly_Transformada"]*exp(MuestraOrdenada[,"Correccion"])))
MuestraOrdenada <- cbind(MuestraOrdenada, wCorregido)
PesosXwCorregido <- MuestraOrdenada[,"Pesos"]*MuestraOrdenada[,"wCorregido"]
MuestraOrdenada <- cbind(MuestraOrdenada, PesosXwCorregido)
PesosXwCorregidoCuadrado <-  MuestraOrdenada[,"Pesos"]*MuestraOrdenada[,"wCorregido"]^2
MuestraOrdenada <- cbind(MuestraOrdenada, PesosXwCorregidoCuadrado)
SumaPesos <- cumsum(MuestraOrdenada[,"Pesos"])
MuestraOrdenada <- cbind(MuestraOrdenada, SumaPesos)
SumaPesosXwCorregido <- cumsum(PesosXwCorregido)
MuestraOrdenada <- cbind(MuestraOrdenada, SumaPesosXwCorregido)
Estimacion <- MuestraOrdenada[,"SumaPesosXwCorregido"]/MuestraOrdenada[,"SumaPesos"]
MuestraOrdenada <- cbind(MuestraOrdenada, Estimacion)
SumaPesosXwCorregidoCuadrado <- cumsum(PesosXwCorregidoCuadrado)
MuestraOrdenada <- cbind(MuestraOrdenada, SumaPesosXwCorregidoCuadrado)
SumaPesosXwCorregidoCuadradoEntreSumaPesos <- MuestraOrdenada[,"SumaPesosXwCorregidoCuadrado"]/MuestraOrdenada[,"SumaPesos"]
MuestraOrdenada <- cbind(MuestraOrdenada, SumaPesosXwCorregidoCuadradoEntreSumaPesos)
s <- sqrt(MuestraOrdenada[,"SumaPesos"]*round(MuestraOrdenada[,"SumaPesosXwCorregidoCuadradoEntreSumaPesos"]-MuestraOrdenada[,"Estimacion"]^2,8)/(MuestraOrdenada[,"SumaPesos"]-1))
MuestraOrdenada <- cbind(MuestraOrdenada, s)
rndSumaPesosMenos1 <- round(MuestraOrdenada[,"SumaPesos"]-1)
MuestraOrdenada <- cbind(MuestraOrdenada, rndSumaPesosMenos1)
t <- ifelse(MuestraOrdenada[,"rndSumaPesosMenos1"]>30, MuestraOrdenada[,"rndSumaPesosMenos1"],qt(.975, 1:30)[MuestraOrdenada[,"rndSumaPesosMenos1"]])
MuestraOrdenada <- cbind(MuestraOrdenada, t)
sxt <- MuestraOrdenada[,"s"]*MuestraOrdenada[,"t"]
MuestraOrdenada <- cbind(MuestraOrdenada, sxt)
MuestraOrdenadaFiltrada <- tail(MuestraOrdenada, 25)
ValorDistribucionT <- ifelse(min(MuestraOrdenadaFiltrada[,"sxt"])>=log(1.05), min(MuestraOrdenadaFiltrada[,"sxt"]), MuestraOrdenadaFiltrada[,"sxt"][25])

Seleccionados <- head(MuestraOrdenada, 5)
Adicionales <- subset(MuestraOrdenadaFiltrada, MuestraOrdenadaFiltrada[,"sxt"] == ValorDistribucionT)
Seleccionados <- rbind(Seleccionados, Adicionales)

N <- nrow(Seleccionados)
Pesos <- sum(Seleccionados[,"Pesos"])
PW <- sum(Seleccionados[,"PesosXwCorregido"])
PW2 <- sum(Seleccionados[,"PesosXwCorregidoCuadrado"])
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

