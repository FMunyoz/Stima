#Caso a estimar
CasoA_Estimar <- read.csv("C:/Users/federico.munyoz/Documents/Stima/Datos/CasoA_Estimar.csv", sep = ";", dec = ",")

#Leyendo Muestra
DatosMuestra <- read.csv("C:/Users/federico.munyoz/Documents/Stima/Datos/DatosMuestra.csv", sep = ";", dec = ",")
#Leyendo Parametros de Calculo
ParametrosCalculo <- read.csv("C:/Users/federico.munyoz/Documents/Stima/Datos/ParametrosCalculo.csv", sep = ";", dec = ",")
Variables <- as.matrix(subset(DatosMuestra, select = c(3,4,5,6,7,8)))
# Si es de tipologia 1 se usa la Superficie de parcela 
Variables <- as.matrix(subset(DatosMuestra, select = c(3,4,11,6,7,8)))

VariablesAuxiliares <- as.matrix(DatosMuestra[, 9:13])
VariablesNormalizada <- Variables + t(matrix(c(0, 2, 2, 0, 0, 2), nrow=6, ncol=nrow(DatosMuestra)))
LogaritmoVariables <- log(VariablesNormalizada)

#Obtengo la media del ValorUnitario
MediaY <- mean(LogaritmoVariables[,"ValorUnitarioDeMercado"])

#Modelo AJ
#Tipologia: 1 = Unifamiliar, 2 = Multifamiliar
#Calculo de valores minimos y maximos. Opcion de usar range
ParametrosSeleccionados <- abs(subset(ParametrosCalculo, Tipologia=="Unifamiliar" & Tipo=="beta", select = c(4:9))) 
ParametrosSeleccionados$cte <- sum(ParametrosSeleccionados)

Coeficientes <- ParametrosSeleccionados/CoeficienteBase
ValoresMinimos <- apply(LogaritmoVariables, 2, min)
ValoresMaximos <- apply(LogaritmoVariables, 2, max)
Rangos <- ValoresMaximos - ValoresMinimos
Medias <- apply(LogaritmoVariables, 2, mean)
DesviacionTipica <- apply(LogaritmoVariables, 2, sd)
MediaMenos3DesviacionesTipicas <- Medias - 3 * DesviacionTipica
MediaMas3DesviacionesTipicas <- Medias + 3 * DesviacionTipica

