ObtieneDatos <- function(Directorio) {
ListaDeDatos <- LeerArchivos(Directorio)

TipologiaEstimacion <- ListaDeDatos$Estimacion[,"Tipologia"]

Variables <- SeleccionarVariables(ListaDeDatos$Muestra, TipologiaEstimacion)
VariablesCaso <- SeleccionarVariables(ListaDeDatos$Estimacion, TipologiaEstimacion)

#Datos Auxiliares
OtrasVariablesCaso <- SeleccionarOtrasVariables(ListaDeDatos$Estimacion)
OtrasVariables <- SeleccionarOtrasVariables(ListaDeDatos$Muestra)

Coeficientes <- ObtieneDatosDelModelo(ListaDeDatos$Parametros, TipologiaEstimacion)

if (TipologiaEstimacion==1){
  Matriz_b <- subset(ListaDeDatos$Parametros, Tipologia=="Unifamiliar" & Tipo=="b", select = c(4:9))
} else {
  Matriz_b <- subset(ListaDeDatos$Parametros, Tipologia=="Plurifamiliar" & Tipo=="b", select = c(4:9))      
}
Matriz_b <- Matriz_b[rep(row.names(Matriz_b), nrow(Variables)), 1:6]

list(TipologiaEstimacion, VariablesCaso, Variables, OtrasVariablesCaso, "OtrasVariables" = OtrasVariables, Coeficientes, Matriz_b)
}