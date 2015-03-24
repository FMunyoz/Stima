ObtieneDatosDelModelo <- function(ParametrosDelModelo, Tipologia){
  #Modelo AJ
  #Tipologia: 1 = Unifamiliar, 2 = Plurifamiliar
  #Calculo de valores minimos y maximos. Opcion de usar range
  TipoDeTipologia = ifelse(Tipologia == 1, "Unifamiliar", "Plurifamiliar")
  Beta <- abs(subset(ParametrosDelModelo, Tipologia==TipoDeTipologia & Tipo=="beta", select = c(4:9)))
  B <- subset(ParametrosDelModelo, Tipologia==TipoDeTipologia & Tipo=="b", select = c(4:9))
  Beta$cte <- sum(Beta)
  B_OtrasVariables <- subset(ParametrosDelModelo, Tipologia==TipoDeTipologia & Tipo=="b", select = c(11:16))
  
  CoeficienteBase <- Beta$cte
  Beta/CoeficienteBase
}