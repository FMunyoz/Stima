GrabarTasacion <- function(Directorio, Muestra){
  N <- nrow(Muestra)
  Pesos <- sum(Muestra[,"Pesos"])
  PW <- sum(Muestra[,"PesosXwCorregido"])
  PW2 <- sum(Muestra[,"PesosXwCorregidoCuadrado"])
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
  Estimacion <- c(Estimacion, LimiteInferior, LimiteSuperior, PorcentajeDesviacionLimiteInferior, PorcentajeDesviacionLimiteSuperior)
  write.table(Estimacion, paste(Directorio,  "Tasacion.csv", sep = "/"), dec = ",", row.names = FALSE, col.names = FALSE)
  Estimacion
}

