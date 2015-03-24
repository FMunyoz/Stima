TransformarVariables <- function(Muestra){
  Datos <- Muestra + t(matrix(c(0, 2, 2, 0, 0, 2), nrow=6, ncol=nrow(Muestra)))
  log(Datos)
}