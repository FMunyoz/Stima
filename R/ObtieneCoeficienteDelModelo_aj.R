ObtieneCoeficienteDelModelo_aj <- function(VariablesCasoTransformadas, Rangos, Coeficientes){
CotaDistancia <- (VariablesCasoTransformadas / (1000 * Rangos))^2
sum(CotaDistancia[,2:6] * Coeficientes[,1:5])
}