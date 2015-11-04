ObtieneRangosDelModelo_aj <- function(VariablesMuestraTransformadas, VariablesCasoTransformadas) {
ValoresMinimos <- t(as.matrix(apply(rbind(VariablesMuestraTransformadas, VariablesCasoTransformadas), 2, min)))
ValoresMaximos <- t(as.matrix(apply(rbind(VariablesMuestraTransformadas, VariablesCasoTransformadas), 2, max)))
Rangos <- ValoresMaximos - ValoresMinimos
Rangos[Rangos==0] <- 1
rownames(Rangos) <- "1"
Rangos
}