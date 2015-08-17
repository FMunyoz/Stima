GrabarTasacion <- function(Directorio, Muestra){
  write.table(Muestra, paste(Directorio,  "Tasacion.csv", sep = "/"), dec = ",", row.names = FALSE, col.names = FALSE)
  Muestra
}

