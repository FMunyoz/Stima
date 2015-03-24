Tasacion <- function(Directorio){
  Estimacion = sample.int(1000, 1)
  write.table(Estimacion, paste(Directorio,  "Tasacion.csv", sep = "/"), row.names = FALSE, col.names = FALSE)
}