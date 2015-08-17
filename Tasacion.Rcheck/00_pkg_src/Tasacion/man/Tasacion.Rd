\name{Tasacion}
\alias{Tasacion}
\title{Genera una tasacion}
\description{
Genera una tasacion a partir de 3 archivos csv ubicados en el directorio indicado:
 - El caso a estimar (CasoEstimacion.csv)
 - Los parametros del modelo (ParametrosCalculo.csv)
 - La muestra de tasaciones de la zona (DatosMuestra.csv)
}
\usage{
Tasacion(Directorio)
}
\arguments{
  \item{Directorio}{
Directorio en el que estan ubicados los archivos CasoEstimacion.csv, ParametrosCalculo.csv y DatosMuestra.csv
}
}
\details{
Cabecera de los archivos csv (separador es el punto y coma)

CasoEstimacion.csv
-------------------
V005;FechaDeCierre;NumeroDeExpediente;ValorUnitarioDeMercado;NumeroDeBanos;NumeroDeDormitorios;SuperficieConstruidaDeLaVivienda;CosteDeConstruccionBrutoUnitarioVivienda;PorcentajeDepreciacionVivienda;NumeroDeAscensores;Tipologia;SuperficieDeLaParcela;DatosDeLaPlanta;NumeroDePlanta;ExteriorInterior

DatosMuestra.csv
-----------------
FechaDeCierre;NumeroDeExpediente;ValorUnitarioDeMercado;NumeroDeBanos;NumeroDeDormitorios;SuperficieConstruidaDeLaVivienda;CosteDeConstruccionBrutoUnitarioVivienda;PorcentajeDepreciacionVivienda;NumeroDeAscensores;Tipologia;SuperficieDeLaParcela;DatosDeLaPlanta;NumeroDePlanta;TipoZona;idZona;CodZona;Distancia;CodZonaReferencia;ExteriorInterior

ParametrosCalculo.csv
---------------------
Tipologia;Tipo;Fecha;x1;x2;x3;x4;x5;cte;media zona;SinAscensor;EsPlantaBaja;EsSemiSotano;EsInterior;EsAtico;NDePlanta

La tasacion se graba en un archivo Tasacion.csv en el mismo directorio Sin cabecera con la siguiente informacion:

Valor Estimado
Valor Estimado Inferior
Valor Estimado Superior
Valor Estimado Inferior (en tanto por 1)
Valor esitmado Superior (en tanto por 1)
}
\value{
Devuelve la siguiente informacion

Valor Estimado
Valor Estimado Inferior
Valor Estimado Superior
Valor Estimado Inferior (en tanto por 1)
Valor esitmado Superior (en tanto por 1)
}
\references{}
\author{}
\note{} 

\seealso{}

\examples{}
\keyword{}
\keyword{}
