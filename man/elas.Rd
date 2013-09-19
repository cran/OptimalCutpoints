\name{elas}
\alias{elas}
\docType{data}
\title{
Leukocyte Elastase Data
}
\description{
The \code{elas} data set was obtained from the Cardiology Department at the Galicia General Hospital (Santiago de Compostela, Spain). This study was conducted to assess the clinical usefulness of leukocyte elastase determination in the diagnosis of coronary artery disease (CAD).
}
\usage{data(elas)}
\format{
  A data frame with 141 observations on the following 3 variables.
  \describe{
    \item{\code{elas}}{leukocyte elastase. Numerical vector}
    \item{\code{status}}{true disease status (presence/absence of coronary artery disease). Numerical vector (0=absence, 1=presence)}
    \item{\code{gender}}{patient's gender. Factor with \code{Male} and \code{Female} levels}
  }
}

\source{
Amaro, A., Gude, F., Gonzalez-Juanatey, R., Iglesias, C., Fernandez-Vazquez, F., Garcia-Acuna, J. and Gil, M. (1995). Plasma leukocyte elastase concentration in angiographically diagnosed coronary artery disease. \emph{European Heart Journal} \bold{16}, 615--622.
}
\references{
Amaro, A., Gude, F., Gonzalez-Juanatey, R., Iglesias, C., Fernandez-Vazquez, F., Garcia-Acuna, J. and Gil, M. (1995). Plasma leukocyte elastase concentration in angiographically diagnosed coronary artery disease. \emph{European Heart Journal} \bold{16}, 615--622.
}
\examples{
data(elas)
summary(elas)
}
\keyword{datasets}


