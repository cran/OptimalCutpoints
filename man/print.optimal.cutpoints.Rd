\name{print.optimal.cutpoints}
\alias{print.optimal.cutpoints}
\title{
Print method for optimal.cutpoints objects
}
\description{
  Default print method for objects fitted with \code{optimal.cutpoints()} function. A short summary is printed with: the call to the \code{optimal.cutpoints()} function; the main descriptive statistics in diseased and healthy populations for each categorical covariate level
(if the \code{categorical.cov} argument of the \code{optimal.cutpoints} function is not NULL); and the value of sample prevalence.
}
\usage{
\method{print}{optimal.cutpoints}(x, digits = max(3, getOption("digits") - 3), ...)
}
 
\arguments{
  \item{x}{
     an object of class \code{optimal.cutpoints} as produced by \code{optimal.cutpoints()}.
}
  \item{digits}{
     controls number of digits printed in the output
}
  \item{\dots}{
     further arguments passed to or from other methods.
}
}
\author{
  Monica Lopez-Raton and Maria Xose Rodriguez-Alvarez
}
\seealso{
 \code{\link{optimal.cutpoints}}, \code{\link{summary.optimal.cutpoints}}
}

\examples{
library(OptimalCutpoints)
data(elas)
###########################################################
# Youden Index Method ("Youden"): Covariate gender
###########################################################
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, marker = "elas", 
status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)

optimal.cutpoint.Youden

print(optimal.cutpoint.Youden)  
}

