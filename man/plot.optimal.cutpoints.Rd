\name{plot.optimal.cutpoints}
\alias{plot.optimal.cutpoints}
\title{
Default optimal.cutpoints plotting
}
\description{
On the basis of an \code{\link{optimal.cutpoints}} object, it plots the Receiver Operating Characteristic (ROC) curve, the Predictive ROC (PROC) curve, and in some methods, the values of the optimal criterion used as a function of the cutoffs.

}
\usage{
\method{plot}{optimal.cutpoints}(x, legend = TRUE, ...)
}

\arguments{
  \item{x}{
     an object of class \code{optimal.cutpoint} as produced by \code{optimal.cutpoints()}.
}
  \item{legend}{
     a logical value for including the legend of optimal coordinates with specific characteristics. The default is TRUE.
}
  \item{\dots}{
    further arguments passed to or from other methods.
}
}

\author{
  Monica Lopez-Raton and Maria Xose Rodriguez-Alvarez
}

\seealso{
 \code{\link{optimal.cutpoints}}, \code{\link{control.cutpoints}}
}
\examples{
library(OptimalCutpoints)
data(elas)
###########################################################
# Youden Index method ("Youden"): Covariate gender
###########################################################
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, marker = "elas", 
status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)

# Plot by default
plot(optimal.cutpoint.Youden)

#  Not including the optimal coordinates
plot(optimal.cutpoint.Youden, legend = FALSE)
# Change the colour
plot(optimal.cutpoint.Youden, col = "blue")    
}  
