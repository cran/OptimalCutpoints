\name{summary.optimal.cutpoints}
\alias{summary.optimal.cutpoints}
\title{
Summary method for optimal.cutpoints objects
}
\description{
 Produces a summary of a \code{optimal.cutpoints} object. The following are printed: the call to the \code{optimal.cutpoints()} function; the optimal cutpoint(s) obtained with the method(s) selected; 
its/their accuracy measures and the area under ROC curve (AUC) estimates at each categorical covariate level (if the \code{categorical.cov} argument in the \code{optimal.cutpoints()} function is not NULL). 
If \code{optimal.cutpoints()} was called with the \code{ci.fit = TRUE} argument, confidence intervals for accuracy measures at the optimal cutpoint are also printed.
}
\usage{
\method{summary}{optimal.cutpoints}(object, ...)
}

\arguments{
  \item{object}{
     an object of class \code{optimal.cutpoints} as produced by \code{optimal.cutpoints()}
}
  \item{\dots}{
     further arguments passed to or from other methods. None are used in this method.
}
}
\details{
  The \code{summary.optimal.cutpoints} function produces a list of summary information for a fitted \code{optimal.cutpoints} object. The result depends on the three arguments, namely, \code{methods}, \code{categorical.cov} and \code{ci.fit} of the \code{optimal.cutpoints()} function used in the optimal cutpoints computing process.
}
\value{

Returns an object of class "summary.optimal.cutpoints" with the same components as the \code{optimal.cutpoints} function (see \code{\link{optimal.cutpoints}}) plus:
 
 	\item{p.table}{a list with all the numerical information to be shown on the screen.}     
}

\author{
  Monica Lopez-Raton and Maria Xose Rodriguez-Alvarez
}
\seealso{
 \code{\link{optimal.cutpoints}}
}
\examples{
library(OptimalCutpoints)
data(elas)
###########################################################
# Youden Index Method ("Youden"): Covariate gender
###########################################################
optimal.cutpoint.Youden<-optimal.cutpoints(X = "elas", status = "status", tag.healthy = 0, 
methods = "Youden", data = elas, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)

summary(optimal.cutpoint.Youden) 
}

