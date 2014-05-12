\name{OptimalCutpoints-package}
\alias{OptimalCutpoints-package}
\alias{OptimalCutpoints}
\docType{package}
\title{
Computing Optimal Cutpoints in diagnostic tests
}
\description{
Continuous biomarkers or diagnostic tests are often used to discriminate between diseased and healthy populations. In clinical practice, it is necessary to select a cutpoint or discrimination value c which defines the positive and negative test results. 
Several methods for selecting optimal cutpoints in diagnostic tests have been proposed in the literature depending on the underlying reason for this choice. This package allows the user to compute the optimal cutpoint for a diagnostic test or continuous marker. Various approaches for selecting optimal cutoffs have been implemented, including methods based on cost-benefit analysis and diagnostic test accuracy measures (Sensitivity/Specificity, Predictive Values and Diagnostic Likelihood Ratios) or prevalence. Numerical and graphical output for all methods is easily obtained.
}
\details{
\tabular{ll}{
Package: \tab OptimalCutpoints\cr
Type: \tab Package\cr
Version: \tab 1.1-2\cr
Date: \tab 2014-05-12\cr
License: \tab GPL\cr
}
In the OptimalCutpoints package all these methods have been incorporated in a way designed to be clear and user-friendly for the end-user. For all methods, the optimal cutoff value obtained is always one of the values of the diagnostic marker, and the Receiver Operating Characteristic (ROC) and Predictive ROC (PROC) curves and accuracy measures are empirically estimated.
The program only requires a data frame, which can be built from a data-entry file or from something else (a database, direct entry, predictions from another function,...), which must, at minimum, contain the following variables: diagnostic marker; disease status (diseased/healthy); and whether adjustment is to be made for any (categorical) covariate of interest, a variable that indicates the levels of this covariate. 
A standard-type data input structure is used, with each row of the database indicating a patient/case and each column referring to a variable. 

The most important functions in the package are the \code{optimal.cutpoints()}, \code{control.cutpoints()}, \code{summary.optimal.cutpoints()} and \code{plot.optimal.cutpoints()} functions.  
The \code{optimal.cutpoints()} function computes the optimal cutpoint(s) with its accuracy measures, according to the criterion selected. More than one criterion can be chosen for selecting the optimal cutpoint.
The \code{control.cutpoints()} function is used to set several parameters that are specific of each method, such as the cost values or the minimum values for diagnostic accuracy measures.
The \code{summary.optimal.cutpoints()} and \code{plot.optimal.cutpoints()} functions produce numerical and graphical output, respectively. 
Numerical output includes information relating to: the optimal cutpoint; the method used for selecting the optimal value, together with the number of optimal cutpoints (in some cases there may be more than one value); and the optimal cutoff(s) and its/their accuracy-measure estimates. 
Graphical output includes the plots of the ROC and PROC curves, indicating the optimal cutpoint on these plots.
}

\author{
Monica Lopez-Raton and Maria Xose Rodriguez-Alvarez

Maintainer: Monica Lopez-Raton <monica.lopez.raton@usc.es>  
}

