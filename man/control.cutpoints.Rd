\name{control.cutpoints}

\alias{control.cutpoints}

\title{Controlling the optimal-cutpoint selection process}

\description{Used to set various parameters controlling the optimal-cutpoint selection process}

\usage{
control.cutpoints(costs.ratio = 1, CFP = 1, CFN = 1,
  valueSp = 0.85, valueSe = 0.85, lowerValueSp = 0.45,
  upperValueSp = 0.85, lowerValueSe = 0.45, upperValueSe = 0.85,
  maxSp = TRUE,
  generalized.Youden = FALSE,
  costs.benefits.Youden = FALSE,
  costs.benefits.Efficiency = FALSE,
  weighted.Kappa = FALSE,
  standard.deviation.accuracy = FALSE,
  valueNPV = 0.85, valuePPV = 0.85,
  maxNPV = TRUE,
  valueDLR.Positive = 2,
  valueDLR.Negative = 0.5,
  adjusted.pvalue = c("PADJMS","PALT5","PALT10"),
  ci.SeSp = c("Exact","Quadratic","Wald","AgrestiCoull","RubinSchenker"),
  ci.PV = c("Exact","Quadratic","Wald","AgrestiCoull","RubinSchenker",
  "Transformed","NotTransformed","GartNam"),
  ci.DLR = c("Transformed","NotTransformed","GartNam"))
}

\arguments{
  \item{costs.ratio}{
     a numerical value meaningful only in the "CB" method. It specifies the costs ratio: \deqn{CR=\frac{C_{FP}-C_{TN}}{C_{FN}-C_{TP}}}
     where \eqn{C_{FP}}, \eqn{C_{TN}}, \eqn{C_{FN}} and \eqn{C_{TP}} are the costs of False Positive, True Negative, False Negative and True Positive decisions, respectively. The default value is 1.
} 
  \item{CFP}{
     a numerical value meaningful only in the "MCT", "Youden" and "MaxKappa" methods. It specifies the cost of a False Positive decision. The default value is 1.
}
  \item{CFN}{
     a numerical value meaningful only in the "MCT", "Youden" and "MaxKappa" methods. It specifies the cost of a False Negative decision. The default value is 1.
}
  \item{valueSp}{
     a numerical value meaningful only in the "MinValueSp", "ValueSp" and "MinValueSpSe" methods. It specifies the (minimum or specific) value set for Specificity. The default value is 0.85.
}   
  \item{valueSe}{
     a numerical value meaningful only in the "MinValueSe", "ValueSe" and "MinValueSpSe" methods. It specifies the (minimum or specific) value set for Sensitivity. The default value is 0.85.
}
  \item{lowerValueSp}{
     a numerical value meaningful only in the "RangeSp" method. It specifies the lower value for Specificity. The default value is 0.45.
}
  \item{upperValueSp}{
     a numerical value meaningful only in the "RangeSp" method. It specifies the upper value for Specificity. The default value is 0.85.
}
  \item{lowerValueSe}{
     a numerical value meaningful only in the "RangeSe" method. It specifies the lower value for Sensitivity. The default value is 0.45.
}
  \item{upperValueSe}{
    a numerical value meaningful only in the "RangeSe" method. It specifies the upper value for Sensitivity. The default value is 0.85.
}
  \item{maxSp}{
    a logical value meaningful only in the "MinValueSpSe" method, in a case where there is more than one cutpoint fulfilling the conditions. If TRUE, those of the cutpoints which yield maximum Specificity are computed. Otherwise the cutoff that yields maximum Sensitivity is computed. The default is TRUE.
}
  \item{generalized.Youden}{
     a logical value meaningful only in the "Youden" method. If TRUE, the Generalized Youden Index is computed. The default is FALSE.       
}
  \item{costs.benefits.Youden}{
     a logical value meaningful only in the "Youden" method. If TRUE, the optimal cutpoint based on cost-benefit methodology is computed. The default is FALSE. 
}
  \item{costs.benefits.Efficiency}{
     a logical value meaningful only in the "MaxEfficiency" method. If TRUE, the optimal cutpoint based on cost-benefit methodology is computed. The default is FALSE.       
}
  \item{weighted.Kappa}{
     a logical value meaningful only in the "MaxKappa" method. If TRUE, the Weighted Kappa Index is computed. The default is FALSE.      
}
  \item{standard.deviation.accuracy}{
     a logical value meaningful only in the "MaxEfficiency" method. If TRUE, standard deviation associated with accuracy (or efficiency) at the optimal cutpoint is computed. The default is FALSE.         
}
  \item{valueNPV}{
     a numerical value meaningful only in the "MinValueNPV" and "MinValueNPVPPV" methods. It specifies the minimum value set for Negative Predictive Value. The default value is 0.85.
}
  \item{valuePPV}{
     a numerical value meaningful only in the "MinValuePPV" and "MinValueNPVPPV" methods. It specifies the minimum value set for Positive Predictive Value. The default value is 0.85.
}
  \item{maxNPV}{
     a logical value meaningful only in the "MinValueNPVPPV" method, in a case where there is more than one cutpoint fulfilling the conditions. If TRUE, those of the cutpoints which yield the maximum Negative Predictive Value are computed. Otherwise the cutoff that yields the maximum Positive Predictive Value is computed. The default is TRUE.
}
  \item{valueDLR.Positive}{
     a numerical value meaningful only in the "ValueDLR.Positive" method. It specifies the value set for the Positive Diagnostic Likelihood Ratio. The default value is 2.
}
  \item{valueDLR.Negative}{
     a numerical value meaningful only in the "ValueDLR.Negative" method. It specifies the value set for the Negative Diagnostic Likelihood Ratio. The default value is 0.5.
}
  \item{adjusted.pvalue}{
     a character string meaningful only in the "MinPvalue" method. It specifies the method for adjusting the p-value, i.e., "PADJMS" for the Miller and Siegmund method, and "PALT5", "PALT10" for the Altman method (see details). The default is "PADJMS".
}
  \item{ci.SeSp}{
     a character string meaningful only when the argument ci.fit of the \code{optimal.cutpoints} function is TRUE. It indicates how the confidence interval for Sensitivity and Specificity measures is estimated. Options are "Exact" (Clopper and Pearson 1934), "Quadratic" (Fleiss 1981), "Wald" (Wald and Walfowitz 1939), "AgrestiCoull" (Agresti and Coull 1998) and "RubinSchenker" (Rubin and Schenker 1987) (see details). The default is "Exact".                                                                                                                                                                            
}
  \item{ci.PV}{
     a character string meaningful only when the argument ci.fit of the \code{optimal.cutpoints} function is TRUE. It indicates how the confidence interval for Predictive Values is estimated. Options are "Exact" (Clopper and Pearson 1934), "Quadratic" (Fleiss 1981), "Wald" (Wald and Walfowitz 1939), "AgrestiCoull" (Agresti and Coull 1998), "RubinSchenker" (Rubin and Schenker 1987), "Transformed" (Simel et al. 1991), "NotTransformed" (Koopman 1984) and "GartNam" (Gart and Nam 1988) (see details). The default is "Exact".
}
  \item{ci.DLR}{
     a character string meaningful only when the argument ci.fit of the function \code{optimal.cutpoints} is TRUE. It indicates how the confidence interval for Diagnostic Likelihood Ratios is estimated. Options are "Transformed" (Simel et al. 1991), "NotTransformed" (Koopman 1984) and "GartNam" (Gart and Nam 1988)(see details). The default is "Transformed".
}
}

\details{
  The value yielded by this function is used as the control argument of the \code{optimal.cutpoints()} function. 
  
  Several methods for correcting the increase in type-I error associated with the "MinPvalue" criterion have been proposed. In this package, two methods for adjusting the p-value have been implemented, i.e., the Miller and Siegmund (1982) and Altman (1994) methods. The first of these ("PADJMS" option) uses the minimum observed p-value (\eqn{pmin}) and the proportion (\eqn{\epsilon}) of sample data which is below the lowest (\eqn{\epsilon_{low}}) (or above the highest, \eqn{\epsilon_{high}}) cutpoint considered: 
  \deqn{p_{acor}=\phi(z)(z-\frac{1}{z})log\left(\frac{\epsilon_{high}(1-\epsilon_{low})}{(1-\epsilon_{high})\epsilon_{low}}\right)+4\frac{\phi(z)}{z}}
  where \eqn{z} is the \eqn{(1- pmin/2)} quantile of the standard normal distribution and \eqn{\phi} its corresponding density function. The second method is a simplification of the above formula, which considers specific values for \eqn{\epsilon}: with \eqn{\epsilon=\epsilon_{low} = \epsilon_{high}} = 5\% ("PALT5" option): \eqn{p_{alt5}=-3.13p_{min}\left(1+1.65ln(p_{min})\right)} with \eqn{\epsilon=\epsilon_{low} = \epsilon_{high}} = 10\% ("PALT10" option): \eqn{p_{alt10}=-1.63p_{min}\left(1+2.35ln(p_{min})\right)}. These approaches work well for low \eqn{pmin} values (0.0001<\eqn{pmin}<0.1) and are easy to apply.
  
  
  For inference performed on Sensitivity and Specificity measures (which are proportions), some of the most common confidence intervals have been considered. If \eqn{pr=x/n} is the proportion to be estimated and 1-\eqn{\alpha} is the confidence level, the options are as follows:                                                                                                                                                                                                                                        
    
  \code{"Exact"}: The exact confidence interval of Clopper and Pearson (1934) based on the exact distribution of a proportion:
  \deqn{\left[\frac{x}{(n-x+1)F_{\alpha/2,2(n-x+1),2x}+x}, \frac{(x+1)F_{\alpha/2,2(x+1),2(n-x)}}{(n-x)+(x+1)F_{\alpha/2,2(x+1),2(n-x)}}\right]} 
  where \eqn{F_{\alpha/2,a,b}} is the (1-\eqn{\alpha}/2) quantile of a Fisher-Snedecor distribution with \eqn{a} and \eqn{b} degrees of freedom.    
  
  \code{"Quadratic"}: Fleiss' quadratic confidence interval (Fleiss 1981). It is based on the asymptotic normality of the estimator of a proportion but adding a continuity correction. This approach is valid in a situation where \eqn{x} and \eqn{n-x} are greater than 5:                                                                 
  \deqn{\frac{1}{n+z^{2}_{1-\alpha/2}}\left[(x \mp 0.5)+\frac{z^{2}_{1-\alpha/2}}{2} \mp z_{1-\alpha/2}\sqrt{\frac{z^{2}_{1-\alpha/2}}{4}+\frac{(x \mp 0.5)(n-x \mp 0.5)}{n}}\right]}
  where \eqn{z_{1-\alpha/2}} is the (1-\eqn{\alpha}/2) quantile of the standard normal distribution.     
    
  \code{"Wald"}: Wald's confidence interval (Wald and Wolfowitz 1939) with a continuity correction. It is based on maximum-likelihood estimation of a proportion, and adds a continuity correction. This approach is valid where \eqn{x} and \eqn{n-x} are greater than 20:
  \deqn{\hat{pr} \mp z_{1-\alpha/2}\sqrt{\frac{\hat{pr}(1-\hat{pr})}{n}}+\frac{1}{2n}}    
   
  \code{"AgrestiCoull"}: The confidence interval proposed by Agresti and Coull (1998). It is a score confidence interval that does not use the standard calculation for the binomial proportion:
  \deqn{\frac{\hat{pr}+\frac{z^{2}_{1-\alpha/2}}{2n} \mp z_{1-\alpha/2}\sqrt{\frac{\hat{pr}(1-\hat{pr})+\frac{ z^{2}_{1-\alpha/2}}{4n}}{n}}} {1+\frac{ z^{2}_{1-\alpha/2}}{n}}} 
      
  \code{"RubinSchenker"}: Rubin and Schenker's logit confidence interval (1987). It uses logit transformation and Bayesian arguments with an a priori Jeffreys distribution.
  \deqn{logit\left[logit\left(\frac{x+0.5}{n+1}\right) \mp \frac{z_{1-\alpha/2}}{\sqrt{(n+1)\left(\frac{x+0.5}{n+1}\right)\left(1-\frac{x+0.5}{n+1}\right)}}\right]}
  where the \eqn{logit} function is \eqn{logit(q)=log\left(\frac{q}{1-q}\right)}.
  
  Since Diagnostic Likelihood Ratios represent a ratio between two probabilities, obtaining a confidence interval for them is less direct than it is for Sensitivity and Specificity. Let \eqn{pr_{1}=x_{1}/n_{1}} be the proportion in the numerator and \eqn{pr_{2}=x_{2}/n_{2}}, the proportion in the denominator. Based on the logarithmic transformation of the Likelihood Ratio (\code{"Transformed"} option), the 100(1-\eqn{\alpha})\% confidence interval is (Simel et al., 1991):
  \deqn{exp\left[ln\left(\frac{\widehat{pr}_{1}}{\widehat{pr}_{2}}\right) \mp z_{1-\alpha/2}\sqrt{\frac{1-\widehat{pr}_{1}}{n_{1}\widehat{pr}_{1}} +\frac{1-\widehat{pr}_{2}} {n_{2}\widehat{pr}_{2}}}\right]}
  These confidence intervals tend to perform better than do untransformed confidence intervals (Koopman 1984) (\code{"NotTransformed"} option) because the distribution of the Likelihood Ratios is asymmetric (Simel et al., 1991; Roldan Nofuentes and Luna del Castillo, 2007):
  \deqn{\frac{\widehat{pr}_{1}}{\widehat{pr}_{2}} \mp \sqrt{\frac{\widehat{pr}_{1}(1-\widehat{pr}_{1})}{n_{1}\widehat{pr}^{2}_{2}} +\frac{\widehat{pr}^{2}_{1}\widehat{pr}_{2}(1-\widehat{pr}_{2})}{n_{2}\widehat{pr}^{4}_{2}}}}
  
  Another confidence interval (\code{"GartNam"} option) is based on the calculation of the interval for the ratio between two independent proportions (Gart and Nam, 1988). The following quadratic equation must be solved:
  \deqn{\frac{\left(\widehat{pr}_{1}-\frac{pr_{1}}{pr_{2}}\widehat{pr}_{2}\right)^{2}}{\frac{\widehat{pr}_{1}(1-\widehat{pr}_{1}}{n_{1}} +\frac{\left(\frac{pr_{1}}{pr_{2}}\right)^{2}\widehat{pr}_{2}(1-\widehat{pr}_{2})}{n_{2}}}  =z^{2}_{1-\alpha/2}}
    
  Inference of the Predictive Values depends on the type of study, i.e., whether cross-sectional(prevalence can be estimated on the basis of the sample) or case-control. In the former case, the approaches for computing the confidence intervals of the Predictive Values are exactly the same as for the Sensitivity and Specificity measures. However, in a case control study, where prevalence is not estimated from the sample, the confidence intervals are based on the intervals of the Likelihood Ratios. Hence, once a prevalence estimator \eqn{\hat{p}} is computed and substituting each limit of these intervals into the expressions 
  \deqn{\left(1+\frac{1-\hat{p}}{\hat{p}\widehat{DLR}^{+}}\right)^{-1}} and 
  \deqn{\left(1+\frac{\hat{p}}{1-\hat{p}}\widehat{DLR}^{-}\right)^{-1}} confidence intervals for the Positive and Negative Predictive Values are obtained, where \eqn{DLR+} and \eqn{DLR-} are the Positive and Negative Diagnostic Likelihood Ratios, respectively.                                                                                                                                                                                                                                                                                                                                                                                            

}

\references{
  Agresti, A. and Coull, B.A. (1998). Approximate is better than "exact" for interval estimation of binomial proportions. \emph{The American Statistician} \bold{52}, 119--126.
   
  Altman, D.G., Lausen, B., Sauerbrei, W. and Schumacher, M. (1994). Dangers of using "optimal" cutpoints in the evaluation of prognostic factors. \emph{Journal of the National Cancer Institute} \bold{86}(11), 829--835.
  
  Clopper, C. and Pearson, E.S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. \emph{Biometrika} \bold{26}, 404--413.
  
  Fleiss, J.L. (1981). Statistical methods for rates and proportions. John Wiley & Sons, New York.
  
  Gart, J.J. and Nam, J. (1998). Aproximate interval estimation of the ratio of binomial parameters: a review and corrections for skewness. \emph{Biometrics} \bold{44}, 323--338.

  Koopman PAR (1984). Confidence limits for the ratio of two binomial proportions. \emph{Biometrics} \bold{40}, 513--517.
  
  Miller, R. and Siegmund, D. (1982). Maximally selected chi square statistics. \emph{Biometrics} \bold{38}, 1011--1016.
  
  Roldan Nofuentes, J.A. and Luna del Castillo, J.D. (2007). Comparing of the likelihood ratios of two binary diagnostic tests in paired designs. \emph{Statistics in Medicine} \bold{26}, 4179--4201.
  
  Rubin, D.B. and Schenker, N. (1987). Logit-based interval estimation for binomial data using the Jeffreys prior. \emph{Sociological Methodology} \bold{17}, 131--144.

  Simel, D.L., Samsa, G.P. and Matchar, D.B. (1991). Likelihood ratios with confidence: sample size estimation for diagnostic test studies. \emph{Journal of Clinical Epidemiology} \bold{44}(8), 763--770.

  Wald A, Wolfowitz J (1939). Confidence limits for continuous distribution functions. \emph{The Annals of Mathematical Statistics} \bold{10} 105--118.
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
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, 
marker = "elas", status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = 
"gender", control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)

summary(optimal.cutpoint.Youden)

# Change the method for computing the confidence interval 
# of Sensitivity and Specificity measures
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, marker = "elas", 
status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(ci.SeSp = "AgrestiCoull"), ci.fit = TRUE, conf.level = 0.95, 
trace = TRUE)

summary(optimal.cutpoint.Youden)

# Compute the Generalized Youden Index
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, marker = "elas", 
status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(generalized.Youden = TRUE), ci.fit = TRUE, conf.level = 0.95, 
trace = TRUE)

summary(optimal.cutpoint.Youden)
                                 
}      
