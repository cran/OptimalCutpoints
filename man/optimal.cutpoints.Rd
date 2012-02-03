\name{optimal.cutpoints}
\alias{optimal.cutpoints}

\title{
Computing Optimal Cutpoints in diagnostic tests
}
\description{
  optimal.cutpoints calculates optimal cutpoints in diagnostic tests. Several methods or criteria for selecting optimal cutoffs have been implemented, including methods based on cost-benefit analysis and diagnostic test accuracy measures
(Sensitivity/Specificity, Predictive Values and Diagnostic Likelihood Ratios) or prevalence.
}
\usage{
optimal.cutpoints(methods, data, marker, status, tag.healthy,
categorical.cov = NULL, pop.prev = NULL, control = control.cutpoints(), 
ci.fit = FALSE, conf.level = 0.95, trace = TRUE)
}

\arguments{
  \item{methods}{
   a character vector selecting the method(s) to be used: "CB" (cost-benefit method); "MCT" (minimizes Misclassification Cost Term); "MinValueSp" (a minimum value set for Specificity);
  "MinValueSe" (a minimum value set for Sensitivity); "RangeSp" (a range of values set for Specificity); "RangeSe" (a range of values set for Sensitivity); "ValueSp" (a value set for Specificity);
  "ValueSe" (a value set for Sensitivity); "MinValueSpSe" (a minimum value set for Specificity and Sensitivity); "MaxSp" (maximizes Specificity); "MaxSe" (maximizes Sensitivity);
  "MaxSpSe" (maximizes Sensitivity and Specificity simultaneously); "MaxSumSpSe" (maximizes the sum of Sensitivity and Specificity); "MaxProdSpSe" (maximizes the product of Sensitivity and Specificity); "ROC01" (minimizes distance between ROC plot and point (0,1));
  "SpEqualSe" (Sensitivity = Specificity); "Youden" (Youden Index); "MaxEfficiency" (maximizes Efficiency or Accuracy); "Minimax" (minimizes the most frequent error);
  "AUC" (maximizes concordance which is a function of AUC); "MaxDOR" (maximizes Diagnostic Odds Ratio); "MaxKappa" (maximizes Kappa Index); 
  "MaxAccuracyArea" (maximizes Accuracy Area); "MinErrorRate" (minimizes Error Rate); "MinValueNPV" (a minimum value set for Negative Predictive Value); "MinValuePPV" (a minimum value set for Positive Predictive Value);
  "MinValueNPVPPV" (a minimum value set for Predictive Values); "PROC01" (minimizes distance between PROC plot and point (0,1)); "NPVEqualPPV" (Negative Predictive Value = Positive Predictive Value); "ValueDLR.Negative" (a value set for Negative Diagnostic Likelihood Ratio);
  "ValueDLR.Positive" (a value set for Positive Diagnostic Likelihood Ratio); "MinPvalue" (minimizes p-value associated with the statistical Chi-squared test which measures the association between the marker and the binary result obtained on using the cutpoint); "ObservedPrev" (The closest value to observed prevalence); "MeanPrev" (The closest value to the mean of the diagnostic test values); or "PrevalenceMatching" (The value for which predicted prevalence is practically equal to observed prevalence).
}
  \item{data}{
     a data frame containing all needed variables.
}
  \item{marker}{
     a character string with the name of the diagnostic test variable.
}
  \item{status}{
     a character string with the name of the variable that distinguishes healthy from diseased individuals.
}
  \item{tag.healthy}{
     the value codifying healthy individuals in the \code{status} variable .
}
  \item{categorical.cov}{
     a character string with the name of the categorical covariate according to which optimal cutpoints are to be calculated. The default is NULL (no categorical covariate).
}
  \item{pop.prev}{
     the value of the disease's prevalence. The default is NULL (prevalence is estimated on the basis of sample prevalence). It can be a vector indicating
     the prevalence values for each categorical covariate level.
}
  \item{control}{
     output of the \code{\link{control.cutpoints}} function.
}
  \item{ci.fit}{
     a logical value. If TRUE, inference is performed on the accuracy measures at the optimal cutpoint. The default is FALSE.
}
  \item{conf.level}{
    a numerical value with the confidence level for the construction of the confidence intervals. The default value is 0.95.
}
  \item{trace}{
    a logical value. If TRUE, information on progress is shown. The default is TRUE.
}
}
\details{

  Continuous biomarkers or diagnostic tests are often used to discriminate between diseased and healthy populations. In clinical practice, it is necessary to select a cutpoint or discrimination value c which defines positive and negative test results. Several methods for selecting optimal cutpoints in diagnostic tests have been proposed in the literature depending on the underlying reason for this choice. In this package, thirty-five criteria are available.
  Before describing the methods in detail, mention should be made of the following notation: \eqn{C_{FP}}, \eqn{C_{TN}}, \eqn{C_{FN}} and \eqn{C_{TP}} are the costs of False Positive, True Negative, False Negative and True Positive decisions, respectively; \eqn{p} is disease prevalence; \eqn{Se} is Sensitivity; and \eqn{Sp} is Specificity. 
   
  \code{"CB"}: Criterion based on cost-benefit methodology by means of calculating the slope of the ROC curve at the optimal cutoff as \deqn{ S=\frac{1-p}{p}CR=\frac{1-p}{p}\frac{C_{FP}-C_{TN}}{C_{FN}-C_{TP}}} (McNeill et al. 1975; Metz et al. 1975; Metz 1978). This method thus weighs the relative costs of the different predictions in the diagnosis. By default, the costs ratio is 1, and this is the \code{costs.ratio} argument in the \code{control.cutpoints} function.
  
  \code{"MCT"}: Criterion based on the minimization of the Misclassification Cost Term (MCT) defined as \deqn{MCT(c)=\frac{C_{FN}}{C_{FP}}p(1-Se(c))+(1-p)(1-Sp(c))}(Smith 1991; Greiner 1995,1996). By default, \eqn{C_{FN}=C_{FP} =} 1, and these are the \code{CFN} and \code{CFP} arguments in the \code{control.cutpoints} function.

  \code{"MinValueSp"}: Criterion based on setting a minimum value for Specificity and maximizing Sensitivity, subject to this condition (Shaefer 1989; Vermont et al. 1991; Gallop et al. 2003). Hence, in a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Sensitivity are chosen. If several cutpoints still remain, those yielding the greatest Specificity are chosen. By default, the minimum value for Specificity is 0.85, and this is the \code{valueSp} argument in the \code{control.cutpoints} function.    
     
  \code{"MinValueSe"}: Criterion based on setting a minimum value for Sensitivity and maximizing Specificity, subject to this condition (Shaefer 1989; Vermont et al. 1991; Gallop et al. 2003). Hence, in a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Specificity are chosen. If several cutpoints still remain, those yielding the greatest Sensitivity are chosen. By default, the minimum value for Sensitivity is 0.85, and this is the \code{valueSe} argument in the \code{control.cutpoints} function.

  \code{"RangeSp"}: Criterion based on setting a range of values for Specificity (Navarro et al. 1998). In a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Sensitivity are chosen. If several cutpoints still remain, those yielding the greatest Specificity are chosen. By default, the range of values for Specificity is (0.45-0.85) and these are the \code{lowerValueSp} and \code{upperValueSp} arguments in the \code{control.cutpoints} function.
       
  \code{"RangeSe"}: Criterion based on setting a range of values for Sensitivity (Navarro et al. 1998). In a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Specificity are chosen. If several cutpoints still remain, those yielding the greatest Sensitivity are chosen. By default, the range of values for Sensitivity is (0.45-0.85) and these are the \code{lowerValueSe} and \code{upperValueSe} the arguments in the \code{control.cutpoints} function.

  \code{"ValueSp"}: Criterion based on setting a particular value for Specificity (Rutter and Miglioretti 2003). In a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Sensitivity are chosen.
  
  \code{"ValueSe"}: Criterion based on setting a particular value for Sensitivity (Rutter and Miglioretti 2003). In a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Specificity are chosen.

  \code{"MinValueSpSe"}: Criterion based on setting minimum values for Sensitivity and Specificity measures (Shaefer 1989). In a case where there is more than one cutpoint fulfilling these conditions, those which yield maximum Sensitivity or maximum Specificity are chosen. The user can select one of these two options by means of the \code{maxSp} argument in the \code{control.cutpoints} function. If TRUE (the default value), the cutpoint/s yielding maximum Specificity is/are computed. If there are still several cutpoints which maximize the chosen measure, those which also maximize the other measure are chosen. 

  \code{"MaxSp"}: Criterion based on maximization of Specificity (Bortheiry et al. 1994; Hoffman et al. 2000; Alvarez-Garcia et al. 2003). If there is more than one cutpoint fulfilling this condition, those which yield maximum Sensitivity are chosen.
    
  \code{"MaxSe"}: Criterion based on maximization of Sensitivity (Filella et al. 1995; Hoffman et al. 2000; Alvarez-Garcia et al. 2003). If there is more than one cutpoint fulfilling this condition, those which yield maximum Specificity are chosen.

  \code{"MaxSpSe"}: Criterion based on simultaneously maximizing Sensitivity and Specificity (Riddle and Stratford 1999; Gallop et al. 2003).
  
  \code{"MaxSumSpSe"}: Criterion based on maximizing the sum of Specificity and Sensitivity (Albert and Harris 1987; Zweig and Campbell 1993). 
  
  \code{"MaxProdSpSe"}: Criterion based on maximizing the product of Specificity and Sensitivity (Lewis et al. 2008).
  
  \code{"ROC01"}: Criterion of the point on the ROC curve closest to the point (0,1), i.e, upper left corner of the unit square (Metz 1978; Vermont et al. 1991).
  
  \code{"SpEqualSe"}: Criterion based on the equality of Sensitivity and Specificity (Greiner et al. 1995; Hosmer and Lemeshow 2000; Peng and So 2002). Since Specificity may not be exactly equal to Sensitivity, the absolute value of the difference between them is minimized.
  
  \code{"Youden"}: Criterion based on Youden's Index (Youden 1950; Aoki et al. 1997; Shapiro 1999; Greiner et al. 2000)defined as \eqn{YI(c)=max_{c}(Se(c)+Sp(c)-1)}. Costs of misclassifications can be considered in this criterion and for using the Generalized Youden Index: \eqn{GYI(c)=max_{c}(Se(c)+rSp(c)-1} (Geisser 1998; Greiner et al. 2000; Schisterman et al. 2005), where \deqn{r=\frac{1-p}{p}\frac{C_{FN}}{C_{FP}}}. If the \code{generalized.Youden} argument in the \code{control.cutpoints} function is TRUE, Generalized Youden Index is computed. The default is FALSE. The \code{CFN} and \code{CFP} arguments in the \code{control.cutpoints} function indicate the cost values, and by default, \eqn{C_{FN}=C_{FP}=}1.
  Moreover, the optimal cutpoint based on Youden's Index can be computed by means of cost-benefit methodology (see "CB" method), with the slope of the ROC curve at the optimal cutoff being \eqn{S=1} for the Youden Index and \eqn{S=\frac{1-p}{p}\frac{C_{FN}}{C_{FP}}} for the Generalized Youden Index. If the \code{costs.benefits.Youden} argument in the \code{control.cutpoints} function is TRUE, the optimal cutpoint based on cost-benefit methodology is computed. By default, it is FALSE.  
  
  \code{"MaxEfficiency"}: Criterion based on maximization of the Efficiency, Accuracy, Validity Index or percentage of cases correctly classified defined as \eqn{Ef(c)=pSe(c)+(1-p)Sp(c)} (Feinstein 1975; Galen 1986; Greiner 1995, 1996).
  Moreover, the optimal cutpoint based on this method can be computed by means of cost-benefit methodology (see "CB" method), with the slope of the ROC curve at the optimal cutoff being \eqn{S=\frac{1-p}{p}}. If the \code{costs.benefits.Efficiency} argument in the \code{control.cutpoints} function is TRUE, the optimal cut-point based on cost-benefit methodology is computed. By default, it is FALSE.  
    
  \code{"Minimax"}: Criterion based on minimization of the most frequent error (Hand 1987): \eqn{min_{c}(max(p(1-Se(c)),(1-p)(1-Sp(c))))}. In a case where there is more than one cutpoint fulfilling this condition, those which yield maximum Sensitivity or maximum Specificity are chosen. The user can select one of these two options by means of the \code{maxSp} argument in the \code{control.cutpoints} function. If TRUE (the default value), the cutpoint/s yielding maximum Specificity is/are computed. If there are still several cutpoints which maximize the chosen measure, those which also maximize the other measure are chosen. 
     
  \code{"AUC"}:  Criterion based on maximizating the concordance (monotone function of the Area Under ROC Curve, AUC) defined as \deqn{AUC(c)=\frac{Se(c)-(1-Sp(c))+1}{2}} (Begg et al. 2000; Goenen and Sima 2008).

  \code{"MaxDOR"}: Criterion based on maximizating the Diagnostic Odds Ratio (DOR), defined as \deqn{DOR(c)=\frac{Se(c)}{(1-Se(c))}\frac{Sp(c)}{(1-Sp(c))}} (Kraemer 1992; Greiner et al. 2000; Boehning et al. 2011).

  \code{"MaxKappa"}: Criterion based on maximization of the Kappa Index (Cohen 1960; Greiner et al. 2000). Kappa makes full use of the information in the confusion matrix to assess the improvement over chance prediction. Costs of misclassifications can be considered in this criterion and for using the Weighted Kappa Index (Kraemer 1992; Kraemer et al. 2002) defined as 
  \deqn{PK(c)=\frac{p(1-p)(Se(c)+Sp(c)-1)}{p(p(1-Se(c))+(1-p)Sp(c))r+(1-p)(pSe(c)+(1-p)(1-Sp(c)))(1-r)}}
  where \deqn{r=\frac{C_{FP}}{ C_{FP}+ C_{FN}}}. If the \code{weighted.Kappa} argument in the \code{control.cutpoints} function is TRUE, the Weighted Kappa Index is computed. The default value is FALSE. The \code{CFN} and \code{CFP} arguments in the \code{control.cutpoints} function indicate the cost values, and by default, \eqn{C_{FP}=C_{FN}}= 1.
                                                                                                                                                             
  \code{"MaxAccuracyArea"}: Criterion based on maximization of the Accuracy Area (Greiner 1995, 1996) \deqn{AA(c)=Se(c)Sp(c)} where \eqn{TP}, \eqn{TN}, \eqn{FN} and \eqn{FP} are the number of True Positives, True Negatives, False Negatives and False Positives classifications, respectively.  
  
  \code{"MinErrorRate"}: Criterion based on maximization of the Misclassification Rate which measures the error in cases where diseased and disease-free patients are misdiagnosed (Metz 1978). It is defined as \eqn{ER(c)= p(1-Se(c))+(1-p)(1-Sp(c))}.
   
  \code{"MinValueNPV"}: Criterion based on setting a minimum value for Negative Predictive Value (Vermont et al. 1991). In a case where there is more than one cutpoint fulfilling this condition, those which yield the maximum Positive Predictive Value are chosen. If several cutpoints still remain, those yielding the highest Negative Predictive Value are chosen. By default, the minimum value for Negative Predictive Value is 0.85 and this is the \code{valueNPV} argument in the \code{control.cutpoints()} function. 
  
  \code{"MinValuePPV"}: Criterion based on setting a minimum value for Positive Predictive Value (Vermont et al. 1991). In a case where there is more than one cutpoint fulfilling this condition, those which yield the maximum Negative Predictive Value are chosen. If several cutpoints still remain, those yielding the highest Positive Predictive Value are chosen. By default, the minimum value for Positive Predictive Value is 0.85, and this is specified by the \code{valuePPV} argument in the \code{control.cutpoints()} function.

  \code{"MinValueNPVPPV"}: Criterion based on setting minimum values for Predictive Values (Vermont et al. 1991). In a case where there is more than one cutpoint fulfilling these conditions, those which yield the maximum Negative or maximum Positive Predictive Value are chosen. The user can select one of these two options by means of the \code{maxNPV} argument in the \code{control.cutpoints} function. If TRUE (the default value), the cutpoint/s yielding maximum Negative Predictive Value is/are computed. If there are still several cutpoints which maximize the chosen measure, those which also maximize the other measure are chosen.
    
  \code{"PROC01"}: Criterion of the point on the PROC curve closest to the point (0,1), i.e., upper left corner of the unit square (Vermont et al. 1991; Gallop et al. 2003).
  
  \code{"NPVEqualPPV"}: Criterion based on the equality of Predictive Values (Vermont et al. 1991). Since the Positive Predictive Value may not be exactly equal to the Negative Predictive Value, the absolute value of the difference between them is minimized.

  \code{"ValueDLR.Negative"}: Criterion based on setting a particular value for the Negative Diagnostic Likelihood Ratio (Boyko 1994; Rutter and Miglioretti 2003). The default value is 0.5, and it is specified by the \code{valueDLR.Negative} argument in the \code{control.cutpoints} function.

  \code{"ValueDLR.Positive"}: Criterion based on setting a particular value for the Positive Diagnostic Likelihood Ratio (Boyko 1994; Rutter and Miglioretti 2003). The default value is 2, and it is specified by the \code{valueDLR.Positive} argument in the \code{control.cutpoints} function.
  
  \code{"MinPvalue"}: Criterion based on the minimum p-value associated with the statistical Chi-squared test which measures the association between the marker and the binary result obtained on using the cutpoint (Miller and Siegmund 1982; Lausen and Schumacher 1992; Altman et al. 1994; Mazumdar and Glasman 2000). 
    
  \code{"ObservedPrev"}: Criterion based on setting the closest value to observed prevalence, i.e., \eqn{c/max_{c}{|c-p|}}, with p being prevalence estimated from the sample. This criterion is thus indicated/valid in cases where the diagnostic test takes values in the interval (0,1), and it is a useful method in cases where preserving prevalence is of prime importance (Manel et al. 2001).
                                                                                                                                          
  \code{"MeanPrev"}: Criterion based on setting the closest value to the mean of the diagnostic test values. This criterion is usually used in cases where the diagnostic test takes values in the interval (0,1), i.e., the mean probability of ocurrence, e.g., based on the results of a statistical model(Manel et al. 2001; Kelly et al. 2008).
                                                                                                                                                                                 
  \code{"PrevalenceMatching"}: Criterion based on the equality of sample and predicted prevalence: \eqn{pSe(c)+(1-p)(1-Sp(c))} where \eqn{p} is the prevalence estimated from the sample (Manel et al. 2001; Kelly et al. 2008). This criterion is usually used in cases where the diagnostic test takes values in the interval (0,1), i.e., the predicted probability, e.g., based on a statistical model.                                                                                                                                                                                                                                 
  
}                 

\value{                                                                                                                                                                                                                                     
Returns an object of class "optimal.cutpoints" with the following components:

  \item{methods}{a character vector with the value of the \code{methods} argument used in the call.}    
  \item{levels.cat}{a character vector indicating the levels of the categorical covariate if the \code{categorical.cov} argument in the \code{optimal.cutpoints} function is not NULL.}
  \item{call}{the matched call.}
  \item{data}{the data frame with the variables used in the call.}
  
  For each of the methods used in the call, a list with the following components is obtained:  
                
        \item{"measures.acc"}{a list with all possible cutoffs, their associated accuracy measures (Sensitivity, Specificity, Predictive Values, Diagnostic Likelihood Ratios and Area under ROC Curve, AUC),
        the prevalence and the sample size for both healthy and diseased populations.}
             
        \item{"optimal.cutoff"}{a list with the optimal cutoff(s) and its/their associated accuracy measures (Sensitivity, Specificity, Predictive Values, Diagnostic Likelihood Ratios and the number of False Positive and False Negative decisions).}
        
        The following components only appear in some methods:
        \item{"criterion"}{the value of the method considered for selecting the optimal cutpoint for each cutoff.}
        \item{"optimal.criterion"}{the optimal value of the method considered for selecting the optimal cutpoint, i.e., the value of the criterion at the optimal cutpoint.}
}

\references{
 Albert, A. and Harris, E.K. (1987). Multivariate Interpretation of Clinical Laboratory Data. Marcel Dekker, New York, NY.

  Altman, D.G., Lausen, B., Sauerbrei, W. and Schumacher, M. (1994). Dangers of using "optimal" cutpoints in the evaluation of prognostic factors. \emph{Journal of the National Cancer Institute} \bold{86} (11), 829--835.

  Alvarez-Garcia, G. et al. (2003). Influence of age and purpose for testing on the cut-off selection of serological methods in bovine neosporosis. \emph{Veterinary Research} \bold{34}, 341--352.

  Aoki, K., Misumi, J., Kimura, T., Zhao, W. and Xie, T. (1997). Evaluation of cutoff levels for screening of gastric cancer using serum pepsinogens and distributions of levels of serum pepsinogens I, II and Of PG I/PG II ratios in a gastric cancer case-control study. \emph{Journal of Epidemiology} \bold{7}, 143--151.

  Begg, C.B., Cramer, L.D., Venkatraman, E.S. and Rosai, J. (2000). Comparing tumour staging and grading systems: a case study and a review of the issues, using thymoma as a model. \emph{Statistics in Medicine} \bold{19}, 1997--2014.
  
  Boehning, D., Holling, H. and Patilea, V. (2011). A limitation of the diagnostic-odds ratio in determining an optimal cut-off value for a continuous diagnostic test. \emph{Statistical Methods in Medical Research}, \bold{20}(5), 541--550.

  Bortheiry, A.L., Malerbi, D.A. and Franco, L.J. (1994). The ROC curve in the evaluation of fasting capillary blood glucose as a screening test for diabetes y IGT. \emph{Diabetes Care} \bold{17}, 1269--1272.

  Boyko, E.J. (1994). Ruling out or ruling in disease with the most sensitive or specific diagnostic test: short cut or wrong turn?. \emph{Medical Decision Making} \bold{14}, 175--179.
  
  Cohen, J. (1960). A coefficient of agreement for nominal scales. \emph{Educ Psychol Meas} \bold{20}, 37--46.

  Feinstein, S.H. (1975). The accuracy of diver sound localization by pointing. \emph{Undersea. Biomed.Res} \bold{2}(3), 173--184.

  Filella, X., Alcover, J., Molina, R. et al. (1995). Clinical usefulness of free PSA fraction as an indicator of prostate cancer. \emph{Int. J. Cancer} \bold{63}, 780--784.

  Galen, R.S. (1986). Use of predictive value theory in clinical immunology. In: N.R.Rose, H. Friedmann and J.L. Fahey (Eds.), Manual of Clinical Laboratory Immunology. American Society of Microbiology. Washington, DC, pp 966-970.

  Gallop, R.J., Crits-Christoph, P., Muenz, L.R. and Tu, X.M. (2003). Determination and Interpretation of the Optimal Operating Point for ROC Curves Derived Through Generalized Linear Models. \emph{Understanding Statistics} \bold{2}(4), 219--242.

  Geisser, S. (1998). Comparing two tests used for diagnostic or screening processes. \emph{Statistics Probability Letters} \bold{40}, 113--119.

  Goenen, M. and Sima, C. (2008). Optimal cutpoint estimation with censored data. Memorial Sloan-Kettering Cancer Center Department of Epidemiology and Biostatistics Working Paper Series.
  
  Greiner, M. (1995). Two-graph receiver operating characteristic (TG-ROC): a Microsoft-EXCEL template for the selection of cut-off values in diagnostic tests. \emph{Journal of Immunological Methods} \bold{185}(1),145--146.

  Greiner, M. (1996). Two-graph receiver operating characteristic (TG-ROC): update version supports optimisation of cut-off values that minimise overall misclassification costs. \emph{J. Immunol. Methods} \bold{191}, 93--94.

  Greiner, M., Pfeiffer, D. and Smith, R.D. (2000). Principals and practical application of the receiver operating characteristic analysis for diagnostic tests. \emph{Preventive Veterinary Medicine} \bold{45}, 23--41.

  Hand, D. (1987). Screening vs Prevalence Estimation. \emph{Applied Statistics} \bold{36}, 1--7.
  
  Hoffman, R.M., Clanon, D.L., Littenberg, B., Frank, J.J. and Peirce, J.C. (2000). Using the Free-to-total Prostate-specific Antigen Ratio to Detect Prostate Cancer in Men with Nonspecific Elevations of Prostate-specific Antigen Levels. \emph{J. Gen. Intern Med} \bold{15}, 739--748.

  Hosmer, D.W. and Lemeshow, S. (2000). Applied Logistic Regression. Wiley-Interscience, New York, USA.
  
  Kelly, M.J., Dunstan, F.D., Lloyd, K. and Fone, D.L. (2008). Evaluating cutpoints for the MHI-5 and MCS using the GHQ-12: a comparison of five different methods. \emph{BMC Psychiatry} \bold{8}, 10.

  Kraemer, H.C. (1992). Risk ratios, odds ratio, and the test QROC. In: Evaluating medical tests. Newbury Park, CA: SAGE Publications, Inc.; pp 103--113.
  
  Kraemer, H.C., Periyakoil, V.S. and Noda, A. (2002). Kappa coefficients in medical research. \emph{Statistics in Medicine} \bold{21}, 2109--2129.
  
  Lausen, B. and Schumacher, M. (1992). Maximally selected rank statistics. \emph{Biometrics} \bold{48}, 73--85.

  Lewis, J.D., Chuai, S., Nessel, L., Lichtenstein, G.R., Aberra, F.N. and Ellenberg, J.H. (2008). Use of the Noninvasive Components of the Mayo Score to Assess Clinical Response in Ulcerative Colitis. \emph{Inflamm Bowel Dis} \bold{14}(12), 1660--1666.

  Manel, S., Williams, H. and Ormerod, S. (2001). Evaluating Presence-Absence Models in Ecology: the Need to Account for Prevalence. \emph{Journal of Applied Ecology} \bold{38}, 921--931.
  
  Mazumdar, M. and Glassman, J.R. (2000). Categorizing a prognostic variable: review of methods, code for easy implementation and applications to decision-making about cancer treatments. \emph{Statistics in Medicine} \bold{19}, 113--132.

  McNeill, B.J., Keeler, E. and Adelstein, S.J. (1975). Primer on certain elements of medical decision making, with comments on analysis ROC. \emph{N. Engl. J Med} \bold{293}, 211--215.

  Metz, C.E., Starr, S.J., Lusted, L.B. and Rossmann, K. (1975). Progress in evaluation of human observer visual detection performance using the ROC curve approach. In: Raynaud C, Todd-Pokropek AE eds. Information processing in scintigraphy. Orsay, France: CEA, 420--436.

  Metz, CE. (1978). Basic principles of ROC analysis. \emph{Seminars Nucl. Med.} \bold{8}, 283--298.
  
  Miller, R. and Siegmund, D. (1982). Maximally selected chi square statistics. \emph{Biometrics} \bold{38}, 1011--1016.
  
  Navarro, J.B., Domenech, J.M., de la Osa, N. and Ezpeleta, L. (1998). El analisis de curvas ROC en estudios epidemiologicos de psicopatologia infantil: aplicacion al cuestionario CBCL. \emph{Anuario de Psicologia} \bold{29} (1), 3--15.

  Peng, C.Y.J. and So, T.S.H. (2002). Logistic Regression Analysis and Reporting: A Primer. \emph{Understanding Statistics} \bold{1}(1), 31--70.
  
  Riddle, D.L. and Stratford, P.W. (1999). Interpreting validity indexes for diagnostic tests: an illustration using the Berg Balance Test. \emph{Physical Therapy} \bold{79}, 939--950.
  
  Rutter, C.M. and Miglioretti, D.L. (2003). Estimating the accuracy of psychological scales using longitudinal data. \emph{Biostatistics} \bold{4}(1), 97--107.
  
  Shaefer, H. (1989). Constructing a cut-off point for a quantitative diagnostic test. \emph{Statistics in Medicine} \bold{8}, 1381--1391.
  
  Schisterman, E.F., Perkins, N.J., Liu, A. and Bondell, H. (2005). Optimal cutpoint and its corresponding Youden index to discriminate individuals using pooled blood samples. \emph{Epidemiology} \bold{16}, 73--81.
  
  Shapiro, D.E. (1999). The interpretation of diagnostic tests. \emph{Statistical Methods in Medical Research} \bold{8}, 113--134.
  
  Smith, R.D. (1991). Evaluation of diagnostic tests. In: R.D. Smith (Ed.), Veterinary Clinical Epidemiology. Butter-worth-Heinemann. Stoneham, pp 29--43.
  
  Vermont J, Bosson JL, Francois P, Robert C, Rueff A, Demongeot J. (1991). Strategies for graphical threshold determination. \emph{Computer Methods and Programs in Biomedicine} \bold{35}, 141--150.
  
  Youden, W.J. (1950). Index for rating diagnostic tests. \emph{Cancer} \bold{3}, 32--35.
  
  Zweig, M.H., Campbell, G. (1993). Receiver-operating characteristics (ROC) plots: a fundamental evaluation tool in clinical medicine. \emph{Clinical Chemistry} \bold{39}, 561--577.  
  
}

\author{
	Monica Lopez-Raton and Maria Xose Rodriguez-Alvarez
}
\note{
 Individuals with a test value greater than (or equal to) the cutoff are classified as diseased (positive test),
whereas patients with a lower value are classified as healthy (negative test). If this is not the case, however, and the low values
are related to the disease, then the test results must be multiplied by (-1) prior to analysis.   
}

\seealso{
 \code{\link{control.cutpoints}}, \code{\link{summary.optimal.cutpoints}}
}

\examples{
library(OptimalCutpoints)
data(elas)
####################
# marker: elas
# status: status
# categorical covariates:
#		gender
####################

###########################################################
# Youden Index Method ("Youden"): Covariate gender
###########################################################
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, marker = "elas", 
status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = TRUE)

summary(optimal.cutpoint.Youden)

plot(optimal.cutpoint.Youden)

#  Inference on the test accuracy measures 
optimal.cutpoint.Youden<-optimal.cutpoints(methods = "Youden", data = elas, marker = "elas", 
status = "status", tag.healthy = 0, pop.prev = NULL, categorical.cov = "gender", 
control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = TRUE)

summary(optimal.cutpoint.Youden)

##########################################################################
# Sensitivity equal to Specificity Method ("SpEqualSe"): Covariate gender
##########################################################################
optimal.cutpoint.SpEqualSe<-optimal.cutpoints(methods = "SpEqualSe", data = elas, 
marker = "elas", status = "status", tag.healthy = 0, pop.prev = NULL, 
categorical.cov = "gender", control = control.cutpoints(), ci.fit = TRUE, 
conf.level = 0.95, trace = TRUE)

summary(optimal.cutpoint.SpEqualSe)

plot(optimal.cutpoint.SpEqualSe)  

} 
