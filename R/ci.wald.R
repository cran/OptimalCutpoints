ci.wald <-
function(x, y, accuracy.measure, measure, n, conf.level) {  
	if ((any (x <= 20)) | (any(y <= 20))) {
      	warning(paste("Wald's approach with continuity correction for calculating the", accuracy.measure[1], "confidence \n interval may not be valid.\n This approach is valid for the", accuracy.measure[1], "in cases where \n", accuracy.measure[2], "and", accuracy.measure[3], "are greater than 20.\n You must check these conditions at the optimal cutpoint. \n\n"), call. = FALSE, immediate. = FALSE)
	}
    z <- qnorm(1-((1-conf.level)/2))        
    
    ll <- measure-(z*sqrt((measure*(1-measure))/n)+1/(2*n))
    ul <- measure+(z*sqrt((measure*(1-measure))/n)+1/(2*n))
    
    res <- list (ci = matrix(c(ll,ul), ncol = 2))      
}
