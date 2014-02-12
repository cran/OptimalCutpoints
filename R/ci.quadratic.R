ci.quadratic <-
function(x, y, accuracy.measure, conf.level) {  
	if ((any (x <= 5)) | (any(y <= 5))) {
		warning(paste("The Fleiss quadratic approximation for calculating the confidence \n interval of the", accuracy.measure[1], "may not be valid.\n This approach is valid for the", accuracy.measure[1], "in cases where \n", accuracy.measure[2], "and", accuracy.measure[3], "are greater than 5.\n You must check these conditions at the optimal cutpoint. \n\n"), call. = FALSE, immediate. = FALSE)		 	
    }
	z <- qnorm(1-((1-conf.level)/2))
    
    ll <- (1/(x+y+z^2))*((x-0.5)+(z^2/2)-z*sqrt(z^2/4+((x-0.5)*(y-0.5))/(x+y)))
    ul <- (1/(x+y+z^2))*((x+0.5)+(z^2/2)+z*sqrt(z^2/4+((x+0.5)*(y+0.5))/(x+y)))
       
    res <- list (ci = matrix(c(ll,ul), ncol = 2))           
}
