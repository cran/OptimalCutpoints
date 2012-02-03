function.RangeSp <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	if (control$lowerValueSp < 0 || control$lowerValueSp > 1) {
      	stop("The lower value for Specificity must be between 0 and 1.", call. = FALSE)
	}
    	if (control$upperValueSp < 0 || control$upperValueSp > 1) {
      	stop("The upper value for Specificity must be between 0 and 1.", call. = FALSE)
    	}
    	if (control$lowerValueSp > control$upperValueSp)
    	{ 
        	stop("The upper value for Specificity must be greater than the lower value.", call. = FALSE)            
    	}
    	if (control$lowerValueSp == 0 & control$upperValueSp == 1)
    	{
      	warning ("You have entered the minimum and maximum possible values for \n Specificity. All the cutpoints fulfill the condition. Please check these values.", call. = FALSE, immediate. = TRUE)       	
    	}
    	if (control$lowerValueSp == control$upperValueSp)
    	{
      	warning ("You have entered a single value instead of a range of values for Specificity . Please check these values.\n", call. = FALSE, immediate. = TRUE)
      	      
      	if (control$lowerValueSp == 0 & control$upperValueSp == 0)
      	{
		    	warning("In addition, you have entered the minimum possible value for Specificity.", call. = FALSE, immediate. = TRUE) 		    	
      	}
      	if (control$lowerValueSp == 1 & control$upperValueSp == 1)
      	{
		    	warning("In addition, you have entered the maximum possible value for Specificity.", call. = FALSE, immediate. = TRUE)		    
      	}    
    	}

    	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)   
    
    	index.cutpoints <- which(measures.acc$Sp[,1] >= control$lowerValueSp & measures.acc$Sp[,1] <= control$upperValueSp)      
    	if (length(index.cutpoints)== 0)
    	{
      	if (control$lowerValueSp == control$upperValueSp)
        	{
          		warning("There is no cutpoint that yields the exact Specificity designated. \n The cutpoint having the closest value to the designated Specificity has therefore been selected.", call. = FALSE, immediate. = TRUE)
          		difference <- abs(control$lowerValueSp-measures.acc$Sp[,1])
          		index.cutpoints <- which(round(difference,10) == round(min(difference),10))             		
        	}
        	if (control$lowerValueSp != control$upperValueSp)
        	{
          		warning("There is no cutpoint that fulfills these conditions. Please enter a new range of values, if desired.", call. = FALSE, immediate. = TRUE)          		
          		crangeSp <- NULL
        	}
    	}
    	if (length(index.cutpoints)!= 0)
    	{
        	if (length(index.cutpoints)== 1)
        	{
            	crangeSp <- measures.acc$cutoffs[index.cutpoints]                    
        	}
        	if (length(index.cutpoints)> 1)
        	{
            	cutpoints <- measures.acc$cutoffs[index.cutpoints]
            	Senew <- obtain.optimal.measures(cutpoints, measures.acc)$Se
            	cutpointsSenew <- cutpoints[which(round(Senew[,1],10) == round(max(Senew[,1]),10))]                       
            
            	if (length(cutpointsSenew)> 1) 
            	{
              		Spnew <- obtain.optimal.measures(cutpointsSenew, measures.acc)$Sp
              		crangeSp <- cutpointsSenew[which(round(Spnew[,1],10) == round(max(Spnew[,1]),10))]               		
            	}
            	if (length(cutpointsSenew)== 1) 
            	{
              		crangeSp <- cutpointsSenew
            	}
        	}    
      }
      
      optimal.cutoff <- obtain.optimal.measures(crangeSp, measures.acc)
    
      res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff)
      res
}
