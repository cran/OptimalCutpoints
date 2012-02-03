function.RangeSe <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	if (control$lowerValueSe < 0 || control$lowerValueSe > 1) {
      	stop("The lower value for Sensitivity must be between 0 and 1.", call. = FALSE)
    	}
    	if (control$upperValueSe < 0 || control$upperValueSe > 1) {
      	stop("The upper value for Sensitivity must be between 0 and 1.", call. = FALSE)
    	}
    	if (control$lowerValueSe > control$upperValueSe)
    	{
      	stop("The upper value for Sensitivity must be greater than the lower value.", call. = FALSE) 
    	}
    	if (control$lowerValueSe == 0 & control$upperValueSe == 1)
    	{
      	warning ("You have entered the minimum and maximum possible values for \n Sensitivity. All the cutpoints fulfill the condition. Please check these values.", call. = FALSE, immediate. = TRUE)      	
    	}
    	if (control$lowerValueSe == control$upperValueSe)
    	{
		   	warning ("You have entered a single value instead of a range of values for Sensitivity. Please check these values.\n", call. = FALSE, immediate. = TRUE)
       	if (control$lowerValueSe == 0 & control$upperValueSe == 0)
       	{
			    warning("In addition, you have entered the minimum possible value for Sensitivity.", call. = FALSE, immediate. = TRUE)
       	}
       	if (control$lowerValueSe == 1 & control$upperValueSe == 1)
       	{
		      warning("In addition, you have entered the maximum possible value for Sensitivity.", call. = FALSE, immediate. = TRUE)
       	}    
    	}

    	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)     
   
    	index.cutpoints <- which(measures.acc$Se[,1] >= control$lowerValueSe & measures.acc$Se[,1] <= control$upperValueSe)
    	if (length(index.cutpoints) == 0)
    	{
      	if (control$lowerValueSe == control$upperValueSe)
        	{
          		warning("There is no cutpoint that yields the exact Sensitivity designated. \n The cutpoint having the closest value to the designated Sensitivity has therefore been selected.", call. = FALSE, immediate. = TRUE)
          		difference <- abs(control$lowerValueSe-measures.acc$Se[,1])
          		index.cutpoints <- which(round(difference,10) == round(min(difference),10))            		
        	}
        	if (control$lowerValueSe != control$upperValueSe)
        	{
          		warning("There is no cutpoint that fulfills these conditions. Please enter a new range of values, if desired.", call. = FALSE, immediate. = TRUE)
          		crangeSe <- NULL
        	}
    	}
    	if (length(index.cutpoints)!= 0)
    	{
      	if (length(index.cutpoints)== 1)
        	{
            	crangeSe <- measures.acc$cutoffs[index.cutpoints]            
        	}
        	if (length(index.cutpoints)> 1)
        	{
            	cutpoints <- measures.acc$cutoffs[index.cutpoints]
            	Spnew <- obtain.optimal.measures(cutpoints, measures.acc)$Sp
            	cutpointsSpnew <- cutpoints[which(round(Spnew[,1],10) == round(max(Spnew[,1]),10))]                        
                                  
            	if (length(cutpointsSpnew)> 1) 
            	{
              		Senew <- obtain.optimal.measures(cutpointsSpnew, measures.acc)$Se
              		crangeSe <- cutpointsSpnew[which(round(Senew[,1],10) == round(max(Senew[,1]),10))]                		
            	}
            	if (length(cutpointsSpnew)== 1) 
            	{ 
              		crangeSe <- cutpointsSpnew
            	}
        	}
    	} 
    
    	optimal.cutoff <- obtain.optimal.measures(crangeSe, measures.acc)
      
    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff)
    	res       
}
