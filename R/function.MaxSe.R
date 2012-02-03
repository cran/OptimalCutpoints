function.MaxSe <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){   
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)
    
    	cutpointsSe <- measures.acc$cutoffs[which(round(measures.acc$Se[,1],10) == round(max(measures.acc$Se[,1]),10))]
             
    	if (length(cutpointsSe)> 1)
    	{
      	Spnew <- obtain.optimal.measures(cutpointsSe, measures.acc)$Sp
      	cMaxSe <- cutpointsSe[which(round(Spnew[,1],10) == round(max(Spnew[,1]),10))]       	
    	}
    	if (length(cutpointsSe)== 1)
    	{
      	cMaxSe <- cutpointsSe 
    	}
    
    	optimal.cutoff <- obtain.optimal.measures(cMaxSe, measures.acc)
    
    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff)
    	res
}
