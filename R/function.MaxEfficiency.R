function.MaxEfficiency <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){    
	   if (is.logical(control$costs.benefits.Efficiency) == FALSE) {
      	stop("'costs.benefits.Efficiency' must be a logical-type argument.", call. = FALSE)
    	}     
      if (is.logical(control$standard.deviation.accuracy) == FALSE) {
      	stop("'standard.deviation.accuracy' must be a logical-type argument.", call. = FALSE)
    	} 
    	
    	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)
       
    	Efficiency <- pop.prev*measures.acc$Se[,1]+(1-pop.prev)*measures.acc$Sp[,1]
         
    	if (control$costs.benefits.Efficiency == FALSE)
    	{ 
      	cMaxEfficiency <- measures.acc$cutoffs[which(round(Efficiency,10) == round(max(Efficiency),10))]        	
    	}
    
    	if (control$costs.benefits.Efficiency == TRUE)
    	{ 
      	control$costs.ratio <- 1
      	cMaxEfficiency <- function.CB(data, marker, status, tag.healthy = 0, control = control, pop.prev, ci.fit, conf.level)$optimal.cutoff$cutoff
    	} 
    	
    	optimal.Efficiency <- max(Efficiency)
    	
    	optimal.cutoff <- obtain.optimal.measures(cMaxEfficiency, measures.acc)
    	
    	# Standard deviation associated with accuracy or efficiency at the optimal cutpoint is computed:
    	if (control$standard.deviation.accuracy == TRUE)
    	{
      	optimal.Efficiency.sd <- ((optimal.Efficiency * (1 - optimal.Efficiency))/(measures.acc$n$d+measures.acc$n$h - 1))^0.5
    	}
    
    	if (control$standard.deviation.accuracy == FALSE)
    	{
      	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = Efficiency, optimal.criterion = optimal.Efficiency)
    	}
    	if (control$standard.deviation.accuracy == TRUE)
    	{
      	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = Efficiency, optimal.criterion = optimal.Efficiency, sd.maximum.Efficiency = optimal.Efficiency.sd)
    	} 
    
    	res
}
