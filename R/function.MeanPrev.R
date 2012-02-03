function.MeanPrev <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)
    	if (data[, marker] < 0 || data[, marker] > 1)
    	{
        	warning("Diagnostic marker values are not between 0 and 1 for this \n criterion. A data transformation has been performed.", call. = FALSE, immediate. = TRUE)
        	tmarker <- (data[, marker] - min(data[, marker]))/(max(data[, marker])-min(data[, marker]))
        	difference <- abs(tmarker-mean(tmarker))
    	}
    	else
    	{    
        	difference <- abs(data[, marker]-mean(data[, marker]))
    	}
    
    	cMeanPrev <- data[, marker][which(round(difference,10) == round(min(difference),10))]
    	    	    
    	optimal.cutoff <- obtain.optimal.measures(cMeanPrev, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff)
    	res     
}
