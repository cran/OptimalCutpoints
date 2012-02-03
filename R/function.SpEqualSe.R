function.SpEqualSe <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)  
         
    	difference <- abs(measures.acc$Sp[,1] - measures.acc$Se[,1])
    	cSpEqualSe <- measures.acc$cutoffs[which(round(difference,10) == round(min(difference),10))]
    	optimal.difference <- min(difference)
    
    	optimal.cutoff <- obtain.optimal.measures(cSpEqualSe, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = difference, optimal.criterion = optimal.difference)
    	res
}
