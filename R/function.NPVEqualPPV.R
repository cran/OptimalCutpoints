function.NPVEqualPPV <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)    
    
    	difference <- abs(measures.acc$NPV[,1]-measures.acc$PPV[,1])       
    	cNPVEqualPPV <- measures.acc$cutoffs[which(round(difference,10) == round(min(difference, na.rm=TRUE),10))]
    	
    	optimal.difference <- min(difference, na.rm=TRUE)
        
    	optimal.cutoff <- obtain.optimal.measures(cNPVEqualPPV, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = difference, optimal.criterion = optimal.difference)
    	res
}
