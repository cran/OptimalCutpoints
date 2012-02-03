function.AUC <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)        
    
    	concordanceAUC <- (measures.acc$Se[,1]-(1-measures.acc$Sp[,1])+1)/2      
    	cAUC <- measures.acc$cutoffs[which(round(concordanceAUC,10) == round(max(concordanceAUC),10))]
    	optimal.concordanceAUC <- max(concordanceAUC)
    
    	optimal.cutoff <- obtain.optimal.measures(cAUC, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = concordanceAUC, optimal.criterion = optimal.concordanceAUC)
    	res
}
