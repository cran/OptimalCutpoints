function.MaxAccuracyArea <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints (), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)   
    
    	TP <- measures.acc$Se[,1]*measures.acc$n$d
    	TN <- measures.acc$Sp[,1]*measures.acc$n$h
    	FN <-(1-measures.acc$Se[,1])*measures.acc$n$d
    	FP <-(1-measures.acc$Sp[,1])*measures.acc$n$h

    	AA <-(TP*TN)/((TP+FN)*(FP+TN))
    
    	cMaxAA <- measures.acc$cutoffs[which(round(AA,10) == round(max(AA, na.rm = TRUE),10))]
    	optimal.AA <- max(AA, na.rm = TRUE)

    	optimal.cutoff <- obtain.optimal.measures(cMaxAA, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = AA, optimal.criterion = optimal.AA)
    	res
}
