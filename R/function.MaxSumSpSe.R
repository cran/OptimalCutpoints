function.MaxSumSpSe <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)
    
    	sum <- measures.acc$Se[,1] + measures.acc$Sp[,1]    
    	cmaxSumSpSe <- measures.acc$cutoffs[which(round(sum,10) == round(max(sum),10))]
    	optimal.sum <- max(sum)

    	optimal.cutoff <- obtain.optimal.measures(cmaxSumSpSe, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = sum, optimal.criterion = optimal.sum)
    	res
}
