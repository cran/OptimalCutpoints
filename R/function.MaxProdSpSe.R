function.MaxProdSpSe <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)
    
    	prod <- measures.acc$Sp[,1] * measures.acc$Se[,1]     
    	cmaxProdSpSe <- measures.acc$cutoffs[which(round(prod,10) == round(max(prod),10))]
    	optimal.prod <- max(prod)
    
    	optimal.cutoff <- obtain.optimal.measures(cmaxProdSpSe, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = prod, optimal.criterion = optimal.prod)
    	res
}
