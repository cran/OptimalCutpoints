function.MinErrorRate <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
   measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)   
    
    	ErrorRate <- pop.prev*(1-measures.acc$Se[,1])+(1-pop.prev)*(1-measures.acc$Sp[,1])

    	cMinErrorRate <- measures.acc$cutoffs[which(round(ErrorRate,10) == round(min(ErrorRate),10))]
    	optimal.ErrorRate <- min(ErrorRate)

    	optimal.cutoff <- obtain.optimal.measures(cMinErrorRate, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = ErrorRate, optimal.criterion = optimal.ErrorRate)
    	res
}
