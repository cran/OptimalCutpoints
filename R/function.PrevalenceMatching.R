function.PrevalenceMatching <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)  
        
    	sample.prev <- calculate.sample.prev(data, status, tag.healthy)     
    	predicted.prev <- measures.acc$Se[,1]*sample.prev+(1-measures.acc$Sp[,1])*(1-sample.prev)
     
    	difference <- abs(sample.prev-predicted.prev)
         
    	cPrevalenceMatching <- measures.acc$cutoffs[which(round(difference,10)==round(min(difference, na.rm = TRUE),10))]
      optimal.difference <- min(difference)
      
    	optimal.cutoff <- obtain.optimal.measures(cPrevalenceMatching, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = difference, optimal.criterion = optimal.difference)
    	res
}
