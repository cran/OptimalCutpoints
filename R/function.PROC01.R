function.PROC01 <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level)    
     
    	distance <-(measures.acc$PPV[,1]-1)^2+(measures.acc$NPV[,1]-1)^2    
    	optimal.distance <- min(distance, na.rm = TRUE)
    	cPROC01 <- measures.acc$cutoffs[which(round(distance,10) == round(optimal.distance,10))] 
    
    	optimal.cutoff <- obtain.optimal.measures(cPROC01, measures.acc)
    
    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = distance, optimal.criterion = optimal.distance)
    	res
}