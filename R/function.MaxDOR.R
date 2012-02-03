function.MaxDOR <-
function(data, marker, status, tag.healthy = 0, control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, pop.prev, control, ci.fit, conf.level) 
        
    	TP <- measures.acc$Se[,1]*measures.acc$n$d    
    	for (i in 1:length(TP))
    	{
      	if (TP[i] == 0) TP[i] <- 0.5
    	}
    	TN <- measures.acc$Sp[,1]*measures.acc$n$h    
    	for (i in 1:length(TN))
    	{
        	if (TN[i] == 0) TN[i] <- 0.5
    	}
    	FN <- (1-measures.acc$Se[,1])*measures.acc$n$d
    	for (i in 1:length(FN))
    	{
        	if (FN[i] == 0) FN[i] <- 0.5
    	}
    	FP <- (1-measures.acc$Sp[,1])*measures.acc$n$h
    	for (i in 1:length(FP))
    	{
      	if (FP[i] == 0) FP[i] <- 0.5
    	}

    	DOR <- (TP*TN)/(FN*FP)

    	cMaxDOR <- measures.acc$cutoffs[which(round(DOR,10) == round(max(DOR, na.rm = TRUE),10))]
    	optimal.DOR <- max(DOR, na.rm = TRUE)

    	optimal.cutoff <- obtain.optimal.measures(cMaxDOR, measures.acc)

    	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = DOR, optimal.criterion = optimal.DOR)
    	res
}
