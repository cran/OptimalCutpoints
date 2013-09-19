function.MaxSumNPVPPV <-
function(data, marker, status, tag.healthy = 0, direction = c("<", ">"), control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	direction <- match.arg(direction)
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, direction, pop.prev, control, ci.fit, conf.level)
	
	sum <- measures.acc$PPV[,1] + measures.acc$NPV[,1]	
	cmaxSumNPVPPV <- measures.acc$cutoffs[which(round(sum,10) == round(max(sum),10))]
	optimal.sum <- max(sum)

	optimal.cutoff <- obtain.optimal.measures(cmaxSumNPVPPV, measures.acc)

	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff, criterion = sum, optimal.criterion = optimal.sum)
	res
}
