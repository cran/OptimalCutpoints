function.MeanPrev <-
function(data, marker, status, tag.healthy = 0, direction = c("<", ">"), control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	direction <- match.arg(direction)
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, direction, pop.prev, control, ci.fit, conf.level)
	if (measures.acc$cutoffs < 0 || measures.acc$cutoffs > 1) {
		warning("Diagnostic marker values are not between 0 and 1 for this \n criterion. A data transformation has been performed.", call. = FALSE, immediate. = TRUE)
		tmarker <- (measures.acc$cutoffs - min(measures.acc$cutoffs))/(max(measures.acc$cutoffs)-min(measures.acc$cutoffs))
		difference <- abs(tmarker-mean(tmarker))
	} else {	
		difference <- abs(measures.acc$cutoffs-mean(measures.acc$cutoffs))
	}
	cMeanPrev <- measures.acc$cutoffs[which(round(difference,10) == round(min(difference),10))]
				
	optimal.cutoff <- obtain.optimal.measures(cMeanPrev, measures.acc)

	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff)
	res	 
}
