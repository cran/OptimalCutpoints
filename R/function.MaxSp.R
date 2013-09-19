function.MaxSp <-
function(data, marker, status, tag.healthy = 0, direction = c("<", ">"), control = control.cutpoints(), pop.prev, ci.fit = FALSE, conf.level = 0.95){
	direction <- match.arg(direction)
	measures.acc <- calculate.accuracy.measures(data, marker, status, tag.healthy, direction, pop.prev, control, ci.fit, conf.level)
	cutpointsSp <- measures.acc$cutoffs[which(round(measures.acc$Sp[,1],10) == round(max(measures.acc$Sp[,1]),10))]				
	if (length(cutpointsSp)> 1) {
		Senew <- obtain.optimal.measures(cutpointsSp, measures.acc)$Se 
		cMaxSp <- cutpointsSp[which(round(Senew[,1],10) == round(max(Senew[,1]),10))]		 	
	}
	if (length(cutpointsSp)== 1) {
		cMaxSp <- cutpointsSp
	}
	optimal.cutoff <- obtain.optimal.measures(cMaxSp, measures.acc)

	res <- list(measures.acc = measures.acc, optimal.cutoff = optimal.cutoff)
	res
}
