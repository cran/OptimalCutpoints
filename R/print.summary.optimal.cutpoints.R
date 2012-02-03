print.summary.optimal.cutpoints <-
function(x, ...) {
	print.optimal.cutpoints(x)
	cat("\n*************************************************\n")
	cat("OPTIMAL CUTOFF")	
	cat("\n*************************************************\n")
	for(i in 1: length(x$p.table)) {
		if(!is.null(x$levels.cat)) {
			cat("\n*************************************************\n")
			cat(names(x$p.table)[i])
			cat("\n*************************************************\n")
		}
		cat("\nArea under the ROC curve (AUC): ", x$p.table[[i]][["AUC_CI"]], "\n")
		for (j in 1:(length(x$p.table[[i]]) - 1)) {
			cat("\n---------------------------------\n")
			cat(paste("CRITERION: " , names(x$p.table[[i]])[j], "\n", sep = ""))
			cat(paste("Number of optimal cutoffs: ", length(x$p.table[[i]][[j]]), sep = ""))
			cat("\n---------------------------------\n")
			if(length(x$p.table[[i]][[j]]) != 0) {
				for (k in 1:length(x$p.table[[i]][[j]])) {
					print(x$p.table[[i]][[j]][[k]], quote = FALSE, justify = "right", na.print = "-")
					cat("\n")
				}
			}
		}
	}
	cat("\n\n")   
   	invisible(x)
}
