plot.optimal.cutpoints <-
function(x, legend = TRUE, ...) {
	op <- par(pty = "s")
  	opt.criterion.methods <- c("MaxAccuracyArea","MCT","MaxSpSe", "MaxSumSpSe", "MaxProdSpSe", "ROC01", "SpEqualSe", "Youden", "MaxEfficiency", "Minimax", "AUC", "MaxDOR", "MaxKappa", "MaxAccuracy", "MinErrorRate", "PROC01", "NPVEqualPPV", "MinPvalue", "PrevalenceMatching")
	methods <- x[x$methods]
	n.levels.cat <- if(is.null(x$levels.cat)) {1} else {length(x$levels.cat)}
	levels.cat <- if(is.null(x$levels.cat)) {NULL} else {x$levels.cat}
	for (i in 1:n.levels.cat) {
		for(j in 1:length(methods)) {
			if(length(methods[[j]][[i]][["optimal.cutoff"]][[1]])== 0) {
				if(is.null(x$levels.cat)) {
					cat(paste(names(methods)[j], ": There are no cutoff values that fulfill the criterion \n", sep = ""))
				} else {
					cat(paste(names(methods)[j], ": There are no cutoff values that fulfill the criterion for ", levels.cat[i], "\n", sep = ""))
				}
			}			
			aux.criterion <- names(methods)[j] %in% opt.criterion.methods
			main <- paste("Criterion: ",names(methods)[j], "\n", ifelse(is.null(levels.cat), "", levels.cat[i]), sep = "")
            # ROC curve
            if(i > 1 | j > 1) {
            	readline("Press return for next page....")            		
            }
            m <- methods[[j]][[i]]
            plot(1-m[["measures.acc"]][["Sp"]][,1], m[["measures.acc"]][["Se"]][,1], xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve. ", main, sep = ""), type = "l", cex.lab = 1.3, cex.axis = 1.3,...)
            abline(0,1, col = "grey")
            legend.text <- paste("AUC: ",paste(round(m[["measures.acc"]][["AUC"]][[1]], 3), " (", round(m[["measures.acc"]][["AUC"]][[2]], 3),"",", ", round(m[["measures.acc"]][["AUC"]][[3]], 3),")", sep = ""), sep = "")
            legend(0.4, 0.2, legend.text, bty = "n")
            if(length(m[["optimal.cutoff"]][[1]])!= 0) {
				for(k in 1:length(m[["optimal.cutoff"]][[1]])) {
					x <- 1-m[["optimal.cutoff"]][["Sp"]][[k]]
					y <- m[["optimal.cutoff"]][["Se"]][[k]]
					lines(rep(x,2), c(0,y), lty = 2)
					lines(c(0,x), rep(y,2), lty = 2)
					points(x,y, pch = 16, cex = 0.7)
					if(legend) {
						legend.text <- paste("(",round(x,3), ", ", round(y,3),")", sep = "")
		            	legend(x, y, legend.text, bty = "n", xjust = 0.5, yjust = 0, ...)
		            }
				}
			}
			readline("Press return for next page....") 
			# PROC curve
			plot(1-m[["measures.acc"]][["NPV"]][,1], m[["measures.acc"]][["PPV"]][,1], xlab = "1 - Negative predictive value", ylab = "Positive predictive value", main = paste("PROC Curve. ", main, sep = ""), type = "l", cex.lab = 1.3, cex.axis = 1.3, xlim = c(0,1), ylim = c(0,1), ...)
			if(length(m[["optimal.cutoff"]][[1]])!= 0) {
				for(k in 1:length(m[["optimal.cutoff"]][[1]])) {
					x <- 1 - m[["optimal.cutoff"]][["NPV"]][[k]]
					y <- m[["optimal.cutoff"]][["PPV"]][[k]]
					lines(rep(x,2), c(0,y), lty = 2)
					lines(c(0,x), rep(y,2), lty = 2)
					points(x,y, pch = 16, cex = 0.7)
					if(legend) {
						legend.text <- paste("(",round(x,3), ", ", round(y,3),")", sep = "")
		            	legend(x, y, legend.text, bty = "n", xjust = 0.5, yjust = 0, ...)
		            }
				}
			}
			# Auxiliar plot
			if(aux.criterion) {
				readline("Press return for next page....")
				plot(m[["measures.acc"]][["cutoffs"]], m[["criterion"]], xlab = "Cutoffs values", ylab = "Optimal criterion", main = main, type = "l", cex.lab = 1.3, cex.axis = 1.3, ...)
				if(length(m[["optimal.cutoff"]][[1]])!= 0) {
					for(k in 1:length(m[["optimal.cutoff"]][[1]])) {
						x <- m[["optimal.cutoff"]][["cutoff"]][[k]]
						y <- m[["optimal.criterion"]][[1]]
						lines(rep(x,2), c(0,y), lty = 2)
						lines(c(0,x), rep(y,2), lty = 2)
						points(x,y, pch = 16, cex = 0.7)
						if(legend) {
							legend.text <- paste("(",round(x,3), ", ", round(y,3),")", sep = "")
			            		legend(x, y, legend.text, bty = "n", xjust = 0.5, yjust = 0, ...)
			            	}
					}
				}
			}
		}
	}
	par(op)
}
