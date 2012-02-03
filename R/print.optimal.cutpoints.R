print.optimal.cutpoints <-
function(x, digits = max(3, getOption("digits") - 3), ...) {
	nam <- c("Min", "1Q", "Median", "Mean", "3Q", "Max", "Std. dev")	
   	cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
	if(is.null(x$levels.cat)) {	
		data.h <- x$data[x$data[,x$call$status] == x$call$tag.healthy, ]
		cat(paste("Healthy: ",nrow(data.h), "individuals", "\n"))
		p <- c(summary(data.h[,x$call$marker]), sd(data.h[,x$call$marker]))
       	print(structure(zapsmall(p,digits + 1), names = nam),  digits = digits, ...)
		data.d <- x$data[x$data[,x$call$status] != x$call$tag.healthy, ]
		cat("-------------------------------------------------\n")
		cat(paste("Diseased: ", nrow(data.d), "individuals", "\n"))
		p <- c(summary(data.d[,x$call$marker]), sd(data.d[,x$call$marker]))
		print(structure(zapsmall(p, digits + 1), names = nam),  digits = digits, ...)
		cat("\n\n")
		cat(paste("Sample prevalence: ", round(nrow(data.d)/(nrow(data.d) + nrow(data.h))*100,3),"%", "\n", sep = ""))
	} else {
		for (i in x$levels.cat) {
			cat("*************************************************\n")
			cat(i)
			cat("\n*************************************************\n")
			data.h <- x$data[x$data[,x$call$status] == x$call$tag.healthy & x$data[,x$call$categorical.cov] == i, ]
			cat(paste("Healthy: ",nrow(data.h), "individuals", "\n"))
			p <- c(summary(data.h[,x$call$marker]), sd(data.h[,x$call$marker]))
			print(structure(zapsmall(p, digits + 1), names = nam),  digits = digits, ...)
			data.d <- x$data[x$data[,x$call$status] != x$call$tag.healthy & x$data[,x$call$categorical.cov] == i, ]
			cat("-------------------------------------------------\n")
			cat(paste("Diseased: ", nrow(data.d), "individuals", "\n"))
			p <- c(summary(data.d[,x$call$marker]), sd(data.d[,x$call$marker]))
			print(structure(zapsmall(p, digits + 1), names = nam),  digits = digits, ...)
			cat("\n\n")
			cat(paste("Sample prevalence: ", round(nrow(data.d)/(nrow(data.d) + nrow(data.h))*100,3),"%", "\n", sep = "")) 			
		}
	}
	cat("\n")
   	invisible(x)
}
