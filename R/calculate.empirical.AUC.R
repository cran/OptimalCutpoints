calculate.empirical.AUC <-
function(data, marker, status, tag.healthy, conf.level) {
	marker.diseased = data[data[,status] != tag.healthy, marker]
    	n.diseased = length (marker.diseased)
    
    	marker.healthy = data[data[,status] == tag.healthy, marker]
    	n.healthy = length(marker.healthy)

    	# Function that counts the number of elements in a vector that take a value of zero:
    	count.zeros <- function(x)
    	{
        	length(x[x == 0])
    	}

    	# Function that counts the number of elements in a vector that take a value lower than zero:
    	count.neg <- function(x)
    	{
        	length(x[x < 0])
    	}

    	marker.diseasedmat <- matrix(rep(marker.diseased,n.healthy), nrow = n.healthy, byrow = T)
    	marker.healthymat <- matrix(rep(marker.healthy,n.diseased), nrow = n.healthy, byrow = F)
    	diffmat <- marker.healthymat-marker.diseasedmat
    	area <- (length(diffmat[diffmat < 0])+0.5*length(diffmat[diffmat == 0]))/(n.diseased*n.healthy)
    	neg2 <- apply(diffmat,2,count.neg)
    	zeros2 <- apply(diffmat,2,count.zeros)
    	sum1 <- 0
    	for (i in 1:length(neg2)) {
        	sum1 <- sum1+((neg2[i]+0.5*zeros2[i])/n.healthy-area)^2
    	}
    	first.term <- sum1/(n.diseased*(n.diseased-1)) 
        
    	neg1 <- apply(diffmat,1,count.neg)
    	zeros1 <- apply(diffmat,1,count.zeros)
    	sum2 <- 0
    	for (j in 1:length(neg1)) {
        	sum2 <- sum2+((neg1[j]+0.5*zeros1[j])/n.diseased-area)^2
    	}
    	second.term <- sum2/(n.healthy*(n.healthy-1)) 

    	# The variance is computed:
    	var <- first.term+second.term
    	z <- qnorm(1-((1-conf.level)/2))     	
    	# Lower end of (1-conf.level)% confidence interval:       	 
    	inf <- area-z*sqrt(var)
    	# Upper end of (1-conf.level)% confidence interval:     	
    	sup <- area+z*sqrt(var)

    	res <- list(AUC = area, ll = inf, ul = sup)
    	return(res)
}
