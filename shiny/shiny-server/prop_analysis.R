library(jsonlite)

# ignore warnings
options(warn=-1)

c1 <- c(31,24,2,2,1,1,1,1)
c2 <- c(34,30,2,96,331,355,1,1030)

prop_analysis <- function(c1, c2)
{
  counts <- cbind(c1, c2)
  
  s1 <- sum(c1, na.rm=T)
  s2 <- sum(c2, na.rm=T)
  
  perc <- apply(counts, 1, function(x) { sprintf("%3.2f%% vs. %3.2f%%",  x[1] * 100.0 / s1, x[2] * 100.0 / s2)})
  res <- apply(counts, 1, function(x) { prop.test(c(x[1], x[2]), c(s1, s2), correct=T) })
  pvalues <- sapply(res, function(x) { format(x$p.value, scientific=T, digits=3) })
  BH <- p.adjust(pvalues, "fdr")
  #BH <- runif(length(c1), 0, 0.1)
  
  return(list(perc, BH))
}

prop_analysis(c1, c2)
