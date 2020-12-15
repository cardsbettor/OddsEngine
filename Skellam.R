Skellam <- function(diff,mu1,mu2){
  
  #Probability a Poisson dist. with lambda = mu1 exceeds Poisson dist. with lambda = mu2 by diff or more.
  
  k = 0
  for (i in diff:10){
    k <- k + skellam::dskellam(i,mu1,mu2)
  }
  k
}
