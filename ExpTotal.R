ExpTotal <- function(line,underFair){

  # Deriving expected totals from totals line and fair odds for the Unders, assuming Poisson distribution.

  t <- vector(length = length(line))

  for (i in (1:length(line))) {

    if(is.na(underFair[i])) {
      t[[i]] <- NA
    }  else {

  f_0 <- function(lambda) {ppois(line[i]-1, lambda)/(1-dpois(line[i], lambda)) - 1/underFair[i]}
  f_0.25 <- function(lambda) {sqrt((ppois(line[i]-1-0.25, lambda)/(1-dpois(line[i]-0.25, lambda)))*ppois(line[i]-0.25, lambda)) - 1/underFair[i]}
  f_0.5 <- function(lambda) {ppois(line[i]-0.5, lambda) - 1/underFair[i]}
  f_0.75 <- function(lambda) {sqrt((ppois(line[i]-1+0.25, lambda)/(1-dpois(line[i]+0.25, lambda)))*ppois(line[i]-0.75, lambda)) - 1/underFair[i]}

  f <- switch(as.character(line[i]%%1),
              "0" = f_0,
              "0.25" = f_0.25,
              "0.5" = f_0.5,
              "0.75" = f_0.75
              )

  t[i] <- round(uniroot(f, c(max(line[i]-1,0), line[i]+1), extendInt="yes")$root,2)
    }
    }
  t
}
