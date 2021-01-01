Demarg <- function(o1,o2,o3=rep(NA,length(o1))){

  # Returns the fair prices from a set of marginated odds (assuming margin is proportionally distributed).

  overround <- ifelse(splus2R::is.missing(o3), 1/o1 + 1/o2, 1/o1 + 1/o2 + 1/o3)

  t <- data.frame(
      round(o1*overround,3),
      round(o2*overround,3),
      round(o3*overround,3)
      )
  names(t) <- c("o1","o2","o3")

  t
}
# Demarg(c(1.8,2,2.3),c(3.7,2.7,3.5),c(4.7,4,3.5))



