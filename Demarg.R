Demarg <- function(o1,o2){

  #Returns the fair prices from a pair of marginated odds (assuming margin is proportionally distributed).

  overround <- 1/o1 + 1/o2

  t <- data.frame(
    round(o1*overround,3),
    round(o2*overround,3)
    )
  names(t) <- c("o1","o2")

  t
}



