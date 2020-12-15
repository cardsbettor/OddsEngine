
Margin <- function(o1,o2,o3=NULL) {

  # Returns the margin built into a set of marginated odds (assuming margin is proportionally distributed).

  ifelse(splus2R::is.missing(o3),
         margin <- 1/o1 + 1/o2 - 1,
         margin <- 1/o1 + 1/o2 + 1/o3 - 1)

  round(margin,4)
  }
