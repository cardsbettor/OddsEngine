ExpPoints <- function(home,draw,away){
  
  # Returns the fair prices from a set of odds (marginated or not).
  
  overround <- 1/home + 1/draw + 1/away
  
  p <- data.frame(
    round(1/(home*overround),3),
    round(1/(draw*overround),3),
    round(1/(away*overround),3)
    )
  names(p) <- c("home_prob","draw_prob","away_prob")
  
  exp_home_points <- 3*p$home_prob + 1*p$draw_prob
  exp_away_points <- 3*p$away_prob + 1*p$draw_prob
  
  data.frame(exp_home_points,exp_away_points)
}
