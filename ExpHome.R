ExpHome <- function(line,total,homeFair){

    # Deriving expected home goals from Asian hcap line, expected totals and
    # fair odds for home team to beat the Asian hcap.

    t <- vector(length = length(line))

    for (i in (1:length(line))) {

      if(is.na(homeFair[i])) {
        t[[i]] <- NA
        }  else {

    f_0 <- function(lambda1) {Skellam(-line[i]+1,lambda1,total[i]-lambda1)/(1-skellam::dskellam(-line[i],lambda1,total[i]-lambda1)) - 1/homeFair[i]}
    f_0.25 <- if(line[i]<0) {function(lambda1) {sqrt((Skellam(-line[i]-0.25+1,lambda1,total[i]-lambda1)/(1-skellam::dskellam(-line[i]-0.25,lambda1,total[i]-lambda1)))*Skellam(-line[i]+0.25+0.5,lambda1,total[i]-lambda1)) - 1/homeFair[i]}}
    else {function(lambda1) {sqrt((Skellam(-line[i]+0.25+1,lambda1,total[i]-lambda1)/(1-skellam::dskellam(-line[i]+0.25,lambda1,total[i]-lambda1)))*Skellam(-line[i]-0.25+0.5,lambda1,total[i]-lambda1)) - 1/homeFair[i]}}
    f_0.5 <- function(lambda1) {Skellam(-line[i]+0.5,lambda1,total[i]-lambda1) - 1/homeFair[i]}
    f_0.75 <- if(line[i]<0) {function(lambda1) {sqrt((Skellam(-line[i]+0.25+1,lambda1,total[i]-lambda1)/(1-skellam::dskellam(-line[i]+0.25,lambda1,total[i]-lambda1)))*Skellam(-line[i]-0.25+0.5,lambda1,total[i]-lambda1)) - 1/homeFair[i]}}
    else {function(lambda1) {sqrt((Skellam(-line[i]-0.25+1,lambda1,total[i]-lambda1)/(1-skellam::dskellam(-line[i]-0.25,lambda1,total[i]-lambda1)))*Skellam(-line[i]+0.25+0.5,lambda1,total[i]-lambda1)) - 1/homeFair[i]}}

    f <- switch(as.character(line[i]%%1),
                "0" = f_0,
                "0.25" = ifelse(line[i]<0,f_0.75,f_0.25),
                "0.5" = f_0.5,
                "0.75" = ifelse(line[i]<0,f_0.25,f_0.75)
                )

   t[i] <- round(uniroot(f, c( (total[i]-abs(line[i]))/2 - 0.1, (total[i]+abs(line[i]))/2 ), extendInt="yes")$root,2)
        }
      }
    t
  }
