GetExpects <- function(hcapLine,homeOdds,awayOdds,totalsLine,overOdds,underOdds){

  Exp <- data.frame(matrix(nrow=length(hcapLine),ncol=2))
  names(Exp) <- c("Home","Away")

  if(is.na(homeOdds) || is.na(overOdds)){
    Exp[[1]] <- NA
    Exp[[2]] <- NA
  }  else{
    homeFair <- Demarg(homeOdds,awayOdds)[[1]]
    underFair <- Demarg(overOdds,underOdds)[[2]]

    total <- ExpTotal(totalsLine,underFair)

    Exp[1] <- ExpHome(hcapLine,total,homeFair)
    Exp[2] <- total-Exp[1]
  }

  Exp
}
