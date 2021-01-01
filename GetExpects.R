GetExpects <- function(hcapLine,homeOdds,awayOdds,totalsLine,overOdds,underOdds){

  Exp <- data.frame(matrix(nrow=length(hcapLine),ncol=2))
  names(Exp) <- c("Home","Away")

  Exp$Home <- ifelse(is.na(homeOdds) | is.na(overOdds),
                     NA,
                     ExpHome(hcapLine,ExpTotal(totalsLine,Demarg(overOdds,underOdds)[[2]]),
                             Demarg(homeOdds,awayOdds)[[1]]
                             )
                     )
  Exp$Away <- ifelse(is.na(homeOdds) | is.na(overOdds),
                     NA,
                     ExpTotal(totalsLine,Demarg(overOdds,underOdds)[[2]])-Exp$Home
                     )

  Exp
}
