UsToDec <- function(price){
  if(price <= -100){
    round(100/abs(price) + 1,2)
  }
  else{
    if(price >= 100){
      round(price/100 + 1,2)
    }
    else {NULL}
  }
}
