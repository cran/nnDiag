## Set of Utiliy Functions for nnDiag
## Brian Walters; bfwalters83@yahoo.com

#################################################################
## Rounds down to find the lower limit for the axis of a graph ##
#################################################################
## Created September 24, 2009
## Modified October 12, 2009

loLim <- function(num){
  if(num < -100000){floor(num/20000)*20000}
  else if(num < -10000 && num >= -100000){floor(num/10000)*10000}
  else if(num <= -2000 && num >= -10000){floor(num/2000)*2000}
  else if(num <= -1000 && num > -2000){floor(num/1000)*1000}
  else if(num <= -200 && num > -1000){floor(num/200)*200}
  else if(num <= -100 && num > -200){floor(num/100)*100}
  else if(num < -1 && num > -100){floor(num/10)*10}
  else if(num >= -1 && num <= 1){floor(num/1)*1}  
  else if(num > 1 && num < 100){floor(num/10)*10}
  else if(num >= 100 && num < 200){floor(num/100)*100}
  else if(num >= 200 && num < 1000){floor(num/200)*200}
  else if(num >= 1000 && num < 2000){floor(num/1000)*1000}
  else if(num >= 2000 && num <=10000){floor(num/2000)*2000}
  else if(num > 10000 && num <=100000){floor(num/10000)*10000}
  else if(num > 100000){floor(num/20000)*20000}
}


###############################################################
## Rounds up to find the upper limit for the axis of a graph ##
###############################################################
## Created September 24, 2009
## Modified October 12, 2009

hiLim <- function(num){
  if(num < -100000){ceiling(num/20000)*20000}
  else if(num < -10000 && num >= -100000){ceiling(num/10000)*10000}
  else if(num <= -2000 && num >= -10000){ceiling(num/2000)*2000}
  else if(num <= -1000 && num > -2000){ceiling(num/1000)*1000}
  else if(num <= -200 && num > -1000){ceiling(num/200)*200}
  else if(num <= -100 && num > -200){ceiling(num/100)*100}
  else if(num < -1 && num > -100){ceiling(num/10)*10}
  else if(num >= -1 && num <= 1){ceiling(num/1)*1}  
  else if(num > 1 && num < 100){ceiling(num/10)*10}
  else if(num >= 100 && num < 200){ceiling(num/100)*100}
  else if(num >= 200 && num < 1000){ceiling(num/200)*200}
  else if(num >= 1000 && num < 2000){ceiling(num/1000)*1000}
  else if(num >= 2000 && num <=10000){ceiling(num/2000)*2000}
  else if(num > 10000 && num <=100000){ceiling(num/10000)*10000}
  else if(num > 100000){ceiling(num/20000)*20000}
}
