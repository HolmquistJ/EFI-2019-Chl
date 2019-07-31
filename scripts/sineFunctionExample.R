



trigTime <- function(time, phase=.25,
                     a=14, b=0.01) {
  return(sinTime <- (b + a) + a * sin(pi*time*2 - pi*phase*2))
}

fractionalYear <- seq(0,3, by=0.05)

trigTimesY <- trigTime(fractionalYear)

plot(fractionalYear, trigTimesY)


     