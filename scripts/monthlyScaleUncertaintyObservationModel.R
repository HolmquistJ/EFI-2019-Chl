

# First run linear model on high res dataset

# Priors
# B
# tau.1
# tau.2
# 

# for (month in 1:M) {
#   for (i in 1:N) {
#     dailyChl[month, i] <- B0 + B1*Temp[i] + B2*Flow[i] + B3*[dailyChl[i-1]
#     y1[month, i] ~ dnorm(dailyChl[i], tau.1) T(0,)
#     daily.exceeded[month, i] <- ifelse(dailyChl[month,i]>= threshold, 1, 0)
#   }
#
#  # Create a derrived quantity
#  monthly.exceeded[month] <- sum(daily.exceeded[month, i])
#  
#  # Caluclate same model on monthly summary or pseudo sampled data
#  monthlyChl[month] <- B0 + B1*MTemp[i] + B2*MFlow[i] + B3*[monthlyChl[i-1]
#  y2[month] ~ dnorm(monthlyChl[i], tau.2) T(0,)
#  monthlyChlExceeded[month] <- ifelse(y2[month]>= threshold, 1, 0)
#   
#  logit(detection.prob[month]) <- B8 + B9*monthly.exceeded[month]
#  monthlyChlExceeded[month] <- ifelse(y2[month]>= threshold, 1, 0)
#
#  monthlyChlExceeded[month] ~ dbern(detection.prob[month]) 
# 
# }
# 
# 
# 


}


# 