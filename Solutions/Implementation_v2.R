# Luai Al Labadi # 
# Tahir Muhammad # 
# Implementation of Testing Exponenentiality based on KL Information
# with Type II censored data by Sangun Park

# Import relevant libraries
library(pcensmix)

# Generate the exponential distribution
X <- rexp(10000,1)
# Sort the data points for ordered statistics 
X <- sort(X)
compute_Hbar <- function(m,n,r){
  
  # Compute the subtraction at the end of Hbar(m,n,r)
  subtraction <-1* ((1 - (r/n))) * (log(1-(r/n)))
  
  # Compute stuff inside of the sum, and store it
  summation <- numeric(r)
  for(i in 1:r){
    
    # If i + m > n --> then make it n 
    if (i+m >n){
      j <- n
      summation[i] <- log( (n/2*m)*(X[j] - X[i-m]) )
    }
    # if i -m < 1 --> then make it 1 
    else if (i-m < 1){
      k <- 1
      summation[i] <- (log( (n/2*m)*(X[i+m] - X[k]) )) 
    }
    else{
      summation[i] <- (log( (n/2*m)*(X[i+m] - X[i-m]) ))
    }
  }
  summed <- sum(summation)
  
  # Put it all together
  Hbar <- (1/n) * summed - subtraction
  
  return(Hbar)
}

n = 30; r = 15; m = 3

T_Hbar <- -1 * compute_Hbar(m,n,r)
sum2 <- numeric(r)
for(i in 1:r){
  sum2[i] <- X[i] + (n-r)*X[r]
}
summed2 <- sum(sum2)

# If this is close to zero, it means the data is from Exponential Distribution
TestStatistic <- T_Hbar + (r/n) *( (log((1/r)*summed2)) + 1)

