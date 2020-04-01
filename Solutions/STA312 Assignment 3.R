# STA312 Assignment 3 | University of Toronto

######################################
#####  Question 1e, Assignment 3 #####
#####      Muhammad Tahir       ######
#####        1002537613         ######
######################################

# We want to generate a Empirical CDF for the Beta(1,1) distribution, and compare it to the true CDF.
m = 100
x <- rbeta(m, 1, 1)
Fn <- ecdf(x)
t <- seq(0,1,length=m)
curve(pbeta(x,1,1), col = 'black', xlim=c(0,1),lwd=2)
lines(Fn, verticals = TRUE, do.points = FALSE, col="red", lwd=2)
title("ECDF vs Beta(1,1) CDF")
legend(0, 1, legend=c("ECDF", "Beta(1,1)"), lty=c(2,1),col=c("red","black"), cex=0.8)
# Here, we can see that the two distributions are pretty much the same. 

######################################
#####  Question 3b, Assignment 3 #####
#####      Muhammad Tahir       ######
#####        1002537613         ######
######################################

# Sample Size
m = 10000
# Get the Chi-squared
x = rchisq(m, df=8)
counter = 0
for (i in 1:m) {if (x[i] > 20.5)
  counter = counter + 1}
# The estimated value
estimate = counter/m
# The actual Value
actual = 1 - pchisq(20.5,df = 8)
print(estimate); print(actual)
# Find the Variance of the estimate. 
g_x <- (x>20.5)
variance <- mean((g_x - mean(g_x))^2)/m
print(variance)
cdf <- mean(g_x)
c(cdf, variance)
#The confidence interval
c(cdf-1.96*sqrt(variance), cdf+1.96*sqrt(variance))


######################################
#####  Question 3d, Assignment 3 #####
#####      Muhammad Tahir       ######
#####        1002537613         ######
######################################

# Estimating Theta with IS function
y <- rexp(10000, rate = 1/8)
#weight = g/f, where f has exp(8) and g has chisq(8)
weights = (y^3)*exp(-3*y/8)/12
indicatorIS = c(rep(0,10000))
for(i in 1:10000){if (y[i]>20.5){
  indicatorIS[i]=1}}
valuesIS = indicatorIS*weights
IS_est = mean(valuesIS)
exact = 1 - pchisq(20.5, df = 8)
print(IS_est); print(exact)
# Variance
ISvariance <- var(valuesIS)/10000
print(ISvariance)
#Confidence Interval
c(IS_est-qexp(0.025,8)*sqrt(ISvariance), IS_est+qexp(0.975,8)*sqrt(ISvariance))
