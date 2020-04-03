# STA312 | Professor Luai Al Labadi | Project 3
# BY:
# Tahir Muhammad | 1002537613
# Saurabh Sant   | 1002537613

# Set the directory to the data folder
setwd("/home/tahir/Downloads/")

# Read in the data
data = read.table("lifetime.txt",header = FALSE)

# Display our data
data

# For easier access to the columns 
attach(data)

# Make the data into one column
#library(tidyr)
# unite(data, remove = FALSE, na.rm = FALSE)
library(dplyr)
# Our data
lifetime <- coalesce(V1,V2,V3,V4,V5)

#########################################
######        Course: STA312       ######
###### Final Project: Question 3A  ######
###### Muhammad Tahir | 1002537613 ######
###### Sant Saurabh   | 1002434047 ######
#########################################

# Find E(theta), the expected value
theta <- mean(lifetime) # This is the point estimate
print(theta)

# Q: Find the confidence Interval for Theta
# The confidence interval takes the form
# thetahat +- criticalValue * se(thetahat). 
# We don't know the se(thetahat) as it is just one value. 
# Thus, we resample using bootstrample, and find se(theta).

# seed for replicapability
set.seed(2020)

# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(lifetime)   # sample size
R <- numeric(B)         # storage for replicates

# Bootstrapping Estimate of standard deviation
for (b in 1:B){
  i <- sample(1:n, size = n, replace=TRUE)
  lifetime.boot <- lifetime[i]
  # Store the mean of each sample
  R[b] <-mean(lifetime.boot)
}
# R is a vector of average lifetime for each sample
print(R)
boostrapped.theta <- mean(R)
bootstrapped.sd <- sd(R)
# Print bootstrapped results
print(c(boostrapped.theta, bootstrapped.sd))

# Now we need  to find the last piece of the confidence interval, the critical value. 
# For the critical value, we look at our sampling distribution of the thetas and see if it is normal. 
par(mfrow=c(1,3))
hist(R)
qqnorm(R)
boxplot(R)

# After looking at the plots, we can conclude normality seems to hold. 
# hence, we can apply CLT and the normal distribution for the critical values. 

Lower_tail <- theta - qnorm(0.975)*(bootstrapped.sd/sqrt(length(lifetime)))
Upper_tail <- theta + qnorm(0.975)*(bootstrapped.sd/sqrt(length(lifetime)))

# Thus, our 95% CI for theta is: 
print(c(Lower_tail,Upper_tail))
