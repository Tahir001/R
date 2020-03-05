# Tahir Muhammad | University of Toronto
# Advanced concepts in R: Simulation, Inverse Transformation Method, Acceptance Rejection Method, Monte Carlo Estimation & Transformations
#    Stimulation of Random Numbers: How do we simulate the uniform distribution
#    How can we simulate any distribution? Two methods: 
#    I) Inverse Transfomation Method & II) Acceptance-Rejection Method
#    Transformation Methods: Convulution, Mixtures, Box-Mueller Transformation
#    Solving any integration problems: Monte Carlo Estimations 

####################################
#### Stimulating random numbers ####
####################################
# How does a computer stimulate random numbers?
# Linear Congruential Generators -> uses a mathematical formula, r_i = ar_i + b(mod d)
seed = 1 # Setting seed to be able to replicate: must be positive
a = 5 # the multiplier
b = 3 # shift
d = 16 #modulus
n = 20 # lenght of run
r = numeric(n)
r[1] = seed
for (i in 1:(n-1)){
  r[i+1] = (a*r[i]+b) %% d
}
# r is now an array filled with random numbers 
r
# It is usually good practice to generate numbers over (0,1). 
# Why do we do this? --> Because once you have uniform (0,1), you are able to generate any distribution.
# Make the random generated numbers over (0,1)

# Initialize
a = 1093; b = 18257; d = 86436; seed = 2
m = 10000; r = numeric(m); r[1] = seed
# Generate random numbers using Linear Congruential Generator
for (i in 1:(m-1)){
  r[i+1] = (a*r[i]+b) %% d
}
# Make all numbers between 0,1
u <- (r+0.5)/d
# Plot to see that these are unifomaly distributed! -> Our numbers where random on the interval (0,d), or (0,1) for u. 
hist(u)
hist(r)
# The above holds a powerful result. This is one way the r-uniform distribution can be generated in R, under the hood. 

# To use R's built in commands, we can just do:
x <- runif(n=10000,min=0,max=1)
hist(x)

########################################
#### Inverse Transformation Method #####
########################################
# Given a density function, can generate it's  distribution. 
# How? For Continous Distributions:
#   i) Generate a random variable from U(0,1)
#   ii) Deliver x = Fx^-1 (u) 

# Example: Find the distribution of f(x) = 3x^2, where 0 < x < 1
n = 1000
u <- runif(n,0,1)
x <- u^(1/3)
hist(x, prob=TRUE, main = bquote(f(x) == 3*x^2), col="green")
# Sample
y <- seq(0, 1, .01)
# Density Curve
lines(y, 3*y^2, col="black") 

# Inverse Value Method; Discrete Case: Recursive

#########################################
######## Mixtures & Convulutions ########
#########################################
# Convulution is a transformation method which allows you to find the distribution of the sum of two random variables.
# Example: Adding hieght of class 1 + hieght of class 2. (They must come from the same distribution)
# Some common distributions are convolutions! For example, Binomial is a sum of bernoulli -> convulution. Exponential -> Gamma, .. etc.
# Mixtures is a derived density from one or more distributions. Ex: Adding male distribution to female distribution
# The sum of the wieghts must add up to 1. Ex: 05Fx_1(x) + 0.3Fx_2(x) + 0.2F_x3(x) => 0.5+0.2 +0.3 = 1. 

# Convulution Example: What is the density of S, where S = X1 + X2 + X3, and X1 ~ N(0,1), X2 ~ N(11,20), & X3 ~ N(-1,1) ?
n <- 10000
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,11,20)
x3 <- rnorm(n,-1,1)
S <- x1 + x2 + x3
hist(S) # We can see the resulting density is also normal. 

# Mixtures Example: 
# To simulate mixtures, we apply the composition technique. It is briefly summarized below.
# 


# Monte Carlo Stimulation






