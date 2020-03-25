# Tahir Muhammad | University of Toronto
# Advanced concepts in R: Simulation, Inverse Transformation Method, Acceptance Rejection Method,Transformations & Monte Carlo Estimation
#    Stimulation of Random Numbers: How do we simulate the uniform distribution
#    How can we simulate any distribution? Two methods: 
#    I) Inverse Transfomation Method & II) Acceptance-Rejection Method
#    Transformation Methods: Convulution, Mixtures, Box-Mueller Transformation
#    Solving any integration problems: Monte Carlo Estimations 

####################################
#### Simulating random numbers #####
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
# How? Well, think about how every CDF is between 0,1. This implies that every CDF can be thought of as a Unif(0,1).
# However, the uniform(0,1) most likely does not look like the density you are trying to generate. Thus, what we do is 
# We simulate uniform(0,1) (can derive it as we shown above), and then apply the F inverse to get the density x! 
# Steps:  
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
# To simulate mixtures, we apply the composition technique (algorithm). It is briefly summarized below.
# Suppose you want to simulate from: F(x) = (1/15)X1 + (2/15)X2 + (3/15)X3 + (4/15)X4 + (5/15)X5
# sample intgers 1-4, with each integer having the probability of a weight in the equation above. 
# generate X1 - X4 from thier corresponding density and compute your equation. 

# Generating corresponding density of X1, X2, X3, X4, X5
n <- 10000
X1 <- rgamma(n,1,1)
X2 <- rnorm(n,0,1)
X3 <- rbeta(n,1,1)
X4 <- runif(n,0,1)
X5 <- rexp(n,1)

# Sample K accordingly. Note that sum of Prob(k1 to k4) = 1. 
K <- sample(1:5, size=n, replace=TRUE, prob=(1:5)/15)

# We now have a integer of 10,000 K, each sampled based on thier probabilities. 
# Let's compute the Mixture now
x <- 0
i <- 0
for i in(1:k){
  if k[i] == 1{
    x = x + k[i]*X1
  }
  if k[i] == 2{
    x = x + k[i]*X2
  }
  if k[i] == 3{
    x = x + k[i]*X3
  }
  if k[i] == 4{
    x = x + k[i]*X4
  }
  if k[i] == 5{
    x = x + k[i]*X4
  }
}
# We have now computed the density of X, which is a mixture of of gamma, normal, beta, uniform, and exponential densities! 
hist(x)


#########################################
######## Monte Carlo Stimulation ########
#########################################
# Monte Carlo estimation is a way to estimate/solve complex integration problems we are unable to compute/solve otherwise. 
# The generic algorithm always goes something like this: i) Genere x1, x2, ... xn from some distribution. ii) deliver thetahat = E(x)*constants/rvs, etc. 
# We can do is in 5 ways:
# 1) Estimate intervals which are bounded by [0,1] by the uniform distribution
# 2) Transform bounded intervals to be on [0,1] and then use (1)
# 3) Transform the integral to any PDF, and estimate using that. 
# 4) Hit or Miss / Indicator functions:  
# 5) Importance Sampling Method: Multiply your integrand g(x) by a function h(x), which has the following properties: 
#     a) We know it's density/it'seasy to simulate
#     b) We choose h(x) to be close as possible as g(x) in order to be computationally efficient 

# Example 1: Approximate the value of:  int(0,pi/3) sin(t) dt, and compare it with the exact value
n <- 100000
u <- runif(n,0,(pi/3))

# Compute exact value
f_x <- function(x){sin(x)}
theta <- integrate(f_x,0,pi/3)

# Compute estimated value
theta_hat <- (pi/3)*mean(sin(u))

# Print out exact and estimated values
theta
theta_hat

# Note that to get the variance of the estimate, we need alot of theta_hats, which in turn will help us estimate the variance.
# To understand the statement above intuitionally, recall the sampling distribution. 
y <- replicate(1000, expr = {
  u <- runif(n,0,(pi/3))
  theta_hat <- (pi/3)*mean(sin(u))
})
mean(y)
sd(y)


# Example 2: Write a Monte Carlo function to estimate P(X=1), where X ~ beta density(3,3).

# True value
theta <- rbeta(1,3,3) 

# Estimate 
theta_hat <- function(m){
  n <- m
  u <- runif(n,0,1)
  estimate <- 30*mean((u^2)*((1-u)^2))
  return(estimate)
}
theta_hat(1000)
  
# Example 3: Demonstrate the hit or miss approach. 











