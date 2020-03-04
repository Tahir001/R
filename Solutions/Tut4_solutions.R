# Tahir Muhammad | University of Toronto
# Monte carlo integration 

# Question 1) 
# Exact value
integrand <- function(x){ sin(x) }
theta <- integrate(integrand, lower = 0, upper = pi/3)

# Estimate
m <- 10000
x <- runif(m, 0, (pi/3))
theta.hat <- (pi/3)*(mean(sin(x)))

# Compare. We can see they are both extremely close. 
print(theta.hat)
print(theta)

# Question 2)
# Monte carlo estimate 
# Estimate
m <- 10000
t <- runif(1,0,100000)
x <- runif(m, 0, t)
theta.hat <- 0.5 + mean(1/(sqrt(2*pi))*(exp(-((x)^2)/2)))
theta.hat







