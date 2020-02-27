###################################
#### Question 1c, Assignment 2 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################

# Original credits - to Professor, as the code is taken from the slides and modified. 

# function to generate from a logarithmic distribution
simPoisson <- function(n.gen,lambda){
  urandom <- runif(n.gen)
  sim.vector <- rep(0,n.gen)
  for(j in 1:n.gen){
    i <- 0
    p <- (exp(-lambda))
    F <- p
  while(urandom[j] >= F){
    p <- (lambda*p)/(i+1)
    F <- F+p
    i <- i+1
  }
  sim.vector[j] <- i
  }
  # output
  sim.vector
}
n.gen=1000
lambda = 2
out1 <- simPoisson(n.gen,lambda)
sim.mean=mean(out1)
sim.mean
sim.variance = var(out1)
sim.variance


###################################
#### Question 2c, Assignment 2 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################

for(i in 1:1000){
  exponential <- rexp(20,1)
  y[i]= max(exponential)
}
hist(y, main = "Q2C: Histogram of max from of an exponential(1) sample",ylab="Frequency", xlab= "Y",prob= T)
simulated_mean<-mean(y)
simulated_mean
sim.var <-var(y)
sim.var


###################################
#### Question 3b, Assignment 2 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################
n <- 1000
for(i in 1:n){
  random_unif <- runif(12,-1/2, 1/2)
  y[i]= sum(random_unif)
}
hist(y, main = "Q3B: Histogram of N(0,1)",ylab="Frequency", xlab= "Y",prob= T)
sim.mean <-mean(y)
sim.mean
sim.variance <-var(y)
sim.variance


###################################
#### Question 4c, Assignment 2 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################
# code taken from slides
n <- 1000
k <- 0
j <- 0
x <- numeric(n)
while(k<n){
  u <- runif(1)
  j <- j+1
  y = rnorm(1)
  f = ((1/5)*(((sin(6*y))^2)+ 3*(((cos(y))^2)*((sin(4*y))^2))+1))
  if(f>u){
    k = k+1
    x[k] <- y
  }
}
sim.mean = mean(x)
sim.mean
sim.variance = var(x)
sim.variance
hist(x, prob=T, main = "Q4C: Generating distribution of random variable, F by AR method")
