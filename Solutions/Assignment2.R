# Assignment 2 Soutions
# University of Toronto | Applied Statistics
# Tahir Muhammad

# Question 1

# To generate a random integer between (1,100), inclusive 
randomInteger <- sample(1:100,1)
randomInteger

# Part A
#Generating a random number
mymean<-round(runif(1,1,100))
mymean

# Part b, Generating a random sample of size 25 from Normal distribution 
Muhammad <- rnorm(25,mymean,10)
Muhammad

#Getting Mean of my random sample, and adding it to the margin of error.
lower<-mean(Muhammad)+qnorm(0.01)*10/sqrt(25)
upper <- mean(Muhammad)-qnorm(0.01)*10/sqrt(25)
lower
upper

# Confidence intervals dont exist in r but we can do...
# Average +- Z*(sigma/sqrt(n))

# Question 2
mycount2 <- 0
for (i in 1:50){
  mysample <- rnorm(20,mymean,10)
  aa <- mean(mysample) + qnorm(0.1)*10/sqrt(20)
  bb <- mean(mysample)+qnorm(0.9)*10/sqrt(20)
  if(aa<mymean & mymean<bb) mycount2<-mycount2+1
}
mycount2

# Question 3
mymean
mycount3<-0
for (i in 1:75){
  mysample<-rnorm(15,mymean,10)
  cc <- 2*pnorm(abs(mean(mysample)), mymean,10/sqrt(15))
  if (cc<=0.04) mycount3<-mycount3+1
}
mycount3

# Explanations of the code are in the A2 PDF.
