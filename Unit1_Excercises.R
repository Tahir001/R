#R Weekly Excerices Unit 1
#Excercise #1
tobinQ <- read.csv("/home/tahir/Desktop/STA258/R/tobinQ.csv")
summary(tobinQ)
hist(tobinQ$USA)
boxplot(tobinQ$USA)
qqplot(tobinQ$USA, y = numbers)
#f) Skewed, to the right
#since it is 50 observations and the distribution does not look normal, we cannot conclude that it comes from a normal

#Excercise #2
dens <- read.csv("/home/tahir/Desktop/STA258/R/density.csv")
summary(dens)
mean(dens$earth)
hist(dens$earth)
boxplot(dens$earth)
#No evidence that the data is NOT normally distributed. 

Excercsies #3
clonel <- read.csv("/home/tahir/Desktop/STA258/R/colonel.csv")
summary(clonel$age)
mean(clonel$age)
hist(clonel$age)
qqnorm(clonel$age)
qqline(clonel$age)
boxplot(clonel$age)
#skeweto to the left
#Evidence shows that this data is not from the normal  distribution

#Excersice #4
Q4 <- rnorm(60, 3, 4) #This generates sample data coming from Normal dist
summary(Q4)
mean(Q4)
sd(Q4)
median(Q4)
hist(Q4)
qqnorm(Q4)
qqline(Q4)
boxplot(Q4)
#The sample shows no evidence that it is NOT normal

Excercise #5 #RUn these commands and ask about skewness.  (Right vs left/ Positive vs Negative)
Q5 <- rgamma(40, 3, 4)
summary(Q5)
mean(Q5)
sd(Q5)
median(Q5)
hist(Q5)
qqnorm(Q5)
qqline(Q5)
boxplot(Q5)

#The Graphs shows no evidence of it being normal and seems positively skewed

#Excercise #6
Q6 <- runif(80, -3, 4)
summary(Q6)
mean(Q6)
sd(Q6)
median(Q6)
hist(Q6)
qqnorm(Q6)
qqline(Q6)
boxplot(Q6)

#The graphs show non-normal sample. 
