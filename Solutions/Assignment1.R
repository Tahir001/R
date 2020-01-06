# Tahir Muhammad
# Unviversity of Toronto | Applied Statistics
# Assignment 1 Solutions - Code

# Question 1)
muhammad_means10 <- c(rep(NA,5000))
muhammad_means10
for (i in 1:5000){
  muhammad_means10[i] <-mean(rgamma(10,1,1))
}
par(mfrow=c(1,3))
hist(muhammad_means10, main="muhammad_means10: Histogram")
qqnorm(muhammad_means10, main="muhammad_means10: QQ Normal")
qqline(muhammad_means10)
boxplot(muhammad_means10, main="muhammad_means10: Boxplot")

mean(muhammad_means10)
sd(muhammad_means10)

# Question 2) 
muhammad_means50 <- c(rep(NA,5000))
for (i in 1:5000){
  muhammad_means50[i] <-mean(rgamma(50,1,1))
}
par(mfrow=c(1,3))
hist(muhammad_means50, main="muhammad_means50: Histogram")
qqnorm(muhammad_means50, main="muhammad_means50: QQ Normal")
qqline(muhammad_means50)
boxplot(muhammad_means50, main="muhammad_means50: Boxplot")

mean(muhammad_means50)
sd(muhammad_means50)

# Question 3)
# Discussed in PDF. 
