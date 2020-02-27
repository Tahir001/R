####################################
#### Question 1.2, Assignment 2 ####
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
data = c(7,7,15,11,9,12,17,12,18,18,14,19,19,18,18,19,25,22,19,23,7,10,11,15,11)
treatments <- factor(rep(c("15% Cotton", "20% Cotton", "25% Cotton", "30% Cotton","35% Cotton"), each = 5))
dataFrame <- data.frame(x=treatments, y=data)
boxplot(data~treatments, xlab = "", ylab = "", data=dataFrame, main="Percentage of Cottons")

####################################
#### Question 1.3, Assignment 2 ####
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
with(dataFrame, tapply(data, treatments, mean))
mean(data)
with(dataFrame, tapply(data, treatments, var))
var(data)

####################################
#### Question 1.4, Assignment 2 ####
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
Anova <- anova(lm(data~treatments, data = dataFrame))
Anova

####################################
#### Question 1.6, Assignment 2 ####
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
linear_model <- lm(y~x - 1, data=dat)
MatrixA <- matrix(c(1, -1/4, -1/4, -1/4, 0, 1, -1/3, -1/3,-1/3, 0, 0 ,1, -1/2, -1/2, 0 , 0, 0, 1, -1), byrow=T, nrow=4)
summary(glht(linear_model, MatrixA), test = adjusted("none"))

####################################
#### Question 1.7, Assignment 2 ####
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
# Based on the values from the output above, we can get the SS for each contrast.
PhiSS1 = (-6.550)^2/0.25
PhiSS1
PhiSS2 = (-1.267)^2/(4/15)
PhiSS2
PhiSS3 = (1.4^2)/(0.3)
PhiSS3
PhiSS4 = (10.8^2)/(0.4)
PhiSS4

SST =  sum(PhiSS1, PhiSS2, PhiSS3, PhiSS4)
SST

####################################
#### Question 1.9, Assignment 2 ####
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
MatrixB = matrix(c(1, -1/4, -1/4, -1/4, 0, 1, -1/3, -1/3, -1/3), byrow=T, nrow=2)
summary(glht(fit,MatrixB), test = adjusted("bonferroni"))

####################################
#### Question 1.10, Assignment 2 ###
####      Muhammad Tahir       #####
####        1002537613         #####
####################################
TukeyHSD(aov(lm(data~treatments, data=dataFrame)), conf.level = 0.95)
CI_Comparisons = TukeyHSD(aov(lm(data~treatments, data=dataFrame)), facto = treatments, conf.level = 0.95)
plot(CI_Comparisons)

