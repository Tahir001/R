# The following R code is for Unit 2 Excercises
# We do Hypothesis Testing with Regression, Anova, and contrasts. 

# Setting directory
setwd("/home/tahir/Downloads/")

# Read the data in
farming_data <- read.csv("farmingfertilizerdata.csv")
farming_data

# Attach the data for easier access
attach(farming_data)

# Conduct a pooled t test for the means. 
# H0: u1 - u2 - u3 - u4 = 0
# H1: u1 - u2 - u3 - u4 =/= 0
t.test(Fertilizer~CropYield, var.equal=TRUE, data=farming_data) # Wont work for groups > 2
# Workaround for groups > 2
y <- c (65 , 54 , 56 , 60 , 55 , 58 , 62 , 65 , 64 , 67 , 70 , 74 , 60 , 64 , 68 , 70)
treatments <-factor(rep(c("a1","a2", "a3", "a4"), each=4)) 
with(farming_data, pairwise.t.test(y, treatments, p.adj = "none"))

# 6) Fit the linear Model
reg_model <- lm(CropYield ~ Fertilizer, farming_data)
summary(reg_model)

# 8) Anova Model
anova(reg_model) 

# Q11) Contrasts
# Conduct mean of each group
with(farming_data, tapply(y, treatments, mean))
# Testing all 4 contrasts at the same time

fit <- lm (y ~ x-1 , data=farming_data )
L <- matrix (c(
  1 , 0 , -1, 0 ,
  0 , 1 , -1, 0 ,
  1 , 1 , -2, 0 ,
  1 , 1 , -3, 1 ) , byrow=T, nrow=5)
require( multcomp )
library( multcomp )
summary(glht (fit ,L), test=adjusted("none"))



