# STA305 Assignment 3 | University of Toronto

###################################
####  Question 2, Assignment 3 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################

# Set the directory to the data folder
setwd("/home/tahir/Downloads/")

# Read in the data
data = read.csv("Problem2.csv",header = TRUE)

# Display our data
data

###################################
#### Question 2b, Assignment 3 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################
# We first need to make a data frame 
# Code is used from lecture slides 

data <- data.frame(y = data$Response, temp=data$Temprature, fabric = data$X)
data <- within(data,{
  x<- paste(temp, fabric)
})
# Print out our data to check
data 

average_temp = with(data, tapply(y, temp, mean))
average_fabric = with(data, tapply(y, fabric, mean))
average_cell = with(data, tapply(y, list(temp,fabric),mean))
grand_mean = mean(data$y)

temp_effect = average_temp - grand_mean
fabric_effect = average_fabric - grand_mean

Interaction_effect = matrix(0,4,3)
for (i in 1:4){
  for (j in 1:3)
    Interaction_effect[i,j] = average_cell[i,j]-temp_effect[i]-temp_effect[j]-grand_mean
}
Interaction_effect

###################################
#### Question 2c, Assignment 3 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################

with(data, interaction.plot(temp, fabric, data$y, col = c("red","blue"), main="Interaction Plot", xlab="Temperature Mean", ylab="Percent Shrinkage"))

###################################
#### Question 2f, Assignment 3 ####
####      Muhammad Tahir       ####
####        1002537613         ####
###################################
the_model = lm(y ~ temp*fabric, data = data)
summary(the_model)
anova(the_model)

#############################################
#### Question 2(part 7& 8), Assignment 2 ####
########      Muhammad Tahir       ##########
########        1002537613         ##########
#############################################
#The new model only contains the main effects. 
new_model = lm(y ~ temp + fabric, data = dyeeffect)
summary(new_model)
anova(new_model)





