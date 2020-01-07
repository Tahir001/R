# A Quick Tutorial on R
# Tahir Muhammad | University of Toronto
# Introduction to R ! (First Level)

#Get your working directory by getwd(), and set your working directory by setwd("---File Path---) command. 
#rm(list = ls()) //Clears your current working enviroment

# Arithmetic Operators
5+1
6-9
11*11
4/5

# Special numbers
pi
exp(1)
exp(2)

# Basic vector operation
x <- c(1,4,6,-8,5)
mean(x)
var(x)
sd(x)
var(x)
median(x)
sum(x)
prod(x)

# Variables
x <- 12
# Storing a vector into a variable, y 
y <- c(1,2,3,4,5)
# Adding an element to a vector
x+y
# Multiplying by a scalar to vector y
x*y
x %*% y

# Plotting 
# Qualitative data: Make a frequency table
x = c( "yes", "no", "yes", "no", "yes", "yes", "no", "yes", "yes", "no")
table(x)
# Partition the space into a matrix (row,columns)
par(mfrow=c(2,2))

# Bar chart
barplot(table(x))
barplot(table(x), col=c("red","blue"))

# Pie chart 
par(mfrow=c(2,1)) #2 rows, 1 column
pie(table(x))
pie(table(x), col=c("green","purple"), main="People who like starbucks at UTM")

# Probability distribution
#The command for Normal distribution is pnorm(n, mean, standard deviation).
#Example 1) Probability (Z < 1.96)? #This is the CDF
pnorm(1.96, 0, 1) #Takes in a value and gives you probability
#Example2) Let X~N(25,100), what is P(10 < X < 30)?
pnorm(30, mean = 25, sd = 10) - pnorm(10, mean = 25, sd = 10)
#Note: When you put in the value in pnorm by default, it thinks its the standardized value.
pnorm(1.96)

#Normal Distribution Quantile
#Takes probabilties and give u values. i.e: "What is the critical value given this much probability and it's distribution?" 
qnorm(0.975, mean =0, sd =1)  
#The default gives you the value from a standard z table 
qnorm(0.95) 

#The command for Gamma distribution is pnorm(n, mean, standard deviation).
#X~gamma(5,5), what is P(1 < X < 6)?
pgamma(6, shape = 6, scale = 6) - pgamma(1, shape = 6, scale = 6)

#Stimulating data and making graphs
# The rnorm, rgamma, r-distribution function does that for us. 
#To assessing normality look at Histograms, QQPlot, AND Boxplot
z = rnorm(10000)
par(mfrow=c(1,3)) #Showing multiple plots
hist(z)
qqnorm(z) #QQ plot
boxplot(z) 

# Gamma distribution
g <- rgamma(100000, shape=2, scale=0.5) #Shape is called the alpha, and beta is scale
par(mfrow=c(1,3)) #Showing multiple plots
hist(g)
qqnorm(g) #QQ plot
boxplot(g) 

# T-distribution
t <- rt(100000, df=6)
par(mfrow=c(1,3))
hist(t)
qqnorm(t)
boxplot(t)

# Importing and Reading CSV files
reading = read.csv("/home/tahir/Downloads/reading.csv", header = TRUE) 
reading #displays your data. Note: It's / for linux, and \ for windows. 

reading$score #Gives us the column score
reading$treatment #Gives us the column Treatment

control = reading[reading$treatment == 'Control',] # Gives us all the things that were controlled
treated = reading[reading$treatment == 'Treated',] 

mean(control$score) 
mean(treated$score)

# Trees dataset
data(trees)
x = trees
summary(x)

#Lets assess the normality of the Trees data set (i.e of Volume, Hieght and Girth)
par(mfrow=c(3,3))
#Girth
hist(x$Girth)
qqnorm(x$Girth)
boxplot(x$Girth)

# Volume
hist(x$Height)
qqnorm(x$Height)
boxplot(x$Height)

#Hieght
hist(x$Volume)
qqnorm(x$Volume)
qqline(x$Volume)
boxplot(x$Volume)
#Conclusion: Height seems normal, volume does not seem normal. 

# Some useful R librarys 
#ggplot2 = Graphics in r
#tidyr = cleaning up info
#stringr = text info
#httr = Website data
#shiny = Interactive applications for websites
