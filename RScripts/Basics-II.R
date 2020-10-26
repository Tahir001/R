# Tahir Muhammad | University of Toronto
# Basics II: Data analysis and visualization
# Introduction to Statistical Reasoning and Data Science -- STA130.
# Full course Implemented in R with Examples, and used for the purpose of teaching others. 

# Installing a package with its dependencies
#install.packages("tidyverse", dependencies = TRUE)

# Loading a package
library(dplyr)
library(ggplot2)
library(tidyverse)

# Lets load some data
getwd()
setwd("/home/tahir/Desktop/TA/Teaching/Datasets")
coffee <- read.csv("coffee_ratings.csv")

# Seeing your data
View(coffee) # Views and opens it on another page
glimpse(coffee) # A transposed version of print: columns run down the page, and data runs across.
head(coffee) # Displays the top 5 rows in your dataframe

# The pipe operator in R
# This allows us to redirect output. The commands before the pipe, %>%, is consired as input to the command after the pipe.
head(coffee) %>% glimpse()  # Notice how the number of rows has changed. 

# If we look at our data, we have several different data types of columns
# Data types in R include: 
# logical vectors <lgl>, contain TRUE or FALSE.
# double vectors <dbl>, contain real numbers.
# integer vectors <int>, contain integers.
# character vector <chr>, contain strings made with "".
# dates <dt>, record a date.
# factors <fct>, which are used to represent categorical variables can take one of a fixed and known set of possible values (called the levels).
# All of these combined can be made into a tibble / Dataframe. 
glimpse(coffee)

# Lets visualize some of the variables. 
#What kind of variable is species? Nominal categorical variable# What kind(s) of plot is appropriate to visualize the distribution of this variable? Barplot!# Let's visualize!
ggplot(data=coffee,aes(x=farm_name, color=species, fill=farm_name)) +
  geom_bar() +
  labs(x="Species of coffee bean") +
  coord_flip()
