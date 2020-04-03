# Tahir Muhammad | Saurabh Sant
# 1002537613     | 1002537613

# Set the directory to the data folder
setwd("/home/tahir/Downloads/")

# Read in the data
data = read.table("lifetime.txt",header = FALSE)

# Display our data
data

# For easier access to the columns 
attach(data)

# Make the data into one column
#library(tidyr)
# unite(data, remove = FALSE, na.rm = FALSE)

library(dplyr)
data <- coalesce(V1,V2,V3,V4,V5)





# Practice
LSAT=c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
GPA=c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.8,2.96)
print(cor(LSAT, GPA))
#set up the bootstrap
B <- 10           #number of replicates
n <- length(LSAT)   #sample size
R <- numeric(B)     #storage for replicates
#bootstrap estimate of standard error of R
for (b in 1:3) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  print(i)
  LSAT.boot <- LSAT[i]       #i is a vector of indices
  print(LSAT.boot)
  GPA.boot <-  GPA[i]
  print(GPA.boot)
  R[b] <- cor(LSAT.boot, GPA.boot)
  print(R)
  print(length(R))
}

print(R)
cor(LSAT,GPA)
hist(R)

#output
print(se.R <- sd(R))
hist(R, prob = TRUE)

