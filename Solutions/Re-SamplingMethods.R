# Tahir Muhammad | Saurabh Sant
# 1002537613     | 1002537613

# Set the directory to the data folder
setwd("/home/tahir/Downloads/")

# Read in the data
data = read.csv("lifetime.txt" ,header = FALSE)

# Display our data
x = data$V1

# Make everything into one column
array = numeric(length(data))
i = 1
for (i in data){
  array[i] = data$V1[i]
}

array



