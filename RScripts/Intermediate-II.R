# Tahir Muhammad | University of Toronto
# Intermediate-II Topics in Statistics, Implemented in R. 
# Experimental Design.
# One and Two way Anova, Contrasts, Simple Linear Regression & MLR (Dummy coding model, cell-means model) 

# Recall that when we are comparing two means using any of the hypothesis testing techniques we have learnt
# so far, it is always about one group mean or two. Now, we introduce Anova, (One-way), which can be used to test 
# multiple group means at the same time. 

# Example: Test if the group means are the same for 4 different groups.  
values <- c(65, 54, 56, 60, 55, 58, 62, 65, 64, 67, 70, 74, 60, 64, 68, 70)
groups <- c(rep("a1",4), rep("a2", 4), rep("a3", 4), rep("a4", 4))
dataset <- data.frame(x = groups, y = values)
# Display our constructed relational data table
dataset
# Find group means
with(dataset, tapply(values, groups, mean))
# ybar1 = 59, ybar2 = 60, ybar3 = 68.75, ybar4 = 65.5
# Quick comparison of the means
boxplot(values ~ groups)
# Anova: Method 1
anova(lm(values~groups,data = dataset))
# Anova: Method 2 (Both give same output)
anova(aov(values~groups, data=dataset))

