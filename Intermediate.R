# Intermediate topics in R.
# Confidence Intervals & Hypothesis Testing 
# Note that there is no direct function which calculates just the confidence intervals, hence we will need to create it from scratch for each case.
# However, we also know that Confidence Intervals are always point_estimate +/- Margin of Error. 


######## CONFIDENCE INTERVALS ########

#Lets get some data. Here is the salary of 20 data scientists. 
salary_data = c(117313, 104002, 113038, 101936, 84560, 113136, 80740, 100536, 105052, 87201, 91986, 94868, 90745, 102848, 85927, 112276, 108637, 96818, 92307, 114564, 109714, 108833, 115295, 89279, 81720, 89344, 114426, 90410, 95118, 113382)

# Calculate a 95% Confidence Interval for the given dataset. 
# Since n = 30, we can use CLT to approximate this to a normal distribution.
x_bar <- mean(salary_data)
s <- sd(salary_data)
n <- 30
# Getting the Z_alpha/2.
# Recall that R always gets the area under the curve from right to left.
# Since we have a two tailed test, we must account for it accordingly.
qnorm(0.975) # Implies 95% probability in between. 
#Thus, our 95% CI is:
Lower_tail <- x_bar - qnorm(0.975)*(s/sqrt(n))
Upper_tail <- x_bar + qnorm(0.975)*(s/sqrt(n))
# Hence we conclude that we are 95% confident that the true average salary of a data scientist is between 96-104k. 

# Confidence Intervals; Small Sample Size 
# Suppose now we had data of n = 9. 
salary_data2 <- c(78000, 90000,75000,117000, 105000, 96000, 89500, 102300, 80000)
# Find the appropriate statistic, taking into consideration the degrees of freedom (if applicable) for 99% confidence
xbar <- mean(salary_data2)
std <- sd(salary_data2)
n <- 9
df <- 8
# Find the 99% CI
# value of t with 0.05% in each tail. Find t_n-1;alpha/2
tvalue <- qt(0.005, 8, lower.tail = TRUE)
# This gets us P(X <= x), i.e the quantile which has probability less than & equal to 0.005.
# Note that since t-distribution is symmetric, we can just take the positive value of that quantile
# Now, we can construct our 99% CI.
Lower_tail<- xbar + tvalue*(std/sqrt(n))
Upper_tail <- xbar - tvalue*(std/sqrt(n))
# Display our CI
Lower_tail
Upper_tail
# Hence we can be 99% confident that the mean salary for a data scientist is between $76,950 to $108,115. 
# Notice how the interval got bigger as we increased the confidence level. 


######## HYPOTHESIS TESTING ########

# A hypothesis test Always concludes 4 steps: 1) Determine H_0, H_a, 2) Find Test Statistic, 3) Determine the p-value and 4) Conclude. 
# In R, we modify the steps a bit to 

