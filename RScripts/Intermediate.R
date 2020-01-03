# Intermediate topics in R.
# Confidence Intervals & Hypothesis Testing 
# Note that there is no direct function which calculates just the confidence intervals, hence we will need to create it from scratch for each case.
# However, we also know that Confidence Intervals are always point_estimate +/- Margin of Error.


#######################################   CONFIDENCE INTERVALS #######################################

# One Sample: Large Samples

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

# Hypothesis Test: 
# 1) H0: mu = 113,000 & H1: mu =/= 113,000
# 2) Calculate your test statistic. 
t.test(salary_data, mu=113000, alternative="two.sided")
# 3) Make a decision, then make a real-world conclusion. 
#Here we can see that since the p-value is below 0.05, we have statistical evidence that the average salary of data scientists is not $113,000.

#Some other cases in R
# Note that by default, R does a two-sided test. i.e the following is same as above. 
t.test(salary_data, mu=113000)

# Lower tailed test
t.test(salary_data, mu=113000, alternative="less")

# Upper tailed test
t.test(salary_data, mu=113000, alternative="greater")

# One Sample; Small Sample Size
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

# Hypothesis Test

# Question: Estimate if the email open rate of one of our firm's competitor is above your company's. 
# Open rate data for a competitior firm
open_rate_data <- c(0.26,0.23,0.42,0.49,0.23,0.59,0.29,0.29,0.57,0.40)

# Test at a significance level of 0.05. (Assume it is 0.05 when a significance level is not given)
# 1) H0: mu <= 0.40 & H1: mu > 0.40
# 2) Calculate your test statistic
# The Student's T distribution works here because of small small, and variance unknown. 
t.test(open_rate_data, mu = 0.40, alternative="greater")
# 3) Make a decision: Since Pvalue > alpha, we FTR H0. 

# Two Samples; Dependent --> Paired Data, Positive Co-relation, pairs are independent. 
# Usually use this test for when researching same subject over time -> Before & After situations, positive co-relation.
# Or when looking at cause and effect situations 
# the test is same as above for one mean, except the difference is:
difference = before - after
dbar = mean(difference)
# Construct CI like above, i.e:
CI <- dbar +/- (z_alpha/2 or t_n-1;alpha/2) * (std/sqrt(n))

# Hypothesis Testing
# Test to see if a drug companies claims are true about thier new drug increasing magnesium levels in patients. 

# Getting our data
before <- c(2,1.4,1.3,1.1,1.8,1.6,1.5,0.7,0.9,1.5)
after <- c(1.7,1.7,1.8,1.3,1.7,1.5,1.6,1.7,1.7,2.4)

# Check to see if our data is co-related
cor(before,after)
plot(before,after)

# H0: mu_d >= 0 & H1: mu_d < 0

# Two samples, paired t-test.
t.test(before,after,paired=T)

# Here we can conlcude that at 5% significance level, we have sufficient evidence that the company's new drug does indeed increase patients magnesium levels. 

# Lets construct a CI for this example aswell. 
difference <- c( before - after)

# Calculate the standard error
dbar <- mean(difference)
s <- sd(difference)
n <- length(difference)
SE <- s/(sqrt(n))

# The 95% CI
lower_tail <- dbar - qt(0.975,9)*SE
upper_tail <- dbar + qt(0.975,9)*SE

# As we can see, this matches our CI which we got from the built-in t.test command! Nice =). 

#Two Samples; Independent. Case I) Population variance is known
# Calculate the 99% confidence interval for the difference of two means. 
# Sample 1 
n_1 <- 100
xbar <- 58
sigma_1 <- 10

# Sample 2
n_2 <- 70
ybar <- 65
sigma_2 <-5

# Calculate Standard error
var1 <- (sigma_1^2)/n_1
var2 <- (sigma_2^2)/n_2
standard_error <- sqrt((var1 + var2))

Lower_tail <- (ybar - xbar) + (qnorm(0.005))*(standard_error)
Upper_tail <- (ybar - xbar) - (qnorm(0.005))*(standard_error)

#Hence, we conclude that we are 99% confidence that the true difference between engineering and history students scores is between 4 to 10 points. 

#Two Samples; Independent. Case II) Population variance is unknown
# Question: Estimate the difference in price of apples between NewYork and LA.
# Lets get some data
NY_PriceOfApples <- c(3.80, 3.76, 3.87, 3.99, 4.02, 4.25, 4.13, 3.98, 3.99, 3.62) # Sample 1 
LA_PriceOfApples <- c(3.02, 3.22, 3.24, 3.02, 3.06, 3.15, 3.81, 3.44) # Sample 2

# Calculate the 95% Confidence Interval
# Sample 1
xbar <- mean(NY_PriceOfApples)
n1 <- 10
s1 <- sd(NY_PriceOfApples)

# Sample 2
ybar <- mean(LA_PriceOfApples)
n2 <- 8
s2 <- sd(LA_PriceOfApples)

# Pooled variance
PooledVar_numerator <- ((n1-1)*(s1^2)) + ((n2-1)*(s2^2))
PooledVar_denomenator <- n1 + n2 - 2
PooledVariance <- PooledVar_numerator/PooledVar_denomenator

# Standard Error
SE <- sqrt(((PooledVariance/n1) + (PooledVariance/n2)))

# T-value. DOF = n1 + n2 -2 = 16
t_value <- qt(0.025, 16, lower.tail = FALSE)

# 95% CI
Lower_tail <- (xbar - ybar) - (t_value*SE)
Upper_tail <- (xbar - ybar) + (t_value*SE)

# We can see that the mean difference is between (0.47, 0.92). Since the interval does not include 0, we can conclude that we are 
# 95% confident NewYork Apples are more expensive than the apples in LA


#######################################  HYPOTHESIS TESTING ####################################### 

# Let's shift our focus on Hypothesis testing. The different cases of Hypothesis testing (situations where they are applicable), is the
# same as thier counter-part confidence Intervals. Hence, the cases we will be covering are: 
# Single Populaion/One Samples: Large & Small Sample Size for One mean
# Multiple Populations/ Two Samples: Dependent Samples, and Independent Samples (I) Variance Known, (II) Variance unkown but assumed equal
# and (III) Variance unknown and not equal. Lets start with our first case. 

# One Sample; Large Sample Size 
# Lets get some data



# II) One mean Small Samples
# III) Two samples, mean
# IV) 
# V)
# VI)


