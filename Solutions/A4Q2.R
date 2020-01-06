# Getting the data and reading it into a table    
mortgage_url = "https://mcs.utm.utoronto.ca/~nosedal/data/mortgage.txt"
mortgage_data=read.table(mortgage_url,header=TRUE)

# Question 2: One measure of the state of the economy is the amount of money homeowners pay on their mortgage each month. 
# To determine the extent of change between this year and 5 years ago, a random sample of 150 homeowners was drawn. 
# The monthly mortgage payments for each homeowner for this year and for 5 years ago were recorded. 
# (The amounts have been adjusted so that we’re comparing constant dollars.) 
# Can we infer that mortgage payments have risen over the past 5 years? (α = 0.05)

# Lets take a look at our data
head(mortgage_data)

# Attaching it for better access to variables
attach(mortgage_data)

#Check for normality
boxplot(X5YearsAgo,ThisYear)

# Check for co-relation
plot(X5YearsAgo,ThisYear)

# Since assumptions hold for the paired t test, we use that. 

# State the hypothesis
# H0: mu_diff = 0 , Ha: mu_diff > 0

# Assume null is true, and find the test statistic

t.test(ThisYear, X5YearsAgo, mu = 0, alternative = "greater", paired=T, conf.level = 0.95)

# Here we can see that the Pvalue is 0.06, and since pvalue > alpha = 0.05, we FTR the null. I.e. we can conclude that: 
# We do not have sufficient evidence to conclude that mortgage payments have increased over the last 5 years. 
