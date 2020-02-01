# Tahir Muhammad | University of Toronto
# Advanced concepts in R: Stimulation, Inverse Transfomation Method, Monte Carlo, and the Apply family of functions. 

# Stimulating random numbers 
# How does a computer stimulate random numbers?
# Linear Congruential Generators -> uses a mathematical formula, r_i = ar_i + b(mod d)
set seed 1000 # Setting seed to be able to replicate
a = 5
b = 3
d = 16
n = 20
r = numeric(n)
for (i in 1:(n-1)){
  r[i+1] = (a*r[i]+b) %% d
}
# R is now an array filled with random numbers 
r

# Inverse Transformation Method. 


