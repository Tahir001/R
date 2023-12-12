#########################################
######        Course: STA315       ######
######      Sparse Group Lasso     ######
###### Muhammad Tahir | 1002537613 ######
#########################################

# Uncomment to install the packages, as needed.
# install.packages("SGL")
# install.packages("finalfit")
# install.packages("dplyr")
library(SGL)
library(finalfit)
library(dplyr)

# Set seed for replication
set.seed(2021)

# Simulated Data -- Section 6
# Replicate the simulation process. Define the criterion.
# Lets focus on the 1, horizontal row first. 

##############################################################################
###################### First row in Table (1) ################################
###################### n = 60, p = 1500, m=10 ################################
##############################################################################

# Simulation function which calculates SGL and Lasso Ratio.
simulation <- function(num_rows, num_cols, m, groups_index, beta){

  # Compute the normally distributed data matrix X
  X <- matrix(rnorm(num_rows * num_cols, mean=0, sd=1), ncol = num_cols, nrow = num_rows)
    
  # Compute the matrix - vector computation of data * coefficients (this is X*B)
  y = X[,1:length(beta)]%*%beta
  
  # Choose the variance of the error (noise) such that signal to noise ratio (SNR) is 2
  snr = 2
  error = rnorm(length(y), mean = 0, sd=sqrt(var(y)/snr))
  
  # Generate the response with simulated data matrix x, coefficients b, and noise added as needed. 
  y = y + error
  
  # Use the entire dataset for training and fitting the model. 
  # This is because the authors did the same thing, no holdout / validation set defined. 
  train.idx = sample(num_rows, num_rows)
  
  # Input data for the iterative 
  data.train = list(x=X[train.idx,], y=y[train.idx])
  
  # Sparse group lasso (alpha = 0.95) from the paper, and alpha = 1 is the lasso
  fit_sgl = SGL(data.train, group_index, type = "linear", alpha=0.95) 
  fit_lasso = SGL(data.train, group_index, type='linear', alpha = 1) #
  
  # find the closest value to the true coefficient, and record, return it.
  # unfortunately I cant extract the number of Non zero coefficients from this class object...
  # Thus, we have to do it manually below (for 10 trials) 
  fitted_models <- list(fit_sgl, fit_lasso)
  return(fitted_models) 
}

###############################################
###### First Generative Group Results     #####
###### Here we use beta_m1, 5 true coefs  #####
###### Result for 1st row, 1st column     #####
###############################################

num_rows = 60
num_cols = 1500
# The number of groups our predictors are divided into
num_groups = 10
# This means group index is 1500 / 10 or 150 predictors per group. 
group_index_row1= rep(1:150, each=10)

# Define the true number of coefficients, beta, for generative groups 1,2,3 as "true coefficeints" in each group
# The 3 generative groups, m1, ... m3 have corresponding true coefficients of 5, 10 and 15.
beta_m1 <- 1:5
beta_m2 <- 1:10
beta_m3 <- 1:15

# Get the fitted sgl and lasso models, 1o times and average out their ratio. 
# At each print statement, we see if the num.nonzero coefficient contains 5.
# If it doesn't contain 5, we use the next bigger number as so in the paper. 
# The Ratio is then the num of true coefs / num of non_zero coefs closes to 5 (must be above 5)
# We do this for 10 trials and record the results in SGL_ratios, and Lasso_ratio
for (i in 1:10){
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row1, beta_m1)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Comment this out to not see the simulation run 10 times
}
#  This is the results for horizontal row 1, generative group 1. 
SGL_ratio_row1_m1 = (5/7 + 5/5 + 5/7 + 5/6 + 5/8 + 5/9 + 5/5 + 5/6 + 5/6 + 5/9) / 10 
Lasso_ratio_row1_m1 = (5/8 + 5/6 + 5/8 + 5/9 + 5/9 + 5/5 + 5/5 + 5/5 + 5/5 + 5/6) /10 

# We can now redo this for the first row, second column, with generative groups, m = 10. (10 true coefs)
###############################################
#####   Second Generative Group Results   #####
##### Here we use beta_m2, 10 true coefs  #####
###############################################

# Use true coefs generative group 2 , with 10 true non-zero coefficients 
for (i in 1:10){
  # Now changed to beta_m2 (for the second generative group)
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row1, beta_m2)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Comment this out to not see the simulation run 10 times.  
}
#  This is the results for horizontal row 1, generative group 2
SGL_ratio_row1_m2 = (10/12 + 10/10 + 10/10 + 10/16 + 10/11 + 10/12 + 10/11 + 10/11 + 10/12 + 10/10) / 10 
Lasso_ratio_row1_m2 = (10/10 + 10/10 + 10/11 + 10/17 + 10/13 + 10/11 + 10/13 + 10/13 + 10/11 + 10/13) /10 

# We can now redo this for the first row, second column, with generative groups, m = 10. (10 true coefs)
###############################################
#####   Third Generative Group Results    #####
##### Here we use beta_m3, 15 true coefs  #####
###############################################

# Use true coefs group 3, with 15 true coefficients 
# beta_m3 corresponds to the 2nd generative group.
for (i in 1:10){
  # Now changed to beta_m3 (for the third generative group)
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row1, beta_m3)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Comment this out to not see the simulation run 10 times.  
}
#  This is the results for horizontal row 1, generative group 3
SGL_ratio_row1_m3 = (15/15 + 15/15 + 15/15 + 15/20 + 15/15 + 15/21 + 15/18 + 15/16 + 15/16 + 15/16) / 10
Lasso_ratio_row1_m3 = (15/15 + 15/18 + 15/18 + 15/16 + 15/15 + 15/19 + 15/15 + 15/15 + 15/16 + 15/16) /10 


################################################################################
###################### Second row in Table (1) #################################
###################### n = 70, p = 2000, m=200 #################################
################################################################################

num_rows = 70
num_cols = 2000
# The number of groups our predictors are divided into
num_groups = 200
# This means group index is 2000 / 200 or 10 predictors per group. 
group_index_row2= rep(1:10, each=200)

# lets call the simulation function on it with these new parameters
#############################################
##### First Generative Group Results    #####
##### Here we use beta_m1, 5 true coefs #####
#############################################

for (i in 1:10){
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row2, beta_m1)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Un comment to see the simulation done 10 times.  
}
#  This is the results for horizontal row 2, generative group 1
SGL_ratio_row2_m1 = (5/5 + 5/6 + 5/6 + 5/7 + 5/5 + 5/6 + 5/6 + 5/5 + 5/6 + 5/5) / 10  
Lasso_ratio_row2_m1 = (5/5 + 5/5 + 5/5 + 5/7 + 5/6 + 5/6 + 5/5 + 5/5 + 5/5 + 5/9) /10  

##############################################
##### Second Generative Group Results    #####
##### Here we use beta_m2, 10 true coefs #####
##############################################
# Repeat the simulation for row 2 but now with generative group 2, true coefs = 10
for (i in 1:10){
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row2, beta_m2)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Uncomment to see the simulation done 10 times.  
}
#  This is the results for horizontal row 2, generative group 2
SGL_ratio_row2_m2 = (10/17 + 10/13 + 10/11 + 10/12 + 10/13 + 10/10 + 10/12 + 10/15 + 10/10 + 10/14) / 10 
Lasso_ratio_row2_m2 = (10/10 + 10/13 + 10/13 + 10/11 + 10/10 + 10/10 + 10/12 + 10/15 + 10/11 + 10/11) /10 

##############################################
##### Third Generative Group Results    ######
##### Here we use beta_m3, 15 true coefs #####
##############################################

# Use true coefs group 3, with 15 true coefficients 
# beta_m3 corresponds to the 3rd generative group.
for (i in 1:10){
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row2, beta_m3)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Un comment to see the simulation done 10 times.  
}
#  This is the results for horizontal row 2, generative group 3
SGL_ratio_row2_m3 = (15/15 + 15/19 + 15/15 + 15/16 + 15/11 + 15/26 + 15/15 + 15/18 + 15/16 + 15/17) / 10
Lasso_ratio_row2_m3 = (15/16 + 15/17 + 15/15 + 15/18 + 15/17 + 15/21 + 15/18 + 15/17 + 15/15 + 15/20) /10 

##################################################################################
######################   Third row in Table (1)  #################################
###################### n = 150, p = 10000, m=100 #################################
##################################################################################

num_rows = 150
num_cols = 10000
# The number of groups our predictors are divided into
num_groups = 100
# This means group index is 10000 / 100 or 1000 predictors per group. 
group_index_row3= rep(1:1000, each=10)

# lets call the simulation function on it with these new parameters
#############################################
##### First Generative Group Results    #####
##### Here we use beta_m1, 5 true coefs #####
#############################################

for (i in 1:10){
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row3, beta_m1)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Un comment to see the simulation done 10 times.  
}
#  This is the results for horizontal row 3, generative group 1 (beta_m1)
SGL_ratio_row3_m1 = (5/5 + 5/5 + 5/5 + 5/6 + 5/5 + 5/6 + 5/5 + 5/7 + 5/7 + 5/5) / 10  
Lasso_ratio_row3_m1 = (5/5 + 5/5 + 5/9 + 5/5 + 5/5 + 5/6 + 5/6 + 5/7 + 5/6 + 5/5) /10 

##############################################
##### Second Generative Group Results    #####
##### Here we use beta_m2, 10 true coefs #####
##############################################
# Repeat the simulation for row 2 but now with generative group 2, true coefs = 10
for (i in 1:10){
  fitted_sgl_and_lasso_models <- simulation(num_rows, num_cols, num_groups, group_index_row3, beta_m2)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Uncomment to see the simulation done 10 times.  
}
#  This is the results for horizontal row 1, m =2 (group 2)
SGL_ratio_row3_m2 = (10/10 + 10/11 + 10/12 + 10/10 + 10/10 + 10/14 + 10/11 + 10/16 + 10/14 + 10/13) / 10 
Lasso_ratio_row3_m2 = (10/13 + 10/10 + 10/11 + 10/10 + 10/19 + 10/16 + 10/10 + 10/11 + 10/12 + 10/15) /10 

# lets call the simulation function on it with these new parameters
##############################################
##### Third Generative Group Results     #####
##### Here we use beta_m3, 15 true coefs #####
##############################################

# Use true coefs group 3, with 15 true coefficients 
# beta_m3 corresponds to the 3rd generative group.
for (i in 1:10){
  fitted_sgl_and_lasso_models <-  simulation(num_rows, num_cols, num_groups, group_index_row3, beta_m3)
  # Note that the first model printed is the SGL model, and second model is the LASSO model
  print(fitted_sgl_and_lasso_models) # Un comment to see the simulation done 10 times.  
}
#  This is the results for horizontal row 2, m =3 (group 3)
SGL_ratio_row3_m3 = (15/21 + 15/15 + 15/16 + 15/18 + 15/18 + 15/16 + 15/15 + 15/16 + 15/15 + 15/16) / 10 
Lasso_ratio_row3_m3 = (15/21 + 15/17 + 15/15 + 15/15 + 15/19 + 15/21 + 15/17 + 15/21 + 15/17 + 15/15) /10


#################################################################################
#####################   Last row in Table (1)     ###############################
##################### n = 200, p = 20000, m = 400 ###############################
#################################################################################

# Due to computationalL time being too long with my slow laptop -- This row is omitted. 
num_rows = 200
num_cols = 20000
# The number of groups our predictors are divided into
num_groups = 400
# This means group index is 20000 / 400 or 50 predictors per group. 
group_index_row4= rep(1:50, each=400)


#### Print out the results from the first three rows 
print("--------------- Simulation Results -----------------------")
print("-------- Replicated Row 1 --------")
# Row 1 
SGL_ratio_row1_m1
Lasso_ratio_row1_m1
SGL_ratio_row1_m2
Lasso_ratio_row1_m2
SGL_ratio_row1_m3
Lasso_ratio_row1_m3

print("-------- Replicated Row 2 ---------")

SGL_ratio_row2_m1
Lasso_ratio_row2_m1
SGL_ratio_row2_m2
Lasso_ratio_row2_m2
SGL_ratio_row2_m3
Lasso_ratio_row2_m3

print("-------- Replicated Row 3 ---------")
SGL_ratio_row3_m1
Lasso_ratio_row3_m1
SGL_ratio_row3_m2
Lasso_ratio_row3_m2
SGL_ratio_row3_m3
Lasso_ratio_row3_m3
