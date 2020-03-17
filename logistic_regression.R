library(dplyr)
library(OpenMx)
library(matlib)
library(caret)
library(glmnet)
library(profvis)
library(MASS)
library(data.table)

# Laad Functies in

'%ni%' <- Negate('%in%')

# function for computing logistic regression for all lambdas and folds
# input:  lambda_vector             -   vector of lambdas
#         train                     -   list of training folds
#         validation                -   list of validation folds
#         epsilon                   -   convergence parameter (set at 10^-4)
#         userlist                  -   vector of users that need to be trained (i.e., above the threshold)
#         AE_list_under_threshold   -   list with error vectors per fold
# output: MAE_table                 -   table representing the MAE on total and MAE on trained users only, per lambda and per threshold
#         probs                     -   vector of probs of the last fold (for example purposes)
#         norms                     -   table representing the norm of the betas excluding intercept per lambda and per threshold
itReLS <- function(lambda_vector, train, validation, epsilon, userlist, AE_list_under_threshold){
  AE_folds <- list()
  MAE_folds <- rep(NA, length(train))
  MAE_lambda <- rep(NA, length(lambda_vector))
  MAE_folds_boven_threshold <- rep(NA, length(length(train)))
  MAE_lambda_boven_threshold <- rep(NA, length(lambda_vector))
  norm_betas_folds <- rep(NA, length(train))
  norm_betas_lambda <- rep(NA, length(lambda_vector))
  
  
  for (l in 1:length(lambda_vector)) {
    print(paste0('Lambda ', l))
    lambda <- lambda_vector[l]
    
    for (i in 1:length(train)) {
      print(paste0('Fold ', i))
      start_time <- Sys.time()
      
      train_kfold <- as.data.table(train[[i]])
      validation_kfold <- as.data.table(validation[[i]])
      total_probs <- list()
      norm_betas <- rep(NA, length(userlist))
      
      for (j in 1:length(userlist)){
        # training set
        train_set <- train_kfold[USERID == userlist[j]]
        train_set <- train_set[OfferDetails, on = c(OFFERID = 'OFFERID', MAILID = 'MAILID'), nomatch = 0] # merge training data on content
        train_set <- train_set[,-c("OFFERID", "MAILID", "USERID")] 
        
        # test set
        test_set_complete <- validation_kfold[USERID == userlist[j]]
        test_set <- test_set_complete[OfferDetails, on = c(OFFERID = 'OFFERID', MAILID = 'MAILID'), nomatch = 0] # merge testing data on content
        test_set <- test_set[,-c("OFFERID", "MAILID", "USERID")]
        
        if (nrow(test_set) == 0 || nrow(train_set)== 0) { # if  nothing is in the train or test set, nothing should be fitted (for computation time purposes)
          # print('no forecasts needed')
          # print(j)
          next
        }
        
        # create initial parameters -> OLS estimates
        ols <- lm(CLICK~., train_set)
        parm <- ols$coefficients
        parm[is.na(parm)] <- 0

        # Parameter estimation with Iteratively Re-weighted Least Squares
        opt_parm <- RidgeRegr(parm, train_set, lambda, epsilon)
        norm_betas[j] <- norm(opt_parm[-1], '2')

        # Fit test observations
        prob <- FitRidge(opt_parm, test_set)
        x <- test_set_complete
        x$click_prob <- prob
        total_probs[[j]] <- x
      }
      game_probs <- dplyr::bind_rows(total_probs)
      norm_betas_folds[i] <- mean(norm_betas, na.rm = TRUE)
      
      AE_folds[[i]] <- c(abs(game_probs$CLICK - game_probs$click_prob),AE_list_under_threshold[[i]]) # combine the errors of the trained users and the users under threshold
      MAE_folds[i] <- mean(AE_folds[[i]])
      MAE_folds_boven_threshold[i] <- mean(abs(game_probs$CLICK - game_probs$click_prob)) # error of only trained users
      
      end_time <- Sys.time()
      print(end_time - start_time)
    }
    
    # compute mean over all folds
    MAE_lambda[l] <- mean(MAE_folds, na.rm = TRUE) 
    MAE_lambda_boven_threshold[l] <- mean(MAE_folds_boven_threshold, na.rm = TRUE)
    norm_betas_lambda[l] <- mean(norm_betas_folds)
  }
  
  return(list(MAE_table = rbind(MAE_lambda, MAE_lambda_boven_threshold), probs = game_probs, norms = norm_betas_lambda))
}

# function for Newton-Raphson method to find optimal solution
# input:  parm                      -   initial parameters
#         data                      -   input data
#         lambda                    -   penalty parameter
#         epsilon                   -   convergence parameter (set at 10^-4)
# output: beta_k1                   -   parameters for optimal solution
RidgeRegr <- function(parm, data, lambda, epsilon){
  n <- dim(data)[1]
  beta_k <- rep(Inf, length(parm))
  beta_k1 <- parm
  gradient <- Inf
  x <- as.matrix(cbind(intercept = rep(1,n),data[,-1])) # create intercept on the first column
  y <- as.matrix(data[,1])  # first column being the CLICK variable
  j = 0
  z <- rep(0,n)
  while (sum(gradient)^2 > epsilon & j < 40) {
    beta_k = beta_k1
    for (i in 1:n) {
      b <- exp(sum(x[i,]*beta_k))
      if (b == Inf){ # sometimes b gets too large that it is set to Inf, then z = Inf/(Inf+1), which is equal to 1 in this case
        z[i] <- 1 
        next
      }
      z[i] <- b/(1 + b)
    }
    # compute gradient and hessian based on the algorithm
    W <- z*(1-z)
    q <- diag(lambda, length(parm))
    q[1,1] <- 0
    Hessian <- t(W * x) %*% x + q
    r <- lambda * beta_k
    r[1] <- 0
    gradient <- t(x) %*% (y - z) - r
    # compute next iteration
    beta_k1 <- beta_k + ginv(Hessian) %*% gradient
    j = j+1
  }
  return(beta_k1)
}

# function to fit the predictions
# input:  parm                      -   parameters to fit the model with
#         data                      -   input data
# output: prob                      -   vector of fits based on the data and parameters
FitRidge <- function(parm,data){
  prob <- rep(NA,dim(data)[1])
  x <- as.matrix(cbind(rep(1,dim(data)[1]),data[,-1])) # create intercept on th first column
  for (i in 1:dim(data)[1]) {
    z <- exp(sum(parm*x[i,]))
    prob[i] <- z/(1+z)
  }
  return(prob)
}

# STAP 0: read in the data (Observations_report.csv)
Observations = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)
OfferDetails <- as.data.table(OfferDetails) # compute OfferDetails matrix by running the 'data_cleaning.R' script

# STAP 1: Remove clickers with less than 3 observations (these users are irrelevant since the splitting of the data causes to never train a model or predict probabilities for these users)
obs_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = length)
nonzero_clickers <- obs_per_user$user[obs_per_user$x > 2]
Observations <- Observations[Observations$USERID %in% nonzero_clickers,]

# STAP 2: Split data in train-test (all data)
set.seed(1908)
intrain <- createDataPartition(Observations$USERID, p = 0.8, list = F) 
training <- as.data.table(Observations[intrain,]) # train
testing  <- as.data.table(Observations[-intrain,]) # test

# STAP 3: Set parameters
n_folds <- 5
lambda_vector <- c(0.1, 1, exp(1),  exp(4),  exp(7))
epsilon <- 10^-4
threshold_vector <- seq(0,0.95, by = 0.05)
total_results <- matrix(NA, 2*length(threshold_vector), length(lambda_vector))
norm_results <- matrix(NA, length(threshold_vector), length(lambda_vector))

MAE_nulschatting_boven_threshold <- rep(NA, length(threshold_vector))
#run for different thresholds (0.95, 0.90, 0.85, 0.80, 0.75, 0.70)
for (j in 1:length(threshold_vector)) { 

  threshold <- threshold_vector[j]
  
  train_boven_threshold <- list()
  validation_boven_threshold <- list()
  AE_under_threshold <- list()
  
  for (i in 1:n_folds){
    # compute repeated holdout
    set.seed(2*i)
    intrain_fold <- training[,sample(.N, floor(.N*0.8))]
    tussenstap_train <- training[intrain_fold,]
    tussenstap_validation <- training[-intrain_fold,]
    # compute clickrate per user in the fold
    clickrate_training <- tussenstap_train[, .(x = mean(CLICK)), by = USERID]
    # get userlist for only trained users
    userlist_threshold <- clickrate_training$USERID[clickrate_training$x > threshold]
    
    # filter data for only trained users
    train_boven_threshold[[i]] <- tussenstap_train[tussenstap_train$USERID %in% userlist_threshold,]
    validation_boven_threshold[[i]] <- tussenstap_validation[tussenstap_validation$USERID %in% userlist_threshold,]
    # filter data for users under threshold
    validation_under_threshold <- tussenstap_validation[tussenstap_validation$USERID %ni% userlist_threshold,]
    AE_under_threshold[[i]] <- validation_under_threshold$CLICK
  }
  # compute base-model for only trained users
  validationset <- dplyr::bind_rows(validation_boven_threshold)
  MAE_nulschatting_boven_threshold[j] <- mean(validationset$CLICK)
  # run algorithm
results <- itReLS(lambda_vector = lambda_vector, train = train_boven_threshold, validation = validation_boven_threshold, epsilon = epsilon, userlist = userlist_threshold, AE_list_under_threshold = AE_under_threshold)#, clickrate = clickrate_per_user, AE_clickrate_list_under_threshold = AE_clickrate_under_threshold)

print(results$MAE_table)
print(results$norms)

total_results[(1+(j-1)*2):(2+(j-1)*2),1:length(lambda_vector)] <- results$MAE_table
norm_results[j,] <- results$norms
}