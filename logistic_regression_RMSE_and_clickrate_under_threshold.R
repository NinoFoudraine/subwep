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

itReLS <- function(lambda_vector, train, validation, epsilon, userlist, SE_list_under_threshold){
  SE_folds <- list()
  MSE_folds <- rep(NA, length(train))
  MSE_lambda <- rep(NA, length(lambda_vector))
  MSE_folds_boven_threshold <- rep(NA, length(length(train)))
  MSE_lambda_boven_threshold <- rep(NA, length(lambda_vector))
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
        train_set <- train_set[OfferDetails, on = c(OFFERID = 'OFFERID', MAILID = 'MAILID'), nomatch = 0]
        train_set <- train_set[,-c("OFFERID", "MAILID", "USERID")] 
        
        
        # test set
        test_set_complete <- validation_kfold[USERID == userlist[j]]
        test_set <- test_set_complete[OfferDetails, on = c(OFFERID = 'OFFERID', MAILID = 'MAILID'), nomatch = 0]
        test_set <- test_set[,-c("OFFERID", "MAILID", "USERID")]
        
        if (nrow(test_set) == 0 || nrow(train_set)== 0) {
          # print('no forecasts needed')
          # print(j)
          next
        }
        
        # create parameters
        ols <- lm(CLICK~., train_set)
        parm <- ols$coefficients
        parm[is.na(parm)] <- 0

        # Parameter estimation with Iteratively Re-weighted Least Squares
        opt_parm <- RidgeRegr(parm, train_set, lambda, epsilon)
        norm_betas[j] <- norm(opt_parm[-1], '2')
        
        if (any(opt_parm) == 'warning') {
          print(paste0('warning, diverges for user: ', j, ' - ', userlist[j]))
          next
        }

        # Fit test observations
        prob <- FitRidge(opt_parm, test_set)
        x <- test_set_complete
        x$click_prob <- prob
        total_probs[[j]] <- x
      }
      game_probs <- dplyr::bind_rows(total_probs)
      norm_betas_folds[i] <- mean(norm_betas, na.rm = TRUE)
      
      
      SE_folds[[i]] <- c((game_probs$CLICK - game_probs$click_prob)^2,SE_list_under_threshold[[i]])
      MSE_folds[i] <- mean(SE_folds[[i]])
      MSE_folds_boven_threshold[i] <- mean((game_probs$CLICK - game_probs$click_prob)^2)
      
      end_time <- Sys.time()
      print(end_time - start_time)
    }
    
    MSE_lambda[l] <- mean(MSE_folds, na.rm = TRUE)
    MSE_lambda_boven_threshold[l] <- mean(MSE_folds_boven_threshold, na.rm = TRUE)
    norm_betas_lambda[l] <- mean(norm_betas_folds)
  }
  MSE_lambda <- sqrt(MSE_lambda)
  MSE_lambda_boven_threshold <- sqrt(MSE_lambda_boven_threshold)
  
  return(list(MSE_table = rbind(MSE_lambda, MSE_lambda_boven_threshold), probs = game_probs, norms = norm_betas_lambda))
}


RidgeRegr <- function(parm, data, lambda, epsilon){
  n <- dim(data)[1]
  beta_k <- rep(Inf, length(parm))
  beta_k1 <- parm
  gradient <- Inf
  x <- as.matrix(cbind(intercept = rep(1,n),data[,-1]))
  y <- as.matrix(data[,1])
  j = 0
  z <- rep(0,n)
  while (sum(gradient)^2 > epsilon & j < 40) {
    beta_k = beta_k1
    for (i in 1:n) {
      b <- exp(sum(x[i,]*beta_k))
      if (is.na(b)) {
        return('warning')
      }
      if (b == Inf){
        z[i] <- 1 
        next
      }
      z[i] <- b/(1 + b)
    }
    W <- z*(1-z)
    q <- diag(lambda, length(parm))
    q[1,1] <- 0
    Hessian <- t(W * x) %*% x + q
    r <- lambda * beta_k
    r[1] <- 0
    gradient <- t(x) %*% (y - z) - r
    if (any(is.na(gradient))) {
      return('warning')
    }
    beta_k1 <- beta_k + ginv(Hessian) %*% gradient
    j = j+1
  }
  #print(j)
  return(beta_k1)
}

# Fit Ridge function 
FitRidge <- function(parm,data){
  prob <- rep(NA,dim(data)[1])
  x <- as.matrix(cbind(rep(1,dim(data)[1]),data[,-1]))
  for (i in 1:dim(data)[1]) {
    z <- exp(sum(parm*x[i,]))
    prob[i] <- z/(1+z)
  }
  return(prob)
}

# STAP 0: Data inladen
Observations = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)
OfferDetails <- as.data.table(OfferDetails)

# STAP 1: Delete zero clickers
#clickrate_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = mean)
obs_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = length)
nonzero_clickers <- obs_per_user$user[obs_per_user$x > 2]#[clickrate_per_user$x > 0 & obs_per_user$x > 2] ## en observaties meer dan 2 
Observations <- Observations[Observations$USERID %in% nonzero_clickers,]

# STAP 2: Split data in train-test (met alle data)
set.seed(1908)
intrain <- createDataPartition(Observations$USERID, p = 0.8, list = F) 
training <- as.data.table(Observations[intrain,]) # train
testing  <- as.data.table(Observations[-intrain,]) # test

# STAP 3: Zet parameters
n_folds <- 2
lambda_vector <- c(0.1, 1, exp(1),  exp(4),  exp(7))
epsilon <- 10^-4
threshold_vector <- 0.05
total_results <- matrix(NA, 2*length(threshold_vector), length(lambda_vector)) # rows van matrix lengte van 4*j in forloop hieronder
norm_results <- matrix(NA, length(threshold_vector), length(lambda_vector))

MSE_nulschatting_boven_threshold <- rep(NA, length(threshold_vector))
#run for different thresholds (0.95, 0.90, 0.85, 0.80, 0.75, 0.70)
for (j in 1:length(threshold_vector)) { 

  threshold <- threshold_vector[j]

  # STAP 4: Split in Repeated Holdout folds
  # 
  
  train_boven_threshold <- list()
  validation_boven_threshold <- list()
  SE_under_threshold <- list()
  
  for (i in 1:n_folds){
    set.seed(2*i)
    intrain_fold <- training[,sample(.N, floor(.N*0.8))]
    tussenstap_train <- training[intrain_fold,]
    tussenstap_validation <- training[-intrain_fold,]
    clickrate_training <- tussenstap_train[, .(x = mean(CLICK)), by = USERID]
    userlist_threshold <- clickrate_training$USERID[clickrate_training$x > threshold]
    
    # STAP 5: Haal 0 schattingen eruit
    train_boven_threshold[[i]] <- tussenstap_train[tussenstap_train$USERID %in% userlist_threshold,]
    validation_boven_threshold[[i]] <- tussenstap_validation[tussenstap_validation$USERID %in% userlist_threshold,]
    validation_under_threshold <- tussenstap_validation[tussenstap_validation$USERID %ni% userlist_threshold,]
    validation_under_threshold <- validation_under_threshold[clickrate_training, on = (USERID = 'USERID'), nomatch = 0]
    SE_under_threshold[[i]] <- (validation_under_threshold$CLICK - validation_under_threshold$x)^2
  }
  validationset <- dplyr::bind_rows(validation_boven_threshold)
  MSE_nulschatting_boven_threshold[j] <- mean(validationset$CLICK)
  # STAP 6: Run Algoritme
results <- itReLS(lambda_vector = lambda_vector, train = train_boven_threshold, validation = validation_boven_threshold, epsilon = epsilon, userlist = userlist_threshold, SE_list_under_threshold = SE_under_threshold)#, clickrate = clickrate_per_user, SE_clickrate_list_under_threshold = SE_clickrate_under_threshold)

print(results$MSE_table)
print(results$norms)

total_results[(1+(j-1)*2):(2+(j-1)*2),1:length(lambda_vector)] <- results$MSE_table
norm_results[j,] <- results$norms
}



#### Run last test set
lambda = 1
threshold = 0.5
clickrate_training <- training[, .(x = mean(CLICK)), by = USERID]
userlist_threshold <- clickrate_training$USERID[clickrate_training$x > threshold]
train_boven_threshold[[1]] <- training[training$USERID %in% userlist_threshold,]
test_boven_threshold[[1]] <- testing[testing$USERID %in% userlist_threshold,]
test_under_threshold <- testing[testing$USERID %ni% userlist_threshold,]
SE_under_threshold[[1]] <- test_under_threshold$CLICK

final_results <- itReLS(lambda_vector = lambda, train = train_boven_threshold, validation = test_boven_threshold, epsilon = epsilon, userlist = userlist_threshold, SE_list_under_threshold = SE_under_threshold)
final_results$MSE_table
View(final_results$probs)
aggregate(final_results$probs$click_prob, by = list(final_results$probs$CLICK), FUN = mean)