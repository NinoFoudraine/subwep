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

itReLS <- function(lambda_vector, train, validation, epsilon, userlist, AE_list_under_threshold){
  AE_folds <- list()
  MAE_folds <- rep(NA, length(train))
  MAE_lambda <- rep(NA, length(lambda_vector))
  MAE_folds_boven_threshold <- rep(NA, length(length(train)))
  MAE_lambda_boven_threshold <- rep(NA, length(lambda_vector))
  
  for (l in 1:length(lambda_vector)) {
    print(paste0('Lambda ', l))
    lambda <- lambda_vector[l]
    
    for (i in 1:length(train)) {
      print(paste0('Fold ', i))
      start_time <- Sys.time()
      
      train_kfold <- as.data.table(train[[i]])
      validation_kfold <- as.data.table(validation[[i]])
      total_probs <- list()
      
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
        
        #print(j)
        #print(userlist[j])
        # Parameter estimation with Iteratively Re-weighted Least Squares
        opt_parm <- RidgeRegr(parm, train_set, lambda, epsilon)
        
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
      
      
      AE_folds[[i]] <- c(abs(game_probs$CLICK - game_probs$click_prob),AE_list_under_threshold[[i]])
      MAE_folds[i] <- mean(AE_folds[[i]])
      MAE_folds_boven_threshold[i] <- mean(abs(game_probs$CLICK - game_probs$click_prob))
      
      end_time <- Sys.time()
      print(end_time - start_time)
    }
    
    MAE_lambda[l] <- mean(MAE_folds, na.rm = TRUE)
    MAE_lambda_boven_threshold[l] <- mean(MAE_folds_boven_threshold, na.rm = TRUE)
  }
  
  return(list(MAE_table = rbind(MAE_lambda, MAE_lambda_boven_threshold), probs = game_probs))
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
clickrate_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = mean)
obs_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = length)
nonzero_clickers <- clickrate_per_user$user[clickrate_per_user$x > 0 & obs_per_user$x > 2] ## en observaties meer dan 2 
Observations <- Observations[Observations$USERID %in% nonzero_clickers,]

# STAP 2: Split data in train-test (met alle data)
set.seed(1908)
intrain <- createDataPartition(Observations$USERID, p = 0.8, list = F) 
training <- as.data.table(Observations[intrain,]) # train
testing  <- as.data.table(Observations[-intrain,]) # test

# STAP 3: Zet parameters
n_folds <- 5
lambda_vector <- c(0, 1, exp(1),  exp(4),  exp(7), exp(10), exp(13))
epsilon <- 10^-4
threshold_vector <- seq(0,1, by = 0.05)
threshold_vector <- threshold_vector[19] #### voor Luuk
#threshold_vector <- threshold_vector[10:21] #### voor Nino
total_results <- matrix(NA, 2*length(threshold_vector), length(lambda_vector)) # rows van matrix lengte van 4*j in forloop hieronder

#run for different thresholds (0.95, 0.90, 0.85, 0.80, 0.75, 0.70)
for (j in 1:length(threshold_vector)) { 

  threshold <- threshold_vector[j]

  # STAP 4: Split in Repeated Holdout folds
  # 
  
  train_boven_threshold <- list()
  validation_boven_threshold <- list()
  AE_under_threshold <- list()

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
    AE_under_threshold[[i]] <- validation_under_threshold$CLICK
  }

  # STAP 6: Run Algoritme
results <- itReLS(lambda_vector = lambda_vector, train = train_boven_threshold, validation = validation_boven_threshold, epsilon = epsilon, userlist = userlist_threshold, AE_list_under_threshold = AE_under_threshold)#, clickrate = clickrate_per_user, AE_clickrate_list_under_threshold = AE_clickrate_under_threshold)

print(results$MAE_table)

total_results[(1+(j-1)*2):(2+(j-1)*2),1:length(lambda_vector)] <- results$MAE_table

}
