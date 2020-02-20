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

itReLS <- function(lambda_vector, train, validation, epsilon, userlist){
  AE_folds <- list()
  MAE_lambda <- rep(NA, length(lambda_vector))
  MAE_zero_folds <- rep(NA, length(folds))
  MAE_clickrate_folds <- rep(NA, length(folds))
  
  for (l in 1:length(lambda_vector)) {
    print(paste0('Lambda ', l))
    lambda <- lambda_vector[l]
    
    for (i in 1:length(folds)) {
      print(paste0('Fold ', i))
      start_time <- Sys.time()
      
      train_kfold <- as.data.table(train[[i]])
      validation_kfold <- as.data.table(validation[[i]])
      total_probs <- list()
      
      for (j in 1:length(userlist)){
        # training set
        train_set <- train_kfold[USERID == userlist[j]]
        train_set <- merge(train_set, OfferDetails, by = c("OFFERID", "MAILID"))
        train_set <- train_set[,-c("OFFERID", "MAILID", "USERID")] #subset(train_set,select = c(CLICK, PRICE, JANUARY:DECEMBER, DISCOUNT_RATE, REVIEW_RATING, STAR_RATING, DURATION, BULGARIA:ISRAEL)) # train_set <- subset(data_complete, select = -c(OFFERID, MAILID, USERID, ROOM_OCCUPANCY, ACCOMMODATION_NAME, USP1, USP2, USP3, DEPARTURE_DATE, ROOMTYPE, MONTH_SUM))
        
        # test set
        test_set_complete <- validation_kfold[USERID == userlist[j]]
        test_set <- merge(test_set_complete, OfferDetails, by = c("OFFERID", "MAILID"))
        test_set <- test_set[,-c("OFFERID", "MAILID", "USERID")]#subset(data_test_complete,select = c(CLICK, PRICE, JANUARY:DECEMBER, DISCOUNT_RATE, REVIEW_RATING, STAR_RATING, DURATION, BULGARIA:ISRAEL)) #test_set <- subset(data_test_complete, select = -c(OFFERID, MAILID, USERID, ROOM_OCCUPANCY, ACCOMMODATION_NAME, USP1, USP2, USP3, DEPARTURE_DATE, ROOMTYPE, MONTH_SUM))
        
        if (nrow(test_set) == 0) {
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
        
        #print(opt_parm)

        # Fit test observations
        prob <- FitRidge(opt_parm, test_set)
        x <- test_set_complete
        x$click_prob <- prob
        x$AE_clickrate <- abs(x$CLICK - mean(train_set$CLICK))
        total_probs[[j]] <- x
        
      }
      game_probs <- dplyr::bind_rows(total_probs)
      
      AE_folds[[i]] <- abs(game_probs$CLICK - game_probs$click_prob)
      
      MAE_zero_folds[i] <- mean(game_probs$CLICK)
      click_rate <- mean(train_set$CLICK)
      MAE_clickrate_folds[i] <- mean(x$AE_clickrate)
      
      end_time <- Sys.time()
      print(end_time - start_time)
    }
    MAE_lambda[l] <- mean(MAE_folds)
  }
  MAE_zero <- mean(MAE_zero_folds)
  MAE_clickrate <- mean(MAE_clickrate_folds) 
  
  return(list(MAE_table = rbind(MAE_lambda, MAE_zero, MAE_clickrate), probs = game_probs))  
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
    beta_k1 <- beta_k + ginv(Hessian) %*% gradient
    j = j+1
  }
  #print(j)
  return(round(beta_k1,5))
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

# STAP 1: Delete zero clickers
clickrate_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = mean)
nonzero_clickers <- clickrate_per_user$user[clickrate_per_user$x > 0]
Observations <- Observations[Observations$USERID %in% nonzero_clickers,]

# STAP 2: Split data in train-test (met alle data)
set.seed(1908)
intrain <- createDataPartition(Observations$USERID, p = 0.8, list = F) 
training <- Observations[intrain,] # train
testing  <- Observations[-intrain,] # test

# STAP 3: Zet parameters
n_folds <- 4
lambda_vector <- c(0.1, exp(1),  exp(4),  exp(7), exp(10), exp(12), exp(15))
epsilon <- 10^-4
threshold <- 0.1 # pas deze aan

# STAP 4: Split in Repeated Holdout folds
# clickrate_training <- aggregate(training$CLICK, by = list(user = training$USERID), FUN = mean)
userlist_threshold <- clickrate_per_user$user[clickrate_per_user$x > threshold]
train_boven_threshold <- list()
validation_boven_threshold <- list()
AE_under_threshold <- list()

for (i in 1:n_folds){
  set.seed(2*i)
  intrain_fold <- createDataPartition(training$USERID, p = 0.8, list = F) 
  tussenstap_train <- training[intrain_fold,]
  tussenstap_validation <- training[-intrain_fold,]
  
  # STAP 4: Haal 0 schattingen eruit
  train_boven_threshold[[i]] <- tussenstap_train[tussenstap_train$USERID %in% userlist_threshold,]
  validation_boven_threshold[[i]] <- tussenstap_validation[tussenstap_validation$USERID %in% userlist_threshold,]
  validation_under_threshold <- tussenstap_validation[tussenstap_validation$USERID %ni% userlist_threshold,]
  AE_under_threshold[[i]] <- validation_under_threshold$CLICK
}

# STAP 5: Run Algoritme

results <- itReLS(lambda_vector = lambda_vector, folds = folds, epsilon = epsilon, training = training, userlist = userlist_part_1)




#userlist2 <- clicks_per_user$user[clicks_per_user$x > 0 & obs_per_user$x >= 8 & clickrate_per_user$x >= 0.5 & clickrate_per_user$x< 1]
userlist2 <- clicks_per_user$user[clickrate_per_user$x > 0] ## 0 aanpassen naar ondergrens
zeroclickers <- clicks_per_user$user[-userlist2]

### error bepalen van zeroclickers
Observations_zero <- Observations[Observat]

# STAP 2: Split data in 4 stukken (data opnieuw inloaden bij nieuwe part)
userlist_part_1 <- userlist2[1:round(1/2*length(userlist2))]
#userlist_part_1 <- 44811506
  # part2 <- userlist
# etc
Observations1 <- Observations[Observations$USERID %in% userlist_part_1,]

# STAP 3: Data split in train-test set (voor kfold train-validation)
set.seed(1908)
intrain <- createDataPartition(Observations1$USERID, p = 0.8, list = F) 
training <- Observations1[intrain,] # train
testing  <- Observations1[-intrain,] # test


# STAP 4: Zet parameters
n_folds <- 4
lambda_vector <- c(0.1, exp(1),  exp(4),  exp(7), exp(10), exp(12), exp(15))
epsilon <- 10^-4

# STAP 5: Maak Folds
set.seed(1941)
folds <- createFolds(training$USERID, k = n_folds, list = TRUE, returnTrain = FALSE)

# STAP 6: Run Algoritme

results <- itReLS(lambda_vector = lambda_vector, folds = folds, epsilon = epsilon, training = training, userlist = userlist_part_1)
results$MAE_table
View(results$probs)
aggregate(results$probs$click_prob, by = list(results$probs$CLICK), FUN = mean)
