library(dplyr)
library(OpenMx)
library(matlib)
library(caret)
library(glmnet)
library(profvis)
library(MASS)
library(data.table)


# STAP 0: data inladen
Observations = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)

# STAP 1: delete zero clickers
clicks_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = sum)
userlist2 <- clicks_per_user$user[clicks_per_user$x > 0]

# STAP 2: split data in 4 stukken (data opnieuw inloaden bij nieuwe part)
userlist_part_1 <- userlist2[1:round(length(userlist2)/20)]
# part2 <- userlist
# etc
Observations <- Observations[Observations$USERID %in% userlist_part_1,]

# STAP 3: DATA PARTITIONING IN TRAIN-TEST (before kfold train-validation)
set.seed(1908)
intrain <- createDataPartition(Observations$USERID, p = 0.8, list = F) 
training <- Observations[intrain,] #TRAIN
testing  <- Observations[-intrain,] #TEST

# STAP 4: Zet parameters
n_folds <- 5
lambda_vector <- c(0, exp(5), exp(10), exp(15))
epsilon <- 10^-2

# STAP 5: Maak Folds
set.seed(1941)
folds <- createFolds(training$USERID, k = n_folds, list = TRUE, returnTrain = FALSE)

# STAP 6: Run Algoritme

itReLS(lambda_vector = lambda_vector, folds = folds, epsilon = epsilon, training = training, userlist = userlist_part_1)

itReLS <- function(lambda_vector, folds, epsilon, training, userlist){
  MAE_folds <- rep(NA, length(folds))
  MAE_lambda <- rep(NA, length(lambda_vector))
  MAE_zero_folds <- rep(NA, length(folds))
  MAE_clickrate_folds <- rep(NA, length(folds))
  
  for (l in 1:length(lambda_vector)) {
    lambda <- lambda_vector[l]
    
    for (i in 1:length(folds)) {
      start_time <- Sys.time()
      train_kfold <- as.data.table(training[-folds[[i]],])
      validation_kfold <- as.data.table(training[folds[[i]],])
      total_probs <- list()
      
      for (j in 1:length(userlist)){
        print(userlist[j])
        print(j)
        # training set
        train_set <- train_kfold[USERID == userlist[j]]
        train_set <- merge(train_set, OfferDetails, by = c("OFFERID", "MAILID"))
        train_set <- train_set[,-c("OFFERID", "MAILID", "USERID")] #subset(train_set,select = c(CLICK, PRICE, JANUARY:DECEMBER, DISCOUNT_RATE, REVIEW_RATING, STAR_RATING, DURATION, BULGARIA:ISRAEL)) # train_set <- subset(data_complete, select = -c(OFFERID, MAILID, USERID, ROOM_OCCUPANCY, ACCOMMODATION_NAME, USP1, USP2, USP3, DEPARTURE_DATE, ROOMTYPE, MONTH_SUM))
        
        # test set
        test_set_complete <- validation_kfold[USERID == userlist[j]]
        test_set <- merge(test_set_complete, OfferDetails, by = c("OFFERID", "MAILID"))
        test_set <- test_set[,-c("OFFERID", "MAILID", "USERID")]#subset(data_test_complete,select = c(CLICK, PRICE, JANUARY:DECEMBER, DISCOUNT_RATE, REVIEW_RATING, STAR_RATING, DURATION, BULGARIA:ISRAEL)) #test_set <- subset(data_test_complete, select = -c(OFFERID, MAILID, USERID, ROOM_OCCUPANCY, ACCOMMODATION_NAME, USP1, USP2, USP3, DEPARTURE_DATE, ROOMTYPE, MONTH_SUM))
        
        if (nrow(test_set) == 0) {
          print('no forecasts needed')
          next
        }
        #print(paste0('click-rate in train_set: ',mean(train_set$CLICK)))
        
        # create parameters
        ols <- lm(CLICK~., train_set)
        parm <- ols$coefficients
        parm[is.na(parm)] <- 0
        # parm <- rep(0, dim(train_set)[2])
        
        # Parameter estimation with Iteratively Re-weighted Least Squares
        opt_parm <- RidgeRegr(parm, train_set, lambda, epsilon)
        
        # Fit test observations
        prob <- FitRidge(opt_parm, test_set)
        x <- test_set_complete
        x$click_prob <- prob
        total_probs[[j]] <- x
      }
      game_probs <- dplyr::bind_rows(total_probs)
      MAE_folds[i] <- mean(abs(game_probs$CLICK - game_probs$click_prob))
      
      MAE_zero_folds[i] <- mean(game_probs$CLICK)
      click_rate <- mean(train_set$CLICK)
      MAE_clickrate_folds[i] <- mean(abs(game_probs$CLICK - click_rate))
      
      end_time <- Sys.time()
      print(end_time - start_time)
    }
    MAE_lambda[l] <- mean(MAE_folds)
  }
  MAE_zero <- mean(MAE_zero_folds)
  MAE_clickrate <- mean(MAE_clickrate_folds)  

  return(rbind(MAE_lambda, MAE_zero, MAE_clickrate))  
}


RidgeRegr <- function(parm, data, lambda, epsilon){
  n <- dim(data)[1]
  beta_k <- rep(Inf, length(parm))
  beta_k1 <- parm
  gradient <- Inf
  x <- as.matrix(cbind(intercept = rep(1,n),data[,-1]))
  y <- as.matrix(data[,1])
  j = 0
  z <- rep(NA,n)
  while (sum(gradient)^2 > epsilon & j < 20) {
    beta_k = beta_k1
    for (i in 1:n) {
      b <- exp(sum(x[i,]*beta_k))
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
    #print(norm(beta_k1[-1], "2"))
    #print(sum(gradient)^2)
    #print(beta_k1[1])
    j = j+1
  }
  print(j)
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

