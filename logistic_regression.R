library(dplyr)
library(OpenMx)
library(matlib)
library(caret)
library(glmnet)
library(profvis)
library(MASS)
library(data.table)


Observations = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)
# Observations_game = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)
# OfferDetails via cleaned code

# remove never clicked users
clicks_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = sum)
##offers_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = length)
userlist2 <- clicks_per_user$user[clicks_per_user$x > 0]
Observations <- Observations[Observations$USERID %in% userlist2,]

### test case
click_rate_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = mean)
userlist2 <- click_rate_per_user$user[click_rate_per_user$x >= 0.2 & click_rate_per_user$x < 0.45]

#DATA PARTITIONING
Observations_partitioning <- as.factor(Observations$USERID)
set.seed(1908)
intrain <- createDataPartition(Observations_partitioning, p = 0.8, list = F) 
training <- Observations[intrain,]
testing  <- Observations[-intrain,]

userlist <- userlist2[5]
total_probs <- list()

# # opdelen totale matrix in verschillende blocks van users voor snelheid
# training <- training[training$USERID %in% userlist,]

training <- as.data.table(training)
testing <- as.data.table(testing)

# set parameters
epsilon <- 10^-2
lambda <- 9000000

for (j in 1:length(userlist)) {
  start_time <- Sys.time()
  # training set
  train_set_complete <- training[USERID == userlist[j]]
  data_train_complete <- merge(train_set_complete, OfferDetails, by = c("OFFERID", "MAILID"))
  train_set <- data_train_complete[,-c(1:3)]#subset(data_train_complete,select = c(CLICK, PRICE, JANUARY:DECEMBER, DISCOUNT_RATE, REVIEW_RATING, STAR_RATING, DURATION, BULGARIA:ISRAEL)) # train_set <- subset(data_complete, select = -c(OFFERID, MAILID, USERID, ROOM_OCCUPANCY, ACCOMMODATION_NAME, USP1, USP2, USP3, DEPARTURE_DATE, ROOMTYPE, MONTH_SUM))
  
  # test set
  test_set_complete <- testing[USERID == userlist[j]]
  data_test_complete <- merge(test_set_complete, OfferDetails, by = c("OFFERID", "MAILID"))
  test_set <- data_test_complete[,-c(1:3)]#subset(data_test_complete,select = c(CLICK, PRICE, JANUARY:DECEMBER, DISCOUNT_RATE, REVIEW_RATING, STAR_RATING, DURATION, BULGARIA:ISRAEL)) #test_set <- subset(data_test_complete, select = -c(OFFERID, MAILID, USERID, ROOM_OCCUPANCY, ACCOMMODATION_NAME, USP1, USP2, USP3, DEPARTURE_DATE, ROOMTYPE, MONTH_SUM))
  
  if (nrow(test_set) == 0) {
    print('no forecasts needed')
    next
  }
  print(paste0('click-rate in train_set: ',mean(train_set$CLICK)))
  
  # create parameters
  ols <- lm(CLICK~., train_set)
  parm <- ols$coefficients
  parm[is.na(parm)] <- 0
  # parm <- rep(0, dim(train_set)[2])
  print(userlist[j])
  
  # Parameter estimation with Iteratively Re-weighted Least Squares
  opt_parm <- RidgeRegr4(parm,train_set, lambda, epsilon)
  
  # Fit test observations
  prob <- FitRidge(opt_parm,test_set)
  x <- test_set_complete
  x$click_prob <- prob
  total_probs[[j]] <- x
  
  end_time <- Sys.time()
  print(end_time - start_time)
}

game_probs <- dplyr::bind_rows(total_probs)
aggregate(game_probs$click_prob, by = list(game_probs$CLICK), FUN = mean)
MAE <- mean(abs(game_probs$CLICK - game_probs$click_prob))
MAE_zero <- mean(abs(game_probs$CLICK))
MAE_one <- mean(abs(game_probs$CLICK - 1))


RidgeRegr4 <- function(parm,data,lambda,epsilon){
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
    print(beta_k1[1])
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

# Ridge function
Ridge <- function(parm,data, lambda){
  f <- 0
  for (i in 1:dim(data)[1]){
    z <- parm[1]+(sum(parm[-1]*data[i,-1]))
    f <- f + data[i,1]*z - log(1 + exp(z))
  }
  rho <- -f
  return(rho + 1/2 * lambda * sum(parm^2))
}

