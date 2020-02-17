# remove never clicked users and all click users
clicks_per_user <- aggregate(Observations$CLICK, by = list(user = Observations$USERID), FUN = sum) # amount of clicks per user
list_clickers <- clicks_per_user$user[clicks_per_user$x > 0] # list of clicking users
Observations_clickers <- Observations[Observations$USERID %in% list_clickers,] # only keep clickers
offers_per_user_clickers <- data.frame(table(Observations_clickers$USERID)) # amount of offers per USERID
clicks_per_user_clickers <- clicks_per_user[clicks_per_user$x > 0, ] # clicks per user for the clickers
clickrate <- cbind(clicks_per_user_clickers$user, clicks_per_user_clickers$x/offers_per_user_clickers[,2]) # clicks per user for the clickers
user_one_clickrate <- list_clickers[clickrate[,2]==1] # all clickers to be predicted to click
user_zero_clickrate <- clicks_per_user$user[clicks_per_user$x == 0] # zero clickers to be predicted at zero
'%ni%' <- Negate('%in%')
Observations_cleaned <- Observations_clickers[Observations_clickers$USERID %ni% user_one_clickrate,] # cleaned for 100p and 0p clickers


sum(click_rate_per_user[,2])
