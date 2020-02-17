########################################################################
######################### Initialize ###################################
########################################################################

rm(AmountOfOffers)

#load packages
library(utils)
library(stringr)
library(ngram) #for preprocess(x) function to lose capitals and extra spaces

########################################################################
######################### Import and format data #######################
########################################################################

Observations = read.csv(file.choose(), header = T, sep = ';')

Obstest <- Observations[1:1000,]

users <- unique(Obstest$USERID)
mails <- unique(Observations$MAILID)
offers <- unique(Obstest$OFFERID)
clicks <- (Obstest$CLICK)

length(users)  #amount of unique users 297572
length(offers) #amount of unique offers 2130
sum(clicks)    #amount of clicks 705292
sum(clicks)/length(clicks) #clickrate 0.02192184

clicksperuser <- rep(0,length(users))
totalObsperuser <-rep(0,length(users))

n_users <- length(unique(Obstest$USERID))
rownames <- unique(Obstest$USERID)

clicksperuser <- rep(0,length(users))
totalObsperuser <-rep(0,length(users))


counter = 1
for( i in 1 : nrow(Obstest)){
  print(counter)
  UserIndex = which(rownames == Obstest[i,1] )

  totalObsperuser[i] = totalObsperuser[i]+1
  
  if(Obstest[i,4]==1)
  {
    clicksperuser[i]=  clicksperuser[i]+1
  }
  

  counter = counter +1
}






#matrix <- rep(NaN, n_users, n_offers)
#rownames <- unique(Obstest$USERID)
#colnames <- unique(Obstest$OFFERID)
#rownames(matrix) <- rownames
#colnames(matrix) <- colnames

#thomas zn mooie loepje
#counter = 1
#for( i in 1 : nrow(Obstest)){
#  print(counter)
#  UserIndex = which(rownames == Obstest[i,1] )
#  ItemIndex = which(colnames == Obstest[i,3])
#  if(is.na(matrix[UserIndex,ItemIndex]))
#  {
#    matrix[UserIndex,ItemIndex] = Obstest[i,4]     
#  }
#  else {
#    matrix[UserIndex,ItemIndex] = matrix[UserIndex,ItemIndex] + Obstest[i,4]     
#  }
#  counter = counter +1
#}

UserClicks <- c(Observations$USERID, Observations$CLICK)
AOF <- table(UserClicks)
AmountOfOffers <- AmountOfOffers[order(-as.numeric(AOF[,2])),]

AOF <- table(UserClicks)
AOF <- cbind(names(AOF),as.numeric(AOF))
AmountOfOffers <- AOF[order(-as.numeric(AOF[,2])),]




