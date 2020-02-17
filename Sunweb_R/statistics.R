########################################################################
################## GET INTERESTING STATISTICS OF DATA ##################
########################################################################

#1 number of unique mail IDs
length(table(OfferDetails$MAILID))

#2 number of unique offer IDs
length(table(OfferDetails$OFFERID))

#3 offer visualisation ???

#4 Offer Position, min max avg 
summary(OfferDetails$OFFER_POSITION)
hist(OfferDetails$OFFER_POSITION, main = '',xlab = 'Offer position',breaks = c(1:14))

#5 Country Name, most common top ?5? 
x <- table(OfferDetails$COUNTRY_NAME)
x <- cbind(names(x),as.numeric(x))
x[order(-as.numeric(x[,2])),]

#6 Region Name, idem as above for region.
y <- table(OfferDetails$REGION_NAME)
y <- cbind(names(y),as.numeric(y))
y[order(-as.numeric(y[,2])),]

#7 City Name, idem for city.
z <- table(OfferDetails$CITY_NAME)
z <- cbind(names(z), as.numeric(z))
z[order(-as.numeric(z[,2])),]

#8 Accommodation Name, idem for accommodation.
p <- table(OfferDetails$ACCOMMODATION_NAME)
p <- cbind(names(p), as.numeric(p))
p[order(-as.numeric(p[,2])),]

#9 Review Rating, min, max, avg, number of missing values --> what we do with missing values in Data??
summary(as.numeric(OfferDetails$REVIEW_RATING))
hist(as.numeric(OfferDetails$REVIEW_RATING), main = '', xlab = 'Review rating', breaks = 12)

#10 USP (1,  2 and 3), most common words -> what we do with this in Data??

# check voor meest voorkomende woorden
test <- strsplit(c(OfferDetails$USP1,OfferDetails$USP2,OfferDetails$USP3), " ")
test2 <- unlist(test)
testfreq <- table(test2)
result <- cbind(names(testfreq),as.numeric(testfreq))
result[order(-as.numeric(result[,2])),]

#11 Room Occupancy, ???

#12 Duration, most frequent amount (=8) -> why this is most common in data??
q <- table(OfferDetails$DURATION)
q <- cbind(names(q), as.numeric(q))
q[order(-as.numeric(q[,2])),]

#13 Departure Date, most frequent month, number of zeros (and why)?

# departure month binary vars

# multiple options per month
OfferDetails$MONTH_SUM <- rowSums(OfferDetails[,which(colnames(OfferDetails)=="JANUARY"):which(colnames(OfferDetails)=="DECEMBER")])
# offers per month
colSums(OfferDetails[,which(colnames(OfferDetails)=="JANUARY"):which(colnames(OfferDetails)=="DECEMBER")])
# number of zeros
length(which(OfferDetails$MONTH_SUM==0))

#14 Price; min, max, avg
summary(as.numeric(OfferDetails$PRICE))

#15 Original Price; min, max, avg
summary(as.numeric(OfferDetails$PRICE_ORIGINAL))

#16 Meal Plan; frequency per plan 
r <- table(OfferDetails$MEAL_PLAN)
r <- cbind(names(r),as.numeric(r))
r[order(-as.numeric(r[,2])),]

#17 Roomtype; -

#18 Star Rating; min, max, avg, ??correlation star rating - review rating??

summary(as.numeric(OfferDetails$STAR_RATING[OfferDetails$STAR_RATING > 0]))
cor(as.numeric(OfferDetails$REVIEW_RATING), as.numeric(OfferDetails$STAR_RATING)) #0.2307674

#19 plot for position of offers
barplot(table(OfferDetails$OFFER_POSITION))

#20 plot for offers per month
barplot(colSums(subset(OfferDetails, select = JAN:DEC)))

# number of zeros
length(which(OfferDetails$MONTH_SUM==0))

#21 distribution of review rating
