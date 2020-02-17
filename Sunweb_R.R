########################################################################
######################### Initialize ###################################
########################################################################

rm(list = ls())

#load packages
library(utils)
library(stringr)
library(ngram) #for preprocess(x) function to lose capitals and extra spaces

########################################################################
######################### Import and format data #######################
########################################################################

OfferDetails = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)
Occupancy_table = read.csv(file.choose(), header = T, sep = ';',stringsAsFactors = FALSE)

########################################################################
######################### Clean data ###################################
########################################################################

#delete USP from row 2310 and shift values
for (i in 12:19){
  OfferDetails[2310,i] <- OfferDetails[2310,i+1]
}
OfferDetails[2310,20] <- OfferDetails[2311,1]
OfferDetails <- OfferDetails[-2311,]

for (i in 1:dim(OfferDetails)[1]){
  OfferDetails$USP1[i] = preprocess(OfferDetails$USP1[i])
  OfferDetails$USP2[i] = preprocess(OfferDetails$USP2[i])
  OfferDetails$USP3[i] = preprocess(OfferDetails$USP3[i])
  OfferDetails$MEAL_PLAN[i] = preprocess(OfferDetails$MEAL_PLAN[i])
}

#replace string 'NULL' by NA
OfferDetails[ OfferDetails == 'NULL' ] <- NA

# review rating, change '-' and zeros to empty cell
OfferDetails$REVIEW_RATING[OfferDetails$REVIEW_RATING == '-'] <- '' 
OfferDetails$REVIEW_RATING[OfferDetails$REVIEW_RATING == 0] <- ''
OfferDetails$REVIEW_RATING <- str_replace(OfferDetails$REVIEW_RATING, ',', '.')

# star rating, change comma by dot for numeric values
OfferDetails$STAR_RATING <- str_replace(OfferDetails$STAR_RATING, ',', '.')

# change country_name costa del sol to Spanje
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Costa del Sol'] = 'Spanje'
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Spanje '] = 'Spanje'


# change country_name = roda and city_name = Griekenland around
OfferDetails$CITY_NAME[OfferDetails$COUNTRY_NAME == 'Roda'] = 'Roda'
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Roda'] = 'Griekenland'

#change meal plans
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'ultra all inclusive', "UAI")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'logies met ontbijt', "LO")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'logies en ontbijt', "LO")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'logies & ontbijt', "LO")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'all inclusive', "AI")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'logies ontbijt', "LO")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'half pension', "HP")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'ail inclusive', "AI")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'all inlcusive', "AI")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'volpension', "VP")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'logies', "LG")
OfferDetails$MEAL_PLAN = str_replace(OfferDetails$MEAL_PLAN,'halfpension', "HP")

# dubbele spelingen van zelfde gebieden universeren
## Tunesia and Tunesie
OfferDetails$COUNTRY_NAME[str_detect(OfferDetails$COUNTRY_NAME,'Tunes') == TRUE] <- 'Tunesie'
## costa de Almeria
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME,'Costa de Almer') == TRUE] <- 'Costa de Almeria'
## Costa del Sol
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Costa del Sol') == TRUE] <- 'Costa del Sol'
## Cyprus, Cyprus. and Cyprus (PFO)
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME,'Cyprus') == TRUE] <- 'Cyprus'
## Epirus and Epirug (Parga)
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Epirus') == TRUE] <- 'Epirus'
## Kreta and Kreta (HER)
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Kreta') == TRUE] <- 'Kreta'
## Peloponnesos correctly spelled
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Pel') == TRUE] <- 'Peloponnesos'
## Turkish/Turkse riviera spelling samenvoegen
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Turk') == TRUE] <- 'Turkse Riviera'
## Zwarte zee gebied generaliseren -> Zwarte zee, Zwarte zeekust en Zwarte zeekust Varna samenvoegen
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Zwart') == TRUE] <- "Zwarte zee"
## (Noord/Zuid) Egeische zee
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Ege') == TRUE & str_detect(OfferDetails$REGION_NAME, 'Zuid') == FALSE & str_detect(OfferDetails$REGION_NAME, 'Noord') == FALSE] <- 'Egeische kust'
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Ege') == TRUE & str_detect(OfferDetails$REGION_NAME, 'Zuid') == TRUE] <- 'Zuid-Egeische kust'
OfferDetails$REGION_NAME[str_detect(OfferDetails$REGION_NAME, 'Ege') == TRUE & str_detect(OfferDetails$REGION_NAME, 'Noord') == TRUE] <- 'Noord-Egeische kust'


# discount rate
OfferDetails$DISCOUNT_RATE <- (as.numeric(OfferDetails$PRICE_ORIGINAL) - as.numeric(OfferDetails$PRICE)) / as.numeric(OfferDetails$PRICE_ORIGINAL)

# personen per kamer
OfferDetails = merge(OfferDetails, Occupancy_table, by.x = "ROOM_OCCUPANCY", by.y = "ï..ROOM_OCCUPANCY", sort = FALSE)


# review rating correction
OfferDetails$REVIEW_RATING[ OfferDetails$REVIEW_RATING == 0 ] <- NA
OfferDetails$REVIEW_RATING[ OfferDetails$REVIEW_RATING == '-' ] <- NA
OfferDetails$REVIEW_RATING = str_replace(OfferDetails$REVIEW_RATING,',', '.')
OfferDetails$REVIEW_RATING <- as.numeric(OfferDetails$REVIEW_RATING)


# departure month binary vars
OfferDetails$JANUARY <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Ja')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'ja'))
OfferDetails$FEBRUARY <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Fe')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'fe'))
OfferDetails$MARCH <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Mar')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'ma'))
OfferDetails$APRIL <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Ap')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'ap'))
OfferDetails$MAY <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'May')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'me'))
OfferDetails$JUNE <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Jun')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'jun'))
OfferDetails$JULY <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Jul')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'jul'))
OfferDetails$AUGUST <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Au')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'au'))
OfferDetails$SEPTEMBER <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Se')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'se'))
OfferDetails$OCTOBER <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'Oc')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'ok'))
OfferDetails$NOVEMBER <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'No')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'no'))
OfferDetails$DECEMBER <- as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'De')) + as.numeric(str_detect(OfferDetails$DEPARTURE_DATE, 'de'))
# multiple options per month
OfferDetails$MONTH_SUM <- rowSums(OfferDetails[,which(colnames(OfferDetails)=="JANUARY"):which(colnames(OfferDetails)=="DECEMBER")])

# binary USP 
# check voor meest voorkomende woorden
test <- strsplit(c(OfferDetails$USP1,OfferDetails$USP2,OfferDetails$USP3), " ")
test2 <- unlist(test)
testfreq <- table(test2)
result <- cbind(names(testfreq),as.numeric(testfreq))



OfferDetails$BEACH <- as.numeric(str_detect(OfferDetails$USP1, 'strand')) + as.numeric(str_detect(OfferDetails$USP2, 'strand')) +  as.numeric(str_detect(OfferDetails$USP3, 'strand'))


# order by offerid + mailid
OfferDetails = OfferDetails[order(OfferDetails$OFFERID, OfferDetails$MAILID),]

########################################################################
######################### Get statistics from data #####################
########################################################################

#getting summary
#OfferDetails$ROOM_OCCUPANCY <- as.factor(OfferDetails$ROOM_OCCUPANCY)
#OfferDetails$OFFER_VISUALISATION <- as.factor(OfferDetails$OFFER_VISUALISATION)
#OfferDetails$COUNTRY_NAME <- as.factor(OfferDetails$COUNTRY_NAME)
#OfferDetails$REGION_NAME <- as.factor(OfferDetails$REGION_NAME)
#OfferDetails$CITY_NAME <- as.factor(OfferDetails$CITY_NAME)
#OfferDetails$ACCOMODATION_NAME <- as.factor(OfferDetails$ACCOMODATION_NAME)
#OfferDetails$REVIEW_RATING <- as.factor(OfferDetails$REVIEW_RATING)
#summary(OfferDetails)

#1 number of unique mail IDs
length(table(OfferDetails$MAILID))

#2 number of unique offer IDs
length(table(OfferDetails$OFFERID))

#3 offer visualisation ???


#4 Offer Position, min max avg 
summary(OfferDetails$OFFER_POSITION)


#5 Country Name, most common top ?5? 
x <- table(OfferDetails$COUNTRY_NAME)
x <- cbind(names(x),as.numeric(x))
x <- x[order(-as.numeric(x[,2])),]
x
length(x[,1])

#6 Region Name, idem as above for region.
y <- table(OfferDetails$REGION_NAME)
y <- cbind(names(y),as.numeric(y))
y <- y[order(-as.numeric(y[,2])),]
y
length(y[,1])

#7 City Name, idem for city.
z <- table(OfferDetails$CITY_NAME)
z <- cbind(names(z), as.numeric(z))
z <- z[order(-as.numeric(z[,2])),]
z
length(z[,1])

#8 Accommodation Name, idem for accommodation.
p <- table(OfferDetails$ACCOMMODATION_NAME)
p <- cbind(names(p), as.numeric(p))
p <- p[order(-as.numeric(p[,2])),]
p
length(p[,1])

#9 Review Rating, min, max, avg, number of missing values --> what we do with missing values in Data??
summary(as.numeric(OfferDetails$REVIEW_RATING))

#10 USP (1,  2 and 3), most common words -> what we do with this in Data??

result[order(-as.numeric(result[,2])),]

#11 Room Occupancy, ???

#12 Duration, most frequent amount (=8) -> why this is most common in data??
q <- table(OfferDetails$DURATION)
q <- cbind(names(q), as.numeric(q))
q <- q[order(-as.numeric(q[,2])),]
q

#13 Departure Date, most frequent month, number of zeros (and why)?
# offers per month
colSums(OfferDetails[,which(colnames(OfferDetails)=="JANUARY"):which(colnames(OfferDetails)=="DECEMBER")])
# number of zeros
length(which(OfferDetails$MONTH_SUM==0))



#14 Price; min, max, avg
options(digits = 8)
summary(as.numeric(OfferDetails$PRICE))

#15 Original Price; min, max, avg
summary(as.numeric(OfferDetails$PRICE_ORIGINAL))

#16 Meal Plan; frequency per plan 
r <- table(OfferDetails$MEAL_PLAN)
r <- cbind(names(r),as.numeric(r))
r[order(-as.numeric(r[,2])),]

#17 Roomtype; -

#18 Star Rating; min, max, avg, ??correlation star rating - review rating??

summary(as.numeric(OfferDetails$STAR_RATING))
length(which(OfferDetails$STAR_RATING==0))

cor(as.numeric(OfferDetails$REVIEW_RATING), as.numeric(OfferDetails$STAR_RATING)) #0.2307674


