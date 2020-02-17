########################################################################
######################## DATA CLEANING #################################
########################################################################

########################################################################
######################### Initialize ###################################
########################################################################

rm(list = ls())

#load packageas
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
OfferDetails$STAR_RATING[OfferDetails$STAR_RATING == 0] <- 3 

# change country_name costa del sol to Spanje
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Costa del Sol'] = 'Spanje'
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Spanje '] = 'Spanje'

# change country_name 'Egypte ' to 'Egypte'
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Egypte '] = 'Egypte'

# change country_name = roda and city_name = Griekenland around
OfferDetails$CITY_NAME[OfferDetails$COUNTRY_NAME == 'Roda'] = 'Roda'
OfferDetails$COUNTRY_NAME[OfferDetails$COUNTRY_NAME == 'Roda'] = 'Griekenland'

# change meal plans
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


########################################################################
#################### data transformation ###############################
########################################################################

# personen per kamer
OfferDetails = merge(OfferDetails, Occupancy_table, by.x = "ROOM_OCCUPANCY", by.y = "ï..Room_types", sort = FALSE)

# discount rate
OfferDetails$DISCOUNT_RATE <- (as.numeric(OfferDetails$PRICE_ORIGINAL) - as.numeric(OfferDetails$PRICE)) / as.numeric(OfferDetails$PRICE_ORIGINAL)

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

# categorical values
OfferDetails$COUNTRY_NAME <- as.factor(OfferDetails$COUNTRY_NAME)
OfferDetails$MEAL_PLAN <- as.factor(OfferDetails$MEAL_PLAN)
OfferDetails$OFFER_VISUALISATION <- as.factor(OfferDetails$OFFER_VISUALISATION)
OfferDetails$REGION_NAME <- as.factor(OfferDetails$REGION_NAME)
OfferDetails$CITY_NAME <- as.factor(OfferDetails$CITY_NAME)

# numerical values
OfferDetails$OFFER_POSITION <- as.numeric(OfferDetails$OFFER_POSITION)
OfferDetails$REVIEW_RATING <- as.numeric(OfferDetails$REVIEW_RATING)
OfferDetails$DURATION <- as.numeric(OfferDetails$DURATION)
OfferDetails$PRICE <- as.numeric(OfferDetails$PRICE)
OfferDetails$PRICE_ORIGINAL <- as.numeric(OfferDetails$PRICE_ORIGINAL)
OfferDetails$STAR_RATING <- as.numeric(OfferDetails$STAR_RATING)
OfferDetails$Persoon <- as.numeric(OfferDetails$Persoon)
OfferDetails$max..volwassen <- as.numeric(OfferDetails$max..volwassen)
OfferDetails$max..kinderen <- as.numeric(OfferDetails$max..kinderen)
OfferDetails$DISCOUNT_RATE <- as.numeric(OfferDetails$DISCOUNT_RATE)
OfferDetails$JANUARY <- as.numeric(OfferDetails$JANUARY)
OfferDetails$FEBRUARY <- as.numeric(OfferDetails$FEBRUARY)
OfferDetails$MARCH <- as.numeric(OfferDetails$MARCH)
OfferDetails$APRIL <- as.numeric(OfferDetails$APRIL)
OfferDetails$MAY <- as.numeric(OfferDetails$MAY)
OfferDetails$JUNE <- as.numeric(OfferDetails$JUNE)
OfferDetails$JULY <- as.numeric(OfferDetails$JULY)
OfferDetails$AUGUST <- as.numeric(OfferDetails$AUGUST)
OfferDetails$SEPTEMBER <- as.numeric(OfferDetails$SEPTEMBER)
OfferDetails$OCTOBER <- as.numeric(OfferDetails$OCTOBER)
OfferDetails$NOVEMBER <- as.numeric(OfferDetails$NOVEMBER)
OfferDetails$DECEMBER <- as.numeric(OfferDetails$DECEMBER)

# review rating correction
OfferDetails$REVIEW_RATING[ OfferDetails$REVIEW_RATING == 0 ] <- NA
OfferDetails$REVIEW_RATING[ OfferDetails$REVIEW_RATING == '-' ] <- NA
OfferDetails$REVIEW_RATING = str_replace(OfferDetails$REVIEW_RATING,',', '.')
OfferDetails$REVIEW_RATING <- as.numeric(OfferDetails$REVIEW_RATING)

# missing values
OfferDetails$REVIEW_RATING[is.na(OfferDetails$REVIEW_RATING)] <- mean(OfferDetails$REVIEW_RATING[!is.na(OfferDetails$REVIEW_RATING)])
OfferDetails$PRICE[is.na(OfferDetails$PRICE)] <- 299
OfferDetails$PRICE_ORIGINAL[is.na(OfferDetails$PRICE_ORIGINAL)] <- OfferDetails$PRICE[is.na(OfferDetails$PRICE_ORIGINAL)]
OfferDetails$DISCOUNT_RATE[is.na(OfferDetails$DISCOUNT_RATE)] <- 0

# price per day
OfferDetails$PRICE_PER_DAY <- (as.numeric(OfferDetails$PRICE) / as.numeric(OfferDetails$DURATION))

# transforming USPs in to binary variables
OfferDetails$BEACH <- as.numeric(str_detect(OfferDetails$USP1, 'strand')) + as.numeric(str_detect(OfferDetails$USP2, 'strand')) +  as.numeric(str_detect(OfferDetails$USP3, 'strand'))
# ... 
# ...
# more to come

# transform categorical variables to multiple binary variables
## Offer visualisation
OfferDetails$FULL_WIDTH <- ifelse(str_detect(OfferDetails$OFFER_VISUALISATION,'full'),1,0)

## country name (alles = 0 betekent Griekenland)
OfferDetails$BULGARIA <- ifelse(OfferDetails$COUNTRY_NAME == 'Bulgarije',1,0)
OfferDetails$EGYPT <- ifelse(OfferDetails$COUNTRY_NAME == 'Egypte',1,0)
OfferDetails$CYPRUS <- ifelse(OfferDetails$COUNTRY_NAME == 'Cyprus',1,0)
OfferDetails$PORTUGAL <- ifelse(OfferDetails$COUNTRY_NAME == 'Portugal',1,0)
OfferDetails$CROATIA <- ifelse(OfferDetails$COUNTRY_NAME == 'Kroatië',1,0)
OfferDetails$SPAIN <- ifelse(OfferDetails$COUNTRY_NAME == 'Spanje',1,0)
OfferDetails$TURKEY <- ifelse(OfferDetails$COUNTRY_NAME == 'Turkije',1,0)
OfferDetails$MONTENEGRO <- ifelse(OfferDetails$COUNTRY_NAME == 'Montenegro',1,0)
OfferDetails$CAPE_VERDE <- ifelse(OfferDetails$COUNTRY_NAME == 'Kaapverdië',1,0)
OfferDetails$MALTA <- ifelse(OfferDetails$COUNTRY_NAME == 'Malta',1,0)
OfferDetails$ITALY <- ifelse(OfferDetails$COUNTRY_NAME == 'Italië',1,0)
OfferDetails$UAE <- ifelse(OfferDetails$COUNTRY_NAME == 'Verenigde Arabische Emiraten',1,0)
OfferDetails$MOROCCO <- ifelse(OfferDetails$COUNTRY_NAME == 'Marokko',1,0)
OfferDetails$TUNESIA <- ifelse(OfferDetails$COUNTRY_NAME == 'Tunesie',1,0)
OfferDetails$ISRAEL <- ifelse(OfferDetails$COUNTRY_NAME == 'Israël',1,0)

## Meal Plan (alles = 0 betekent AI (all inclusive))
OfferDetails$LG <- ifelse(OfferDetails$MEAL_PLAN == 'LG',1,0)
OfferDetails$LO <- ifelse(OfferDetails$MEAL_PLAN == 'LO',1,0)
OfferDetails$UAI <- ifelse(OfferDetails$MEAL_PLAN == 'UAI',1,0)
OfferDetails$HP <- ifelse(OfferDetails$MEAL_PLAN == 'HP',1,0)
OfferDetails$VP <- ifelse(OfferDetails$MEAL_PLAN == 'VP',1,0)

## Duration (alles = 0 betekent 8 dagen)
OfferDetails$FOUR_DAYS <- ifelse(OfferDetails$DURATION == 4,1,0)
OfferDetails$FIVE_DAYS <- ifelse(OfferDetails$DURATION == 5,1,0)
OfferDetails$SIX_DAYS <- ifelse(OfferDetails$DURATION == 6,1,0)
OfferDetails$SEVEN_DAYS <- ifelse(OfferDetails$DURATION == 7,1,0)
OfferDetails$NINE_DAYS <- ifelse(OfferDetails$DURATION == 9,1,0)
OfferDetails$TEN_DAYS <- ifelse(OfferDetails$DURATION == 10,1,0)
OfferDetails$ELEVEN_DAYS <- ifelse(OfferDetails$DURATION == 11,1,0)
OfferDetails$TWELVE_DAYS <- ifelse(OfferDetails$DURATION == 12,1,0)
OfferDetails$THIRTEEN_DAYS <- ifelse(OfferDetails$DURATION == 13,1,0)

## remove Duration column
OfferDetails <- subset(OfferDetails, select = -DURATION)

# order by offerid + mailid
OfferDetails = OfferDetails[order(OfferDetails$OFFERID, OfferDetails$MAILID),]

# keep only numeric values
OfferDetails <- dplyr::select_if(OfferDetails, is.numeric)
