rm(list=ls())

Avito = read.csv("Downloads/AvitoTrain.csv")

##set up translate
library(translate)
set.key("AIzaSyAJvkZgYIjwNDTiz6pAWZOV-bS4oveRd_4")

######################### CLEAN UP DATA #######################################
##find average of deal probability to see standard
average=mean(Avito$deal_probability)

##delete columns that do not play a role
Avito$item_id=NULL
Avito$user_id=NULL
Avito$image=NULL

##find length of title and description
Avito$titleLength <- nchar(Avito$title,allowNA = TRUE, keepNA = 0)
Avito$descriptionLength <- ifelse (is.na(Avito$description),
c(0), c(nchar(Avito$description)))

## count number of parameters
Avito$P1 <- ifelse (is.na(Avito$param_1),c(0), c(1))
Avito$P2 <- ifelse (is.na(Avito$param_2),c(0), c(1))
Avito$P3 <- ifelse (is.na(Avito$param_3),c(0), c(1))
Avito$numParam <- Avito$P1+Avito$P2+Avito$P3
Avito$P1 = NULL
Avito$P2 = NULL
Avito$P3 = NULL

##convert date to day released (i.e. date to Sunday or Monday)
library(lubridate)
Avito$activation_date <- as.Date(Avito$activation_date)
Avito$day <- wday(Avito$activation_date,label=TRUE)
Avito$activation_date = NULL

Avito$price <- ifelse(is.na(Avito$price),c(0),Avito$price)

##convert deal probability to binary variable of if product sold
Avito$didSell <- ifelse(Avito$deal_probability>0.5,c(1),c(0))

library(ggplot2)
library(dplyr)

##find unique countries and make a table for them
regions=unique(Avito$region)
parentCategories=unique(Avito$parent_category_name)
days = unique(Avito$day)

##translate region
translatedRegions=NULL
for (region in regions){
  transRegion=translate(region,"ru","en")
  translatedRegions=c(translatedRegions,transRegion)
}

Avito$isSverdlovsk = ifelse(Avito$region==regions[1],c(1),c(0))
Avito$isSamara = ifelse(Avito$region==regions[2],c(1),c(0))
Avito$isRostov = ifelse(Avito$region==regions[3],c(1),c(0))
Avito$isTatarstan = ifelse(Avito$region==regions[4],c(1),c(0))
Avito$isVolgograd = ifelse(Avito$region==regions[5],c(1),c(0))
Avito$isNizhnyNovgorod = ifelse(Avito$region==regions[6],c(1),c(0))
Avito$isPermRegion = ifelse(Avito$region==regions[7],c(1),c(0))
Avito$isOrenberg = ifelse(Avito$region==regions[8],c(1),c(0))
Avito$isKhanty = ifelse(Avito$region==regions[9],c(1),c(0))
Avito$isTyumen = ifelse(Avito$region==regions[10],c(1),c(0))
Avito$isBashkortostan = ifelse(Avito$region==regions[11],c(1),c(0))
Avito$isKrasnodar= ifelse(Avito$region==regions[12],c(1),c(0))
Avito$isNovosibirsk= ifelse(Avito$region==regions[13],c(1),c(0))
Avito$isOmsk= ifelse(Avito$region==regions[14],c(1),c(0))
Avito$isBelgorod= ifelse(Avito$region==regions[15],c(1),c(0))
Avito$isChelyabinsk= ifelse(Avito$region==regions[16],c(1),c(0))
Avito$isVoronezh= ifelse(Avito$region==regions[17],c(1),c(0))
Avito$isKemerovo= ifelse(Avito$region==regions[18],c(1),c(0))
Avito$isSaratov= ifelse(Avito$region==regions[19],c(1),c(0))
Avito$isVladimir= ifelse(Avito$region==regions[20],c(1),c(0))
Avito$isKalinigrad= ifelse(Avito$region==regions[21],c(1),c(0))
Avito$isKrasnoyarsk= ifelse(Avito$region==regions[22],c(1),c(0))
Avito$isYaroslavl= ifelse(Avito$region==regions[23],c(1),c(0))
Avito$isUdmurtia= ifelse(Avito$region==regions[24],c(1),c(0))
Avito$isAltai= ifelse(Avito$region==regions[25],c(1),c(0))
Avito$isIrkutsk= ifelse(Avito$region==regions[26],c(1),c(0))
Avito$isStavropol= ifelse(Avito$region==regions[27],c(1),c(0))
Avito$isTula= ifelse(Avito$region==regions[28],c(1),c(0))

##translate parent categories
translatedParents=NULL
for (parent in parentCategories){
  transParent=translate(parent,"ru","en")
  translatedParents=c(translatedParents,transParent)
}

Avito$isPersonal= ifelse(Avito$parent_category_name==parentCategories[1],
c(1),c(0))
Avito$isHome= ifelse(Avito$parent_category_name==parentCategories[2],
c(1),c(0))
Avito$isElectronics= ifelse(Avito$parent_category_name==parentCategories[3],
c(1),c(0))
Avito$isTransport= ifelse(Avito$parent_category_name==parentCategories[4],
c(1),c(0))
Avito$isProperty= ifelse(Avito$parent_category_name==parentCategories[5],
c(1),c(0))
Avito$isAnimals= ifelse(Avito$parent_category_name==parentCategories[6],
c(1),c(0))
Avito$isHobbies= ifelse(Avito$parent_category_name==parentCategories[7],
c(1),c(0))
Avito$isServices= ifelse(Avito$parent_category_name==parentCategories[8],
c(1),c(0))
Avito$isBusiness= ifelse(Avito$parent_category_name==parentCategories[9],
c(1),c(0))

##change days to binary
Avito$isSun= ifelse(Avito$day=="Sun",c(1),c(0))
Avito$isMon= ifelse(Avito$day=="Mon",c(1),c(0))
Avito$isTue= ifelse(Avito$day=="Tue",c(1),c(0))
Avito$isWed = ifelse(Avito$day=="Wed",c(1),c(0))
Avito$isThu = ifelse(Avito$day=="Thu",c(1),c(0))
Avito$isFri = ifelse(Avito$day=="Fri",c(1),c(0))
Avito$isSat = ifelse(Avito$day=="Sat",c(1),c(0))


######################### LOGISTIC REGRESSION ##################################
library(caret)
logRegression=glm(didSell~region+parent_category_name+price+item_seq_number
+user_type+image_top_1+titleLength+descriptionLength+numParam+day,
data=Avito,family=binomial)

summary(logRegression)

logRegression=glm(didSell~isSverdlovsk+isSamara+isRostov+isTatarstan+isVolgograd
+isNizhnyNovgorod+isPermRegion+isOrenberg+isKhanty+isTyumen+isBashkortostan+
isKrasnodar+isNovosibirsk+isOmsk+isBelgorod+isChelyabinsk+isVoronezh+isKemerovo+
isSaratov+isVladimir+isKalinigrad+isKrasnoyarsk+isYaroslavl+isUdmurtia+isAltai
+isIrkutsk+isStavropol+isPersonal+isHome+isElectronics+isTransport+isProperty
+isAnimals+isHobbies+isServices+price+item_seq_number+user_type+image_top_1
+titleLength+descriptionLength+numParam+isSun+isMon+isTue+isWed+isThu+isFri,
data=Avito,family=binomial)

summary(logRegression)

##decision tree

## random forest

##neural network

##gradient boosting tree
