rm(list=ls())

Avito = read.csv("Downloads/AvitoTrain.csv")

##set up translate
library(translate)
set.key("AIzaSyAJvkZgYIjwNDTiz6pAWZOV-bS4oveRd_4")

##translate(text,"ru","en")

######################### CLEAN UP DATA #######################################

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

##split data into training, testing and validation
library(caTools)
library(caret)
set.seed(1951)
Avito$split = sample.split(Avito$deal_probability,SplitRatio=0.05)
AvitoTrainVal= subset(Avito, split==TRUE)
AvitoTest = subset (Avito, split ==FALSE)

AvitoTrainVal$split = sample.split(AvitoTrainVal$deal_probability,
SplitRatio=0.7)
AvitoTrain= subset(AvitoTrainVal, split==TRUE)
AvitoVal = subset (AvitoTrainVal, split ==FALSE)
AvitoTrainBackup=AvitoTrain

#################### MAKE PREDICTIVE MODELS ###################################

## build generalized linear regression model
linearRegression = lm(deal_probability~region+parent_category_name+price
+item_seq_number+user_type+image_top_1+titleLength+descriptionLength+numParam
+day,data=AvitoTrain)

library(devtools)
install_github("dgrtwo/broom")
library(broom)

tidyLR <- tidy(linearRegression)
write.csv(tidyLR,"Downloads/tidyLR.csv")


##regions determined to be significant according to first iteration of MLR
AvitoTrain$isVolgogradRegion <- ifelse (AvitoTrain$region ==
"Волгоградская область",c(1), c(0))
AvitoTrain$isKrasnodarRegion <- ifelse (AvitoTrain$region ==
"Краснодарский край",c(1), c(0))
AvitoTrain$isKrasnoyarskRegion <- ifelse (AvitoTrain$region ==
"Красноярский край",c(1), c(0))
AvitoTrain$isNovosibirskRegion <- ifelse (AvitoTrain$region ==
"Новосибирская область",c(1), c(0))
AvitoTrain$isRostovRegion <- ifelse (AvitoTrain$region ==
"Ростовская область",c(1), c(0))
AvitoTrain$isTyumenRegion <- ifelse (AvitoTrain$region ==
"Тюменская область",c(1), c(0))

AvitoTrain$isSun <- ifelse (AvitoTrain$day =="Sun",c(1), c(0))
AvitoTrain$isMon <- ifelse (AvitoTrain$day =="Mon",c(1), c(0))
AvitoTrain$isTue <- ifelse (AvitoTrain$day =="Tue",c(1), c(0))
AvitoTrain$isWed <- ifelse (AvitoTrain$day =="Wed",c(1), c(0))
AvitoTrain$isThu <- ifelse (AvitoTrain$day =="Thu",c(1), c(0))
AvitoTrain$isFri <- ifelse (AvitoTrain$day =="Fri",c(1), c(0))
AvitoTrain$isSat <- ifelse (AvitoTrain$day =="Sat",c(1), c(0))

linearRegression = lm(deal_probability~isVolgogradRegion+isKrasnodarRegion
+isKrasnoyarskRegion+isNovosibirskRegion +isRostovRegion + isTyumenRegion
+parent_category_name+price+item_seq_number+user_type+image_top_1
+titleLength+descriptionLength+numParam+isSun+isMon+isTue+isWed+
isThu+isFri,data=AvitoTrain)

summary(linearRegression)

##remove volgograd, sunday and friday from equation

linearRegression = lm(deal_probability~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+parent_category_name+price
+item_seq_number+user_type+image_top_1+titleLength+descriptionLength+numParam
+isMon+isTue+isWed+isThu,data=AvitoTrain)

summary(linearRegression)

##we want 5% confidence hence remove tuesday
linearRegression = lm(deal_probability~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+parent_category_name+price
+item_seq_number+user_type+image_top_1+titleLength+descriptionLength+numParam
+isMon+isWed+isThu,data=AvitoTrain)

summary(linearRegression)

##we want 5% confidence hence remove wednesday
linearRegression = lm(deal_probability~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+parent_category_name+price
+item_seq_number+user_type+image_top_1+titleLength+descriptionLength+numParam
+isMon+isThu,data=AvitoTrain)

summary(linearRegression)

##convert parent categories to binary
AvitoTrain$isParentBusiness <- ifelse(AvitoTrain$parent_category_name
=="Для бизнеса",c(1), c(0))
AvitoTrain$isParentHome <- ifelse(AvitoTrain$parent_category_name
=="Для дома и дачи",c(1), c(0))
AvitoTrain$isParentAnimals <- ifelse(AvitoTrain$parent_category_name
 =="Животные",c(1), c(0))
AvitoTrain$isParentPersonal <- ifelse(AvitoTrain$parent_category_name
=="Личные вещи",c(1), c(0))
AvitoTrain$isParentProperty <- ifelse(AvitoTrain$parent_category_name
=="Недвижимость",c(1), c(0))
AvitoTrain$isParentTransport <- ifelse(AvitoTrain$parent_category_name
=="Транспорт",c(1), c(0))
AvitoTrain$isParentServices <- ifelse(AvitoTrain$parent_category_name
=="Услуги",c(1), c(0))
AvitoTrain$isParentHobbies <- ifelse(AvitoTrain$parent_category_name
=="Хобби и отдых",c(1), c(0))

linearRegression = lm(deal_probability~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+isParentBusiness
+isParentHome +isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type
+image_top_1+titleLength+descriptionLength+numParam+isMon+isThu,data=AvitoTrain)

summary(linearRegression)

##build continuous variable decision tree model

##build continuous variable random forest model

##build ANN with continuous outcome

######################### CLASSIFY DATA #######################################

##text mining

##perform clustering
