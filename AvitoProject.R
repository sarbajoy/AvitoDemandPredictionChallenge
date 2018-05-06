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

##convert deal probability to binary variable of if product sold
Avito$didSell <- ifelse(Avito$deal_probability>0.5,c(1),c(0))

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

#################### MULTIPLE LINEAR REGRESSION ###############################
linearRegression = lm(deal_probability~region+parent_category_name+price
+item_seq_number+user_type+image_top_1+titleLength+descriptionLength+numParam
+day,data=AvitoTrain)

library(devtools)
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
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+parent_category_name+
price+item_seq_number+user_type+image_top_1+titleLength+descriptionLength
+numParam+isMon+isWed+isThu,data=AvitoTrain)

summary(linearRegression)

##we want 5% confidence hence remove wednesday
linearRegression = lm(deal_probability~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+parent_category_name
+price+item_seq_number+user_type+image_top_1+titleLength+descriptionLength
+numParam+isMon+isThu,data=AvitoTrain)

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

######################### LOGISTIC REGRESSION ##################################
library(caret)
logRegression=glm(didSell~region+parent_category_name+price+item_seq_number
+user_type+image_top_1+titleLength+descriptionLength+numParam+day,
data=AvitoTrain,family=binomial)

summary(logRegression)

logRegression=glm(didSell~isVolgogradRegion+isKrasnodarRegion
+isKrasnoyarskRegion+isNovosibirskRegion +isRostovRegion + isTyumenRegion
+isParentHome +isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type+image_top_1
+titleLength+descriptionLength+numParam+isSun+isMon+isTue+isWed+isThu+isFri,
data=AvitoTrain,family=binomial)

summary(logRegression)

##remove description length
logRegression=glm(didSell~isVolgogradRegion+isKrasnodarRegion
+isKrasnoyarskRegion+isNovosibirskRegion +isRostovRegion + isTyumenRegion
+isParentHome +isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type+image_top_1
+titleLength+numParam+isSun+isMon+isTue+isWed+isThu+isFri,data=AvitoTrain,
family=binomial)

summary(logRegression)

##remove volgograd
logRegression=glm(didSell~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+isParentHome
+isParentAnimals +isParentPersonal+isParentProperty+isParentTransport
+isParentServices+price+item_seq_number+user_type+image_top_1+titleLength
+numParam+isSun+isMon+isTue+isWed+isThu+isFri,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove friday
logRegression=glm(didSell~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+isParentHome
+isParentAnimals +isParentPersonal+isParentProperty+isParentTransport
+isParentServices+price+item_seq_number+user_type+image_top_1+titleLength
+numParam+isSun+isMon+isTue+isWed+isThu,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove friday
logRegression=glm(didSell~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+isParentHome
+isParentAnimals +isParentPersonal+isParentProperty+isParentTransport
+isParentServices+price+item_seq_number+user_type+image_top_1+titleLength
+numParam+isMon+isTue+isWed+isThu,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove title length
logRegression=glm(didSell~isKrasnodarRegion+isKrasnoyarskRegion
+isNovosibirskRegion +isRostovRegion + isTyumenRegion+isParentHome
+isParentAnimals +isParentPersonal+isParentProperty+isParentTransport
+isParentServices+price+item_seq_number+user_type+image_top_1+numParam+isMon
+isTue+isWed+isThu,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove krasnoyarsk
logRegression=glm(didSell~isKrasnodarRegion+isNovosibirskRegion
+isRostovRegion + isTyumenRegion+isParentHome+isParentAnimals +isParentPersonal
+isParentProperty+isParentTransport+isParentServices+price+item_seq_number
+user_type+image_top_1+numParam+isMon+isTue+isWed+isThu,data=AvitoTrain,
family=binomial)

summary(logRegression)

##remove wednesday
logRegression=glm(didSell~isKrasnodarRegion+isNovosibirskRegion
+isRostovRegion + isTyumenRegion+isParentHome+isParentAnimals +isParentPersonal
+isParentProperty+isParentTransport+isParentServices+price+item_seq_number
+user_type+image_top_1+numParam+isMon+isTue+isThu,data=AvitoTrain,
family=binomial)

summary(logRegression)

##remove Novosibirsk
logRegression=glm(didSell~isKrasnodarRegion+isRostovRegion +isTyumenRegion
+isParentHome+isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type
+image_top_1+numParam+isMon+isTue+isThu,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove monday
logRegression=glm(didSell~isKrasnodarRegion+isRostovRegion +isTyumenRegion
+isParentHome+isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type
+image_top_1+numParam+isTue+isThu,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove tuesday
logRegression=glm(didSell~isKrasnodarRegion+isRostovRegion +isTyumenRegion
+isParentHome+isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type
+image_top_1+numParam+isThu,data=AvitoTrain,family=binomial)

summary(logRegression)

##remove thursday
logRegression=glm(didSell~isKrasnodarRegion+isRostovRegion +isTyumenRegion
+isParentHome+isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type
+image_top_1+numParam,data=AvitoTrain,family=binomial)

summary(logRegression)

####################### CV DECISION TREE MODEL##################################
library(rpart)

decisionTree = rpart(deal_probability~region+isParentBusiness
+isParentHome +isParentAnimals +isParentPersonal+isParentProperty
+isParentTransport+isParentServices+price+item_seq_number+user_type
+image_top_1+titleLength+descriptionLength+numParam+day,data=AvitoTrain,
method="anova")

printcp(decisionTree) # display the results
plotcp(decisionTree) # visualize cross-validation results
summary(decisionTree) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(decisionTree) # visualize cross-validation results

# plot tree
plot(decisionTree, uniform=TRUE,
  	main="Regression Tree for Deal Probability ")
text(decisionTree, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree
post(decisionTree, file = "Downloads/decisionTree.ps",
  	title = "Regression Tree for Deal Probability ")

# prune the tree
pfit<- prune(decisionTree, cp=0.010000) # from cptable

# plot the pruned tree
plot(pfit, uniform=TRUE,
  	main="Pruned Regression Tree for Deal Probability")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "Downloads/prunedTree.ps",
  	title = "Pruned Regression Tree for Deal Probability")

## decision TREE

## random forest

##build ANN with continuous outcome

######################### CLASSIFY DATA #######################################

##text mining

##perform clustering
