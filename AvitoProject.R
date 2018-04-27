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

##create new columns to separate user type
Avito$IsPrivate <- ifelse(Avito$user_type == 'Private',c(1), c(0))
Avito$IsCompany <- ifelse(Avito$user_type == 'Company',c(1), c(0))
Avito$user_type = NULL

##find length of title and description
Avito$titleLength <- nchar(Avito$title,allowNA = TRUE, keepNA = 0)
Avito$descriptionLength <- ifelse (is.na(Avito$description),
c(0), c(nchar(Avito$description)))

## count number of parameters
Avito$P1 <- ifelse (is.na(Avito$param_1),c(0), c(1))
Avito$P2 <- ifelse (is.na(Avito$param_2),c(0), c(1))
Avito$P3 <- ifelse (is.na(Avito$param_3),c(0), c(1))
Avito$numPram <- Avito$P1+Avito$P2+Avito$P3
Avito$P1 = NULL
Avito$P2 = NULL
Avito$P3 = NULL

##convert date to day released (i.e. date to Sunday or Monday)
library(lubridate)
Avito$activation_date <- as.Date(Avito$activation_date)
Avito$day <- wday(Avito$activation_date,label=TRUE)
Avito$activation_date = NULL

##split data into training, testing and validation
library(caTools)
library(caret)
set.seed(1951)
Avito$split = sample.split(Avito$deal_probability,SplitRatio=0.05)
AvitoTrainVal= subset(Avito, split==TRUE)
AvitoTest = subset (Avito, split ==FALSE)

AvitoTrainVal$split = sample.split(AvitoTrainVal$deal_probability,SplitRatio=0.7)
AvitoTrain= subset(AvitoTrainVal, split==TRUE)
AvitoVal = subset (AvitoTrainVal, split ==FALSE)

#################### MAKE PREDICTIVE MODELS ###################################

## build generalized linear regression model

##build continuous variable decision tree model

##build continuous variable random forest model

##build ANN with continuous outcome

######################### CLASSIFY DATA #######################################

##text mining

##perform clustering
