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

AvitoTrainVal$split = sample.split(AvitoTrainVal$deal_probability,
SplitRatio=0.7)
AvitoTrain= subset(AvitoTrainVal, split==TRUE)
AvitoVal = subset (AvitoTrainVal, split ==FALSE)

AvitoTrain$price <- ifelse(is.na(AvitoTrain$price),c(0),AvitoTrain$price)
AvitoVal$price <- ifelse(is.na(AvitoVal$price),c(0),AvitoVal$price)
Avito$price <- ifelse(is.na(Avito$price),c(0),Avito$price)
AvitoTest$price <- ifelse(is.na(AvitoTest$price),c(0),AvitoTest$price)

#################### MAKE PREDICTIVE MODELS ###################################

## build generalized linear regression model
linearRegression = lm(deal_probability~region+city+parent_category_name
+category_name+param_1+param_2+param_3+price+item_seq_number+user_type+
image_top_1+titleLength+descriptionLength+numPram+day,data=AvitoTrain)

library(devtools)
install_github("dgrtwo/broom")
library(broom)

tidyLR <- tidy(linearRegression)
write.csv(tidyLR,"Downloads/tidyLR.csv")

##from csv, look at variables with 1% significance since there are so many
## variables, and make a binary variable out of those

##build continuous variable decision tree model

##build continuous variable random forest model

##build ANN with continuous outcome

######################### CLASSIFY DATA #######################################

##text mining

##perform clustering
