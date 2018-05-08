rm(list=ls())

Avito = read.csv("Downloads/AvitoTrain.csv")

##set up translate
library(translate)
set.key("AIzaSyAJvkZgYIjwNDTiz6pAWZOV-bS4oveRd_4")

##translate(text,"ru","en")

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
parentCategories=unique(Avito$region)
days = unique(Avito$day)

######################### LOGISTIC REGRESSION ##################################
library(caret)
logRegression=glm(didSell~region+parent_category_name+price+item_seq_number
+user_type+image_top_1+titleLength+descriptionLength+numParam+day,
data=Avito,family=binomial)

summary(logRegression)

##decision tree

## random forest

##neural network

##gradient boosting tree
