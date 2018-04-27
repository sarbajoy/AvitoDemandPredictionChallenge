rm(list=ls())

Avito = read.csv("Downloads/AvitoTrain.csv")

##set up translate
library(translate)
set.key("AIzaSyAJvkZgYIjwNDTiz6pAWZOV-bS4oveRd_4")

##translate(region,"ru","en")

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



#drop columns
Avito$region = NULL
Avito$city = NULL
Avito$parent_category_name = NULL
Avito$category_name = NULL

## count number of parameters

##convert date to day released (i.e. date to Sunday or Monday)

##find unique cities and make bin variables

##find unique regions and make bin variables

##find unique cities and make bin variables

##split data into training, testing and validation
set.seed(1951)

#################### MAKE PREDICTIVE MODELS ###################################

## build generalized linear regression model

##build continuous variable decision tree model

##build continuous variable random forest model

##build ANN with continuous outcome

######################### CLASSIFY DATA #######################################

##text mining

##perform clustering
