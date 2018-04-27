rm(list=ls())

avitoTrain = read.csv("Downloads/AvitoTrain.csv")

##set up translate
library(translate)
set.key("AIzaSyAJvkZgYIjwNDTiz6pAWZOV-bS4oveRd_4")

######################### CLEAN UP DATA #######################################

##delete columns that do not play a role
AvitoTrain$item_id=NULL
AvitoTrain$user_id=NULL
AvitoTrain$image=NULL

##create new columns to separate user type
AvitoTrain$IsPrivate <- ifelse(AvitoTrain$user_type == 'Private',c(1), c(0))
AvitoTrain$IsCompany <- ifelse(AvitoTrain$user_type == 'Company',c(1), c(0))
AvitoTrain$user_type = NULL

##find length of title and description
AvitoTrain$titleLength <- nchar(AvitoTrain$title,allowNA = TRUE, keepNA = 0)
AvitoTrain$descriptionLength <- ifelse (is.na(AvitoTrain$description),
c(0), c(nchar(AvitoTrain$description)))

##create new columns for translations
##they will most likely give default values of first row
##next step is to clean up and put actual translations
AvitoTrain$Region <- translate(AvitoTrain$region,"ru","en")
AvitoTrain$City <- translate(AvitoTrain$city,"ru","en")
AvitoTrain$Parent <- translate(AvitoTrain$parent_category_name,"ru","en")
AvitoTrain$Category <- translate(AvitoTrain$category_name,"ru","en")

for (i in 2:nrow(AvitoTrain)){
  #retrieve all variables needed for translation
  region = AvitoTrain [i,1]
  city = AvitoTrain [i,2]
  parent = AvitoTrain [i,3]
  category = AvitoTrain [i,4]

  AvitoTrain[i,19]=translate(region,"ru","en")
  AvitoTrain [i,20] = translate (city,"ru","en")
  AvitoTrain [i,21] = translate (parent,"ru","en")
  AvitoTrain [i,22] = translate (category,"ru","en")
}

#drop columns
AvitoTrain$region = NULL
AvitoTrain$city = NULL
AvitoTrain$parent_category_name = NULL
AvitoTrain$category_name = NULL

## count number of parameters

##convert date to day released (i.e. date to Sunday or Monday)

##find unique cities and make bin variables

##find unique regions and make bin variables

##find unique cities and make bin variables

##split data

#################### MAKE PREDICTIVE MODELS ###################################

## build generalized linear regression model

##build continuous variable decision tree model

##build continuous variable random forest model

##build ANN with continuous outcome

######################### CLASSIFY DATA #######################################

##text mining

##perform clustering
