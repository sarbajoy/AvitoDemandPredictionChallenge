Entry for Kaggle competition: https://www.kaggle.com/c/avito-demand-prediction

The objective of the project is to determine the deal probability of a given advertisement in Avito, a Russian classified 
advertisement website based on variables such as:

  -geographical location of advertisement

  -user type who posted the advertisement

  -category of advertisement

   -price of item advertised

  -description and title of advertisement

  -image quality on advertisement (0 or NA if no image exists on advertisement)

  -date when activated

Processes done:

  -Made user type variable binary
  
  -Dropped ID variables from training data set as ID does not affect the decision of ad being successful or not
  
  -Converting dates to day of the week (e.g converting 27-04-2018 to Friday)
  
  -Counting number of parameter variables present in each row
  
  -Splitting data up
  
  -Generated multiple linear regression model
  
  -Generated logistic regression model

Currently performing:
  
  -Generating continuous variable decision tree
  
  -Generating continuous variable random forest
  
  -Build Artificial Neural Network
  
  -Evaluate predictive models
  
  -Perform clustering and text mining
