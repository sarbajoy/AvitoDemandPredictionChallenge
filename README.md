Entry for Kaggle competition: https://www.kaggle.com/c/avito-demand-prediction

The objective of the project is to determine the deal probability of a given advertisement in Avito, a Russian classified 
advertisement website based on variables such as:

  - geographical location of advertisement

  - user type who posted the advertisement

  - category of advertisement

  - price of item advertised

  - description and title of advertisement

  - image quality on advertisement (0 or NA if no image exists on advertisement)

  - date when activated

Processes done:

 - Found mean deal probability (to determine what is the average deal probability)
  
  - Dropped ID variables from training data set as ID does not affect the decision of ad being successful or not
  
  - Converting dates to day of the week (e.g converting 27-04-2018 to Friday)
  
  - Counting number of parameter variables present in each row
  
  - Found unique regions and parent categories
  
  - Generated logistic regression model
  
  - Create binary variables for regions
  
  - Create binary variables for parent 

  - Modify log regression
  
Currently performing:
  
  - Generating decision tree
  
  - Generating random forest
  
  - Build Artificial Neural Network
  
  - Build Gradient Boosting Tree
  
  - Evaluate predictive models
