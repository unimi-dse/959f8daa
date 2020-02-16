#' Get Your Salary Function
#'
#' @return
#' @export
GetYourSalary <- function()
{
  
  # Predict Your Salary based on a Simple Linear Regression
  # Importing the dataset
  dataset = read.csv('https://raw.githubusercontent.com/unimi-dse/959f8daa/master/data/Salary_Data.csv')
  
  # Splitting the dataset into the Training set and Test set
  # install.packages('caTools')
  #install.packages('caTools')
  library(caTools)	
  set.seed(123)
  split = sample.split(dataset$Salary, SplitRatio = 2/3)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  #Feature Scaling
  #training_set = scale(training_set)
  #test_set = scale(test_set)
  
  # Fitting Simple Linear Regression to the Training set
  regressor = lm(formula = Salary ~ YearsExperience,
                 data = training_set)
  
  
  # Predicting the Test set results
  y_pred = predict(regressor, newdata = test_set)
  
  # Visualising the Training set results
  #install.packages('ggplot2')
  library(ggplot2)
  P1<-ggplot() +
    geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
               colour = 'red') +
    geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
              colour = 'blue') +
    ggtitle('Salary vs Experience (Training set)') +
    xlab('Years of experience') +
    ylab('Salary')
  
  
  # Visualising the Test set results
  library(ggplot2)
  P2<- ggplot() +
    geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
               colour = 'red') +
    geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
              colour = 'blue') +
    ggtitle('Salary vs Experience (Test set)') +
    xlab('Years of experience') +
    ylab('Salary')
  
  #install.packages('grid')
  require(gridExtra)
  grid.arrange(P1, P2, ncol=2)
}

