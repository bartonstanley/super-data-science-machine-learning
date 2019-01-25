# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set.
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Fitting Multiple Linear Regression to the Training set, backward eliminiation
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)

# Fitting Multiple Linear Regression to the Training set, remove State because
# State2 p-value = 0.990 and State3 p-value = 0.943, the two highest p-values
# and both are greater than 0.05.
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)

# Fitting Multiple Linear Regression to the Training set, remove Administration because
# Administration p-value = 0.602, now the highest p-value and is greater than 0.05.
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)

# Fitting Multiple Linear Regression to the Training set, remove Marketing.Spend because
# Marketing.Spend p-value = 0.06, now the highest p-value and is greater than 0.05.
# Note, however that it is close to 0.05 so there may be reasons to leave it in.
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)