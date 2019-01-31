dataset = read.csv('50_Startups.csv')
dataset$State = factor(dataset$State,
                         levels = c('Florida', 'New York', 'California'),
                         labels = c(1, 2, 3))
regressor = lm(formula = Profit ~ ., data = dataset)

summary(step(regressor))