backwardElimination <- function(x, sl) {
  numRows = length(x) + 1
  for (i in c(1:numVars)){
    print(x)
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numRows), "Pr(>|t|)"])
    print(maxVar)
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numRows), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
      print(x)
    }
    numRows = numRows - 1
  }
  return(summary(regressor))
}

dataset = read.csv('50_Startups.csv')
dataset$State = factor(dataset$State,
                         levels = c('Florida', 'New York', 'California'),
                         labels = c(1, 2, 3))
regressor = lm(formula = Profit ~ ., data = dataset)

backwardElimination(dataset, 0.05)