pima <- read.csv("data/pima-indians-diabetes.csv")

model <- glm(class ~ ., data=pima, family="binomial")

# Question 3
predictions <- predict(model, pima)
predictions_class <- predictions >= 0
real <- ifelse(pima$class == "1", TRUE, FALSE)

getMeasures <- function(real, predicted)
{
  truePositives <- sum(predicted & real )
  falsePositives <- sum(predicted & !real)
  trueNegatives <- sum(!predicted & !real)
  falseNegatives <- sum(!predicted & real)
  
  sensitivity <- truePositives/sum(real)
  specificity <- trueNegatives/sum(!real)
  
  PPV <- truePositives/sum(predicted)
  return(list(truePositives = truePositives,
              falsePositives = falsePositives,
              trueNegatives = trueNegatives,
              falseNegatives = falseNegatives,
              sensitivity = sensitivity,
              specificity = specificity,
              PPV = PPV))
}

getMeasures(real, predictions_class)

# Question 4
library(pracma)

getROC <- function(score, class)
{
  x_values <- sapply(sort(unique(score)), function(x)
    {
      measures <- getMeasures(class, score > x)
      return(1 - measures$specificity)
  })
  
  y_values <- sapply(sort(unique(score)), function(x)
  {
    measures <- getMeasures(class, score > x)
    return(measures$sensitivity)
  })
  
  -trapz(x_values, y_values)
}

getROC(predictions, real)

# Question 5
getMetricCV <- function(data, f = getROC, nrFolds = 5)
{
  # generate array containing fold-number for each sample (row)
  folds <- sample(rep_len(1:nrFolds, nrow(data)), nrow(data))
  rocs <- c()
  
  # actual cross validation
  for(k in 1:nrFolds)
  {
    # actual split of the data
    fold <- which(folds == k)
    data.train <- data[-fold,]
    data.test <- data[fold,]
    
    model <- glm(class ~ ., data=data.train, family="binomial")
    preds <- predict(model, data.test)
    real <- ifelse(data.test$class == "1", TRUE, FALSE)
    
    rocs[k] <- getROC(preds, real)
  }
  
  return(rocs)
}

cv_values <- getMetricCV(pima, nrFolds=10)
boxplot(cv_values)
