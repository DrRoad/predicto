library(caret)
source("./cmd/fit_svm.R")
source("./cmd/fit_gbm.R")
source("./cmd/evaluate_classification.R")
source("./cmd/viz_confusion_matrix.R")

# metrics used for regression
METRICS_REG <- c()
# metrics used for classification
METRICS_CLS <- c("Precision", "Recall", "F1", "Accuracy", "Kappa")

model_facade <- function(name, type, df, tst_ratio, label) {
  #
  # Train and evaluate a model on a data set given.
  #
  # Args:
  #   name:       name of algorithm
  #   type:       regression or classification
  #   df:         data.frame
  #   tst_ratio:  ratio of data.frame used for test
  #   label:      label feature name(s)
  #
  # Returns:
  #   Result of evaluation.
  #
  
  # split data set
  train_idx <- round(nrow(df) * (1 - tst_ratio), 0)
  train <- df[1:train_idx, ]
  test <- df[(train_idx + 1):nrow(df), ]
  
  # model and predict
  if (name == "gbm") {
    model <- fit_gbm(train, label, c("datetime", "machineID"))
    pred <- as.data.frame(predict(model, test, n.trees = 50, type = "response"))
    names(pred) <- gsub(".50", "", names(pred))
    pred <- as.factor(colnames(pred)[max.col(pred)])
    
  } else if (name == "svm") {
    model <- fit_svm(train, label, c("datetime", "machineID"))
    pred <- predict(model, test, type = "response")
    
  } else {
    # [TODO] other modeling methods
    return(NULL)
  }
  
  # evaluation
  if (type == "regression") {
    stop("Regression is under construction.")
    
  } else {
    # get model performance
    performance <- evaluate_classification(actual = test$failure, predicted = pred)
    
    # visualize confusion matrix
    viz <- viz_confusion_matrix(performance$confusion_matrix)
    
    # return model and evaluation result
    return(list(confusion_matrix = performance$confusion_matrix,
                metrics = performance$metrics[, METRICS_CLS],
                plot = viz))
  }
}