
fit_svm <- function(train, label, exclude, grid = NULL, seed = format(Sys.Date(), "%Y%m%d")) {
  #
  # Fit gradient boosting model and check performance.
  # 
  # [Args]
  #   train     : data.frame, training data set
  #   label     : character, a target feature that model predicts
  #   exclude   : character, features not to be used for modeling
  #   grid      : data.frame, parameter combinations to be grid searched.
  #               "n.trees", "interaction.depth", "shrinkage", "n.minobsinnode"
  #   seed      : numeric, a seed to be used for randomization
  #   
  # [Return]
  #   SVM model
  #
  # [TODO]
  #   1. Try different kernels
  #
  
  # library
  suppressWarnings(suppressMessages({
    require(kernlab)
  }))
  
  # create formula
  feature_names <- names(train)[!(names(train) %in% c(label, exclude))]
  formula <- as.formula(paste(label,
                              paste(feature_names, collapse=' + '),
                              sep=' ~ '))
  # train model
  ksvm(formula, data = train, scaled = T, type = "C-svc", kernel = "rbfdot")
  
  
  # control <- trainControl(method = "cv", number = 3)
  # if(is.null(grid)) {
  #   grid <- expand.grid(sigma = c(0.1), C = c(0.1))
  # }
  # train(formula, data = train, method = "svmRadial",
  #       trControl = control, tuneGrid = grid)
}
