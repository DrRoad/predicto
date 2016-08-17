
fit_gbm <- function(train, label, exclude, grid = NULL, seed = format(Sys.Date(), "%Y%m%d")) {
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
  #   Gradient Boosting model
  #
  # [TODO]
  #   1. Grid search of parameters
  #
  
  # library
  suppressWarnings(suppressMessages({
    require(gbm)
    # require(caret)
  }))
  
  # create formula
  feature_names <- names(train)[!(names(train) %in% c(label, exclude))]
  formula <- as.formula(paste(label,
                              paste(feature_names, collapse=' + '),
                              sep=' ~ '))
  # train model
  set.seed(seed)
  gbm(formula = formula, data = train, 
      distribution = "multinomial", n.trees=50,
      interaction.depth = 5, shrinkage = 0.1)
  
  # caret version
  # control <- trainControl(method = "cv", number = 3)
  # if(is.null(grid)) {
  #   grid <- expand.grid(n.trees = 50, interaction.depth = 5,
  #                       shrinkage = 0.1, n.minobsinnode = 10)
  # }
  # train(formula, data = train, method = "gbm", 
  #       trControl = control, tuneGrid = grid)
  
}
