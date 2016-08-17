
viz_confusion_matrix <- function(mtrx) {
  #
  # Visualize confusion matrix in percentage.
  #
  # [Args]
  #   mtrx: table, confusion matrix which have Actual in y-axis an Predicted in x-axis.
  #
  # [Return]
  #   A ggplot2 object dipicting confusion matrix.
  #
  
  # library
  suppressWarnings(suppressMessages({
    library(plyr)
    library(ggplot2)
    library(scales)
  }))
  
  # convert confusion matrix into percentage
  for (i in 1:nrow(mtrx)) {
    mtrx[i, ] <- mtrx[i, ] / sum(mtrx[i, ])
  }
  
  # convert table into data.frame
  mtrx <- as.data.frame(mtrx)
  
  # gradient color adjustment, assign NA to correct predictions
  mtrx$Color <- mtrx$Freq
  mtrx <- adply(mtrx, 1, .fun = function(row) {
    if (row$Actual == row$Predicted) {
      row$Color <- NA
    }
    row
  })
  
  # reverse the order of y-axis (Actual)
  mtrx$Actual <- factor(mtrx$Actual, level=rev(levels(mtrx$Actual)))
  
  # build graph
  ggplot() +
    geom_tile(aes(x = Predicted, y = Actual, fill = Color),
              data = mtrx,
              color = "black",
              size = 0.1) +
    ggtitle("Accuracy of Prediction (Percentage)") +
    labs(x = "Predicted", y = "Actual") +
    geom_text(aes(x = Predicted, y = Actual, label = sprintf("%.4f", Freq)),
              data = mtrx, 
              size = 4, 
              colour = "black") +
    scale_fill_gradient(low = 'white', high = muted("red"), na.value = muted('skyblue'), guide = F)
}
