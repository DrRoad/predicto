library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(foreach)
library(zoo)
source("model_facade.R")

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(df_org = NULL,     # original data.frame loaded
                       df_ctrl = NULL,    # controled data.frame by percentage slide bar
                       plot_var = NULL,   # a variable name plotted
                       logit = NULL,      # |
                       nb = NULL,         # |
                       svm = NULL,        # modeling result
                       rf = NULL,         # |
                       gbm = NULL,        # |
                       data_info = NULL,  # data information
                       summary = NULL,    # model performance summary
                       models = NULL,     # name of models trained
                       pred = NULL,       # result of prediction
                       pred_ctrl = NULL)  # controled result by probability slide bar
  
  # ---------------------------------------------------------------------------
  # Load tab ------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  # load button
  observeEvent(input$load, {
    # entire data set
    rv$df_org <- fread(file.path("./input/demo", input$source), stringsAsFactors=T, data.table=F)
    # data set controled by percentage slide bar
    rv$df_ctrl <- rv$df_org[1:get_num_rows(nrow(rv$df_org), input$percent), ]
  })
  
  # percentage slide bar
  observeEvent(input$percent, {
    if (is.null(rv$df_org)) return(NULL)
    rv$df_ctrl <- rv$df_org[1:get_num_rows(nrow(rv$df_org), input$percent), ]
  })
  
  # data table
  output$datatbl <- renderDataTable({
    if (is.null(rv$df_ctrl)) return(NULL)
    datatable(rv$df_ctrl,
              container = TABLE_HEADER,
              rownames = F,
              filter = "top",
              style = "bootstrap",
              options = list(scrollX = T, scrollY = "400px")) %>%
      formatRound(columns = names(rv$df_ctrl)[unlist(lapply(rv$df_ctrl, class)) == "numeric"], 
                  digits = 2)
  })
  
  # ---------------------------------------------------------------------------
  # Explore tab ---------------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  # selectbox - variable plotted
  output$plot_var <- renderUI({
    if (is.null(rv$df_ctrl)) return(NULL)
    if (is.null(input$plot_var)) {
      selectInput("plot_var", "Variable:",
                  choices = VAR_NAMES[c(-1, -2)],
                  selected = "voltmean")
    } else {
      selectInput("plot_var", "Variable:",
                  choices = VAR_NAMES[c(-1, -2)],
                  selected = input$plot_var)
    }
  })
  
  # plot
  output$plot <- renderPlot({
    if (is.null(rv$df_ctrl)) return(NULL)
    if (is.null(input$plot_var)) {
      plot_1d(rv$df_ctrl, "voltmean")
    } else {
      plot_1d(rv$df_ctrl, input$plot_var)
    }
  })
  
  # ---------------------------------------------------------------------------
  # Model tab -----------------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  # action for start modeling button
  observeEvent(input$start_modeling, {
    if (is.null(rv$df_org)) return(NULL)
    
    # [TEMP] force to choose svm and gbm
    demo_algos <- c("svm", "gbm")
    updateCheckboxInput(session, "algorithms", value = demo_algos)
    
    # train and evaluate models
    foreach(algo = demo_algos) %dopar% {
      result <- model_facade(algo, "classification", rv$df_ctrl, input$tst_ratio, "failure")
      rv[[algo]] <- append(result, get_result_titles())
    }
    
    # training information
    rv$data_info <- list(
      title = "<b>Data Summary</b>",
      contents = data.frame("Number" =
                              c(round(nrow(rv$df_ctrl) * (1 - input$tst_ratio), 0),
                                round(nrow(rv$df_ctrl) * input$tst_ratio, 0),
                                nrow(rv$df_ctrl)),
                            row.names = c("Training Cases",
                                          "Test Cases",
                                          "Total")))
    
    # summary of evaluation
    rv$summary <- list(
      title = "<b>Model Performance Summary</b>",
      contents = data.frame("Support_Vector_Machine" = 
                              c(rv$svm$metrics[1, "Accuracy"], 
                                rv$svm$metrics[1, "Kappa"]), 
                            "Gradient_Boosting" = 
                              c(rv$gbm$metrics[1, "Accuracy"],
                                rv$gbm$metrics[1, "Kappa"]),
                            row.names = c("Accuracy", "Kappa")))
    
    # name of models trained with timestamp
    time_stamp <- format(Sys.time(), tz = "Asia/Tokyo", usetz = T)
    time_stamp <- paste0("[", substr(time_stamp, 1, 16), "]")
    model_names <- vapply(demo_algos, 
                          FUN = function(x) paste(time_stamp, get_model_name(x)),
                          FUN.VALUE = "", USE.NAMES = F)
    if (is.null(rv$models)) {
      rv$models <- model_names
    } else {
      rv$models <- c(rv$models, model_names)
    }
  })
  
  # summary
  output$data_info_title <- renderUI(HTML(rv$data_info$title))
  output$data_info <- renderTable(rv$data_info$contents, digits = 0)
  output$summary_title <- renderUI(HTML(rv$summary$title))
  output$summary <- renderTable(rv$summary$contents, digits = 4)
  
  # support vector machine
  output$svm_plot_title <- renderUI(HTML(rv$svm$plot_title))
  output$svm_plot <- renderPlot(rv$svm$plot)
  output$svm_confusion_matrix <- renderTable(rv$svm$confusion_matrix)
  output$svm_metrics_title <- renderUI(HTML(rv$svm$metrics_title))
  output$svm_metrics <- renderTable(rv$svm$metrics, digits = 4)  
  
  # gradient boosting
  output$gbm_plot_title <- renderUI(HTML(rv$gbm$plot_title))
  output$gbm_plot <- renderPlot(rv$gbm$plot)
  output$gbm_confusion_matrix <- renderTable(rv$gbm$confusion_matrix)
  output$gbm_metrics_title <- renderUI(HTML(rv$gbm$metrics_title))
  output$gbm_metrics <- renderTable(rv$gbm$metrics, digits = 4)
  
  # ---------------------------------------------------------------------------
  # Predict tab ---------------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  # model selectbox
  output$model_applied <- renderUI({
    if (is.null(rv$models)) return(NULL)
    selectInput("model_applied", "Model:", choices = rv$models)
  })
  
  # failure probability slide bar
  observeEvent(input$failure_probability, {
    if (is.null(rv$pred)) return(NULL)
    rv$pred_ctrl <- rv$pred[rv$pred$failure.probability >= (input$failure_probability / 100), ]
  })
  
  # apply button
  observeEvent(input$apply, {
    if (is.null(rv$models)) return(NULL)
    
    # create dummy result of prediction
    f_prob <- rnorm(400)
    f_prob <- (f_prob - min(f_prob)) / (max(f_prob) - min(f_prob) + 0.01)
    rv$pred <- data.frame(machineID = rep(1:100, each = 4),
                          component = rep(c(1, 2, 3, 4), 100),
                          failure.probability = f_prob,
                          row.names = NULL)
    
    # reorder result
    rv$pred <- rv$pred[order(rv$pred$failure.probability, decreasing = T), ]
    
    # narrow down by probability specified
    rv$pred_ctrl <- rv$pred[rv$pred$failure.probability >= (input$failure_probability / 100), ]
  })
  
  # prediction result
  output$prediction <- renderDataTable({
    if (is.null(rv$pred_ctrl)) return(NULL)
    rv$pred_ctrl %>%
      datatable(rownames = F,
                colnames = c("Machine ID",
                             "Component No.",
                             "Failure Probability"),
                filter = "none",
                style = "bootstrap",
                options = list(scrollY = "500px",
                               searching = F,
                               lengthChange = F,
                               paging = F)) %>%
      formatRound(columns = "failure.probability", digits = 4)
  })
})


# -----------------------------------------------------------------------------
# Constants -------------------------------------------------------------------
# -----------------------------------------------------------------------------
TABLE_HEADER = htmltools::withTags(table(
  tableHeader(names(VAR_NAMES))
))


# -----------------------------------------------------------------------------
# Functions -------------------------------------------------------------------
# -----------------------------------------------------------------------------

get_num_rows <- function(n_rows, percent) {
  round(n_rows * percent / 100, 0)
}

get_result_titles <- function() {
  list(plot_title = "<br><b>Prediction Summary on Test Dataset:</b>", 
       metrics_title = "<br><b>Performance Metrics:</b>")
}

get_model_name <- function(abbrv) {
  list(logit = "Logistic Regression", 
       nb = "Naive Bayes",
       svm = "Support Vector Machine", 
       rf = "Random Forest",
       gbm = "Gradient Boosting")[[abbrv]]
}


plot_1d <- function(df, var) {
  if (class(df[, var]) %in% c("integer", "factor")) {
    plot_bar(df, var)
  } else if (class(df[, var]) == "numeric") {
    plot_hist(df, var)
  } else {
    stop("Unexpected class is found in data set.")
  }
}

plot_bar <- function(df, var) {
  ggplot(df, aes_string(x = var, fill = 1)) +
    geom_bar(stat = "count", show.legend = F) +
    guides(fill = F) +
    # guides(fill = guide_legend(
    #   title = "Model",
    #   title.theme = element_text(size = 24, angle = 0),
    #   label.theme = element_text(size = 18, angle = 0),
    #   reverse = T)) +
    ggtitle(names(VAR_NAMES)[VAR_NAMES == var]) +
    xlab("") +
    ylab("Count") +
    theme(plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14))
}

plot_hist <- function(df, var) {
  ggplot(df, aes_string(x = var, fill = 1)) +
    geom_histogram(bins = 30, show.legend = F) +
    guides(fill = F) +
    ggtitle(names(VAR_NAMES)[VAR_NAMES == var]) +
    xlab("") +
    ylab("Frequency") +
    theme(plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14))
}

