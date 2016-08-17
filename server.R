library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(foreach)
source("model_facade.R")

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(df_org = NULL,     # original data.frame loaded
                       df_ctrl = NULL,    # controled data.frame by percentage slide bar
                       plot = NULL,       # a list of plot features in explore tab
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
  # Explore tab ---------------------------------------------------------------
  # ---------------------------------------------------------------------------
  
  # load button
  observeEvent(input$load, {
    # data
    rv$df_org <- fread(file.path("./input/demo", input$data), stringsAsFactors=T, data.table=F)
    rv$df_ctrl <- rv$df_org[1:get_num_rows(nrow(rv$df_org), input$percent), ]
    
    # default plot setting (only for 1st request)
    if (is.null(rv$plot)) {
      rv$plot <- list(type = "bar",
                      var = "failure",
                      fill = "model")
    }
  })
  
  # percentage slide bar
  observeEvent(input$percent, {
    if (is.null(rv$df_org)) return(NULL)
    rv$df_ctrl <- rv$df_org[1:get_num_rows(nrow(rv$df_org), input$percent), ]
  })
  
  # plot
  output$plot <- renderPlot({
    # initial screen
    if (is.null(rv$df_ctrl)) return(NULL)
    
    # first load
    df <- rv$df_ctrl
    if (is.null(input$plot_type))
      return(make_dummy_barplot(df[df$failure != "none", ]))
    
    # change in select box
    if (input$plot_type == "bar") {
      # update setting
      rv$plot <- list(type = "bar",
                      var = "failure",
                      fill = "model")
      
      # update ui
      updateSelectInput(session, "plot_var", selected = rv$plot$var)
      updateSelectInput(session, "plot_fill", selected = rv$plot$fill)
      
      # return plot
      make_dummy_barplot(df[df$failure != "none", ])
      
    } else if (input$plot_type == "hist") {
      # update setting
      rv$plot <- list(type = "hist",
                      var = "vibrationmean",
                      fill = "none")
      
      # update ui
      updateSelectInput(session, "plot_var", selected = rv$plot$var)
      updateSelectInput(session, "plot_fill", selected = rv$plot$fill)
      
      # return plot
      make_dummy_hist(df[df$failure != "none", ])
    }
  })
  
  # control element - plot type
  output$plot_type <- renderUI({
    if (is.null(rv$df_ctrl)) return(NULL)
    selectInput("plot_type", "Plot Type:",
                choices = list(Barchart = "bar",
                               Histogram = "hist"),
                selected = isolate(rv$plot$type))
  })
  
  # control element - plot variable
  output$plot_var <- renderUI({
    if (is.null(rv$df_ctrl)) return(NULL)
    selectInput("plot_var", "Variable:",
                choices = names(rv$df_ctrl),
                selected = isolate(rv$plot$var))
  })
  
  # control element - plot fill color
  output$plot_fill <- renderUI({
    if (is.null(rv$df_ctrl)) return(NULL)
    selectInput("plot_fill", "Colored By:",
                choices = c("none", names(rv$df_ctrl)),
                selected = isolate(rv$plot$fill))
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
                options = list(searching = F,
                               lengthChange = F,
                               paging = F)) %>%
      formatRound(columns = "failure.probability", digits = 4)
  })
})


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

make_dummy_barplot <- function(df) {
  ggplot(df, aes(x = failure, fill = model)) + 
    geom_bar() +
    guides(fill = guide_legend(
      title = "Model",
      title.theme = element_text(size = 24, angle = 0),
      label.theme = element_text(size = 18, angle = 0),
      reverse = T)) +
    ggtitle("Count of Failure") +
    xlab("Component") +
    ylab("Count") +
    theme(plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14))
}

make_dummy_hist <- function(df) {
  ggplot(df, aes(x = vibrationmean, fill = 1)) +
    geom_histogram(bins = 30, show.legend = F) +
    guides(fill = F) +
    ggtitle("Mean of Vibration") +
    xlab("Vibration") +
    ylab("Frequency") +
    theme(plot.title = element_text(size = 24),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14))
}

