library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Predicto"),
  headerPanel("Predictive Maintenance"),
  
  navlistPanel(
    widths = c(2, 10),
    "Machine Maintenance",
    tabPanel(
      "AX0101 in Factory XXX", 
      navbarPage(
        "",
        tabPanel(
          "Explore",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectInput("data", "Data Source:",
                          choices = list.files("./input/demo", pattern = ".csv")),
              
              sliderInput("percent",
                          "Percentage of Data Used:",
                          min = 10,
                          max = 100,
                          step = 10,
                          value = 100),
              
              actionButton("load", "Load"),
              
              br(),br(),br(),
              uiOutput("plot_type"),
              uiOutput("plot_var"),
              uiOutput("plot_fill")
            ),
            
            mainPanel(
              splitLayout(
                plotOutput("plot")
              )
            )
          )
        ),
        
        tabPanel(
          "Model",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              sliderInput("tst_ratio",
                          "Ratio of Test Dataset:",
                          min = 0.1,
                          max = 0.5,
                          step = 0.05,
                          value = 0.3),
              
              checkboxGroupInput("algorithms",
                                 label = "Algorithms:",
                                 choices = c("Logistic Regression" = "logit", 
                                             "Naive Bayes" = "nb",
                                             "Support Vector Machine" = "svm", 
                                             "Random Forest" = "rf",
                                             "Gradient Boosting" = "gbm"),
                                 selected = c("svm", "gbm")),
              
              actionButton("start_modeling", "Start Modeling")
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Summary",
                  htmlOutput("data_info_title"),
                  tableOutput("data_info"),
                  htmlOutput("summary_title"),
                  tableOutput("summary")
                ),
                
                tabPanel(
                  "Support Vector Machine",
                  htmlOutput("svm_plot_title"),
                  splitLayout(
                    plotOutput("svm_plot", height = "200px"),
                    tableOutput("svm_confusion_matrix")
                  ),
                  htmlOutput("svm_metrics_title"),
                  tableOutput("svm_metrics")
                ),
                
                tabPanel(
                  "Gradient Boosting",
                  htmlOutput("gbm_plot_title"),
                  splitLayout(
                    plotOutput("gbm_plot", height = "200px"),
                    tableOutput("gbm_confusion_matrix")
                  ),
                  htmlOutput("gbm_metrics_title"),
                  tableOutput("gbm_metrics")
                )
              )
            )
          )
        ),
        
        tabPanel(
          "Predict",
          sidebarLayout(
            sidebarPanel(
              width = 4,
              uiOutput("model_applied"),
              sliderInput("failure_probability",
                          "Failure Probability (%):",
                          min = 50,
                          max = 95,
                          step = 5,
                          value = 80),
              actionButton("apply", "Apply")
            ),
            
            mainPanel(
              dataTableOutput("prediction", width = "70%")
            )
          )
        )
      )
    ),
    
    
    tabPanel(
      "DR0032 in Factory YYY",
      navbarPage(
        "",
        tabPanel("Explore"),
        tabPanel("Model"),
        tabPanel("Predict")
      )
    ),
    tabPanel(
      "PB0001 in Factory ZZZ",
      navbarPage(
        "",
        tabPanel("Explore"),
        tabPanel("Model"),
        tabPanel("Predict")
      )
    ),
    
    "Inventory Optimization",
    tabPanel(
      "Tokyo",
      navbarPage(
        "",
        tabPanel("Explore"),
        tabPanel("Model"),
        tabPanel("Predict")
      )
    ),
    tabPanel(
      "Singapore",
      navbarPage(
        "",
        tabPanel("Explore"),
        tabPanel("Model"),
        tabPanel("Predict")
      )
    )
  )
))
