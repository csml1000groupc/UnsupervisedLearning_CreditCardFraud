#install.packages("shinythemes")
library(shiny)
library(shinythemes)

#view layer
shinyUI(fluidPage(
    
    #input elements
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("Credit Card Data Clustering"),
    tabsetPanel(
        tabPanel("SIMULATION", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         actionButton("button", "Fraud Detection"),
                         h5("Current Implementation - Fraud Detection by ML"),
                         helpText("
                         By clicking 'Fraud Detection' button, 
                         the ML model will actually figure out the fraud creditcard usage from 'Test Dataset',
                         and it will list the result of the examination in the right main panel."),
                         h5("Future Suggestion - Auto-Notification and Log"),
                         helpText("
                         For future, we can set up the auto-notification for the fraudcase based on the above result.
                         The 'Contact' and Notification' columns are pesudo-columns to show how the notificate could be logged.
                                  ")
                         ),
                     mainPanel(
                         tabPanel("SIMULATION", 
                                  h3("Transaction"),
                                  DT::dataTableOutput("detectionResultTable")
                         ),
                         tabPanel("temp")
                     )
                 )
        ),
        tabPanel("K-Mean", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("clusterNumKmean", label = h3("Number of Cluster"), min = 2, 
                                     max = 12, value = 3)
                     ),
                     mainPanel(fluidRow(
                         tabPanel("K-Mean",
                                  h3("CLUSTERING"),
                                  plotOutput('kmeanPlot')
                         )
                     )
                     )
                 )
        ),
        tabPanel("Partitioning Around Medoids", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                                sliderInput("clusterNum", label = h3("Number of Cluster"), min = 2, 
                                            max = 12, value = 3)
                     ),
                     mainPanel(fluidRow(
                         tabPanel("PAM",
                                  h3("CLUSTERING"),
                                  plotOutput('plot1'),
                                  h3("CLUSTERING EVALUATION"),
                                  column(6, h4("Rand Index"),
                                         tableOutput('randIndex')),
                                  column(6, h4("Precision Recall Measure"),
                                         tableOutput('accResult')),
                                  column(6, h4("Confusion Matrix"),
                                         tableOutput('groupResultTable'))
                         )
                     )
                     )
                 )
        )
    )
))
