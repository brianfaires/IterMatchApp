library(shiny)
library(plotly)

# Max upload size of 1 GB
options(shiny.maxRequestSize = 1*1024^3)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("IterMatchApp (Basic)", windowTitle = "IterMatch Shiny App"),
    
    h4("An application for balancing confounding variables in observational studies."),
    h5("This app is designed to run on <1 GB memory. Real-time plot updating is disabled."),
    a("https://github.com/brianfaires/IterMatchAppBasic/", href="https://github.com/brianfaires/IterMatchAppBasic/"),

    hr(),

    textOutput("errorText"),
    tags$head(tags$style("#errorText{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
    )),
    
        
    fluidRow(column(h5("Upload a CSV data file to begin:"), width=4)),
    fluidRow(column(fileInput('input_file', NULL, accept = c('text/csv', 'text/comma-separated-values', '.csv')), width=4)),
    hr(),
    
    
    mainPanel(width=12, fluidRow(conditionalPanel(condition = "output.fileUploaded && !output.errorText", tabsetPanel(id="tabPanel",
            tabPanel(title="Data Setup",
                 br(),
                 fluidRow(column(width=5, downloadButton('downloadData', 'Download Cleaned Data'))),
                 br(),
                 fluidRow(column(width=5, selectInput("labelVar", "Select label variable:", "ERROR: No valid labels"))),
                 fluidRow(column(width=5, selectInput("responseVar", "Select response variable:", "ERROR: No binary variables"))),
                 fluidRow(
                     column(uiOutput("checkboxes"), width=4),
                     column(uiOutput("checkboxes2"), width=4)
                 )
            ),

            tabPanel(title="RF/DM Generation",
                 br(),
                 fluidRow(column(width=5, actionButton("btnLaunch1", label = "Create RF and DM"))),
                 br(),
                 checkboxInput("surrogates", "Use Surrogate Splits"),
                 numericInput("nTrees", "# trees in Random Forest:", 1000, min=1, max=NA, step=1, width = '20%'),
            ),

            tabPanel(title="IterMatch", conditionalPanel(condition = "output.algo1Complete",
                br(),
                fluidRow(column(width=5, actionButton('btnLaunch2', label = "Launch IterMatch"))),
                br(),
                selectInput("smdMethod", "Select SMD method:", c("method-1", "method-2")),
                br(),
                sliderInput(inputId = "smdAll", label="Set all thresholds", value=0.1, min=0, max=1, step=0.001),
                br(),
                uiOutput("smdThresholds")
                )
            ),

            tabPanel(title="Results",
                br(),
                conditionalPanel(condition = "output.algo2Started", plotlyOutput("allIterationResultsPlot")),
                conditionalPanel(condition = "output.algo2Complete",
                    hr(),
                    numericInput("iterationNumber", label = "Iteration #", value=1, min=1, max=1, step=1, width="21%"),
                    downloadButton("exportResults", "Export Group Results"),
                    downloadButton("exportAllResults", "Export All Results"),
                    downloadButton("exportLog", "Export Log File"),
                    hr(),
                    tableOutput("iterationResults")
                ))
            )
    )))
))
