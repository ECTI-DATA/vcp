library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(stringr)
library(openxlsx)
library(corrplot)
library(shinybusy)

shinyUI(fluidPage(tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }"),

    # Application title
    titlePanel("Virtual Crypto Currency Portfolio Builder"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            conditionalPanel("input.set1== 'Crypto Currency Returns' || input.set1== 'Correlation'",radioButtons("upload_selection","Select to get crypto currency data",choices = c("Enter manually","Upload from file"))),
            conditionalPanel("input.set1== 'Crypto Currency Returns' || input.set1== 'Correlation'",uiOutput("choice")),
            conditionalPanel("input.set1== 'Portfolio Performance' ",textInput("benchmark","Select benchmark crypto currency",value = "BTC-USD")),

            conditionalPanel("input.set1== 'Crypto Currency Returns' || input.set1== 'Correlation' || input.set1== 'Portfolio Performance' ",actionButton("submit","Submit")),
            tags$br(),
            selectInput("year","Select starting year",choices = c(2000:2021)),

            radioButtons("return_type","Select frequency",choices = c("Daily"="daily","Weekly"="weekly","Monthly"="monthly")),

            conditionalPanel("input.set1== 'Crypto Currency Returns' || input.set1== 'Correlation'",downloadButton("download_sample","Download sample file")),
            tags$br(),
            conditionalPanel("input.set1== 'Crypto Currency Returns' || input.set1== 'Correlation'",downloadButton("download_list","Download crypto currency symbol list"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id="set1",
                        tabPanel("Crypto Currency Returns",
                                 dygraphOutput("plot1"),
                                 tags$br(),
                                 textOutput("legend")
                                 ),
                        tabPanel("Correlation",
                                 tags$br(),
                                 fluidRow(column(1,uiOutput("icon1")),column(11,uiOutput("corr_message"))),
                                 tags$br(),
                                 plotOutput("cor")
                                 ),
                        tabPanel("Portfolio Performance",
                                 tags$br(),
                                 fluidRow(column(1,uiOutput("icon3")), column(11,uiOutput("message"))),
                                 tags$br(),
                                 fluidRow(column(1,uiOutput("icon2")), column(11,uiOutput("message2"))),
                                 tags$br(),
                                 fluidRow(column(1,uiOutput("icon4")), column(11,uiOutput("message3"))),
                                 dygraphOutput("plot2"),
                                 tags$br(),
                                 textOutput("legend2")
                                 ))
        )
    )
))
