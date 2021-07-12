library(shiny)
library(shinydashboard)
library(shiny)
library(quantmod)
#library(rstudioapi)
library(openxlsx)
#library(xts)        
library(rvest)      
#library(tidyverse) 
library(stringr)    
#library(forcats)    
#library(lubridate)  
library(plotly)     
library(corrplot)
library(shinybusy)
library(TTR)
library(shinyWidgets)
library(thematic)
library(bslib)
library(shinythemes)
library(fresh)
library(shinydashboardPlus)

thematic_shiny(font = "auto")


dashboardPage(
    
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = FALSE,width = "1px"),
    dashboardBody(
        fluidRow(
           box(
               radioButtons("upload_option","Select option to get data",choices = c("Automatically","Upload from file")),
               width = 4
               ),
           box(uiOutput("upload_file"),
               downloadButton("sample","Download sample Cryptocurrency list"),
               width = 4
               ),
           box(
               selectInput("no_of_ticker","Select number of Cryptocurrencies for analysis",choices = c(3,5,10,15,20,30,50),selected = 3),
               actionButton("decide","Submit"),
               width = 4
               )
        ),
        fluidRow( 
            box(title = "Price correlation plot",
                solidHeader = FALSE,
                fluidRow(column(12,align = "center",plotOutput("corr_prices",height = 300,width = 500))),
                fluidRow(
                    column(6,align = "right",actionButton("view_plot1","Zoom Plot")),
                    column(6,align = "left",downloadButton("Download_plot_1","Download Plot"))
                )
                ),
            box(title = "Returns correlation plot",
                solidHeader = FALSE,
                fluidRow(column(12,align = "center",plotOutput("corr_returns",height = 300,width = 500))),
                fluidRow(
                    column(6,align = "right",actionButton("view_plot2","Zoom plot")),
                    column(6,align = "left",downloadButton("Download_plot_2","Download Plot"))
                )
                )
        )
    ),
    controlbar = dashboardControlbar(collapsed = TRUE, skinSelector())
    
)
