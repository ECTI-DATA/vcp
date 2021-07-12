server <- function(input, output) { 
    
    file_path <- eventReactive(input$decide,{
        if (input$upload_option=="Upload from file") {
            file <- input$ticker_upload$datapath
        }else{
            file <- "tickers.xlsx"
        }
        
        file
    })
    
    data <- reactive({
        
        req(file_path())
        show_modal_spinner() # show the modal window
        
        ticker <- read.xlsx(file_path())
        data <- lapply(ticker[1:input$no_of_ticker,1],FUN = getSymbols, auto.assign =FALSE)
        names(data) <- ticker[1:input$no_of_ticker,1]
        remove_modal_spinner()
        data
    })
    
    output$upload_file <- renderUI({
        if (input$upload_option=="Upload from file") {
            fileInput("ticker_upload","Select FIle")
        }
    })
    
    correlation <- reactive({
        data <- data()
        data <- lapply(data,function(x) {
            names(x)=c("Open","High", "Low","Close","Volume","Adjusted")
            x
        })
        
        data1 <- lapply(data, function(x){
            x <- x[,"Adjusted"]
            x
        })
        
        data2 <- lapply(data1, function(x){
            x <- na.approx(x[,"Adjusted"])
            x
        })
        
        row_num <- lapply(data2,function(x){
            nrow(x)
        })
        
        row_num <- unlist(row_num)
        row_num <- row_num[row_num >400]
        
        data_pri <- as.data.frame(NULL)
        data_ret <- as.data.frame(NULL)
        
        for(i in names(row_num)){
            
            data_pri[1:400,i] <- data2[[i]][1:400,"Adjusted"]
            data_ret[1:400,i] <- dailyReturn(data2[[i]][1:400,"Adjusted"],type = "log")
        }
        
        list(data_pri,data_ret)
        
    })
    
    output$corr_prices <- renderPlot({
        data <- correlation()
        data <- data[[1]]
        corrplot(cor(data),tl.col = "black")
    })
    
    output$corr_returns <- renderPlot({
        data <- correlation()
        data <- data[[2]]
        corrplot(cor(data),tl.col = "black")
    })
    
    output$Download_plot_1 <- downloadHandler(filename = function(){
        paste("Correlation_plot_1.png",sep = "")
    },content = function(file){

        png(file,width=1200,height=1200)
        data <- correlation()
        data <- data[[1]]
        corrplot(cor(data),tl.col = "black")
        dev.off()
    })

    output$Download_plot_2 <- downloadHandler(filename = function(){
        paste("Correlation_plot_2.png",sep = "")
    },content = function(file){

        png(file,width=1200,height=1200,units = "px")
        data <- correlation()
        data <- data[[2]]
        corrplot(cor(data),tl.col = "black")
        dev.off()
    })
    
    output$corr_prices_dialog <- renderPlot({
        data <- correlation()
        data <- data[[1]]
        corrplot(cor(data),tl.col = "black")
    })
    
    output$corr_returns_dialog <- renderPlot({
        data <- correlation()
        data <- data[[2]]
        corrplot(cor(data),tl.col = "black")
    })
    
    observeEvent(input$view_plot1,{
        showModal(modalDialog(plotOutput("corr_prices_dialog",height = 1000,width = 1000),
                              footer = modalButton("Dismiss"),
                              easyClose = TRUE,
                              size = "l"))
    })
    
    observeEvent(input$view_plot2,{
        showModal(modalDialog(fluidRow(column(10,plotOutput("corr_returns_dialog",height = 1000,width = 1000))),
                              footer = modalButton("Dismiss"),
                              easyClose = TRUE,size = "l"))
    })
    
    output$sample <- downloadHandler(filename = "Cryptocurreccy_sample.xlsx",content = function(file){
        data <- read.xlsx("tickers.xlsx")
        write.xlsx(data,file)
    })
    
}