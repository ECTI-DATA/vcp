shinyServer(function(input, output) {

    output$choice <- renderUI({
        if (input$upload_selection=="Enter manually") {
            textInput("tickers","Enter crypto currency symbols seperated by comma without space")
        }else{
            fileInput("tickers","Upload excel contaings crypto currency symbols")
        }
    })

    ticker <- eventReactive(input$submit,{
        if (input$upload_selection == "Enter manually") {
            ticker <- input$tickers
            ticker <- ticker <- gsub(" ", "", ticker)
            ticker <- unlist(strsplit(ticker,split = ","))
        }else{
            ticker <- read.xlsx(input$tickers$datapath)[,1]
        }
        ticker <- c(ticker,input$benchmark)
        ticker
    })

    data_base <- reactive({
        ticker <- ticker()

        data <- lapply(ticker, getSymbols, auto.assign = FALSE)
        names(data) <- ticker

        data <- lapply(data,function(x){
            x <- x[,6]
            names(x) <- "Adjusted"
            x
        })

        data <- lapply(data, function(x){
            x <- na.approx(x[,"Adjusted"])
            x
        })
        names(data) <- ticker

        data

    })

    data_returns <- reactive({
        data <- data_base()
        horizon <- paste0(as.character(input$year), "/", as.character(Sys.Date()))

        data <- lapply(data, function(x){
            x <- x[horizon]
            x
        })
        data <- lapply(data, periodReturn,period = input$return_type, type = "arithmetic")
        data

        ##
        data_return <- data[[1]]

        for (i in 1:(length(data)-1)) {
            data_return <- merge.xts(data_return,data[[i+1]])
        }

        #Setting COlumn Names

        ticker <- ticker()
        colnames(data_return) <- ticker

        data_return

    })

    output$plot1 <- renderDygraph({

        data <- data_returns()

        dygraph(data, main = "Crypto currency Comparison") %>%
            dyAxis("y", label = "Return") %>%
            dyRangeSelector() %>%
            dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2"),strokeWidth = 2) %>%
            dyLegend(labelsDiv = "legend")


    })

    output$cor <- renderPlot({
        data <- data_returns()

        if(!(TRUE %in% c(is.na(data)))){
            corrplot(cor(data),method = "number")
        }

    })

    portfolio <- reactive({
        data <- data_returns()
        #removing and NA and filling it interpolation
        for (i in 1:ncol(data)) {
            data[,i] <- na.approx(data[,i])
        }

        weight <- read.xlsx(input$tickers$datapath)[,2]
        portfolio_returns <- Return.portfolio(R = data[,1:(ncol(data)-1)], weights = weight, wealth.index = TRUE)
        benchmark_returns <- Return.portfolio(R = data[,4], wealth.index = TRUE)
        comp <- merge.xts(portfolio_returns, benchmark_returns)
        colnames(comp) <- c("Portfolio", "Benchmark")
        comp
    })

    output$message <- renderUI({
        if(input$upload_selection=="Enter manually"){
            h4(tags$b("Please upload excel file with crypto currency symbols and weights in 'Crypto Currency Returns' tab. Sum of weights of all the crypto currency must be equal to 1."))
        }
    })


    output$icon3 <- renderUI({

        if(input$upload_selection=="Enter manually"){
            img(src='alert.png', align = "center",height = 60, width = 60)
        }
    })

    output$plot2 <- renderDygraph({
        comp <- portfolio()
        weight_data <- read.xlsx(input$tickers$datapath)
        if (sum(weight_data[,2])==1 && sum(is.na(weight_data[,2]))==0) {
            dygraph(comp, main = "Portfolio Performance vs. Benchmark") %>%
                dyAxis("y", label = "Amount ($)") %>%
                dyRangeSelector() %>%
                dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2"),strokeWidth = 2) %>%
                dyLegend(labelsDiv = "legend2")
        }

    })

    output$download_sample <- downloadHandler(filename = "symbols.xlsx",content = function(file){
        symbols <- read.xlsx("data/symbols.xlsx")
        write.xlsx(symbols,file)
    })

    output$download_list <- downloadHandler(filename = "symbols_list.xlsx",content = function(file){
        symbols <- read.xlsx("data/symbols_list.xlsx")
        write.xlsx(symbols,file)
    })

    output$corr_message <- renderUI({
        data <- data_returns()
        if(TRUE %in% c(is.na(data))){
            h4(tags$b("Data is not sufficient to plot the correlation plot. Data for one of the crypto currency is not available for the selected starting starting year. Please select another starting date."))
        }
    })

    output$icon1 <- renderUI({
        data <- data_returns()
        if(TRUE %in% c(is.na(data))){
            img(src='alert.png', align = "center",height = 60, width = 60)
        }
    })

    output$message2 <- renderUI({
        weight_data <- read.xlsx(input$tickers$datapath)
        if (sum(weight_data[,2])!=1 || sum(is.na(weight_data[,2]))!=0) {
            h4(tags$b("Either sum of all the weights is not equal to one, or weight of all the crypto currency are not provided in the excel. So please check the uploaded file and try again."))
        }
    })

    output$message3 <- renderUI({
        data <- data_returns()
        if(TRUE %in% c(is.na(data))){
            h4(tags$b("Data is not sufficient. Data for one of the crypto currency is not available for the selected starting starting year. Please select another starting date."))
        }
    })

    output$icon2 <- renderUI({
        weight_data <- read.xlsx(input$tickers$datapath)
        if (sum(weight_data[,2])!=1 || sum(is.na(weight_data[,2]))!=0) {
            img(src='alert.png', align = "center",height = 60, width = 60)
        }
    })

    output$icon4 <- renderUI({
        data <- data_returns()
        if (TRUE %in% c(is.na(data))) {
            img(src='alert.png', align = "center",height = 60, width = 60)
        }
    })



})
