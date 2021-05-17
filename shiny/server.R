
shinyServer(function(input, output, session) {
    
    mainDF <- eventReactive(
        input$filterButton, 
        {
            withProgress(message = 'Filtering main data frame...', value = 0, {
            
            df <- data.table::fread("www/shinyAppDataFrame.csv") %>% 
                filter(cod_nivel %in% unlist(input$levels) &
                           cod_com_rbd %in% getCodComs_from_market(input$market))
            
            incProgress(0.5) 
            Sys.sleep(1) 
            incProgress(0.5)
            
            return(df)
            
            })
        })
    
    firstYearSAE <- eventReactive(
        input$filterButton, 
        getSaeYear_from_market(input$market)
    )
    
    output$plotSimce <- renderPlot({
        if (input$simcePlot == "Both") {
            p <- plotSimce(mainDF(), 
                           test = input$simceTest,
                           SaeYear = firstYearSAE(),
                           fun = input$simceFun) 
        } else if (input$simcePlot == "Difference") {
            p <- plotSimceDif(mainDF(), 
                              test = input$simceTest, 
                              SaeYear = firstYearSAE(),
                              fun = input$simceFun)
        }
        return(p)
    })
    
    output$plotSegregation <- renderPlot({
        plotSegregationIndex(mainDF(), 
                             index = input$segregationIndex, 
                             SaeYear = getSaeYear_from_market(input$market))
    })
})
