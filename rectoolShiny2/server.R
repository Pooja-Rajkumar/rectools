library(shiny)

function(input, output) {
    
    #allows user to choose from 2 files
    datasetInput <- reactive({
        switch(input$dataset,
            "lme4 df" = ratingsIn
        )
    })
    
    #preview of the table
    output$table <- renderTable({
        datasetInput()
    })
    
    #creates a table by calling recosystem and finding the exact accuracy value
    output$text2 <- renderText({
        cat("Computation done; See R Console")
        source('rectools-master/R/xvalRecoParallel.R')
        results = xvalReco(datasetInput())
        results$acc$exact
    })
    
    #this currently displays text but will display a table by calling nearest k neighbor and finding the exact accuracy value
    output$text3 <- renderText({
        "This would be nearest k neighbor"
    })
    
    #this currently displays text but will display a table by calling the hybrid model and finding the exact accuracy value
    output$text4 <- renderText({
        "This would be the hybrid model"
    })
    
}