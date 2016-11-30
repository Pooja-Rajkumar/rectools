library(shiny)
ratingsIn <- read.csv("data/subset.csv") #first 100 rows of original dataset
fullRatings <- read.csv("data/fullset.csv") #original dataset

function(input, output) {
    
    #allows user to choose from 2 files
    datasetInput <- reactive({
        switch(input$dataset,
            "Movie Lens Data (subset)" = ratingsIn,
            "Movie Lens Data" = fullRatings
        )
    })
    
    #preview of the table
    output$table <- renderTable({
        datasetInput()
    })
    
    #creates a table by calling recosystem and finding the exact accuracy value
    output$table2 <- renderTable({
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