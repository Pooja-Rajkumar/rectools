library(shiny)
library(rectools)

function(input, output) {
    
    #allows user to choose from 2 files
    datasetInput <- reactive({
        switch(input$dataset,
            "lme4 df" = ratingsIn
        )
    })
    
    #numeric variable professor dropdown menu
    output$var1 <- renderUI({
        df <- datasetInput()
        numsMax <- sapply(df, max)
        
        sliderInput("slider1", "Choose a numeric variable for Professor ID", min = 1, max = numsMax[[1]], value = 1) #professor variable d
    })
    
    output$var2 <- renderUI({
        x = input$slider1 #professor ID

        df <- datasetInput()
        df <- subset(df, df$s == x) #creates a subset so user can only choose a student ID that has rated professor ID = x
        numsMax <- sapply(df, max)
        
        sliderInput("slider2", "Choose a numeric variable for Student ID", min = 1, max = numsMax[[2]], value = 1) #student variable s
    })
    
    #preview of the table
    output$table <- renderTable({
        datasetInput()
    })
    
    #creates a table by calling recosystem and finding the exact accuracy value
    output$text2 <- renderText({
        cat("Computation done for Recosystems; See shiny interface\n")
        
        #source('rectools-master/R/xvalRecoParallel.R')
        results = xvalReco(datasetInput())
        results$acc$exact
    })
    
    #this currently displays text but will display a table by calling nearest k neighbor and finding the exact accuracy value
    output$text3 <- renderText({
        cat("Computation done for Nearest K neighbor; See shiny interface\n")
        x = input$slider1 #professor ID
        y = input$slider2 #y is the student ID
        df <- datasetInput()
        
        #code from cosine.R
        ratingsIn <- formUserData(df)
        newUser = ratingsIn[[y]]
        predict.usrData(ratingsIn, newUser, x, 2)

    })
    
    output$text4 <- renderText({
        cat("Computation done; See shiny interface\n")

        "This would be the hybrid model"
    })
    
}











