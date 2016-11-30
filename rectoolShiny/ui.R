library(shiny)
ratingsIn <- read.csv("data/subset.csv") #first 100 rows of original dataset
fullRatings <- read.csv("data/fullset.csv") #original dataset

fluidPage(

    titlePanel('Rectools'), #title

    sidebarLayout(
        sidebarPanel(
            #allows user to pick between the two different datasets
            #the subset is currently the default because it works
            selectInput("dataset", "Choose a dataset:",
                choices = c("Movie Lens Data (subset)", "Movie Lens Data"),
                selected = "Movie Lens Data (subset)"),

            #allows the user to pick the method by clicking a button
            #recosystems is the default because its the only one with a table
            radioButtons("radio", "Choose method:",
                choices = c("Recosystems" , "Nearest K Neighbor", "Hybrid", "Display Data"),
                selected = "Recosystems")
        ),

        mainPanel(
        #based on which radio button the user clicks, the output is different
        #each conditional panel coordinates with a model
        conditionalPanel(
            condition = "input.radio == 'Recosystems'", tableOutput('table2')),
        conditionalPanel(
            condition = "input.radio == 'Nearest K Neighbor'", tableOutput('text3')),
        conditionalPanel(
            condition = "input.radio == 'Hybrid'", tableOutput('text4')),
        conditionalPanel(
            condition = "input.radio == 'Display Data'", tableOutput('table'))
        )
    )
)
