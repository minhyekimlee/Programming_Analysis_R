
# My regression app

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    headerPanel("Interactive Regression"),
    sidebarPanel(
        
        sliderInput("slider1", label = h3("Time range (Month)"), min = 1, 
                    max = 100, value = c(40, 60)),
        sliderInput("slider2", label = h3("Observation range (Sales)"), min = 1, 
                    max = 100, value = c(40, 60)),
        
        checkboxInput("checkbox1", label = "Show confidence band for all data", value = TRUE),
        checkboxInput("checkbox2", label = "Show confidence band for subset of data", value = TRUE)
        
    ),
    
    mainPanel(
        
        plotOutput("dataPlot")
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #read the file
    df <- read.csv(file = "C:/Users/minhyekim/Documents/R/MinhyeRegression/Datasource.csv")
    
    #render plot
    output$dataPlot <- renderPlot({
        if(input$checkbox1 == FALSE) {
            
            #subset our data
            assign("minval1", input$slider1[[1]])
            assign("maxval1", input$slider1[[2]])
            
            assign("minval2", input$slider2[[1]])
            assign("maxval2", input$slider2[[2]])
        
            #create the subset
            df_subset <- subset(df,
                                (Month >= minval1) &
                                (Month <= maxval1) &
                                (Sales >= minval2) &
                                (Sales <= maxval2) 
                                )
            
            #plot data
            library(ggplot2)
            ggplot(data = df_subset, aes(x = Month, y = Sales)) +
                geom_point()
        } else {
            #plot data
            library(ggplot2)
            ggplot(data = df, aes(x = Month, y = Sales)) +
                geom_point()
            
        }
       
    
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

