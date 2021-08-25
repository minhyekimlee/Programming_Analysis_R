#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Minhye's Distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample", "Sample Size",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons("dist", label = h3("Distribution"),
                         choices = list("Normal" = 1, "Poisson" = 2, "Exponential" = 3, "Uniform" = 4), 
                         selected = 1),
            numericInput("bins", label = h3("Numbers of bins"), value = 10),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # Plot the graph
        # figure out the distribution the user wants
        # generate numbers from the observations

        if(input$dist == 1)
            x <- rnorm(input$sample)
        else if(input$dist == 2)
            x <- rpois(input$sample, 5)
        else if(input$dist == 3)
            x <- rexp(input$sample)
        else if(input$dist == 4)
            x <- runif(input$sample)
        
        library(ggplot2)
        ggplot() +
            aes(x) +
            geom_histogram(bins=input$bins)
})
}

# Run the application 
shinyApp(ui = ui, server = server)
