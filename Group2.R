library(shiny)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(plotly)

ui <- fluidPage(
    
    titlePanel("Group # 2 Assignment"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset",
                        h4("Select a Dataset:"),
                        c("Median Wage (USD)",
                          "Unemployment Rate",
                          "Underemployment Rate")),
            br(), br(),br(),
            sliderInput("date_range",
                        h4("[Fig. 1] Date Range in Time Series Data:"),
                        min = as.Date("1990-01-01"), max = as.Date("2020-06-01"),
                        value = c(as.Date("2015-06-01"), as.Date("2020-06-01"))),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            radioButtons("radio", 
                         h4("[Fig. 2] Filter by Major: "),
                         choices = list("All" = 1, "Top 10" = 2, "Bottom 10" = 3), 
                         selected = 2),    
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            radioButtons("y_for_scatter", 
                         h4("[Fig. 3] Select a Dependent Variable:"),
                         choices = list("Unemployment Rate" = 1,"Median Wage(Early Career)" = 2), 
                         selected = 1),
            
            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
            br(),br(),br(),br(),br(),br(),br(),br()
        ),
        
        
        
        mainPanel(
            h3("US College Graduates Majors and Employment"),
            p("For our project, we wanted to answer the questions: How does education affect the wage and employment outcomes of college graduates? What is the relationship among choice of major, underemployment rate, unemployment rate and pay level? We looked at the dataset which spans 30 years of wage and employment data of U.S. college graduates to see what insights we could gather through the use of R."),
            br(),
            textOutput("txtOutput1"),
            plotOutput("distPlot"),
            br(),
            textOutput("txtOutput2"),
            plotOutput("majorBar"),
            br(),
            p("Underemployment is a big problem among college graduates. It is highly related to the choice of major. Underemployment rates vary by over 60 percentage points, from 11% in special education to 73% in criminal justice. When choosing majors in college, many students may wonder how employable a major may be, how likely they will be working in a relevant field, and the level of pay."),
            p("Please choose the dependent variable to check out the unemployment vs. underemployment rate chart and the median wage vs. underemployment rate chart. The previous chart shows the income-level unemployment rate and underemployment rate of different professions. It may help career-oriented students find out what majors or skills are more needed in the job market and less likely to be unemployed. Light-colored dots which indicate majors with higher pay mostly appear to the left of the chart which indicates lower underemployment rate; the same relationship can also be observed in the median wage vs. underemployment rate chart. "),
            br(),
            textOutput("txtOutput3"),
            plotlyOutput("graph"),
            br(), br(), br(), br(),          
            p("Data Source: ", a("https://www.kaggle.com/dbsimpson/us-college-graduates-wages", href="https://www.kaggle.com/dbsimpson/us-college-graduates-wages")),
            
        )
        
    )
)



server <- function(input, output) {
    
    output$txtOutput1 = renderText({
        paste0("Figure 1. Time Series Data for ", input$dataset, " from ", input$date_range[1], " to ", input$date_range[2])
    })
    
    
    output$distPlot <- renderPlot({
        
        minval <- input$date_range[1]
        maxval <- input$date_range[2]
        
        if (input$dataset == 'Median Wage (USD)'){
            df <- read.csv("wages.csv", header = TRUE)
        }
        
        else if (input$dataset == 'Unemployment Rate'){
            df <- read.csv("Unemployment_rate.csv", header = TRUE)
        }
        
        else if (input$dataset == 'Underemployment Rate'){
            df <- read.csv("under_employment_college_grads.csv", header = TRUE)
        }
        
        df = melt(df, id = 'Date')
        df$Date = as.Date(as.character(df$Date), "%m/%d/%Y")
        df_subset <- subset(df,
                            ( Date >= minval ) &
                                ( Date <= maxval ))
        ggplot(df_subset, aes(x = Date, y = value, group = variable, color = variable, shape = variable)) + 
            geom_point() + geom_line()
        
    })
    
    
    output$txtOutput2 = renderText({
        choiceslist = list("All" = 1, "Top 10" = 2, "Bottom 10" = 3)
        namechoice <- names(choiceslist)
        paste0("Figure 2. ", input$dataset, " ", namechoice[as.integer(input$radio)], " Majors")
    })    
    
    output$majorBar <- renderPlot({
        
        
        #median wage 
        if (input$dataset == 'Median Wage (USD)' && input$radio == '1'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            majors <- group_by(dfm, Major) %>% 
                summarise(avgMajor = mean(Median.Wage.Early.Career)) %>% 
                arrange(desc(avgMajor))  
            
            # Bar chart
            ggplot(majors, aes(reorder(Major, avgMajor))) +
                geom_bar(aes(weight = avgMajor), fill = "#009E73") + 
                coord_flip() +
                xlab("Major") +
                ylab("Median Wage Early Career") +
                theme(axis.text = element_text(size=5))
        }
        
        else if (input$dataset == 'Median Wage (USD)' && input$radio == '2'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            # Get top 10 majors
            majors <- group_by(dfm, Major) %>% 
                summarise(avgMajor = mean(Median.Wage.Early.Career)) %>% 
                arrange(desc(avgMajor)) %>% 
                top_n(10)
            
            # Bar chart
            ggplot(majors, aes(reorder(Major, avgMajor))) +
                geom_bar(aes(weight = avgMajor), fill = "#009E73") + 
                coord_flip() +
                xlab("Major") +
                ylab("Median Wage Early Career")
        }
        
        else if (input$dataset == 'Median Wage (USD)' && input$radio == '3'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            # Get bottom 10 majors
            majors <- group_by(dfm, Major) %>% 
                summarise(avgMajor = mean(Median.Wage.Early.Career)) %>% 
                arrange(desc(avgMajor)) %>% 
                top_n(-10)
            
            # Bar chart
            ggplot(majors, aes(reorder(Major, avgMajor))) +
                geom_bar(aes(weight = avgMajor), fill = "#009E73") + 
                coord_flip() +
                xlab("Major") +
                ylab("Median Wage Early Career")
        }
        
        
        
        #Unemployment 
        else if (input$dataset == 'Unemployment Rate' && input$radio == '1'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            unemp <- group_by(dfm, Major) %>% 
                summarise(avgunemp = mean(Unemployment.Rate)) %>% 
                arrange(desc(avgunemp))  
            
            # Bar chart
            ggplot(unemp, aes(reorder(Major, avgunemp))) +
                geom_bar(aes(weight = avgunemp), fill = "tomato3") + 
                coord_flip() +
                xlab("Major") +
                ylab("Unemployment Rate") +
                theme(axis.text = element_text(size=5))
        }
        
        else if (input$dataset == 'Unemployment Rate' && input$radio == '2'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            unemp <- group_by(dfm, Major) %>% 
                summarise(avgunemp = mean(Unemployment.Rate)) %>% 
                arrange(desc(avgunemp)) %>% 
                top_n(10)  
            
            # Bar chart
            ggplot(unemp, aes(reorder(Major, avgunemp))) +
                geom_bar(aes(weight = avgunemp), fill = "tomato3") + 
                coord_flip() +
                xlab("Major") +
                ylab("Unemployment Rate")
        }
        
        else if (input$dataset == 'Unemployment Rate' && input$radio == '3'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            unemp <- group_by(dfm, Major) %>% 
                summarise(avgunemp = mean(Unemployment.Rate)) %>% 
                arrange(desc(avgunemp)) %>% 
                top_n(-10)  
            
            # Bar chart
            ggplot(unemp, aes(reorder(Major, avgunemp))) +
                geom_bar(aes(weight = avgunemp), fill = "tomato3") + 
                coord_flip() +
                xlab("Major") +
                ylab("Unemployment Rate")
        }
        
        
        #Underemployment 
        else if (input$dataset == 'Underemployment Rate' && input$radio == '1'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            underemp <- group_by(dfm, Major) %>% 
                summarise(avgunderemp = mean(Underemployment.Rate)) %>% 
                arrange(desc(avgunderemp))  
            
            # Bar chart
            ggplot(underemp, aes(reorder(Major, avgunderemp))) +
                geom_bar(aes(weight = avgunderemp), fill = "Orange") + 
                coord_flip() +
                xlab("Major") +
                ylab("Underemployment Rate") +
                theme(axis.text = element_text(size=5))
        }
        
        else if (input$dataset == 'Underemployment Rate' && input$radio == '2'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            underemp <- group_by(dfm, Major) %>% 
                summarise(avgunderemp = mean(Underemployment.Rate)) %>% 
                arrange(desc(avgunderemp)) %>% 
                top_n(10)  
            
            # Bar chart
            ggplot(underemp, aes(reorder(Major, avgunderemp))) +
                geom_bar(aes(weight = avgunderemp), fill = "Orange") + 
                coord_flip() +
                xlab("Major") +
                ylab("Underemployment Rate")           
        }
        
        else if (input$dataset == 'Underemployment Rate' && input$radio == '3'){
            dfm <- read.csv("labor_market_college_grads.csv")
            
            underemp <- group_by(dfm, Major) %>% 
                summarise(avgunderemp = mean(Underemployment.Rate)) %>% 
                arrange(desc(avgunderemp)) %>% 
                top_n(-10)  
            
            # Bar chart
            ggplot(underemp, aes(reorder(Major, avgunderemp))) +
                geom_bar(aes(weight = avgunderemp), fill = "Orange") + 
                coord_flip() +
                xlab("Major") +
                ylab("Underemployment Rate")         
        }
        
        
    })
    
    output$txtOutput3 = renderText({
        
        paste0("Figure 3. Relations among Majors and Employment Indicators")
    })   
    
    df<-read.csv("labor_market_college_grads.csv", header=TRUE)
    output$graph<- renderPlotly({
        if(input$y_for_scatter==2)
        {plot_ly(df, 
                 y = ~Median.Wage.Early.Career, 
                 x = ~Underemployment.Rate,
                 name="different majors",
                 color=~Unemployment.Rate,
                 text = ~Major,
                 type="scatter"
        ) %>% layout(
            shapes = list(list(type = "line", fillcolor = "red",
                               line = list(color = "red"),
                               y0 = df[which(df["Major"]=="Overall"),"Median.Wage.Early.Career"], 
                               y1 = df[which(df["Major"]=="Overall"),"Median.Wage.Early.Career"], 
                               x0 = min(df[,"Underemployment.Rate"]), 
                               x1 = max(df[,"Underemployment.Rate"])),
                          list(type = "line", fillcolor = "red",
                               line = list(color = "red"),
                               y0 = min(df[,"Median.Wage.Early.Career"]), 
                               y1 = max(df[,"Median.Wage.Early.Career"]), 
                               x0 = df[which(df["Major"]=="Overall"),"Underemployment.Rate"], 
                               x1 = df[which(df["Major"]=="Overall"),"Underemployment.Rate"])
            )
        ) %>% add_annotations(
            y = df[which(df["Major"]=="Overall"),"Median.Wage.Early.Career"],
            x = df[which(df["Major"]=="Overall"),"Underemployment.Rate"],
            text = "Overall",color="red",arrowcolor="red",font=list(size=10,face="bold",color="red")
        )%>%add_lines(
            line = list(color = "black"),
            name="regression line",
            x = ~df$Underemployment.Rate, 
            y = fitted(lm(df$Median.Wage.Early.Career ~ df$Underemployment.Rate)))
        }
        else
            plot_ly(df, 
                    y = ~Unemployment.Rate, 
                    x = ~Underemployment.Rate,
                    color= ~Median.Wage.Early.Career,
                    text = ~Major,
                    type="scatter",
                    name="different majors"
                    
            ) %>% 
            
            layout(shapes = list(list(type = "line", fillcolor = "red",
                                      line = list(color = "red"),
                                      y0 = df[which(df["Major"]=="Overall"),"Unemployment.Rate"], 
                                      y1 = df[which(df["Major"]=="Overall"),"Unemployment.Rate"], 
                                      x0 = min(df[,"Underemployment.Rate"]), 
                                      x1 = max(df[,"Underemployment.Rate"])),
                                 list(type = "line", fillcolor = "red",
                                      line = list(color = "red"),
                                      y0 = min(df[,"Unemployment.Rate"]), 
                                      y1 = max(df[,"Unemployment.Rate"]), 
                                      x0 = df[which(df["Major"]=="Overall"),"Underemployment.Rate"], 
                                      x1 = df[which(df["Major"]=="Overall"),"Underemployment.Rate"])
                                 
            )
            )%>% add_annotations(
                y = df[which(df["Major"]=="Overall"),"Unemployment.Rate"],
                x = df[which(df["Major"]=="Overall"),"Underemployment.Rate"],
                text = "Overall",color="red",arrowcolor="red",font=list(size=10,face="bold",color="red")
            )%>%add_lines(
                line = list(color = "black"),
                name="regression line",
                x = ~df$Underemployment.Rate, 
                y = fitted(lm(df$Unemployment.Rate ~ df$Underemployment.Rate))) 
        
        
    })
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
