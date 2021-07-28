#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Regrex"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            #Input action button for interactive linear model
            actionButton("go", "Go")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("lmPlot"),
            # Add values for the lmdata outputs
            h5("R-squared Values"),
            textOutput("rsquared"),
            textOutput("intercept"),
            textOutput("slope"),
            textOutput("pvalue"),
            plotOutput("lmdata"),
            tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # Make reactive button to show linear modeling plotline 
    lmplot <- eventReactive(input$go, {
        lm(y ~ x, data = dataInput())
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y, main = "Scatter Plot")
    })
    
    # Create output and plot for linear model based on scatter plot data
    output$lmPlot <- renderPlot({
        plot(x = dataInput()$x, y = dataInput()$y, main = "Linear Model")
        abline(lmplot(), col = "aquamarine3", lwd = 4)
    })
    
    # Need to create outputs for slope, intercept, and correlation coefficient from lm
    output$lmdata <- renderPlot({
        ggplot(lmplot()$model, aes_string(x = names(lmplot()$model)[2], y = names(lmplot()$model)[1])) + 
            geom_point() +
            stat_smooth(method = "lm", col = "blue") +
            ggtitle("Linear Regression Data Labeled") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            geom_label(aes(x = 0, y = 7.5), hjust = 0, 
                       label = paste("Adj R2 = ",signif(summary(lmplot())$adj.r.squared, 5),
                                     "\nIntercept =",signif(lmplot()$coef[[1]],5 ),
                                     " \nSlope =",signif(lmplot()$coef[[2]], 5),
                                     " \nP =",signif(summary(lmplot())$coef[2,4], 5)))
    })
    
    # Add the lmdata values as labels to the top of the lmdata graph
    output$rsquared <- renderText({
        print(paste("Adj R2 = ",signif(summary(lmplot())$adj.r.squared, 5)))
    })
    
    output$intercept <- renderText({
        print(paste("Intercept =",signif(lmplot()$coef[[1]],5)))
    })
    
    output$slope <- renderText({
        print(paste("Slope =",signif(lmplot()$coef[[2]], 5)))
    })
    
    output$pvalue <- renderText({
        print(paste("P =",signif(summary(lmplot())$coef[2,4], 5)))
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
