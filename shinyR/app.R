# Create Shiny app ----
# Importing required library for plotting and shiny
library(shiny)
library(ggplot2)
library(ggpmisc)
# Defining UI for the application
# The app uses code (slightly modified) for reading a csv to shiny app from Source- https://shiny.rstudio.com/gallery/file-upload.html

ui <- fluidPage(
    titlePanel("Regression"),      # Title of the app output
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            sliderInput("Poly",
                        "Order of polynomial model:",
                        min = 0,
                        max = 10,
                        value = 0),
            
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
            checkboxInput("se", "Confidence Interval", TRUE),
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(),
            textOutput("Export"),
            numericInput("width", "Width (inches)", value = 7, min = 1, max = 20),
            numericInput("height", "Height (inches)", value = 7, min = 1, max = 20),
            numericInput("dpi", "DPI", value = 150, min = 72, max = 300),
            downloadButton('Download')
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output scatter Plot with regression
            plotOutput("scatterPlot"),
            plotOutput("residualPlot"),
            tableOutput("contents")
        )
    )
)

# Define server logic to read selected file
# Logic for output of a scatter plot with linear regression from a csv input

server <- function(input, output) {
    
    # Reactive dataframe variable from csv input
    dataInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # Parsing data from csv to a Reactive dat() for plotting
    dat <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,header = input$header,sep = input$sep)
        df
    })
    
    # Scatter plot with regressional analysis using stat_smooth (ggplot2) and stat_poly_eq (from ggpmisc)
    output$scatterPlot <- renderPlot({figure()},height = 400, width = 600)
    output$residualPlot <- renderPlot({residual()},height = 200, width = 600)    
    
    figure <- reactive ({
        ggplot(dat(),aes(x=x,y=y))+geom_point(colour='red')+stat_smooth(                                    # Scatter Plot 
            method = "lm",                                                                                  # Fitting/smoothing to a polynomial fit
            formula = y ~ poly(x, input$Poly, raw = T),                                                     # Formula for the polynomial fit from input
            size = 1,                                                                                       # Line width of the fit
            se=input$se)+stat_poly_eq(                                                                             # Function of equation and r squared value
                formula = y ~ poly(x, input$Poly, raw = T),
                aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")),
                parse = T, label.y="top", label.x="left")
    })
    
    residual <- reactive ({
        req(input$Poly >= 1)
        model <- lm(dat()$y ~ poly(dat()$x, input$Poly, raw = T))
        ggplot(dat(),aes(x=x,y=resid(model)))+geom_point(colour='green') +
            labs(x = "X", y = "Residuals", 
                 title = paste("Residuals Plot"))
    })
    
    output$Download <- downloadHandler(
        filename = function() { paste(tools::file_path_sans_ext(input$file1), 'order', input$Poly, '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = figure(), width = input$width, height = input$height, dpi = input$dpi, units = "in", device = "png")
        })
    
    output$Export <- renderText(
        paste("Download")
    )
}
# Run the application 
shinyApp(ui = ui, server = server)