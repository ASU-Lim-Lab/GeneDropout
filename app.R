library(shiny)
library(plotly)
library(htmlwidgets)


ui <- fluidPage(  
  
  sidebarPanel(
    # User can input file
    downloadButton("download_plotly_widget", "Download Plotly Graph"),
    tags$hr(),
    fileInput(inputId = "file1", label = "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values, text/plain",
                         ".csv")
    ),
    
    # Input: Checkbox if file has header
    checkboxInput("header", "Header", TRUE),
    # Select whether file is comma, semicolon, or tab separated
    radioButtons(inputId ="sep", label = "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    selectInput('xcol', 'X Variable', ""),
    selectInput('ycol', 'Y Variable', "", selected = ""),
    br(),
    p("Click on 'Browse' to upload an input file with sample IDs as the first column and CT values as the rest of the columns.Ensure that all CT value columns contain only numbers"),
    HTML("<p><a href='https://github.com/ASU-Lim-Lab/GeneDropout/blob/main/DeltaDropoutPlotly.csv'>Example Input File</a></p>"),
    p("Under the 'Separator', select whether your input file is a comma, tab, or semicolon separated file"),
    p("Select the X and Y variables to be plotted. The options will show the header names of the input file"),
    p("Click on the 'Plot' tab to display interactive plot of samples IDs and CT values"),
    p("Click on 'Download Plotly Graph' to save copy of graph locally"),
    br(),

  ),
  
  mainPanel(
    tabsetPanel( type = "tabs",
                 tabPanel(
                   # App title
                   titlePanel("Upload CT File"),
                   # Output: Data file
                   tableOutput("contents")
                   
                 ),
                 tabPanel(
                   titlePanel("Plot"),
                   plotlyOutput('MyPlot')
                 ),
                 tabPanel(
                   titlePanel(""),
                   verbatimTextOutput("")
                 )
    ),
    
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[sapply(df, is.numeric)])
    return(df)
    
  })
  output$download_plotly_widget <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(session_store$plt), file, selfcontained = TRUE)
    })
  
  output$contents <- renderTable({
    data()
  })
  
  session_store <- reactiveValues()
  
  output$MyPlot <- renderPlotly({
    x <- data()[, c(input$xcol, input$ycol)]
    
    p <- ggplot(x, aes_string(input$xcol, input$ycol,Label = factor(data()[, 1])))
    p <- p + geom_point()+theme_bw() 
    plotly_p <- plotly::ggplotly(p)
    plotly_p
    session_store$plt <- plotly_p
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
