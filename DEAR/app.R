library(shiny)
library(readxl)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Differential Expression Analysis in R"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput(inputId = "file1", 
                label = "Choose Excel File",
                multiple = FALSE,
                accept = c(".xlsx", ".xls")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons(inputId = "disp", 
                   label = "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Horizontal line ----
      tags$hr(),
      
      actionButton("choice", "incorporate external information"),
      
      # Input: Selector for choosing FC value from the uploaded dataset ----
      selectInput(inputId = "GeneID",
                  label = "Choose the column with gene id:",
                  choices = NULL),
      
      # Input: Selector for choosing FC value from the uploaded dataset ----
      selectInput(inputId = "FoldChange",
                  label = "Choose the column with fold change values:",
                  choices = NULL),
      
      # Input: Selector for choosing  p-values from the uploaded dataset ----
      selectInput(inputId = "p.value",
                  label = "Choose the column with p-value values:",
                  choices = NULL)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
      )
    )
)


server <- function(input, output, session) {
  
  output$contents <- renderTable({
    
    inFile <- req(input$file1)

    df <- read_excel(input$file1$datapath)
    
    vars <- names(df)
    
    if(input$disp == "head") {
      return(head(df))
    } else {
      return(df)
    }
    
    updateSelectInput(session, "GeneID","Choose the column with gene id:", choices = vars)
    
    updateSelectInput(session, "FoldChange","Choose the column with fold change values:", choices = vars)
    
    updateSelectInput(session, "p.value","Choose the column with p-value values:", choices = vars)
    
  })
}



# Create Shiny app ----
shinyApp(ui, server)