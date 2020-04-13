library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("MyDEAr - My Differential Expression Analysis in R"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput(inputId = "file1", 
                label = "Choose Excel File",
                multiple = FALSE,
                accept = c(".xlsx", ".xls")),
      
      actionButton(inputId = "contents", 
                   label = "Read in data"),
      
      # Horizontal line ----
      tags$hr(),
      
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
                  choices = NULL),
      
      # Horizontal line ----
      tags$hr(),
      
      numericInput(inputId = "UP",
                   label = "Fold change upregulation:",
                   value = 0.6,
                   step = 0.1),
      
      numericInput(inputId = "DOWN",
                   label = "Fold change downregulation:",
                   value = -0.6,
                   step = 0.1),
      
      selectInput(inputId = "pval",
                  label = "p-value:",
                  choices = c(0.05, 0.01, 0.001, 0.0001)),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons(inputId = "disp", 
                   label = "Arrange data based on:",
                   choices = c(`Descending p-value` = "desc.pvalue",
                               `Ascending p-value` = "pvalue",
                               `Descending fold change` = "desc.fc",
                               `Ascending fold change` = "fc"),
                   selected = "desc.pvalue"),
      
      # Horizontal line ----
      tags$hr()
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Content", tableOutput("contents")),
                  tabPanel("Volcano Plot", plotOutput("volcano_plot")))
      
    )
  )
)


server <- function(input, output, session) {
  
  info <- eventReactive(list(input$contents), {
    
    inFile <- req(input$file1)
    df <- read_excel(inFile$datapath)
    df <- as.data.frame(df)
    df <- na.omit(df)
    
    ref <- c()

    for (i in names(df)){
      name <- names(df[i])
      # print(name)
      # print(class(df[, name]))
      if (class(df[,i]) == "character") {
        ref <- c(ref, i) } 
      else {
          next()
        }}

    vars1 <- ref
    vars2 <- grep("FC", names(df), value = TRUE)
    vars3 <- grep("val", names(df), value = TRUE)

    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "GeneID", "Choose the column with gene id:", choices = vars1)
    updateSelectInput(session, "FoldChange", "Choose the column with fold change values:", choices = vars2)
    updateSelectInput(session, "p.value", "Choose the column with p-value values:", choices = vars3)
  
    df
    
  })
  
  output$contents <- renderTable({
    
    df <- info()
    
    df <- select(df, input$GeneID, input$FoldChange, input$p.value)
    
    # try isolate number of observations to show
    
    if (input$disp == "desc.pvalue") {
      return(df %>% 
               dplyr::arrange(dplyr::desc(!!rlang::sym(input$p.value))))
    } else if (input$disp == "pvalue") {
      return(df %>% 
               dplyr::arrange(!!rlang::sym(input$p.value)))
    } else if (input$disp == "desc.fc") {
      return(df %>% 
               dplyr::arrange(dplyr::desc(!!rlang::sym(input$FoldChange))))
    } else {
      return(df %>% 
               dplyr::arrange(!!rlang::sym(input$FoldChange)))
    }
    
  }, digits = 5)
  
  output$volcano_plot <- renderPlot({
    
    df <- info()
    
    df <- select(df, input$GeneID, input$FoldChange, input$p.value)
    
    # PROBLEM WITH IFELSE CLASSIFICATION
    df$Threshold <- ifelse((df[ ,input$FoldChange] >= input$UP & df[ ,input$p.value] < input$pval), 
                           "Upregulated", 
                           ifelse((df[ ,input$FoldChange] <= input$DOWN & df[ ,input$p.value] < input$pval), 
                                  "Downregulated", "Not significant"))
    
    vp <- ggplot(data = df,
                 mapping = aes(x = df[,input$FoldChange],
                               y = -log10(df[,input$p.value]),
                               color = df$Threshold)) +
      geom_point(alpha = 0.3, size = 1) +
      scale_color_manual(values = c("dodgerblue", "gold", "deeppink2")) +
      labs(color = "Expression pattern") +
      xlab("log2FC") +
      ylab("-log10(p-value)") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 0, hjust = 1, size = 14),
            axis.text.y = element_text(angle = 0, hjust = 1, size = 14))
    
    vp
    
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)