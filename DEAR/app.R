library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

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
      
      # Input: Select number of rows to display ----
      radioButtons(inputId = "disp", 
                   label = "Arrange data based on:",
                   choices = c(`Descending p-value` = "desc.pvalue",
                               `Ascending p-value` = "pvalue",
                               `Descending fold change` = "desc.fc",
                               `Ascending fold change` = "fc"),
                   selected = "desc.pvalue"),
      
      actionButton(inputId = "choice2", 
                   label = "Arrange data"),
      
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
    
    ref <- c()
    
    for (i in names(df)){
      name <- names(df[i])
      # print(name)
      # print(class(df[, name]))
      if (class(df[,i]) == "character") {
        ref <- c(ref, i) } else {
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
    
    # if (input$disp == "desc.pvalue") {
    # return(df <- df %>% select(input$GeneID, input$FoldChange, input$p.value) %>%
    #          arrange(desc(input$p.value)))
    # } else if (input$disp == "pvalue") {
    #   return(df <- df %>% select(input$GeneID, input$FoldChange, input$p.value) %>%
    #            arrange(input$p.value))
    # } else if (input$disp == "desc.fc") {
    #   return(df <- df %>% select(input$GeneID, input$FoldChange, input$p.value) %>%
    #            arrange(desc(input$FoldChange)))
    # } else {
    #   return(df <- df %>% select(input$GeneID, input$FoldChange, input$p.value) %>%
    #            arrange(input$FoldChange))
    # }
    
    # if(input$disp == "desc.pvalue") {
    # return(arrange_(df, lazyeval::interp(~desc(c), c = as.name(input$p.value))))
    # } else if(input$disp == "pvalue") {
    #   return(arrange(df, lazyeval::interp(~c, c = as.name(input$p.value))))
    # } else if(input$disp == "desc.fc") {
    #   return(arrange(df, lazyeval::interp(~desc(c), c = as.name(input$FoldChange))))
    # } else {
    #   return(arrange(df, lazyeval::interp(~c, c = as.name(input$FoldChange))))
    # }
    
    # if(input$disp == "head") {
    #   return(head(df))
    # } else {
    #   return(df)
    # }
    
    df
    
  })
  
  output$volcano_plot <- renderPlot({
    
    df <- info()
    
    df <- select(df, input$GeneID, input$FoldChange, input$p.value)
    
    UP = 0.6
    DOWN = -0.6
    pval = 0.05
    
    df$Threshold <- with(df, ifelse(df[,2] >= UP & df[,3], "Upregulated",
                                          ifelse(df[,2] <= DOWN & df[,3] < pval,
                                                 "Downregulated", "Not significant")))
    
    vp <- ggplot(data = df,
                 mapping = aes(x = df[,2],
                               y = -log10(df[,3]),
                               color = df[,4])) +
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