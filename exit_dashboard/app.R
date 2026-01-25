library(shiny)

ui <- fluidPage(
  titlePanel("EI Exit Disparities — Prototype"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "exit_cat",
        label   = "Exit category to preview",
        choices = NULL
      )
    ),
    
    mainPanel(
      br(),
      textOutput("preview_caption"),
      verbatimTextOutput("colnames_df"),
      plotOutput("or_plot"), 
      tableOutput("data_preview")
    )
  )
)  

server <- function(input, output, session) {
  
  df <- readRDS("../data/analysis/state_avg_or_by_race_category_all_years.rds")
  
  output$or_plot <- renderPlot({
    
    req(input$exit_cat)
    
    plot_df <- df[df$category == input$exit_cat, ]
    
    plot_df$race_ethnicity <- factor(plot_df$race_ethnicity)
    
    plot(
      x = plot_df$or,
      y = plot_df$race_ethnicity,
      xlab = "Odds Ratio",
      ylab = "Race / Ethnicity",
      main = paste("Odds Ratios by Race/Ethnicity:", input$exit_cat)
    )
    
  })
  
  
  updateSelectInput(
    session,
    inputId = "exit_cat",
    choices = sort(unique(df$category)),
    selected = sort(unique(df$category))[1]
  )
  
  
  output$colnames_df <- renderPrint({
    names(df)
  })
  
  output$preview_caption <- renderText({
    paste("Preview for exit category:", input$exit_cat)
  })
  
  output$data_preview <- renderTable({
    
    req(input$exit_cat)
    
    show_df <- df[df$category == input$exit_cat,
                  c("state", "race_ethnicity", "or", "log_or", "ci_low", "ci_high")]
    
    head(show_df, 10)
  })
  
  
}

shinyApp(ui = ui, server = server)