library(shiny)
library(bslib)
library(dplyr)
library(plotly)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("EI Exit Disparities — Prototype"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "exit_cat",
        label   = "Exit Category",
        choices = NULL,
        selected = "largest"   # NEW: default is largest disparity
      ),
      
      selectInput(
        inputId = "state_sel",
        label   = "State",
        choices = NULL
      )
    ),
    
    mainPanel(
      plotlyOutput("map_plot", height = "600px"),
      br(),
      plotOutput("or_plot")
    )
  )
)

server <- function(input, output, session) {
  
  df <- readRDS("../data/analysis/state_avg_or_by_race_category_all_years.rds")
  map_df <- readRDS("../data/analysis/map_summary_logor_optionA.rds")
  welcome_df <- readRDS("../data/analysis/welcome_map_logor_optionA.rds")
  
  updateSelectInput(
    session,
    inputId = "state_sel",
    choices = sort(unique(df$state)),
    selected = sort(unique(df$state))[1]
  )
  
  updateSelectInput(
    session,
    inputId = "exit_cat",
    choices = c(
      "Largest Disparity Category (All)" = "largest",  # NEW option
      "Dismissed (No Contact)"           = "dismissed",
      "Moved Out"                        = "moved_out",
      "Not Determined"                   = "not_determined",
      "Not Eligible"                     = "not_eligible",
      "Part B Eligible"                  = "part_b_eligible",
      "Withdrawn"                        = "withdrawn"
    ),
    selected = "largest"  # NEW default
  )
  
  # NEW: central reactive that decides which map data to use
  map_data <- reactive({
    req(input$exit_cat)
    
    if (input$exit_cat == "largest") {
      welcome_df
    } else {
      map_df %>% filter(category == input$exit_cat)
    }
  })
  
  output$or_plot <- renderPlot({
    
    req(input$exit_cat, input$state_sel)
    
    # NEW: if "largest" is selected, use that state's chosen category
    if (input$exit_cat == "largest") {
      chosen_cat <- welcome_df %>%
        filter(state == input$state_sel) %>%
        pull(category)
      
      if (length(chosen_cat) == 0 || is.na(chosen_cat[1])) return(NULL)
      
      cat_to_plot <- chosen_cat[1]
    } else {
      cat_to_plot <- input$exit_cat
    }
    
    plot_df <- df %>%
      filter(category == cat_to_plot,
             state == input$state_sel)
    
    if (nrow(plot_df) == 0) return(NULL)
    
    plot_df$race_ethnicity <- factor(plot_df$race_ethnicity)
    
    plot(
      x = plot_df$or,
      y = plot_df$race_ethnicity,
      xlab = "Odds Ratio",
      ylab = "Race / Ethnicity",
      main = paste(
        "Odds Ratios by Race/Ethnicity:",
        cat_to_plot,
        "-",
        input$state_sel
      )
    )
  })
  
  output$map_plot <- renderPlotly({
    
    plot_df <- map_data()   # NEW: unified data source
    
    bad_df  <- plot_df[plot_df$unreliable_state == TRUE, ]
    good_df <- plot_df[plot_df$unreliable_state == FALSE, ]
    
    plot_ly() %>%
      add_trace(
        data = bad_df,
        type = "choropleth",
        locationmode = "USA-states",
        locations = ~state_abb,
        z = ~map_value,
        text = ~hover_text,
        hoverinfo = "text",
        colorscale = list(list(0, "gray80"), list(1, "gray80")),
        showscale = FALSE,
        marker = list(line = list(color = "white", width = 0.5))
      ) %>%
      add_trace(
        data = good_df,
        type = "choropleth",
        locationmode = "USA-states",
        locations = ~state_abb,
        z = ~map_value,
        text = ~hover_text,
        hoverinfo = "text",
        colorscale = list(
          list(0, "green"),
          list(0.5, "yellow"),
          list(1, "red")
        ),
        marker = list(line = list(color = "white", width = 0.5))
      ) %>%
      layout(
        geo = list(scope = "usa"),
        margin = list(l = 0, r = 0, t = 10, b = 0)
      )
  })
  
}

shinyApp(ui = ui, server = server)
