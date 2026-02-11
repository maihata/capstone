library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(dplyr)
library(markdown)

no_nav_lines <- tags$style(HTML("
  .navbar-nav > li > a {
    border: none !important;
  }
  .navbar-nav {
    border-bottom: none !important;
  }
"))

  dashboard_ui <- fluidPage(
    
    # ---- TOP CONTROLS ----
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = "exit_cat",
          label   = "Exit Category",
          choices = NULL,
          selected = "largest"
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = "state_sel",
          label   = "State",
          choices = NULL
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        helpText(
          "Children may exit EI for different reasons, including eligibility determinations at or before age three, family decisions, relocation, or loss of contact. These exits reflect system-level conditions shaped by state policies and data reporting practices, and should not be interpreted as family deficits, lack of dedication, or the true needs of children."
        )
      )
    ),
    
    br(),
    
    # ---- MAP (full width) ----
    fluidRow(
      column(
        width = 12,
        plotlyOutput("map_plot", height = "600px")
      )
    ),
    
    br(),
    
    # ---- POLICY CONTEXT BELOW ----
    fluidRow(
      column(
        width = 12,
        bslib::card(
          bslib::card_header("Policy context"),
          bslib::card_body(
            uiOutput("policy_context_card")
          )
        )
      )
    )
    
  )
  
ui <- page_navbar(
  title = "Maiko Hata's EI Exit Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  header = no_nav_lines,
  navbar_options = navbar_options(
    bg = "#78C2AD",
    fg = "white"
  ),
  nav_panel(
    "Home",
    div(
      style = "max-width: 900px; padding-top: 1rem;",
      includeMarkdown("content/about.md")
    )
  ),
  nav_panel(
    "EI Exit Dashboard",
    dashboard_ui
  ),
  nav_panel(
    "Using the Dashboard",
    div(
      style = "max-width: 900px; padding-top: 1rem;",
      includeMarkdown("content/using.md")
    )
  ),
  nav_panel(
    "About Maiko",
    div(
      style = "max-width: 900px; padding-top: 1rem;",
      includeMarkdown("content/maiko.md")
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
      "Largest Disparity Category (All)" = "largest",
      "Dismissed (No Contact)"           = "dismissed",
      "Moved Out"                        = "moved_out",
      "Not Determined"                   = "not_determined",
      "Not Eligible"                     = "not_eligible",
      "Part B Eligible"                  = "part_b_eligible",
      "Withdrawn"                        = "withdrawn"
    ),
    selected = "largest"
  )
  
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
    
    plot(
      x = plot_df$or,
      y = factor(plot_df$race_ethnicity),
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
  
  # --- REAL MAP (Magma + gray suppressed) ---
  output$or_plot <- renderPlot({
    
    req(input$exit_cat, input$state_sel)
    
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
    
    plot(
      x = plot_df$or,
      y = factor(plot_df$race_ethnicity),
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
  
  # --- REAL MAP (Magma + gray suppressed) ---
  output$map_plot <- renderPlotly({
    
    plot_df <- map_data()
    
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
        colorscale = "Viridis",
        colorbar = list(title = "Log OR"),
        marker = list(line = list(color = "white", width = 0.5))
      ) %>%
      layout(
        geo = list(scope = "usa"),
        margin = list(l = 0, r = 0, t = 10, b = 0)
      )
  })}
  

shinyApp(ui = ui, server = server)
