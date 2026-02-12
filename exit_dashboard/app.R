library(shiny)
library(bslib)
library(plotly)
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
  
  fluidRow(
    column(
      width = 12,
      plotlyOutput("map_plot", height = "350px")
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("State Snapshot"),
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
  elig_df <- readRDS("../data/analysis/eligibility_ABC_long.rds")
  funding_df <- readRDS("../data/analysis/NIEER_funding_table8_clean.rds")
  
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
  
  output$map_plot <- renderPlotly({
    
    plot_df <- map_data()
    req(nrow(plot_df) > 0)
    
    bad_df  <- plot_df[plot_df$unreliable_state == TRUE, ]
    good_df <- plot_df[plot_df$unreliable_state == FALSE, ]
    
    p <- plot_ly(source = "map") %>%
      add_trace(
        data = bad_df,
        type = "choropleth",
        locationmode = "USA-states",
        locations = ~state_abb,
        key = ~state_abb,
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
        key = ~state_abb,
        z = ~map_value,
        text = ~hover_text,
        hoverinfo = "text",
        colorscale = "Viridis",
        colorbar = list(title = "Log OR"),
        marker = list(line = list(color = "white", width = 0.5))
      ) %>%
      layout(
        geo = list(scope = "usa"),
        margin = list(l = 0, r = 0, t = 10, b = 0),
        clickmode = "event+select"
      )
    
    plotly::event_register(p, "plotly_click")
    p
  })
  
  observeEvent(event_data("plotly_click", source = "map"), {
    click <- event_data("plotly_click", source = "map")
    req(click$key)
    
    clicked_abb <- click$key
    
    clicked_name <- tibble(state_abb = state.abb, state = state.name) %>%
      filter(state_abb == clicked_abb) %>%
      pull(state)
    
    if (length(clicked_name) == 0) return()
    
    updateSelectInput(session, "state_sel", selected = clicked_name[1])
  })
  
  output$policy_context_card <- renderUI({
    req(input$state_sel)
    
    row_elig <- elig_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    elig_cat  <- if (nrow(row_elig) == 0) NA_character_ else row_elig$eligibility_category[[1]]
    part_rate <- if (nrow(row_elig) == 0) NA_real_ else row_elig$ei_participation_rate[[1]]
    
    elig_phrase <- dplyr::case_when(
      is.na(elig_cat) ~ "Eligibility criteria information is not available",
      elig_cat == "A" ~ "eligibility criteria are categorized as more restrictive (A)",
      elig_cat == "B" ~ "eligibility criteria are categorized as near the national average (B)",
      elig_cat == "C" ~ "eligibility criteria are categorized as less restrictive (C)",
      TRUE ~ "eligibility category information is available but not recognized"
    )
    
    part_phrase <- if (is.na(part_rate)) {
      "EI participation data are not available"
    } else {
      paste0("EI participation is ", sprintf("%.2f", part_rate), "%")
    }
    
    row_fund <- funding_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    fund_val <- if (nrow(row_fund) == 0) NA_character_
    else row_fund$primary_funding_source_for_early_intervention[[1]]
    
    insurance_val <- if (nrow(row_fund) == 0) NA_character_
    else row_fund$state_bills_private_insurance_for_early_intervention[[1]]
    
    fund_phrase <- dplyr::case_when(
      is.na(fund_val) ~ "Primary funding source information is not available",
      fund_val == "Federal" ~ "Primary funding source is reported as federal",
      fund_val == "State" ~ "Primary funding source is reported as state",
      fund_val == "Not Reported" ~ "Primary funding source is not reported",
      TRUE ~ paste("Primary funding source is reported as", fund_val)
    )
    
    insurance_phrase <- dplyr::case_when(
      is.na(insurance_val) ~ "",
      insurance_val == "Yes" ~ ", and the state bills private insurance for EI services",
      insurance_val == "No" ~ ", and the state does not bill private insurance for EI services",
      insurance_val == "Not Reported" ~ ", and private insurance billing information is not reported",
      TRUE ~ ""
    )
    
    tags$p(
      paste0(
        "In ", input$state_sel, ", eligibility criteria are more restrictive (", elig_cat, 
        ") based on state-defined developmental delay thresholds. ",
        part_phrase, ". ",
        "Primary funding is ", tolower(fund_val), insurance_phrase, ". ",
        "These descriptions reflect system-level structures and do not imply individual-level causation."
      )
    )
    
  })
  
}

shinyApp(ui = ui, server = server)
