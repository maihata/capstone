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
        "Children exit Early Intervention (EI) for different reasons, including eligibility determinations at or before age three, relocation, family decisions, or loss of contact. These exits reflect system-level conditions shaped by state policies and data reporting practices, and should not be interpreted as lack of dedication or the true needs of children. The heatmap visualizes how the likelihood of different exit categories varies across states, highlighting where disparities are more pronounced."
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
      fluidRow(
        column(width = 4, uiOutput("home_image")),
        column(width = 8, includeMarkdown("content/about.md"))
      )
    )
  ),
  
  nav_panel("EI Exit Dashboard", dashboard_ui),
  
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
  
  df         <- readRDS("data/analysis/state_avg_or_by_race_category_all_years.rds")
  map_df     <- readRDS("data/analysis/map_summary_logor_optionA.rds")
  welcome_df <- readRDS("data/analysis/welcome_map_logor_optionA.rds")
  elig_df    <- readRDS("data/analysis/eligibility_ABC_long.rds")
  funding_df <- readRDS("data/analysis/NIEER_funding_table8_clean.rds")
  
  # --- random home image (one per session) ---
  home_images <- list.files("www", pattern = "_circle\\.png$", full.names = FALSE)
  home_images <- sort(home_images)[1:min(5, length(home_images))]
  selected_home_image <- sample(home_images, 1)
  
  output$home_image <- renderUI({
    tags$img(
      src = selected_home_image,
      style = paste(
        "width:280px;",
        "height:280px;",
        "border-radius:50%;",
        "object-fit:cover;",
        "display:block;",
        "margin-left:auto;",
        "margin-right:auto;",
        "margin-top:25px;"
      )
    )
  })
  
  # --- inputs ---
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
  
  # --- map data ---
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
    
    p <- plotly::event_register(p, "plotly_click")
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
  
  # --- state snapshot with top-2 category disparities + top-2 race within each ---
  output$policy_context_card <- renderUI({
    req(input$state_sel, input$exit_cat)
    
    pretty_cat <- function(x) {
      dplyr::case_when(
        is.na(x) ~ "",
        x == "dismissed"       ~ "Dismissed (No Contact)",
        x == "moved_out"       ~ "Moved Out",
        x == "not_determined"  ~ "Not Determined",
        x == "not_eligible"    ~ "Not Eligible",
        x == "part_b_eligible" ~ "Part B Eligible",
        x == "withdrawn"       ~ "Withdrawn",
        TRUE ~ gsub("_", " ", x)
      )
    }
    fmt_or <- function(x) sprintf("%.2f", x)
    
    # eligibility + participation
    row_elig <- elig_df %>%
      dplyr::filter(State == input$state_sel) %>%
      dplyr::slice(1)
    
    elig_cat  <- if (nrow(row_elig) == 0) NA_character_ else row_elig$eligibility_category[[1]]
    part_rate <- if (nrow(row_elig) == 0) NA_real_      else row_elig$ei_participation_rate[[1]]
    
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
    
    # funding + insurance
    row_fund <- funding_df %>%
      dplyr::filter(State == input$state_sel) %>%
      dplyr::slice(1)
    
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
    
    # disparity pool: reportable rows only
    disp_pool <- df %>%
      dplyr::filter(
        state == input$state_sel,
        flag_zero_cell == FALSE,
        flag_small_cell_5 == FALSE,
        !is.na(or), is.finite(or), or > 0,
        !is.na(log_or), is.finite(log_or)
      ) %>%
      dplyr::mutate(strength = abs(log_or))
    
    top_categories <- if (input$exit_cat == "largest") {
      map_df %>%
        dplyr::filter(state == input$state_sel) %>%
        dplyr::arrange(dplyr::desc(abs(map_value))) %>%
        dplyr::slice_head(n = 2) %>%
        dplyr::pull(category)
    } else {
      input$exit_cat
    }
    

    
    disp_text <- ""
    
    if (length(top_categories) > 0 && !all(is.na(top_categories))) {
      
      blocks <- character(0)
      
      for (i in seq_along(top_categories)) {
        
        cat_i <- top_categories[[i]]
        if (is.na(cat_i)) next
        
        top_races <- disp_pool %>%
          dplyr::filter(category == cat_i) %>%
          dplyr::arrange(dplyr::desc(strength)) %>%
          dplyr::slice_head(n = 2)
        
        if (nrow(top_races) == 0) next
        
        bullets <- paste0("\u2022 ", top_races$race_ethnicity, ": OR ", fmt_or(top_races$or))
        
        header_line <- if (i == 1) {
          paste0(
            "In ", input$state_sel,
            ', the largest differences in odds are observed in the "',
            pretty_cat(cat_i), '" category:'
          )
        } else {
          paste0(
            'The second largest differences are observed in the "',
            pretty_cat(cat_i), '" category:'
          )
        }
        
        blocks <- c(blocks, paste(c(header_line, bullets), collapse = "\n"))
      }
      
      if (length(blocks) > 0) {
        disp_text <- paste0("\n\n", paste(blocks, collapse = "\n\n"))
      }
    }
    
    snapshot_text <- paste0(
      "In ", input$state_sel, ", ", elig_phrase,
      " based on state-defined developmental delay thresholds. ",
      part_phrase, ". ",
      fund_phrase, insurance_phrase, ".",
      disp_text,
      "\n\nThese descriptions reflect system-level structures and do not imply individual-level causation."
    )
    
    tags$p(style = "white-space: pre-line;", snapshot_text)
  })
}

shinyApp(ui = ui, server = server)
