# app.R

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(markdown)

no_nav_lines <- tags$style(HTML("
  .navbar-nav > li > a { border: none !important; }
  .navbar-nav { border-bottom: none !important; }
"))

# -------------------------
# Helpers (global)
# -------------------------
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

# -------------------------
# Dashboard UI (reusable)
# -------------------------
dashboard_ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId  = "exit_cat",
        label    = "Exit Category",
        choices  = NULL,
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
        "Children exit Early Intervention (EI) for different reasons, including eligibility determinations, relocation, family decisions, or loss of contact. These exits reflect system-level conditions shaped by policy and reporting practices and should not be interpreted as family deficits. The heatmap displays the log of the odds ratio (OR), which compares the relative likelihood of exit between groups within each category. Values above 1 indicate a higher likelihood and values below 1 indicate a lower likelihood relative to the reference group."
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(width = 12, plotlyOutput("map_plot", height = "350px"))
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("State Snapshot"),
        bslib::card_body(uiOutput("policy_context_card"))
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Equity Implications"),
        bslib::card_body(uiOutput("equity_strategy_card"))
      )
    )
  )
)

# -------------------------
# Main UI
# -------------------------
ui <- page_navbar(
  id = "main_nav",
  title = "Maiko Hata's EI Exit Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  header = no_nav_lines,
  navbar_options = navbar_options(bg = "#78C2AD", fg = "white"),
  
  nav_panel(
    "Home",
    div(
      style = "max-width: 900px; padding-top: 1rem;",
      fluidRow(
        column(width = 4, uiOutput("home_image")),
        column(
          width = 8,
          includeMarkdown("content/about.md"),
          div(
            style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
            actionButton(
              "go_dashboard",
              "Explore the EI Exit Dashboard",
              class = "btn-primary",
              style = "padding: 8px 20px; font-size: 14px;"
            )
          )
        )
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

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {
  
  observeEvent(input$go_dashboard, {
    updateNavbarPage(session, "main_nav", selected = "EI Exit Dashboard")
  })
  
  # -------------------------
  # Data loads
  # -------------------------
  df         <- readRDS("data/analysis/state_avg_or_by_race_category_all_years.rds")
  map_df     <- readRDS("data/analysis/map_summary_logor_optionA.rds")
  welcome_df <- readRDS("data/analysis/welcome_map_logor_optionA.rds")
  elig_df    <- readRDS("data/analysis/eligibility_ABC_long.rds")
  funding_df <- readRDS("data/analysis/NIEER_funding_table8_clean.rds")
  
  disp_path <- "data/analysis/state_category_disparity_spread_log_or_with_race_extremes.rds"
  stopifnot(file.exists(disp_path))
  
  disp_extremes <- readRDS(disp_path)
  disp_extremes$or_high <- exp(disp_extremes$log_or_high)
  disp_extremes$or_low  <- exp(disp_extremes$log_or_low)
  
  disp_extremes$race_high[disp_extremes$race_high == "MU_N"] <- "Multiracial"
  disp_extremes$race_low[disp_extremes$race_low == "MU_N"]  <- "Multiracial"
  
  # -------------------------
  # Build "largest" map layer using the SAME metric as State Snapshot (disparity_spread)
  # Robust join + NA protection to prevent crashes
  # -------------------------
  winners_spread <- disp_extremes %>%
    filter(!is.na(disparity_spread), is.finite(disparity_spread)) %>%
    group_by(state) %>%
    slice_max(order_by = disparity_spread, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # join on trimmed state names (prevents silent mismatches)
  welcome_df_fixed <- welcome_df %>%
    mutate(state_join = trimws(as.character(state))) %>%
    select(-any_of(c("category", "map_value", "hover_text"))) %>%
    left_join(
      winners_spread %>%
        mutate(state_join = trimws(as.character(state))) %>%
        transmute(
          state_join,
          category,
          map_value = disparity_spread,
          hover_text = paste0(
            "State: ", state_join, "\n",
            "Largest disparity category: ", pretty_cat(category), "\n",
            "Highest: ",
            race_high, " (OR ", sprintf("%.2f", exp(log_or_high)),
            ", ln ", sprintf("%.2f", log_or_high), ")\n",
            "Lowest: ",
            race_low, " (OR ", sprintf("%.2f", exp(log_or_low)),
            ", ln ", sprintf("%.2f", log_or_low), ")\n",
            "Largest within-category gap (ln OR): ",
            sprintf("%.2f", disparity_spread)
          )
        ),
      by = "state_join"
    ) %>%
    mutate(
      # prevent plotly from choking on all-NA z or missing hover text
      map_value = ifelse(is.na(map_value), 0, map_value),
      hover_text = ifelse(
        is.na(hover_text),
        paste0(
          "State: ", state_join, "\n",
          "Exit category: Not available\n\n",
          "Data flag: ", ifelse(isTRUE(unreliable_state), "Caution", "OK")
        ),
        hover_text
      )
    ) %>%
    select(-state_join)
  
  # -------------------------
  # Random home image (one per session)
  # -------------------------
  home_images <- list.files("www", pattern = "_circle\\.png$", full.names = FALSE)
  home_images <- home_images[home_images != "maiko_in_kimono_circle.png"]
  selected_home_image <- if (length(home_images) > 0) sample(home_images, 1) else NULL
  
  output$home_image <- renderUI({
    if (is.null(selected_home_image)) return(NULL)
    
    alt_text <- selected_home_image
    alt_text <- gsub("_circle\\.png$", "", alt_text)
    alt_text <- gsub("_", " ", alt_text)
    alt_text <- tools::toTitleCase(alt_text)
    
    tags$img(
      src = selected_home_image,
      alt = alt_text,
      style = paste(
        "width:240px;",
        "height:240px;",
        "border-radius:50%;",
        "object-fit:cover;",
        "display:block;",
        "margin-left:auto;",
        "margin-right:auto;",
        "margin-top:25px;",
        "margin-bottom:25px;"
      )
    )
  })
  
  # -------------------------
  # Inputs
  # -------------------------
  updateSelectInput(
    session,
    inputId = "state_sel",
    choices = sort(unique(df$state)),
    selected = "Oregon"
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
  
  # -------------------------
  # Map data (reactive)
  # -------------------------
  map_data <- reactive({
    req(input$exit_cat)
    if (input$exit_cat == "largest") {
      welcome_df_fixed
    } else {
      map_df %>% filter(category == input$exit_cat)
    }
  })
  
  # -------------------------
  # Map output (safe against empty layers + no input scoping errors)
  # -------------------------
  output$map_plot <- renderPlotly({
    plot_df <- map_data()
    req(nrow(plot_df) > 0)
    
    legend_title <- if (isTRUE(input$exit_cat == "largest")) "Spread (ln OR)" else "Log OR"
    
    bad_df  <- plot_df %>% filter(isTRUE(unreliable_state))
    good_df <- plot_df %>% filter(!isTRUE(unreliable_state))
    
    p <- plot_ly(source = "map")
    
    if (nrow(bad_df) > 0) {
      p <- p %>%
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
        )
    }
    
    if (nrow(good_df) > 0) {
      p <- p %>%
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
          colorbar = list(title = legend_title),
          marker = list(line = list(color = "white", width = 0.5))
        )
    }
    
    p <- p %>%
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
  
  # -------------------------
  # State snapshot
  # -------------------------
  output$policy_context_card <- renderUI({
    req(input$state_sel, input$exit_cat)
    
    row_elig <- elig_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    elig_cat  <- if (nrow(row_elig) == 0) NA_character_ else row_elig$eligibility_category[[1]]
    part_rate <- if (nrow(row_elig) == 0) NA_real_      else row_elig$ei_participation_rate[[1]]
    
    elig_phrase <- dplyr::case_when(
      is.na(elig_cat) ~ "Eligibility criteria information is not available",
      elig_cat == "A" ~ "eligibility criteria are categorized as more expansive (Category A)",
      elig_cat == "B" ~ "eligibility criteria are categorized as moderate (Category B)",
      elig_cat == "C" ~ "eligibility criteria are categorized as more restrictive (Category C)",
      TRUE ~ "Eligibility category information is available but not recognized"
    )
    
    elig_note <- "These categories are based on states’ reported eligibility criteria and reflect general differences in how expansive or restrictive developmental delay thresholds may be across states."
    
    national_avg <- 4.20
    
    part_phrase <- if (is.na(part_rate)) {
      "EI participation data are not available"
    } else {
      paste0(
        "EI participation is ",
        sprintf("%.2f", part_rate),
        "% (national average: ",
        sprintf("%.2f", national_avg),
        "% in 2023)"
      )
    }
    
    row_fund <- funding_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    fund_val <- if (nrow(row_fund) == 0) NA_character_ else row_fund$primary_funding_source_for_early_intervention[[1]]
    insurance_val <- if (nrow(row_fund) == 0) NA_character_ else row_fund$state_bills_private_insurance_for_early_intervention[[1]]
    
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
    
    top_categories <- if (input$exit_cat == "largest") {
      disp_extremes %>%
        filter(state == input$state_sel) %>%
        filter(!is.na(disparity_spread), is.finite(disparity_spread)) %>%
        arrange(desc(disparity_spread)) %>%
        slice_head(n = 2) %>%
        pull(category)
    } else {
      input$exit_cat
    }
    
    disp_text <- ""
    if (length(top_categories) > 0 && !all(is.na(top_categories))) {
      
      blocks <- character(0)
      
      for (i in seq_along(top_categories)) {
        
        cat_i <- top_categories[[i]]
        if (is.na(cat_i)) next
        
        row_cat <- disp_extremes %>%
          filter(state == input$state_sel, category == cat_i) %>%
          slice(1)
        
        if (nrow(row_cat) == 0) next
        
        race_hi <- row_cat$race_high[[1]]
        race_lo <- row_cat$race_low[[1]]
        or_hi   <- row_cat$or_high[[1]]
        or_lo   <- row_cat$or_low[[1]]
        
        header_line <- if (input$exit_cat == "largest") {
          if (i == 1) {
            paste0(
              'The largest between-group OR disparity across exit categories is observed in the "',
              pretty_cat(cat_i), '" category:'
            )
          } else {
            paste0(
              'Another large between-group OR disparity is observed in the "',
              pretty_cat(cat_i), '" category:'
            )
          }
        } else {
          paste0(
            'Within the "', pretty_cat(cat_i),
            '" category in ', input$state_sel,
            ", the largest between-group OR disparity is observed between:"
          )
        }
        
        bullets <- c(
          paste0("• ", race_hi, ": OR ", fmt_or(or_hi)),
          paste0("• ", race_lo, ": OR ", fmt_or(or_lo))
        )
        
        blocks <- c(blocks, paste(c(header_line, bullets), collapse = "\n"))
      }
      
      if (length(blocks) > 0) disp_text <- paste(blocks, collapse = "\n\n")
    }
    
    national_text <- ""
    if (input$exit_cat != "largest" && !is.na(input$exit_cat)) {
      
      winners <- disp_extremes %>%
        group_by(state) %>%
        slice_max(order_by = disparity_spread, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      n_states <- sum(winners$category == input$exit_cat, na.rm = TRUE)
      
      national_text <- paste0(
        "In ", n_states,
        ' states, "', pretty_cat(input$exit_cat),
        '" is the category showing the largest disparities across groups.'
      )
    }
    
    sections <- c(
      paste0(
        "In ", input$state_sel, ", ", elig_phrase, ". ",
        elig_note, " ",
        part_phrase, ". ",
        fund_phrase, insurance_phrase, "."
      ),
      disp_text,
      national_text
    )
    
    sections <- sections[nzchar(sections)]
    snapshot_text <- paste(sections, collapse = "\n\n")
    
    tags$div(
      style = "white-space: pre-line; line-height: 1.5; margin: 0;",
      snapshot_text
    )
  })
  
  # -------------------------
  # Equity Strategy card
  # -------------------------
  output$equity_strategy_card <- renderUI({
    req(input$state_sel, input$exit_cat)
    
    cat_for_strategy <- if (input$exit_cat == "largest") {
      top_cat <- disp_extremes %>%
        filter(state == input$state_sel) %>%
        filter(!is.na(disparity_spread), is.finite(disparity_spread)) %>%
        arrange(desc(disparity_spread)) %>%
        slice(1) %>%
        pull(category)
      
      ifelse(length(top_cat) == 0, NA_character_, top_cat)
    } else {
      input$exit_cat
    }
    
    text_bank <- list(
      overall = paste(
        "Observed disparities should be understood within broader social and policy contexts, not as characteristics of racial or language groups.",
        "Social Determinants of Health, including housing stability, health care access, and early screening opportunities, shape who enters and exits the EI pipeline.",
        "Differences at exit often reflect inequities earlier in the pipeline.",
        "Equitable data practices require attention to how demographic and language data are defined, collected, and interpreted."
      ),
      dismissed = paste(
        "Differences in dismissal due to lost contact may reflect how demographic and language data are defined and recorded.",
        "Social Determinants of Health, including housing stability, insurance access, and transportation, may influence continuity of contact.",
        "Clarifying dismissal due to lost contact procedures, making follow-up steps explicit, and reducing subjectivity can support more equitable decision-making.",
        "Culturally and linguistically responsive engagement strengthens trust and reduces unnecessary service interruption."
      ),
      not_eligible = paste(
        "Differences in Not Eligible determinations may reflect when children enter the EI pipeline and the level of concern at referral.",
        "Racially and linguistically marginalized children often access screening later, which may influence eligibility outcomes.",
        "Patterns in this category should be interpreted alongside referral pathways, outreach practices, and Social Determinants of Health.",
        "Strengthening early outreach may influence how families enter services and shape later transition outcomes."
      ),
      moved_out = paste(
        "Differences in Moved Out exits may reflect housing mobility, migration patterns, and other conditions affecting living environments.",
        "Mobility is shaped by intersecting Social Determinants of Health rather than characteristics of racial or language groups.",
        "Strong inter-agency communication and documentation practices support continuity of services when families relocate.",
        "More detailed and disaggregated data can improve understanding of mobility-related patterns."
      ),
      part_b_eligible = paste(
        "Differences in Part B eligibility patterns may reflect timing of entry into the EI pipeline.",
        "Variation in screening access and Social Determinants of Health influence transition positioning at age three.",
        "Decision points across the EI pipeline are interconnected and shape later eligibility outcomes."
      ),
      not_determined = paste(
        "Differences in Not Determined exits may reflect documentation completeness, follow-up procedures, or communication practices.",
        "Strengthening clarity and consistency in evaluation workflows can reduce uneven outcomes.",
        "As with dismissal due to lost contact, interruption before transition may reflect structural barriers rather than developmental need.",
        "Reviewing timelines and family engagement supports can improve equity and interpretability."
      ),
      withdrawn = paste(
        "Differences in Withdrawn exits may reflect family circumstances, access barriers, or changing priorities.",
        "Social Determinants of Health and service accessibility influence continuity of participation.",
        "Examining outreach, communication clarity, and cultural responsiveness can help interpret patterns in this category.",
        "Data should be interpreted within broader structural contexts rather than as individual family characteristics."
      )
    )
    
    body <- if (input$exit_cat == "largest") text_bank$overall else text_bank[[cat_for_strategy]]
    if (is.null(body)) body <- "Equity strategies will be expanded in a future version."
    
    tags$div(
      style = "white-space: normal; line-height: 1.6; margin: 0;",
      body
    )
  })
}

shinyApp(ui = ui, server = server)