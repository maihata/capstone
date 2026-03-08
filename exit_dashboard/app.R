# ============================================================
# app.R — EI Exit Dashboard (Shiny + bslib + plotly)
# Notes for future you:
# - UI is defined first (what users see).
# - Server defines what gets calculated and how UI updates.
# - plotly map uses two layers: "good" (colored) + "bad" (gray).
# - Most things are driven by input$exit_cat and input$state_sel.
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(markdown)

# ============================================================
# CSS: small visual tweaks applied globally
# ------------------------------------------------------------
# no_nav_lines:
#   Removes default navbar borders/lines to better match Minty.
# invisible_card_css:
#   Provides an optional CSS class (.invisible-card) that can make
#   bslib cards look like plain text blocks (no border/background).
#   Also customizes Minty card headers (except invisible cards).
# ============================================================

no_nav_lines <- tags$style(HTML("
  .navbar-nav > li > a { border: none !important; }
  .navbar-nav { border-bottom: none !important; }
"))

invisible_card_css <- tags$style(HTML("
  .invisible-card {
    border: none !important;
    box-shadow: none !important;
    background-color: transparent !important;
  }
  .invisible-card .card-header {
    background-color: transparent !important;
    border-bottom: none !important;
    padding-left: 0 !important;
    padding-right: 0 !important;
  }
  .invisible-card .card-body {
    padding-left: 0 !important;
    padding-right: 0 !important;
    padding-top: 0.5rem !important;
  }

  /* Minty headers ONLY for regular bslib cards (not the invisible snapshot card) */
  .card:not(.invisible-card) > .card-header {
    background-color: #E6F4F1 !important;
    border-bottom: none !important;
  }
"))

# ============================================================
# Helpers (global)
# ------------------------------------------------------------
# pretty_cat():
#   Converts your internal category keys into human-readable labels.
#   This keeps labeling consistent everywhere (map hover, snapshot, text).
#
# fmt_or():
#   Formats odds ratios with 2 decimals for readability.
# ============================================================

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

# ============================================================
# Dashboard UI (reusable)
# ------------------------------------------------------------
# This is the content shown under the "Dashboard" tab.
#
# Top row:
#   - Exit category dropdown (input$exit_cat)
#   - State dropdown (input$state_sel)
#
# Next row:
#   - Short interpretation text for map reading
#
# Main row:
#   - Left: plotly map
#   - Right: State Snapshot card (compact indicators)
#
# Bottom:
#   - Detailed State Context card (long narrative)
#   - Equity Implications card (long narrative)
# ============================================================

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
        "Children exit Early Intervention (EI) for different reasons, including eligibility determinations, relocation, family decisions, or loss of contact. These exits reflect system-level conditions shaped by policy and reporting practices. Map colors show the log of the odds ratio (log OR) to make differences above and below 1 visually comparable. Hover over a state to see odds ratios (OR), where values above 1 indicate higher likelihood of exit and values below 1 indicate lower likelihood relative to the reference group."
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 8,
      plotlyOutput("map_plot", height = "500px")
    ),
    column(
      width = 4,
      bslib::card(
        bslib::card_header(uiOutput("snapshot_header")),
        bslib::card_body(uiOutput("compact_snapshot"))
      )
    )
  ),
  
  div(style = "height: 4px;"),
  
  fluidRow(
    style = "margin-top: -10px;",
    column(
      width = 12,
      bslib::card(
        bslib::card_header(tags$strong("Detailed State Context")),
        bslib::card_body(uiOutput("policy_context_card"))
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header(tags$strong("Equity Implications")),
        bslib::card_body(uiOutput("equity_strategy_card"))
      )
    )
  )
)


# ============================================================
# Main UI (page_navbar)
# ------------------------------------------------------------
# The overall structure of the app (tabs):
# - Home (landing page + image + about text + button)
# - Dashboard (the interactive map UI above)
# - Guide (markdown)
# - About (markdown)
#
# tags$head favicon:
#   Uses baby2.png in /www
# ============================================================

ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "baby2.png")
  ),
  
  page_navbar(
    id = "main_nav",
    title = "Maiko Hata's EI Exit Dashboard",
    theme = bs_theme(bootswatch = "minty"),
    header = tagList(no_nav_lines, invisible_card_css),
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
    
    nav_panel("Dashboard", dashboard_ui, value = "dashboard"),
    
    nav_panel(
      "Guide",
      div(
        style = "max-width: 900px; padding-top: 1rem;",
        includeMarkdown("content/using.md")
      )
    ),
    
    nav_panel(
      "About",
      div(
        style = "max-width: 900px; padding-top: 1rem;",
        includeMarkdown("content/maiko.md")
      )
    )
    
  )  # closes page_navbar
)    # closes tagList

# ============================================================
# Server
# ------------------------------------------------------------
# This is where:
# - data is loaded (readRDS)
# - dropdowns are populated
# - map layers are built
# - snapshot + narrative cards are generated
# - click on map updates dropdown
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Home page button: jump to Dashboard tab
  # ----------------------------------------------------------
  
  observeEvent(input$go_dashboard, {
    updateNavbarPage(session, "main_nav", selected = "dashboard")
  })
  
  # ----------------------------------------------------------
  # Snapshot header: just prints "<State> Snapshot"
  # Depends on input$state_sel
  # ----------------------------------------------------------
  
  output$snapshot_header <- renderUI({
    req(input$state_sel)
    tags$span(
      style = "font-weight:700; font-size:20px;",
      paste0(input$state_sel, " Snapshot")
    )
  })
  # ----------------------------------------------------------
  # Data loads (RDS)
  # ----------------------------------------------------------
  # df         = state-level OR summaries used mainly for dropdown state list
  # map_df     = map summary by category (was originally log OR based)
  # welcome_df = baseline map scaffold for "largest disparity" view
  # elig_df    = eligibility category (A/B/C) + EI participation rate
  # funding_df = NIEER funding table
  #
  # IMPORTANT:
  # map_df and welcome_df filenames include "logor"
  # which suggests their internal values may be in log space.
  # ----------------------------------------------------------
  
  df         <- readRDS("data/analysis/state_avg_or_by_race_category_all_years.rds")
  map_df     <- readRDS("data/analysis/map_summary_logor_optionA.rds")
  welcome_df <- readRDS("data/analysis/welcome_map_logor_optionA.rds")
  elig_df    <- readRDS("data/analysis/eligibility_ABC_long.rds")
  funding_df <- readRDS("data/analysis/NIEER_funding_table8_clean.rds")
  
  # -------------------------
  # Diagnostic: confirm map scale (manual toggle)
  # ------------------------------------------------------------
  # Goal: quickly verify whether map values look like log(OR) or OR.
  # How to use:
  #   - Change if (FALSE) to if (TRUE)
  #   - Run app once, read the Console output
  #   - Change back to FALSE
  # -------------------------
  if (FALSE) {
    cat("\n--- MAP DIAGNOSTICS ---\n")
    
    # Single-category maps (map_df)
    cat("\nmap_df$map_value summary (single-category maps):\n")
    if ("map_value" %in% names(map_df)) {
      print(summary(map_df$map_value))
      cat("Range:", paste(range(map_df$map_value, na.rm = TRUE), collapse = " to "), "\n")
      cat("Any negative values?:", any(map_df$map_value < 0, na.rm = TRUE), "\n")
      
      # Quick heuristic:
      # - If you see negatives, it's almost certainly log(OR) (because OR can't be negative).
      # - If values cluster around 0 (e.g., -1 to 1), that's typical log(OR).
      # - If values cluster around 1 (e.g., 0.3 to 3), that's typical OR.
    } else {
      cat("map_df has no map_value column\n")
    }
    
    # Largest-disparity source (disp_extremes)
    cat("\ndisp_extremes$disparity_spread summary (largest map source):\n")
    if ("disparity_spread" %in% names(disp_extremes)) {
      print(summary(disp_extremes$disparity_spread))
      cat("Range:", paste(range(disp_extremes$disparity_spread, na.rm = TRUE), collapse = " to "), "\n")
      cat("Any negative values?:", any(disp_extremes$disparity_spread < 0, na.rm = TRUE), "\n")
      
      # Quick heuristic:
      # - disparity_spread is often log_or_high - log_or_low (log scale), so it is usually >= 0.
      # - If you want an OR-ratio version, exp(disparity_spread) gives (OR_high / OR_low).
    } else {
      cat("disp_extremes has no disparity_spread column\n")
    }
    
    cat("--- END DIAGNOSTICS ---\n\n")
  }
  # ----------------------------------------------------------
  # Disparity extremes file (drives "largest disparity" logic)
  # ----------------------------------------------------------
  # disp_extremes contains:
  # - log_or_high / log_or_low: log-scale OR extremes within a state+category
  # - disparity_spread: typically log_or_high - log_or_low (log-scale spread)
  #
  # Then you compute:
  # - or_high/or_low = exp(log_or_high/log_or_low) to convert back to OR scale
  # ----------------------------------------------------------
  
  disp_path <- "data/analysis/state_category_disparity_spread_log_or_with_race_extremes.rds"
  stopifnot(file.exists(disp_path))
  
  disp_extremes <- readRDS(disp_path)
  disp_extremes$or_high <- exp(disp_extremes$log_or_high)
  disp_extremes$or_low  <- exp(disp_extremes$log_or_low)
 
   # Clean labels for Multiracial
  disp_extremes$race_high[disp_extremes$race_high == "MU_N"] <- "Multiracial"
  disp_extremes$race_low[disp_extremes$race_low == "MU_N"]  <- "Multiracial"
  
  # ----------------------------------------------------------
  # Compact Snapshot (right panel)
  # ----------------------------------------------------------
  # Goal:
  # Show 3 mini "icon blocks":
  # 1) EI participation rate (elig_df)
  # 2) Funding source + private insurance billing (funding_df)
  # 3) Largest disparity summary (disp_extremes)
  #
  # Note:
  # You build disparity_text differently depending on:
  # - "largest" (across categories) vs specific category
  #
  # HTML(disparity_text):
  # You use <br> line breaks; HTML() tells Shiny to interpret those as line breaks.
  # ----------------------------------------------------------
  output$compact_snapshot <- renderUI({
    req(input$state_sel, input$exit_cat)
    
    # Pull eligibility info for the selected state
    row_elig <- elig_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    # comparison settings (place near top of compact_snapshot)
    national_avg <- 4.20
    threshold <- 0.5  # +/- 0.5 percentage points considered "about the same"
    
    
    part_rate <- if (nrow(row_elig) == 0) NA_real_ else row_elig$ei_participation_rate[[1]]
    
    elig_cat <- if (nrow(row_elig) == 0) NA_character_ else row_elig$eligibility_category[[1]]
    
    elig_label <- dplyr::case_when(
      is.na(elig_cat) ~ "Not available",
      elig_cat == "A" ~ "More Expansive (Category A)",
      elig_cat == "B" ~ "Moderate (Category B)",
      elig_cat == "C" ~ "More Restrictive (Category C)",
      TRUE ~ paste("Category", elig_cat)
    )
    
    # Pull funding info for the selected state
    row_fund <- funding_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    fund_val <- if (nrow(row_fund) == 0) NA_character_ else row_fund$primary_funding_source_for_early_intervention[[1]]
    insurance_val <- if (nrow(row_fund) == 0) NA_character_ else row_fund$state_bills_private_insurance_for_early_intervention[[1]]
    
    # Human-friendly private insurance line
    insurance_line <- dplyr::case_when(
      is.na(insurance_val) ~ "Private insurance billing: Not available",
      insurance_val == "Yes" ~ "Private insurance billing: Yes",
      insurance_val == "No" ~ "Private insurance billing: No",
      insurance_val == "Not Reported" ~ "Private insurance billing: Not reported",
      TRUE ~ paste0("Private insurance billing: ", insurance_val)
    )
    
    # Identify the relevant category row(s) in disp_extremes
    top_cat <- if (input$exit_cat == "largest") {
      disp_extremes %>%
        filter(state == input$state_sel) %>%
        filter(!is.na(disparity_spread), is.finite(disparity_spread)) %>%
        arrange(desc(disparity_spread)) %>%
        slice(1)
    } else {
      disp_extremes %>%
        filter(state == input$state_sel, category == input$exit_cat) %>%
        slice(1)
    }
    
    # Build readable disparity text for the snapshot
    disparity_text <- if (nrow(top_cat) == 0) {
      "Not available"
    } else {
      
      race_hi <- top_cat$race_high[[1]]
      race_lo <- top_cat$race_low[[1]]
      or_hi   <- top_cat$or_high[[1]]
      or_lo   <- top_cat$or_low[[1]]
      
      if (input$exit_cat == "largest") {
        paste0(
          pretty_cat(top_cat$category[[1]]), "<br>",
          "Highest: ", race_hi, " (OR ", fmt_or(or_hi), "); ",
          "Lowest: ", race_lo, " (OR ", fmt_or(or_lo), ")"
        )
      } else {
        paste0(
          "OR ", fmt_or(or_hi), " (", race_hi, ")",
          " vs ", fmt_or(or_lo), " (", race_lo, ")"
        )
      }
    }
    
    # Render the 4 icon blocks
    tagList(
      # 1) Participation rate
      # --- Participation rate block (replace existing baby icon block) ---
      div(
        style = "display:flex; align-items:flex-start; gap:16px; margin-bottom:22px;",
        tags$img(
          src = {
            if (is.na(part_rate)) {
              "baby.svg"       # fallback if no data
            } else if (part_rate > national_avg + threshold) {
              "circle-arrow-up.svg"
            } else if (part_rate < national_avg - threshold) {
              "circle-arrow-down.svg"
            } else {
              "equal-approx.svg"
            }
          },
          style = "width:48px; height:48px; object-fit:contain; display:block; margin-top:2px;"
        ),
        div(
          div(
            style = "font-size:18px; font-weight:600; line-height:1.2;",
            if (is.na(part_rate)) "Not available" else paste0(sprintf("%.2f", part_rate), "%")
          ),
          div(style = "font-size:14px; color:#666; margin-top:2px;", "EI participation rate"),
          if (!is.na(part_rate)) {
            div(
              style = "font-size:12px; color:#666; margin-top:4px;",
              if (part_rate > national_avg + threshold) {
                "Above national average"
              } else if (part_rate < national_avg - threshold) {
                "Below national average"
              } else {
                "About the national average"
              }
            )
          } else NULL
        )
        ), 
      
      # 2) Eligibility strictness
      div(
        style = "display:flex; align-items:flex-start; gap:16px; margin-bottom:22px;",
        tags$img(
          src   = "door-open.svg",
          style = "width:48px; height:48px; object-fit:contain; display:block; margin-top:2px;"
        ),
        div(
          div(
            style = "font-size:18px; font-weight:600; line-height:1.2;",
            elig_label
          ),
          div(style = "font-size:14px; color:#666; margin-top:2px;", "Eligibility strictness")
        )
      ),
      
      # 3) Funding + insurance
      div(
        style = "display:flex; align-items:flex-start; gap:16px; margin-bottom:22px;",
        tags$img(
          src   = "circle-dollar-sign.svg",
          style = "width:48px; height:48px; object-fit:contain; display:block; margin-top:2px;"
        ),
        div(
          div(
            style = "font-size:18px; font-weight:600; line-height:1.2;",
            if (is.na(fund_val) || fund_val == "Not Reported") "Not available" else fund_val
          ),
          div(style = "font-size:14px; color:#666; margin-top:2px;", "Primary Funding Source"),
          div(style = "font-size:14px; color:#666; margin-top:2px;", insurance_line)
        )
      ),
      
      # 4) Disparity summary
      div(
        style = "display:flex; align-items:flex-start; gap:16px;",
        tags$img(
          src   = "scale_4.svg",
          style = "width:48px; height:48px; object-fit:contain; display:block; margin-top:2px;"
        ),
        div(
          div(
            style = "font-size:14px; font-weight:600; line-height:1.2;",
            HTML(disparity_text)
          ),
          div(
            style = "font-size:14px; color:#666; margin-top:2px;",
            if (input$exit_cat == "largest") {
              "Largest Disparity"
            } else {
              paste0("Disparity in ", pretty_cat(input$exit_cat), " category")
            }
          )
        )
      )
    )  # end tagList
  })   # end renderUI
  
  # ----------------------------------------------------------
  # Winners across categories for each state (largest view)
  # ----------------------------------------------------------
  # winners_spread = for each state, pick the category with max disparity_spread.
  # This is what drives the "Largest Disparity Category (All)" choice.
  # ----------------------------------------------------------
  winners_spread <- disp_extremes %>%
    filter(!is.na(disparity_spread), is.finite(disparity_spread)) %>%
    group_by(state) %>%
    slice_max(order_by = disparity_spread, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # ----------------------------------------------------------
  # welcome_df_fixed (largest view map dataset)
  # ----------------------------------------------------------
  # welcome_df is your base map scaffold (state names, abbreviations, flags).
  # You join in the winner category + map_value + hover_text.
  #
  # NOTE:
  # Currently map_value = disparity_spread (which is likely log-scale spread).
  # If you want the legend to truly be OR-based, you typically want:
  # map_value = exp(disparity_spread) = OR_high / OR_low
  #
  # Also: hover_text uses <br> for consistent line breaks in plotly.
  # ----------------------------------------------------------
  # ----------------------------------------------------------
  # welcome_df_fixed (largest view map dataset)  ✅ reverted to working join
  # ----------------------------------------------------------
  
  winners_join <- winners_spread %>%
    mutate(state_join = trimws(as.character(state))) %>%
    transmute(
      state_join,
      category,
      map_value = disparity_spread,
      high_race = race_high,
      low_race  = race_low,
      high_or   = or_high,
      low_or    = or_low,
      hover_text = paste0(
        "State: ", state, "<br>",
        "Largest disparity category: ", pretty_cat(category), "<br>",
        "Highest: ", race_high, " (OR ", sprintf("%.2f", or_high), ")<br>",
        "Lowest: ",  race_low,  " (OR ", sprintf("%.2f", or_low), ")"
      )
    )
  
  welcome_df_fixed <- welcome_df %>%
    mutate(state_join = trimws(as.character(state))) %>%
    # remove any old columns so the join is clean (safe even if they don't exist)
    select(-any_of(c("category", "map_value", "hover_text", "high_race", "low_race", "high_or", "low_or"))) %>%
    left_join(winners_join, by = "state_join") %>%
    mutate(
      unreliable_state = isTRUE(unreliable_state) | is.na(map_value) | !is.finite(map_value),
      hover_text = ifelse(
        is.na(hover_text),
        paste0(
          "State: ", state, "<br>",
          "Exit category: Not available<br>",
          "Data flag: ", ifelse(isTRUE(unreliable_state), "Caution", "OK")
        ),
        hover_text
      )
    ) %>%
    select(-state_join)
  # ----------------------------------------------------------
  # Random home image (picked once per session)
  # ----------------------------------------------------------
  # Looks in /www for files like *_circle.png
  # Excludes maiko_in_kimono_circle.png
  # Displays the chosen one on Home tab.
  # ----------------------------------------------------------
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
  
  # ----------------------------------------------------------
  # Populate dropdown inputs
  # ----------------------------------------------------------
  # state_sel choices come from df$state
  # exit_cat choices are hard-coded labels -> internal keys
  # ----------------------------------------------------------
  updateSelectInput(
    session,
    inputId = "state_sel",
    choices = sort(setdiff(unique(df$state), "US and Outlying Areas")),
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
  
  # ----------------------------------------------------------
  # Map data reactive
  # ----------------------------------------------------------
  # If "largest":
  #   use welcome_df_fixed (winner category per state)
  # else:
  #   use map_df filtered to that category
  #
  # NOTE:
  # If map_df is log OR, and you want OR, you would do:
  #   mutate(map_value = exp(map_value))
  # ----------------------------------------------------------
  map_data <- reactive({
    req(input$exit_cat)
    
    if (input$exit_cat == "largest") {
      
      return(welcome_df_fixed)
      
    } else {
      
      return(
        map_df %>%
          filter(category == input$exit_cat) %>%
          mutate(
            hover_text = paste0(
              "State: ", state, "<br>",
              "Exit category: ", pretty_cat(category), "<br>",
              "Highest: ", high_race, " (OR ", sprintf("%.2f", high_or), ")<br>",
              "Lowest: ",  low_race,  " (OR ", sprintf("%.2f", low_or), ")"
            )
          )
      )
      
    }
  })
  # ----------------------------------------------------------
  # Map output (plotly choropleth)
  # ----------------------------------------------------------
  # Strategy:
  # - Split into "bad" (unreliable -> gray) and "good" (reliable -> colored)
  # - Draw bad first (so it sits underneath)
  # - Draw good second (main visible layer)
  #
  # Using event_register("plotly_click") enables click events.
  # ----------------------------------------------------------
  # Fixed color scale
  color_range <- list(min = 0, max = 3.5)
  
  output$map_plot <- renderPlotly({
    plot_df <- map_data()
    req(nrow(plot_df) > 0)
    
    legend_title <- "Log OR"
    
    # Gray layer: unreliable states
    bad_df <- plot_df %>%
      filter(isTRUE(unreliable_state)) %>%
      mutate(map_value_plot = ifelse(is.na(map_value) | !is.finite(map_value), 0, map_value))
    
    # Colored layer: reliable states
    good_df <- plot_df %>%
      filter(!isTRUE(unreliable_state))
    
    # sel_df is prepared for optional future use (e.g., outline selected state)
    sel_df <- plot_df %>%
      filter(!is.na(state), state == input$state_sel)
    p <- plot_ly(source = "map")
    
    if (nrow(bad_df) > 0) {
      p <- p %>%
        add_trace(
          data = bad_df,
          type = "choropleth",
          locationmode = "USA-states",
          locations = ~state_abb,
          key = ~state_abb,
          z = ~map_value_plot,
          text = ~hover_text,
          hoverinfo = "text",
          colorscale = list(list(0, "gray80"), list(1, "gray80")),
          showscale = FALSE,
          marker = list(line = list(color = "white", width = 0.5)),
          zmin = color_range$min,
          zmax = color_range$max
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
          colorscale = "YlGnBu",
          colorbar = list(
          title = legend_title,
          x = -0.05,
          xanchor = "right"
          ), 
          marker = list(line = list(color = "white", width = 0.5)),
          zmin = color_range$min,
          zmax = color_range$max
        )
    }
    
    p <- p %>%
      layout(
        geo = list(scope = "usa"),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        clickmode = "event+select"
      )
    
    p <- plotly::event_register(p, "plotly_click")
    p
  })
  
  # ----------------------------------------------------------
  # Click interaction: click a state on the map -> update dropdown
  # ----------------------------------------------------------
  # click$key stores the state abbreviation because you set:
  #   key = ~state_abb
  #
  # lookup_df uses map_data() so it works for both:
  # - largest (welcome_df_fixed)
  # - single-category (map_df)
  # ----------------------------------------------------------
  
  observeEvent(event_data("plotly_click", source = "map"), {
    click <- event_data("plotly_click", source = "map")
    req(click$key)
    
    clicked_abb <- click$key
    
    # Use the data actually being plotted (works even if your abbreviations differ from base R lists)
    lookup_df <- map_data() %>%
      distinct(state, state_abb) %>%
      filter(!is.na(state), !is.na(state_abb))
    
    clicked_name <- lookup_df %>%
      filter(state_abb == clicked_abb) %>%
      pull(state)
    
    if (length(clicked_name) == 0) return()
    updateSelectInput(session, "state_sel", selected = clicked_name[1])
  })
  # ----------------------------------------------------------
  # Detailed State Context card
  # ----------------------------------------------------------
  # Produces a narrative description combining:
  # - eligibility category (A/B/C) and participation rate
  # - funding source + private insurance billing
  # - within-state disparities (top 1 or top 2 categories)
  # - a national context sentence (how common this category is as "largest")
  #
  # White-space is preserved using pre-line so \n\n becomes paragraph breaks.
  # ----------------------------------------------------------
  output$policy_context_card <- renderUI({
    req(input$state_sel, input$exit_cat)
    
    row_elig <- elig_df %>%
      filter(State == input$state_sel) %>%
      slice(1)
    
    elig_cat  <- if (nrow(row_elig) == 0) NA_character_ else row_elig$eligibility_category[[1]]
    part_rate <- if (nrow(row_elig) == 0) NA_real_      else row_elig$ei_participation_rate[[1]]
  
    elig_label <- dplyr::case_when(
      is.na(elig_cat) ~ "Not available",
      elig_cat == "A" ~ "More Expansive (Category A)",
      elig_cat == "B" ~ "Moderate (Category B)",
      elig_cat == "C" ~ "More Restrictive (Category C)",
      TRUE ~ paste("Category", elig_cat)
    )
    
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
        
        # Convert OR to percentage increase/decrease
        pct_hi <- round(100 * (or_hi - 1), 0)
        pct_lo <- round(100 * (or_lo - 1), 0)
        
        # Build the high group description
        high_desc <- if (or_hi > 1) {
          paste0(race_hi, " children are ", fmt_or(or_hi), " times, or approximately ", pct_hi, "% more likely,")
        } else {
          paste0(race_hi, " children are approximately ", abs(pct_hi), "% less likely,")
        }
        
        # Build the low group description
        low_desc <- if (or_lo > 1) {
          paste0("while ", race_lo, " children are ", fmt_or(or_lo), " times, or approximately ", pct_lo, "% more likely")
        } else {
          paste0("while ", race_lo, " children are ", fmt_or(or_lo), " times, or approximately ", abs(round(100 * (1 - or_lo), 0)), "% less likely")
        }
        
        # Add header line for each category
        if (input$exit_cat == "largest") {
          if (i == 1) {
            intro <- paste0('In ', input$state_sel, ', the largest between-group OR disparity across exit categories is observed in the "', pretty_cat(cat_i), '" category:')          } else {
            intro <- paste0('Another large between-group OR disparity is observed in the "', pretty_cat(cat_i), '" category:')
          }
        } else {
          intro <- ""
        }
        
        # Combine into one sentence
        disp_text_sentence <- paste0(high_desc, " than the state average to exit EI via ", pretty_cat(cat_i), ", ", low_desc, " to experience exit via ", pretty_cat(cat_i), ".")
        
        # Add intro + sentence
        if (intro != "") {
          blocks <- c(blocks, paste0(intro, " ", disp_text_sentence))
        } else {
          blocks <- c(blocks, disp_text_sentence)
        }
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
  # Equity Strategy card (unchanged)
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