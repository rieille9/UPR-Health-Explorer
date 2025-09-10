# UPR Health Explorer - Shiny App Conversion
# Converted from Quarto-Shiny to a standalone Shiny app using shinydashboard

# 1. SETUP: LOAD LIBRARIES AND DATA =========================================
# This section runs once when the app starts.

# Ensure pacman is installed to manage packages
if (!require("pacman")) install.packages("pacman")

# Load necessary packages
pacman::p_load(
  shiny,
  shinydashboard,
  here,
  dplyr, forcats, ggplot2, magrittr, readr, readxl, stringr, tibble, tidyr, lubridate,
  janitor,
  DT,
  sf,
  necountries,
  patchwork,
  pdftools
)

# Load custom functions and external data processing scripts
# Make sure these files exist at the specified paths relative to app.R
# source(here("utils.R"))
# source(here("code", "external_data_GBD.R"))

# A helper function from utils.R for the app to run standalone
relabel_na <- function(x) {
  # Replace NA with "No data" in a factor
  x <- as.character(x)
  x[is.na(x)] <- "No data"
  factor(x)
}


# Read in pre-processed datasets
# Make sure these files exist in the specified subdirectories
sdg_data <- readRDS(here("data", "SDG_data_enhanced.rds")) |> droplevels()
state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))
nearest_neighbors_list <- readRDS(here("output", "nearest_neighbors_list.rds"))
theme_labels <- source(here("code", "theme_labels.R"))$value

# Loop through API-generated files to read each one and assign it to an object
for (file_name in list.files(path = here("data", "API_data"),
                             pattern = "\\.rds$",
                             full.names = FALSE)) {
  object_name <- gsub("\\.rds$", "", file_name)
  assign(object_name, readRDS(here("data", "API_data", file_name)))
}


# 2. UI: USER INTERFACE DEFINITION ============================================
# This section defines the layout of the app in the user's web browser.

ui <- dashboardPage(
  skin = "blue",
  
  ## Header ------------------------------------------------------------------
  dashboardHeader(
    title = span(img(src ="logo_5.png", height = "40px", style = "margin-right:10px;"), "UPR Health Explorer"),
    titleWidth = 400
  ),
  
  ## Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("UPR: Regional", tabName = "upr_regional", icon = icon("globe-americas")),
      menuItem("UPR: State Under Review", tabName = "upr_sur", icon = icon("flag")),
      menuItem("Maternal Mortality (MMR)", tabName = "mmr", icon = icon("female")),
      menuItem("Family Planning", tabName = "family_planning", icon = icon("users")),
      
      hr(),
      
      # Input controls, moved from the original Quarto sidebar
      selectInput("selected_regional_grouping", "Select Regional Grouping:",
                  choices = c("Sub-regions", "World Bank regions", "WHO regions", "ECSA-HC Membership", "FCS status"),
                  selected = "Sub-regions"),
      
      selectInput("selected_region", "Select Region:",
                  choices = c("Global"),
                  selected = "Global"),
      
      selectInput("selected_SUR", "Select State Under Review:",
                  choices = sort(unique(state_geo$country)),
                  multiple = FALSE),
      
      # Placeholder for global map
      div(id = "map-box-container",
      box(title = "Selected Region on Global Map", status = "primary", solidHeader = TRUE, width = "400px",
          plotOutput("global_map", height = "160px"))
      ),
      
      # Disclaimer callout from the Quarto file
      box(
        title = "Disclaimer", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = "400px",
        p("This dashboard displays the results of a preliminary analysis regarding recommendations from the first four cycles of the Universal Periodic Review (UPR). Results are subject to change as the classification methodology continues to be refined."),
        p(HTML("Map disclaimer: CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown on these maps. The information is for reference only.")),
        p("For more details on data sources, please see the 'About' page.")
      )
    )
  ),
  
  ## Body --------------------------------------------------------------------
  dashboardBody(
    # # Custom CSS to match Quarto's highlight-block style
    # tags$head(tags$style(HTML("
    #     .highlight-block {
    #         background-color: #f8f9fa;
    #         border-left: 5px solid #007bff;
    #         padding: 15px;
    #         margin-bottom: 20px;
    #     }
    # "))),
    
    # Link to your external stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      # -- About Tab
      tabItem(tabName = "about",
              fluidRow(
                column(width = 12,
                       div(class = "highlight-block",
                           h3("The Right to Health"),
                           p("The Right to Health is central to the fulfillment of broader human rights obligations, serving as a powerful tool to advance well-being, equity, and dignity across all sectors of society."),
                           p("The Right to Health comprises the State's obligations to:"),
                           tags$ul(
                             tags$li(strong("Respect:"), "refrain from interfering directly or indirectly with the enjoyment of the right to health."),
                             tags$li(strong("Protect:"), "take measures that prevent third parties from interfering with the guarantees of the right to health."),
                             tags$li(strong("Fulfill:"), "adopt appropriate legislative, administrative, budgetary, judicial, promotional, and other measures toward the full realization of the right to health.")
                           )
                       ),
                       div(class = "highlight-block",
                           h3("The Universal Periodic Review"),
                           p("The UPR is a State-led, periodic peer review mechanism to evaluate each State’s “human rights obligations and commitments”."),
                           p("Each cycle repeats every 4-5 years - see below illustration for an overview of each cycle."),
                           img(src = "UPR_review_banner2.png", width = "30%"),
                           p("The Reviews are guided by three main pre-session reports (see below):"),
                           tags$ul(
                             tags$li("Reviewing States issue recommendations"),
                             tags$li("The State Under Review can either “Support” (accept) or “Note” each recommendation")
                           ),
                           img(src = "UPR_pre_review.png", width = "60%"),
                           p("More than 90,000 recommendations were issued during the first three cycles of the UPR. There is a growing focus on the right to health.")
                       ),
                       div(class = "highlight-block",
                           h3("CeHDI"),
                           p(HTML('CeHDI has a mission of amplifying and facilitating the inclusion of the priorities and voices of the Global South within the global health architecture and building robust partnerships for global health equity and the right to health.'))
                       ),
                       div(class = "highlight-block",
                           h3("Preliminary results?"),
                           img(src = "full_plot.png", width = "50%")
                       )
                )
              )
      ),
      
      # -- UPR Regional Tab
      tabItem(tabName = "upr_regional",
              h2("UPR Recommendations: Regional View"),
              fluidRow(
                column(width = 8,
                       tabBox(
                         id = "regional_tabs", title = "Regional Recommendation Themes", width = 12,
                         tabPanel("All Recommendations", plotOutput("upr_themes_all_global", height = "700px")),
                         tabPanel("Per UPR Cycle", plotOutput("upr_themes_cycle_global", height = "700px"))
                       )
                ),
                column(width = 4,
                       box(title = "Health-Related Recommendations", status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("global_plot", height = "350px"))
                )
              )
      ),
      
      # -- UPR State Under Review (SUR) Tab
      tabItem(tabName = "upr_sur",
              h2("UPR Recommendations: State Under Review"),
              fluidRow(
                column(width = 8,
                       tabBox(
                         id = "sur_tabs", title = "SUR Recommendation Details", width = 12,
                         tabPanel("All Recommendations", plotOutput("upr_themes_all", height = "700px")),
                         tabPanel("Per UPR Cycle", plotOutput("upr_themes_cycle", height = "700px")),
                         tabPanel("Data Table", DTOutput("DT_table"))
                       )
                ),
                column(width = 4,
                       box(title = "Health-Related Recommendations", status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("plot", height = "700px"))
                )
              )
      ),
      
      # -- MMR Tab
      tabItem(tabName = "mmr",
              h2("Maternal Mortality Ratio (MMR)"),
              fluidRow(
                column(width = 5,
                       box(title = "MMR Estimate Map (2023)", status = "warning", solidHeader = TRUE, width = 12,
                           plotOutput("mmr_map")),
                       box(title = "MMR Trends vs. Neighbors", status = "warning", solidHeader = TRUE, width = 12,
                           plotOutput("mmr_time_plot_neighbors"))
                ),
                column(width = 7,
                       tabBox(
                         id = "mmr_causes_tabs", title = "Causes of Maternal Death", width = 12,
                         tabPanel("Causes (2021)", plotOutput("mmr_causes", height = "600px")),
                         tabPanel("Causes Over Time", plotOutput("mmr_causes_longitudinal", height = "600px"))
                       )
                )
              )
      ),
      
      # -- Family Planning Tab
      tabItem(tabName = "family_planning",
              h2("Family Planning and Abortion"),
              fluidRow(
                column(width = 6,
                       box(title = "Abortion Laws (June 2023)", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("abortion_map_sur")),
                       box(title = "Met Need for Family Planning (%)", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("family_planning"))
                ),
                column(width = 6,
                       box(title = "Estimated Abortion Rate (2015-2019)", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("abortion_rate")),
                       box(title = "Estimated Unintended Pregnancy Rate (2015-2019)", status = "info", solidHeader = TRUE, width = 12,
                           plotOutput("unintended_pregnancy"))
                )
              )
      )
    )
  )
)


# 3. SERVER: REACTIVE LOGIC ===================================================
# This section contains the instructions that R follows to build the objects
# displayed in the UI.

server <- function(input, output, session) {
  
  ## Reactive Expressions for Data Filtering ---------------------------------
  state_geo_reactive <- reactive({
    if (input$selected_regional_grouping == "Sub-regions") {
      state_geo |> mutate(region_dashboard = subregion)
    } else if (input$selected_regional_grouping == "World Bank regions") {
      state_geo |> mutate(region_dashboard = wbregion)
    } else if (input$selected_regional_grouping == "ECSA-HC Membership") {
      state_geo |> mutate(region_dashboard = ECSA_status)
    } else if (input$selected_regional_grouping == "FCS status") {
      state_geo |> mutate(region_dashboard = FCS_status)
    } else {
      state_geo |> mutate(region_dashboard = WHO_region)
    }
  })
  
  sdg_data_dashboard <- reactive({
    sdg_data |>
      left_join(state_geo_reactive() |>
                  st_drop_geometry() |>
                  select(country, region_dashboard),
                by = c("state_under_review" = "country"))
  })
  
  # Filter for the selected State Under Review (SUR)
  filtered_upr <- reactive({
    req(input$selected_SUR)
    sdg_data_dashboard() |>
      filter(state_under_review == input$selected_SUR)
  })
  
  # Filter for the selected Region
  filtered_upr_region <- reactive({
    if (input$selected_region == "Global") {
      sdg_data_dashboard()
    } else {
      sdg_data_dashboard() |>
        filter(region_dashboard == input$selected_region)
    }
  })
  
  region_selection <- reactive({
    if (input$selected_region == "Global") {
      state_geo_reactive()
    } else {
      state_geo_reactive() |>
        filter(region_dashboard == input$selected_region)
    }
  })
  
  ## Observers for Dynamic UI Updates --------------------------------------
  observeEvent(input$selected_regional_grouping, {
    updateSelectInput(
      session, "selected_region",
      choices = c("Global", levels(state_geo_reactive()$region_dashboard)),
      selected = "Global"
    )
  })
  
  observeEvent(input$selected_region, {
    choices <- sort(unique(region_selection()$country))
    updateSelectInput(
      session, "selected_SUR",
      choices = choices,
      selected = choices[1]
    )
  })
  
  ## Helper Reactives for Plotting -----------------------------------------
  SUR_region <- reactive({
    req(input$selected_SUR)
    state_geo_reactive()[state_geo_reactive()$country == input$selected_SUR, ]$region_dashboard
  })
  
  sur_area <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |>
      filter(country %in% c(input$selected_SUR)) |>
      st_area() |> as.numeric()
  })
  
  bbox_selected_SUR <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |>
      filter(country %in% c(input$selected_SUR)) |>
      st_bbox()
  })
  
  bbox_SUR_region <- reactive({
    req(SUR_region())
    state_geo_reactive() |>
      filter(region_dashboard %in% c(SUR_region())) |>
      st_bbox()
  })
  
  
  ## MAPS (from original sidebar) ------------------------------------------
  output$global_map <- renderPlot({
    p1 <- state_geo_reactive() |>
      mutate(selected_sur = factor(case_when(country == input$selected_SUR ~ input$selected_SUR,
                                             .default = "Other"),
                                   levels = c(input$selected_SUR, "Other"))) |>
      ggplot(aes(geometry = polygon, color = selected_sur, fill = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_color_manual(values = c("green4", "grey80")) +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_fill_manual(values = c("green4", "grey90")) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
      ) +
      labs(
        title = NULL,
        fill = NULL,
        color = NULL, lwd = NULL
      ) +
      guides(
        fill = "none", lwd = "none", color = "none"
      )
    
    if (sur_area() > 10^11) {
      p2 <- p1
    } else {
      p2 <- p1 + geom_rect(
        aes(
          xmin = bbox_selected_SUR()["xmin"] - 1,
          xmax = bbox_selected_SUR()["xmax"] + 1,
          ymin = bbox_selected_SUR()["ymin"] - 1,
          ymax = bbox_selected_SUR()["ymax"] + 1
        ),
        fill = "transparent",      # Make the rectangle hollow
        color = "green4",          # Set the border color
        linewidth = 0.5            # Set the border thickness
      )
    }
    
    p2+ theme(plot.margin = margin(t = 1, r = 1, b = -10, l = 1, unit = "pt"))
  })
  
  # Note: The 'regional_map' output from the Quarto file was not used in the UI, so it's omitted here.
  
  ## UPR: REGIONAL Outputs ----------------------------------------------------
  output$global_plot <- renderPlot({
    upr_rec_global <- filtered_upr_region() |>
      droplevels() |>
      group_by(cycle, state_under_review) |>
      count(health_related, .drop = FALSE) |>
      group_by(cycle, health_related) |> mutate(med_n = median(n)) |>
      select(cycle, health_related, med_n) |> distinct() |>
      group_by(cycle) |>
      mutate(
        med_n_tot = sum(med_n),
        perc = (med_n / med_n_tot) * 100,
        perc = case_when(
          health_related == "Other" ~ "",
          .default = paste0(sprintf("%1.0f", perc), "%")
        )
      )
    
    upr_rec_global |>
      ggplot(aes(x = cycle, y = med_n, fill = health_related)) +
      scale_fill_manual(values = c("Health-related" = "#E69F00", "Other" = "grey80")) +
      geom_bar(stat = "identity") +
      labs(
        y = "Median number of recommendations", x = "UPR Cycle",
        title = paste0("Median recommendations received by States*\n", input$selected_region),
        fill = NULL,
        caption = "*Cycle 4 is currently underway"
      ) +
      geom_text(aes(label = perc), position = position_stack(vjust = 0.5), size = 5) +
      geom_text(aes(label = sprintf("%1.0f", med_n_tot), y = med_n_tot, vjust = -0.2), size = 5, fontface = "bold") +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        plot.caption = element_text(size = 14),
        legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 18),
        legend.background = element_blank()
      )
  })
  
  output$upr_themes_cycle_global <- renderPlot({
    req(nrow(filtered_upr_region()) > 0)
    a_1 <- filtered_upr_region() |>
      select(cycle, health_related:maternal_health, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr_region() |>
      select(cycle, health_related:maternal_health, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n_other"
      )
    
    a_3 <- filtered_upr_region() |>
      group_by(cycle) |>
      summarise(health_n = sum(health_related != "Other")) |>
      ungroup()
    
    a <- left_join(a_1, a_2) |>
      left_join(a_3) |>
      mutate(cycle = fct_recode(cycle, "1" = "Cycle 1", "2" = "Cycle 2", "3" = "Cycle 3", "4" = "Cycle 4")) |>
      group_by(cycle, theme) |>
      mutate(
        n_tot = sum(n) + sum(n_other),
        n_tot_theme = sum(n)
      ) |>
      mutate(
        perc = n / n_tot * 100,
        perc_theme = n_tot_theme / n_tot * 100,
        theme_perc_health = n_tot_theme / health_n * 100
      ) |>
      group_by(cycle, theme) |>
      mutate(
        n_sup = paste0("(", sprintf("%1.0f", n / sum(n) * 100), "%)"),
        n_sup = case_when(n_tot_theme == 0 ~ "(NA)", .default = n_sup)
      ) |>
      ungroup() |>
      filter(!theme %in% c("health_related", "TB_malaria", "NTD")) |>
      left_join(theme_labels, by = c("theme" = "variable")) |>
      arrange(fct_rev(cycle), -n_tot_theme) |>
      mutate(
        theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label),
        theme_label = fct_inorder(theme_label)
      )
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    theme_plot <- a |>
      ggplot(aes(x = perc, y = fct_rev(cycle))) +
      geom_col(aes(fill = response_upr)) +
      facet_grid(
        rows = vars(theme_label), switch = "y",
        labeller = labeller(theme_label = label_wrap_gen(50))
      ) +
      labs(
        x = "Proportion of all recommendations per UPR cycle (%)", y = NULL,
        fill = "State's response",
        title = paste0("Health-related recommendations in each cycle of the UPR\n", input$selected_region),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 14),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 1, size = 11),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 3, vjust = 0.25
      )
    theme_plot
  })
  
  output$upr_themes_all_global <- renderPlot({
    req(nrow(filtered_upr_region()) > 0)
    a_1 <- filtered_upr_region() |>
      select(cycle, health_related:maternal_health, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr_region() |>
      select(cycle, health_related:maternal_health, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n_other"
      )
    
    a <- left_join(a_1, a_2) |>
      group_by(theme) |>
      mutate(
        n_tot = sum(n) + sum(n_other),
        n_tot_theme = sum(n)
      ) |>
      mutate(
        perc = n / n_tot * 100,
        perc_theme = n_tot_theme / n_tot * 100
      ) |>
      group_by(theme) |>
      mutate(
        n_sup = paste0("(", sprintf("%1.0f", n / sum(n) * 100), "%)"),
        n_sup = case_when(n_tot_theme == 0 ~ "(NA)", .default = n_sup)
      ) |>
      ungroup() |>
      filter(!theme %in% c("health_related", "TB_malaria", "NTD")) |>
      left_join(theme_labels, by = c("theme" = "variable")) |>
      arrange(-n_tot_theme) |>
      mutate(
        theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label),
        theme_label = fct_inorder(theme_label)
      )
    
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    a |>
      ggplot(aes(x = perc, y = fct_rev(theme_label))) +
      geom_col(aes(fill = response_upr)) +
      labs(
        x = "Proportion of all recommendations (%)", y = NULL,
        fill = "State's response",
        title = paste0("Health-related recommendations of the UPR, up to the fourth cycle\n", input$selected_region),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 14),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 4, vjust = 0.25
      )
  })
  
  ## UPR: SUR Outputs --------------------------------------------------------
  output$plot <- renderPlot({
    req(nrow(filtered_upr()) > 0)
    upr_rec_countries <- filtered_upr() |>
      droplevels() |>
      group_by(cycle, state_under_review) |>
      count(health_related, .drop = FALSE) |>
      group_by(cycle, state_under_review, health_related) |> mutate(med_n = median(n)) |>
      select(cycle, state_under_review, health_related, med_n) |> distinct() |>
      group_by(cycle, state_under_review) |>
      mutate(
        n_tot = sum(med_n),
        perc = (med_n / n_tot) * 100,
        perc = case_when(
          health_related == "Other" ~ "",
          .default = paste0(sprintf("%1.0f", perc), "%")
        )
      )
    
    upr_rec_countries |>
      ggplot(aes(x = cycle, y = med_n, fill = health_related)) +
      scale_fill_manual(values = c("Health-related" = "#E69F00", "Other" = "grey80")) +
      geom_bar(stat = "identity") +
      labs(
        y = "Number of recommendations", x = "UPR Cycle",
        title = "Number of recommendations received by States",
        fill = NULL
      ) +
      geom_text(aes(label = perc), position = position_stack(vjust = 0.5), size = 5) +
      geom_text(aes(label = sprintf("%1.0f", n_tot), y = n_tot, vjust = -0.2), size = 5, fontface = "bold") +
      theme_bw() +
      facet_wrap(. ~ state_under_review, nrow = 2) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 18),
        legend.background = element_blank()
      )
  })
  
  output$upr_themes_cycle <- renderPlot({
    req(nrow(filtered_upr()) > 0)
    a_1 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:maternal_health, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:maternal_health, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n_other"
      )
    
    a_3 <- filtered_upr() |>
      group_by(cycle) |>
      summarise(health_n = sum(health_related != "Other")) |>
      ungroup()
    
    a <- left_join(a_1, a_2) |>
      left_join(a_3) |>
      mutate(cycle = fct_recode(cycle, "1" = "Cycle 1", "2" = "Cycle 2", "3" = "Cycle 3", "4" = "Cycle 4")) |>
      group_by(cycle, theme) |>
      mutate(
        n_tot = sum(n) + sum(n_other),
        n_tot_theme = sum(n)
      ) |>
      mutate(
        perc = n / n_tot * 100,
        perc_theme = n_tot_theme / n_tot * 100,
        theme_perc_health = n_tot_theme / health_n * 100
      ) |>
      group_by(cycle, theme) |>
      mutate(
        n_sup = paste0("(", sprintf("%1.0f", n / sum(n) * 100), "%)"),
        n_sup = case_when(n_tot_theme == 0 ~ "(NA)", .default = n_sup)
      ) |>
      ungroup() |>
      filter(!theme %in% c("health_related", "TB_malaria", "NTD")) |>
      left_join(theme_labels, by = c("theme" = "variable")) |>
      arrange(fct_rev(cycle), -n_tot_theme) |>
      mutate(
        theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label),
        theme_label = fct_inorder(theme_label)
      )
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    theme_plot <- a |>
      ggplot(aes(x = perc, y = fct_rev(cycle))) +
      geom_col(aes(fill = response_upr)) +
      facet_grid(
        rows = vars(theme_label), switch = "y",
        labeller = labeller(theme_label = label_wrap_gen(50))
      ) +
      labs(
        x = "Proportion of all recommendations per UPR cycle (%)", y = NULL,
        fill = "State's response",
        title = paste("Health-related recommendations in each cycle of the UPR:", input$selected_SUR),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.title.position = "plot",
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 1, size = 11),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 3.5, vjust = 0.25
      )
    theme_plot
  })
  
  output$upr_themes_all <- renderPlot({
    req(nrow(filtered_upr()) > 0)
    a_1 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:maternal_health, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:maternal_health, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:maternal_health), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted/Other")) |>
      pivot_longer(
        cols = health_related:maternal_health,
        names_to = "theme",
        values_to = "n_other"
      )
    
    a <- left_join(a_1, a_2) |>
      group_by(theme) |>
      mutate(
        n_tot = sum(n) + sum(n_other),
        n_tot_theme = sum(n)
      ) |>
      mutate(
        perc = n / n_tot * 100,
        perc_theme = n_tot_theme / n_tot * 100
      ) |>
      group_by(theme) |>
      mutate(
        n_sup = paste0("(", sprintf("%1.0f", n / sum(n) * 100), "%)"),
        n_sup = case_when(n_tot_theme == 0 ~ "(NA)", .default = n_sup)
      ) |>
      ungroup() |>
      filter(!theme %in% c("health_related", "TB_malaria", "NTD")) |>
      left_join(theme_labels, by = c("theme" = "variable")) |>
      arrange(-n_tot_theme) |>
      mutate(
        theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label),
        theme_label = fct_inorder(theme_label)
      )
    
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    a |>
      ggplot(aes(x = perc, y = fct_rev(theme_label))) +
      geom_col(aes(fill = response_upr)) +
      labs(
        x = "Proportion of all recommendations per UPR cycle (%)", y = NULL,
        fill = "State's response",
        title = paste("Health-related recommendations in each cycle of the UPR:", input$selected_SUR),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 4, vjust = 0.25
      )
  })
  
  output$DT_table <- renderDT({
    req(nrow(filtered_upr()) > 0)
    filtered_upr() |>
      mutate(state_under_review = factor(state_under_review)) |>
      select(
        text, cycle, response_upr, health_related:maternal_health,
        state_under_review, document_code, paragraph
      ) |>
      DT::datatable(
        filter = "top",
        options = list(
          pageLength = 100,
          deferRender = TRUE,
          scrollY = 800,
          scrollX = TRUE,
          scroller = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = '500px', targets = c(0)),
            list(width = '200px', targets = c(1))
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
  })
  
  ## MMR Outputs -------------------------------------------------------------
  output$mmr_map <- renderPlot({
    mmr_estimate_2023 = MMR |>
      filter(country_name == input$selected_SUR, YEAR == "2023") |>
      pull(NumericValue) |>
      round(0)
    p1 <- MMR |>
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(TimeDimensionValue == 2023, !is.na(country_name)) |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      filter(!is.na(selected_sur)) |>
      ggplot(aes(geometry = polygon, fill = mmr_cat, color = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_brewer(palette = "YlOrRd", na.value = "grey80", labels = relabel_na) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank()
      ) +
      labs(
        title = paste0("Maternal mortality ratio (MMR) estimate in 2023\n", input$selected_SUR, ": ", mmr_estimate_2023, " per 100,000 live births"),
        fill = NULL,
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none") +
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    
    p1 # Simplified for now, patchwork can be added back if needed
  })
  
  output$mmr_time_plot_neighbors <- renderPlot({
    start_year <- "2005"
    dat_plot <- MMR |>
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(country_name %in% c(
        input$selected_SUR,
        nearest_neighbors_list[, state_geo_reactive() |> filter(country == input$selected_SUR) |> pull(rowid)][1:5]
      )) |>
      filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      group_by(country_name) |>
      mutate(
        num_stand = NumericValue - NumericValue[YEAR == start_year],
        num_low = Low - NumericValue[YEAR == start_year],
        num_high = High - NumericValue[YEAR == start_year]
      ) |>
      ungroup()
    
    hline_data <- dat_plot |>
      filter(YEAR == as.numeric(start_year))
    
    dat_plot |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      ggplot(aes(x = year, y = NumericValue)) +
      labs(
        title = paste0("Trends in Maternal Mortality Ratio (MMR), since ", start_year),
        x = NULL, y = "MMR estimate (per 100,000 live births)",
        color = NULL,
        fill = NULL
      ) +
      geom_line(lwd = 1, aes(color = selected_sur)) +
      geom_ribbon(aes(ymin = Low, ymax = High, fill = selected_sur), color = NA, alpha = 0.4) +
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()
  })
  
  output$mmr_causes <- renderPlot({
    maternal_disorders_deaths |>
      filter(country %in% c("Global", input$selected_SUR)) |>
      filter(!cause_name %in% c("Maternal disorders")) |>
      filter(age_name %in% c("Age-standardized")) |>
      mutate(country = fct_relevel(country, "Global")) |>
      arrange(country, val) |>
      mutate(cause_name = fct_inorder(cause_name)) |>
      droplevels() |>
      ggplot(aes(y = cause_name, x = val, fill = country)) +
      scale_fill_manual(values = c("grey40", "tomato3")) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.5, position = position_dodge(width = 0.9)) +
      labs(
        y = "Maternal cause of death", x = "Age-standardized rate (per 100,000)", fill = NULL,
        title = "Distribution of causes of maternal deaths (2021)"
      ) +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(color = "grey"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 16)
      )
  })
  
  output$mmr_causes_longitudinal <- renderPlot({
    maternal_disorders_deaths_longitudinal |>
      filter(country %in% c("Global", input$selected_SUR)) |>
      mutate(country = fct_relevel(country, "Global")) |>
      filter(cause_name != "Maternal disorders") |>
      arrange(country, -year, -val) |>
      mutate(cause_name = fct_inorder(cause_name)) |>
      droplevels() |>
      ggplot(aes(x = year, y = val, color = country)) +
      scale_linewidth_binned(n.breaks = 8) +
      scale_color_manual(values = c("grey40", "tomato3")) +
      geom_line(
        aes(lwd = val)
      ) +
      facet_grid(
        rows = vars(cause_name),
        switch = "y", scales = "free",
        labeller = labeller(cause_name = label_wrap_gen(30))
      ) +
      labs(
        x = NULL, y = NULL,
        lwd = "Rate", color = NULL,
        title = "Longitudinal trends in the causes of maternal deaths\n(Caution: y-axes are variable)"
      ) +
      theme_bw() +
      theme(
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 1, size = 11),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  ## FAMILY PLANNING Outputs -------------------------------------------------
  
  # Reactive for base abortion map to avoid code duplication
  abortion_map_base <- reactive({
    world_abortion_laws |>
      right_join(state_geo_reactive()) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      ggplot(aes(geometry = polygon, fill = category, color = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_manual(
        values = c("chartreuse4", "cyan3", "gold", "chocolate1", "red3", "purple"),
        na.value = "grey90", labels = relabel_na
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank()
      ) +
      guides(color = "none", lwd = "none", label = "none")
  })
  
  output$abortion_map_sur <- renderPlot({
    p1 <- abortion_map_base() +
      labs(title = "Abortion laws by State (current as of June 2023)", fill = "Category") +
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 14)
      ) +
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    p1
  })
  
  output$abortion_rate <- renderPlot({
    p1 <- abortion_rate |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      ggplot(aes(geometry = polygon, fill = NumericValue, color = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_stepsn(
        n.breaks = 8, na.value = "grey80",
        colors = hcl.colors(n = 8, palette = "RdYlBu", rev = TRUE)
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        axis.title = element_blank()
      ) +
      labs(
        title = "Abortion rate (model-estimated), 2015-2019",
        fill = "Annual estimate\n(per 1,000)",
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none") +
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    p1
  })
  
  output$unintended_pregnancy <- renderPlot({
    p1 <- unintended_pregnancy |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      ggplot(aes(geometry = polygon, fill = NumericValue, color = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_fermenter(
        n.breaks = 10,
        palette = "RdYlBu", direction = -1,
        na.value = "grey80",
        labels = relabel_na
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        axis.title = element_blank()
      ) +
      labs(
        title = "Unintended pregnancy (model-estimated), 2015-2019",
        fill = "Annual estimate\n(per 1,000)",
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none") +
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    p1
  })
  
  output$family_planning <- renderPlot({
    p1 <- family_planning |>
      filter(!is.na(COUNTRY)) |>
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      ggplot(aes(geometry = polygon, fill = NumericValue, color = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_fermenter(
        n.breaks = 10,
        palette = "RdYlBu", direction = 1,
        na.value = "grey80",
        labels = relabel_na
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        axis.title = element_blank()
      ) +
      labs(
        title = str_wrap("Women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (%), latest year", 60),
        fill = NULL,
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none") +
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    p1
  })
  
}

# 4. RUN APP ==================================================================
shinyApp(ui, server)