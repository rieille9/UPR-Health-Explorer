# UPR Health Explorer
# Author: Anshu Uppal

# 1. SETUP: LOAD LIBRARIES AND DATA =========================================
if (!require("pacman")) install.packages("pacman")

# Load packages
pacman::p_load(
  shiny,
  bslib, # Modern UI for Shiny dashboard
  here,
  dplyr, forcats, ggplot2, magrittr, readr, readxl, stringr, tibble, tidyr, lubridate,
  ggtext, # allow dynamically wrapped plot titles
  janitor,
  DT, # interactive tables
  sf, # mapping features
  # necountries,
  patchwork,
  pdftools
)

# A helper function from utils.R for the app to run standalone
relabel_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "No data"
  factor(x)
}

source(here("code", "external_data_GBD.R"))

# Read in pre-processed datasets
sdg_data <- readRDS(here("data", "SDG_data_enhanced.rds")) |> droplevels()
state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))
nearest_neighbors_list <- readRDS(here("output", "nearest_neighbors_list.rds"))
theme_labels <- source(here("code", "theme_labels.R"))$value

# Loop through API-generated files
for (file_name in list.files(path = here("data", "API_data"), pattern = "\\.rds$")) {
  object_name <- gsub("\\.rds$", "", file_name)
  assign(object_name, readRDS(here("data", "API_data", file_name)))
}

upr_dpi <- 200
upr_width <- 600
upr_height <- 450

upr_cycle_width <- 550
upr_cycle_height <- 700

# 2. UI: BSLIB USER INTERFACE (with page_navbar) ==============================

# The theme definition remains the same
app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#1c164d",
  primary = "#1c164d",
  base_font = font_google("Lato", local = FALSE)
)

# Switch to page_navbar for a top navigation bar
ui <- page_navbar(
  id = "main_navbar",
  theme = app_theme,
  # Rearrange the tags so the link only wraps the image
  title = span(
    tags$a(
      href = "https://www.cehdi.org/", 
      # label = "Go to CeHDI homepage",
      # target = "_blank",
      img(src = "logo_5.png"
          , height = "50px"
          # , style = "margin-right:10px;"
      )
    )
    # , "Health"
    # , <a href='https://www.ohchr.org/en/health' target='_blank'>**Right to Health**</a>
    # ,tags$a(
    #   href = "https://cehdi-haro.share.connect.posit.cloud/",
    #   label = "Reload",
    #   # target = "_blank",
    #   img(src = "logo_5.png"
    #       , height = "50px"
    #       , style = "margin-right:10px;"
    #   )
    # )
    # ,actionLink(
    #   inputId = "home_button",
    #   label = "HaRO: Health & Rights Observatory",
    #   style = "color: white; text-decoration: none; font-size: 1.125rem; background: none; border: none; padding: 0;"
    # )
  ),
  bg = "#1c164d",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_bslib.css")
  ),
  
  
  ## Sidebar for Controls ----------------------------------------------------
  # This sidebar is now accessible via a button on the navbar
  sidebar = sidebar(
    width = 400,
    bg = "#1c164d",
    # title = "Controls & Map", # Give the sidebar a title
    
    selectInput("selected_regional_grouping", "Select Regional Grouping:",
                choices = c("Global", "WHO regions", "World Bank regions", 
                            # "Sub-regions",
                            # "ECSA-HC Membership", 
                            # "CARICOM Membership", "South Centre Membership", 
                            # "OACPS Membership", "OACPS Member regions", 
                            # "COMESA Membership", 
                            "Fragile and Conflict-affected States (2026)"
                ),
                selected = "Global"),
    
    selectInput("selected_region", "Select Region:",
                choices = c("Global"),
                selected = "Global"),
    
    selectInput("selected_SUR", "Select State:",
                choices = sort(unique(state_geo$country)),
                multiple = FALSE),
    
    card(
      class = "bg-light",
      full_screen = TRUE,
      # card_header("Selected Region on Global Map"),
      card_body(
        plotOutput("global_map", height = "160px"),
        padding = 0
      )
    ),
    
    accordion(
      open = FALSE, 
      width = 390,
      accordion_panel(
        "Disclaimer and data sources",
        markdown("This dashboard displays the results of a **preliminary** analysis regarding recommendations from the first four cycles of the Universal Periodic Review (UPR). ***Results are subject to change as the classification methodology continues to be refined***.

UPR recommendations were downloaded from a database maintained by the Danish Institute for Human Rights: the ['SDG-Human Rights Data Explorer'](https://www.humanrights.dk/sdg-human-rights-data-explorer). Their database in turn relies partly on UPR Info's [Database of Recommendations](https://upr-info-database.uwazi.io/).

Data related to various indicators (e.g. maternal mortality ratio and estimated abortion rates) were accessed via the [Global Health Observatory's API](https://www.who.int/data/gho/info/gho-odata-api), and data regarding the causes of maternal death were obtained using the [IHME's GBD Results tool](https://vizhub.healthdata.org/gbd-results/).

More information on the East, Central and Southern Africa Health Community (**ECSA-HC**) can be found [here](https://ecsahc.org/ecsa-hc-at-a-glance/).

Grouping by Fragile/Conflict-affected Situations (**FCS status**) was made according to the [FCS grouping obtained from the World Bank](https://thedocs.worldbank.org/en/doc/5c7e4e268baaafa6ef38d924be9279be-0090082025/original/FCSListFY26.pdf).

**Map disclaimer:** CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown on these maps. The information is for reference only.")
      )
    )
  ),
  
  ## Main Content Pages ------------------------------------------------------
  # Each nav_panel is now a separate page accessible from the top navbar
  ### About page ------------------
  nav_menu(title = "Health & Rights Observatory", icon = icon("info-circle"),
           nav_panel(title = "About", icon = icon("info-circle"),
                     # card(
                     #   fill = FALSE,
                     #   card_body(
                     markdown(
                       "Welcome to the **Health & Rights Observatory**. This platform has been designed and created by the **Global Center for Health Diplomacy and Inclusion (CeHDI)**, to empower health diplomats, decision-makers, and emerging leaders to actively engage with the Human Rights Council's Universal Periodic Review (UPR) mechanism.  
                       
                       On these pages you will find data and tools to review the ways in which countries have featured health in their UPR reporting cycles and show trends in national health outcomes, particularly in the areas of maternal health and family planning. We encourage you to browse the country profiles and we invite you to contact the CeHDI team at info@cehdi.org for more information or to give feedback."
                       # ))
                     ),
                     card(
                       fill = FALSE,
                       card_header("The Right to Health and the Universal Periodic Review"),
                       card_body(
                         layout_columns(
                           col_widths = c(9,3),
                           markdown("The <a href='https://www.ohchr.org/en/health' target='_blank'>**Right to Health**</a> is an inclusive right that 'extends not only to timely and appropriate health care but also to the underlying determinants of health.' As such, it is central to the fulfillment of broader human rights obligations, serving as a powerful tool to advance well-being, equity, and dignity across all sectors of society. The Right to Health comprises the State's obligations to:  
-  **Respect**: refrain from interfering directly or indirectly with the enjoyment of the right to health.  
-  **Protect**: take measures that prevent third parties from interfering with the guarantees of the right to health.  
-  **Fulfill**: adopt appropriate legislative, administrative, budgetary, judicial, promotional, and other measures toward the full realization of the right to health.  

The <a href='https://www.ohchr.org/en/hr-bodies/upr/basic-facts' target='_blank'>**Universal Periodic Review (UPR)**</a> is a **State-led** mechanism to **evaluate each State’s 'human rights obligations and commitments.'** Reviews involve interactive discussions during which any UN Member State can make recommendations to the States under review, which can either 'support' or 'note' the recommendations."),
                           
                           
                           # --- Column 2: Clickable Image ---
                           # Wrap the image in an actionLink to make it clickable
                           actionLink(
                             inputId = "upr_image_expand", # Give a unique ID to the link
                             label = img(
                               src = "WHO_UPR.png",
                               style = "height: auto; width: 100%; object-fit: contain; cursor: pointer;" # Add cursor style for better UX
                               , markdown("<a href='https://iris.who.int/handle/10665/277114' target='_blank'>Image: WHO</a>  
                                          More than 90,000 recommendations have been issued during the first three cycles of the UPR.")
                             )
                           )
                         )
                       )
                     )
           ),
           nav_panel(title = "Classification of UPR recommendations", icon=icon("book"),
                     card(fill = FALSE,
                          card_header("Methodology"),
                          card_body(markdown("We conducted a comprehensive longitudinal analysis of all recommendations made during the first three cycles of the United Nations' Universal Periodic Review (UPR), spanning from 2008 to 2022. The full dataset of recommendations, including the State Under Review's response (“supported” or “noted”), was sourced from the Danish Institute for Human Rights’ “SDG-Human Rights Data Explorer”. Their database in turn relies partly on UPR Info’s “Database of Recommendations”. This dataset formed the basis for our classification and subsequent statistical modelling to assess the relationship between UPR engagement and health outcomes.  

To systematically analyze the recommendations, we developed a keyword-based classification system using R. Drawing inspiration from <a href='https://iris.who.int/handle/10665/277114' target='_blank'>a recent WHO report on health-related recommendation under the first two cycles of the UPR</a>, recommendations were categorized into non-exclusive thematic health areas (i.e. a single recommendation could fall into mutliple categories), such as health systems, communicable diseases, and environmental health. For this study's focus, we developed specific, detailed sub-classification definitions for themes related to Maternal, Newborn, and Child Health (MNCH) and Sexual and Reproductive Health and Rights (SRHR), with a particular focus on identifying recommendations pertaining to maternal health and family planning."))
                     ))
           # nav_panel(title = "CeHDI",
           #           card(
           #             card_header("Global Center for Health Diplomacy and Inclusion (CeHDI)"),
           #             card_body(
           #               markdown("[CeHDI](https://www.cehdi.org/) has a mission of amplifying and facilitating the inclusion of  the priorities and voices of the Global South within the global health architecture and building robust partnerships for global health equity and the right to health.")
           #               )
           #           )
           # )
           # , card(
           #   card_header("Preliminary results?"),
           #   card_body(img(src = "full_plot.png",
           #                 style = "height: 100%; width: 100%; object-fit: contain;"),
           #             padding = 0)
           # )
  ),
  
  ### UPR impact ----------------------------
  nav_panel(title = "UPR impact", icon = icon("square-poll-vertical"),
            card(
              fill=FALSE,
              card_header("Engagement with the UPR is associated with on-the-ground progress"),
              card_body(
                layout_columns(
                  col_widths = c(7, 5),
                  markdown("A **preliminary analysis** of recommendations related to maternal health suggests that higher engagement with the UPR process, in terms of the number of recommendations issued by reviewing states as well as support of recommendations by States Under Review, is associated with accelerated progress in reducing the maternal mortality ratio (MMR) over time."),
                  actionLink(
                    inputId = "upr_analysis", # Give a unique ID to the link
                    label = img(
                      src = "full_plot.png",
                      style = "height: auto; width: 100%; object-fit: contain; cursor: pointer;" # Add cursor style for better UX
                    )
                  )
                  # ,padding = 0
                )
              )
            )
  ),
  ### UPR recommendations ----------------
  nav_menu(title = "UPR recommendations", icon = icon("people-arrows"),
           #### UPR: Regional -----------------------
           nav_panel(title = "By Region", icon = icon("globe-africa"),
                     "UPR Recommendations by Region",
                     layout_column_wrap(
                       style = css(grid_template_columns = "2fr 1fr"),
                       navset_card_tab(
                         full_screen = TRUE,
                         # title = "Regional Recommendation Themes",
                         nav_panel("All Recommendations", 
                                   card(fill = FALSE,
                                        card_body(
                                          plotOutput("upr_themes_all_global", 
                                                     width = paste0(upr_width,"px"),
                                                     height =  paste0(upr_height,"px")
                                          )),
                                        card_footer(
                                          downloadButton(
                                            outputId = "download_upr_themes_all_global",
                                            label = "Download as PNG"
                                          )
                                        )
                                   )),
                         nav_panel("Per UPR Cycle", 
                                   card(fill=FALSE,
                                     card_body(plotOutput("upr_themes_cycle_global", 
                                                          width = paste0(upr_width,"px"),
                                                          height =  paste0(upr_height*1.6,"px")
                                     ))
                                   )
                         )
                       ),
                       layout_column_wrap(
                         width=1,
                         # This sets a 3:2 height ratio
                         style = css(grid_template_rows = "3fr 2fr"),
                         card(
                           full_screen = TRUE,
                           card_header("Health-Related Recommendations"),
                           card_body(plotOutput("global_plot"))
                         ),
                         card(
                           full_screen = TRUE,
                           card_header("Regional map"),
                           card_body(plotOutput("regional_map"))
                         )
                       )
                     )
           ),
           
           #### UPR: SuR -------------------------------
           nav_panel(title = "By State", icon = icon("flag"),
                     "UPR Recommendations by State",
                     layout_column_wrap(
                       style = css(grid_template_columns = "2fr 1fr"),
                       navset_card_tab(
                         full_screen = TRUE,
                         # title = "SUR Recommendation Details",
                         nav_panel("All Recommendations", 
                                   card(fill = FALSE,
                                        card_body(plotOutput("upr_themes_all", 
                                                             width = paste0(upr_width,"px"), 
                                                             height =  paste0(upr_height,"px"))),
                                        card_footer(
                                          downloadButton(
                                            outputId = "download_upr_themes_all",
                                            label = "Download as PNG"
                                          )
                                        ))
                         ),
                         nav_panel("Per UPR Cycle", 
                                   card(fill = FALSE,
                                     card_body(plotOutput("upr_themes_cycle",
                                                          width = paste0(upr_width,"px"),
                                                          height =  paste0(upr_height*1.6,"px")
                                                          ))
                                   )
                         ),
                         nav_panel("Data Table", DTOutput("DT_table"))
                       ),
                       card(
                         full_screen = TRUE,
                         card_header("Health-Related Recommendations"),
                         card_body(plotOutput("plot", height = "700px"))
                       )
                     )
           )
  ),
  
  ### UHC ---------------------
  nav_panel(title = "UHC", icon = icon("umbrella"),
            markdown("UHC: Universal Health Coverage<br>RMNCH: Reproductive, Maternal, Newborn and Child Health"),
            layout_column_wrap(
              layout_column_wrap(
                width=1,
                card(full_screen = TRUE,card_header("UHC Service Coverage Index (2021)"), plotOutput("UHC_map")),
                card(full_screen = TRUE,card_header("UHC sub-index on RMNCH (2021)"), plotOutput("UHC_RMNCH_map"))
              ),
              card(full_screen = TRUE, card_header("UHC indices over time"), plotOutput("UHC_trend"))
            )
  ),
  
  ### Maternal mortality -----------------------
  nav_panel(title = "Maternal Mortality", icon = icon("person-pregnant"),
            "Maternal Mortality Ratio (MMR): Number of maternal deaths per 100,000 live births.",
            layout_columns(
              full_screen = TRUE,
              col_widths = c(5, 7),
              list(
                card(full_screen = TRUE,card_header("MMR Estimate Map (2023)"), plotOutput("mmr_map")),
                card(full_screen = TRUE,card_header("MMR Trends vs. Neighbors"), plotOutput("mmr_time_plot_neighbors"))
              ),
              navset_card_tab(
                # title = "Causes of Maternal Death",
                full_screen = TRUE,
                nav_panel("Causes of Maternal Death", plotOutput("mmr_causes", height = "600px")),
                nav_panel("Causes Over Time", plotOutput("mmr_causes_longitudinal", height = "600px")),
                nav_panel(
                  shiny::icon("circle-info"),
                  markdown("The below *abbreviated definitions* were compiled from the **IHME's** <a href='https://www.healthdata.org/research-analysis/diseases-injuries-risks/factsheets-hierarchy' target='_blank'>factsheets pages for the level 4 causes of maternal disorders</a>:  
                  
**Maternal haemorrhage** includes both postpartum haemorrhage (defined as blood loss ≥500 ml for vaginal delivery and ≥1000 ml for caesarean delivery) and antepartum haemorrhage (defined as vaginal bleeding from any cause at or beyond 20 weeks of gestation).  

**Maternal sepsis** is defined as a temperature <36°C or >38°C and clinical signs of shock (systolic blood pressure <90 mmHg and tachycardia >120 bpm). **Other maternal infections** are defined as any maternal infections excluding HIV, STI, or not related to pregnancy. 

**Maternal hypertensive disorders** include gestational hypertension (onset after 20 weeks gestation), pre-eclampsia, severe preeclampsia, and eclampsia, but exclude chronic hypertension (onset prior to pregnancy or prior to 20 weeks gestation) unless superimposed preeclampsia or eclampsia develop.

**Maternal obstructed labour and uterine rupture** aggregates obstructed labour (arrest in the first or second stage of active labour despite sufficient contractions), uterine rupture (non-surgical breakdown of uterine wall), and fistula (an abnormal opening between the vagina and the bladder or rectum following childbirth). 

**Abortion** is defined as elective or medically indicated termination of pregnancy at any gestational age. **Miscarriage** is defined as spontaneous loss of pregnancy before 24 weeks of gestation with complications requiring medical care.

**Ectopic pregnancy** is defined as pregnancy occurring outside of the uterus.

**Indirect maternal deaths** are due to existing diseases that are exacerbated by pregnancy. Examples include maternal infections and parasitic diseases complicating pregnancy, childbirth, and the puerperium, and diabetes in pregnancy, childbirth, and the puerperium. 

**Late maternal deaths** are deaths that occur six weeks to one year after the end of pregnancy, excluding incidental deaths.

**Maternal deaths aggravated by HIV/AIDS** are deaths occurring in HIV-positive women whose pregnancy has exacerbated their HIV/AIDS, leading to death.

**Other direct maternal disorders** encompasses a wide range of maternal disorders that do not map to other diseases in the GBD cause list, including other fatal or non-fatal complications occurring during pregnancy, childbirth, and the postpartum period.")
                )
              )
            )
  ),
  ### Abortion ------------------------
  nav_panel(title = "Abortion", icon = icon("house-medical"),
            layout_column_wrap(
              card(fill = FALSE,
                   full_screen = TRUE,
                   card_header("Abortion Laws (June 2023)"),
                   plotOutput("abortion_map_sur"),
                   markdown("Data: <a href='https://reproductiverights.org/maps/worlds-abortion-laws/' target='_blank'>Center for Reproductive Rights</a>")
              ),
              layout_column_wrap(
                width = 1,
                card(
                  full_screen = TRUE,
                  card_header("Estimated Abortion Rate (2015-2019)"),
                  plotOutput("abortion_rate")
                ),
                card(
                  full_screen = TRUE,
                  card_header("Estimated Unintended Pregnancy Rate (2015-2019)"),
                  plotOutput("unintended_pregnancy")
                )
              )
            )
  ),
  ## Family planning ------------------
  nav_panel(title = "Family Planning", icon = icon("people-group"),
            # "Family Planning",
            layout_column_wrap(
              full_screen = TRUE,
              card(
                fill = FALSE,
                full_screen = TRUE,
                card_header("Met Need for Family Planning (%)"),
                plotOutput("family_planning")
              )
            )
  ),
  
  nav_spacer(),
  nav_item(
    tags$a(
      shiny::icon("github", class = "fa-1x"), # The GitHub icon  
      # "Source",              # Optional text next to the icon
      href = "https://github.com/CeHDI-Foundation/UPR-Health-Explorer", # <-- REPLACE with your repo URL
      target = "_blank"      # Opens the link in a new tab
    )
  )
)

# 3. SERVER: REACTIVE LOGIC ============================
server <- function(input, output, session) {
  
  # This reactive expression captures the real-time width of our plot container.
  # It's debounced to wait 500ms after a resize before updating.
  plot_width_mmr_neighbors <- reactive({
    session$clientData$output_mmr_time_plot_neighbors_width
  }) |> debounce(500)
  
  plot_width_UHC_neighbors <- reactive({
    session$clientData$output_UHC_trend_width
  }) |> debounce(500)
  
  ## About page options ------------------------
  
  # observeEvent(input$home_button, {
  #   updateNavbarPage(
  #     session = session,
  #     inputId = "main_navbar",
  #     selected = "HaRO" # The title of the first nav_panel
  #   )
  # })
  
  observeEvent(input$upr_image_expand, {
    showModal(modalDialog(
      title = "Graphical overview of the UPR process",
      img(src = "WHO_UPR.png", style = "width: 100%"),
      size = "xl",           # Make the modal large
      easyClose = TRUE,     # Allow closing by clicking outside
      footer = NULL         # Remove the default buttons
    ))
  })
  
  observeEvent(input$upr_analysis, {
    showModal(modalDialog(
      title = "Preliminary analysis of States' MMR trajectories according to engagement with UPR recommendations related to maternal health",
      img(src = "full_plot.png", style = "width: 100%"),
      size = "l",           # Make the modal large
      easyClose = TRUE,     # Allow closing by clicking outside
      footer = NULL         # Remove the default buttons
    ))
  })
  
  ## Reactive Expressions for Data Filtering ---------------------------------
  state_geo_reactive <- reactive({
    if (input$selected_regional_grouping == "Sub-regions") {
      state_geo |> mutate(region_dashboard = subregion)
    } else if (input$selected_regional_grouping == "World Bank regions") {
      state_geo |> mutate(region_dashboard = wbregion)
    } else if (input$selected_regional_grouping == "WHO regions") {
      state_geo |> mutate(region_dashboard = WHO_region)
    }else if (input$selected_regional_grouping == "ECSA-HC Membership") {
      state_geo |> mutate(region_dashboard = ECSA_status)
    } else if (input$selected_regional_grouping == "Fragile and Conflict-affected States (2026)") {
      state_geo |> mutate(region_dashboard = FCS_status)
    } else if (input$selected_regional_grouping == "CARICOM Membership") {
      state_geo |> mutate(region_dashboard = CARICOM_status)
    } else if (input$selected_regional_grouping == "OACPS Membership") {
      state_geo |> mutate(region_dashboard = OACPS_status)
    } else if (input$selected_regional_grouping == "OACPS Member regions") {
      state_geo |> mutate(region_dashboard = OACPS_region)
    } else if (input$selected_regional_grouping == "COMESA Membership") {
      state_geo |> mutate(region_dashboard = COMESA_status)
    } else if (input$selected_regional_grouping == "South Centre Membership") {
      state_geo |> mutate(region_dashboard = SC_status)
    } else {
      state_geo |> mutate(region_dashboard = "Global")
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
    if (input$selected_regional_grouping == "Global") {
      # If the top-level grouping is "Global", force the region to "Global" as well.
      updateSelectInput(
        session, "selected_region",
        choices = "Global",
        selected = "Global"
      )
    } else {
      # Otherwise, populate the region dropdown with the appropriate sub-regions.
      updateSelectInput(
        session, "selected_region",
        choices = levels(state_geo_reactive()$region_dashboard)
      )
    }
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
        fill = "transparent",
        color = "green4",
        linewidth = 0.5
      )
    }
    
    p2 + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  })
  
  output$regional_map <- renderPlot({
    p1 <- state_geo_reactive() |>
      mutate(selected_region = factor(case_when(region_dashboard == input$selected_region ~ input$selected_region,
                                                .default = "Other"),
                                      levels = c(input$selected_region, "Other"))) |>
      ggplot(aes(geometry = polygon, color = selected_region, fill = selected_region, lwd = selected_region)) +
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
    p1 + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  })
  
  ## UPR: REGIONAL Outputs ----------------------------------------------------
  ### General plot ----------------
  output$global_plot <- renderPlot({
    req(nrow(filtered_upr_region()) > 0) # pause execution until filtered data is ready
    
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
    
    rec_max <- max(upr_rec_global$med_n_tot)
    
    upr_rec_global |>
      mutate(region = input$selected_region) |> 
      ggplot(aes(x = cycle, y = med_n, fill = health_related)) +
      scale_fill_manual(values = c("Health-related" = "#E69F00", "Other" = "grey80")) +
      geom_bar(stat = "identity") +
      labs(
        y = "Median # of recommendations", x = "UPR Cycle",
        title = paste0("Median recommendations received by States"),
        fill = NULL
        # ,caption = "*Cycle 4 is currently underway"
      ) +
      geom_text(aes(label = perc), position = position_stack(vjust = 0.5)
                , size = 4
      ) +
      geom_text(aes(label = sprintf("%1.0f", med_n_tot), y = med_n_tot, vjust = -0.2), 
                size = 5,
                fontface = "bold") +
      scale_y_continuous(limits = c(0,rec_max+25),
                         expand = expansion(mult = c(0, 0.05)))+
      theme_bw() +
      facet_wrap(.~region)+
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 0.8,
                                   ,size = 12
        ),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        # plot.caption = element_text(size = 14),
        legend.position = "bottom",
        # legend.position = c(0, 1),
        # legend.justification = c("left", "top"),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, unit = "pt")
        )
      )
  })
  
  ### Cycle themes ---------------------
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
        # theme_label = str_wrap(theme_label, width = 30),
        theme_label = fct_inorder(theme_label)
      )
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    theme_plot <- a |>
      ggplot(aes(x = perc, y = fct_rev(cycle))) +
      geom_col(aes(fill = response_upr)) +
      facet_grid(
        rows = vars(theme_label), switch = "y"
        # ,labeller = labeller(theme_label = label_wrap_gen(50))
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
        limits = c(0, max_a),
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme(
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(
          size = 10,
          face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,
                                  size = 16,
                                  face = "bold"),
        plot.title.position = "plot",
        # plot.caption = element_text(size = 14),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, 
                                         # vjust = 1
                                         , size = 12
        ),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank()
      ) 
    # geom_text(
    #   data = a |> filter(response_upr == "Supported"),
    #   aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
    #   hjust = -0.15, size = 3, vjust = 0.25
    # )
    theme_plot
  })
  
  ### All cycles themes -------------------
  #### Plot object -------------------
  upr_themes_all_global_object <- reactive({
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
        # theme_label = str_wrap(theme_label, width = 40),
        theme_label = fct_inorder(theme_label)
      )
    
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    a |>
      ggplot(aes(x = perc, y = fct_rev(theme_label))) +
      geom_col(aes(fill = response_upr)) +
      labs(
        x = "Proportion of all recommendations (%)", y = NULL,
        fill = "State's response",
        title = paste0("Health-related recommendations of the UPR\n", input$selected_region),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        # limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.15))
      ) +
      coord_cartesian(clip = "off")+
      theme(
        plot.margin = margin(l=2,t=2,b=2, r = 30, unit = "pt"),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 11),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 3, vjust = 0.25
      )
  })
  #### Plot output ----------------------------
  output$upr_themes_all_global <- renderPlot(
    width = upr_width, 
    height = upr_height,
    res = 96,
    {
      upr_themes_all_global_object()
    })
  
  #### Plot downloader -------------------
  output$download_upr_themes_all_global <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("health-recommendations-", input$selected_region, ".png")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      ggsave(
        file,
        plot = upr_themes_all_global_object(),
        width = 7,
        height = 5,
        dpi = 300,
        units = "in"
      )
    }
  )
  
  ## UPR: SUR Outputs --------------------------------------------------------
  ### General plot --------------------------
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
        legend.position = "bottom",
        # legend.position = c(0.01, 0.99),
        # legend.justification = c("left", "top"),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        )
      )
  })
  
  ### Cycle themes ------------------
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
        title = paste("Health-related recommendations in each cycle of the UPR\n", input$selected_SUR),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        # limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.05))
      ) +
      # geom_text(
      #   data = a |> filter(response_upr == "Supported"),
      #   aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
      #   hjust = -0.15, size = 3.5, vjust = 0.25
      # )+
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
        strip.text.y.left = element_text(angle = 0, 
                                         # vjust = 1, 
                                         size = 12),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank()
      ) 
    theme_plot
  })
  
  ### All cycles themes ----------------------------
  #### Plot object -------------------
  upr_themes_all_object <- reactive({
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
        title = paste("Health-related recommendations in each cycle of the UPR\n", input$selected_SUR),
        caption = "*Numbers after the bars indicate N (% supported)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        # limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.15))
      ) +
      coord_cartesian(clip = "off")+
      theme(
        plot.margin = margin(l=2,t=2,b=2, r = 30, unit = "pt"),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 3, vjust = 0.25
      )
  })
  #### Plot output -------------------
  output$upr_themes_all <- renderPlot(
    width = upr_width, 
    height = upr_height,
    res = 96,
    {
      upr_themes_all_object()
    })
  #### Plot downloader ---------------
  output$download_upr_themes_all <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("health-recommendations-", input$selected_SUR, ".png")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      ggsave(
        file,
        plot = upr_themes_all_object(),
        width = 7,
        height = 5,
        dpi = 300,
        units = "in"
      )
    }
  )
  
  ### Data table ---------------------------
  output$DT_table <- renderDT({
    req(nrow(filtered_upr()) > 0)
    filtered_upr() |>
      mutate(state_under_review = factor(state_under_review)) |>
      select(
        text, cycle, response_upr, health_related:maternal_health,
        state_under_review, document_code, paragraph
      ) |>
      DT::datatable(
        # extensions = "Responsive",
        filter = "top",
        options = list(
          pageLength = 100,
          deferRender = TRUE,
          scrollY = 800,
          scrollX = TRUE,
          scroller = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = '500px', targets = c(0))
            # list(width = '200px', targets = c(1))
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
  })
  
  ## UHC Outputs ----------------------------------------------------------
  output$UHC_map <- renderPlot({
    UHC_estimate_2021 = UHC_all |>
      filter(country_name == input$selected_SUR, YEAR == 2021, 
             IndicatorCode == "UHC_INDEX_REPORTED") |>
      pull(NumericValue) |>
      round(0)
    
    p1<-UHC_all |> 
      filter(YEAR == 2021) |> 
      # group_by(country_name, IndicatorCode) |> slice_max(order_by = YEAR, n=1) |> ungroup() |> 
      filter(SpatialDimType == "COUNTRY") |> 
      filter(IndicatorCode == "UHC_INDEX_REPORTED") |> 
      left_join(state_geo |> select(iso3), join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |> 
      ggplot(aes(geometry = polygon, fill = NumericValue, color = selected_sur, lwd = selected_sur)) +
      geom_sf()+
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_stepsn(n.breaks = 10, na.value = "grey80", 
                        colors = hcl.colors(n = 10, palette = "RdYlBu"))+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(25, "pt"),
        plot.title = ggtext::element_textbox_simple(
          width=grid::unit(1,"npc"),
          halign=0.5,
          margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        )
      )+
      labs(
        title = paste0(input$selected_SUR, ": ", UHC_estimate_2021),
        fill = NULL
      )+
      guides(color = "none", lwd = "none")+
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
  })
  
  output$UHC_RMNCH_map <- renderPlot({
    UHC_estimate_2021 = UHC_all |>
      filter(country_name == input$selected_SUR, YEAR == 2021, 
             IndicatorCode == "UHC_SCI_RMNCH") |>
      pull(NumericValue) |>
      round(0)
    
    p1<-UHC_all |> 
      filter(YEAR == 2021) |> 
      # group_by(country_name, IndicatorCode) |> slice_max(order_by = YEAR, n=1) |> ungroup() |> 
      filter(SpatialDimType == "COUNTRY") |> 
      filter(IndicatorCode == "UHC_SCI_RMNCH") |> 
      left_join(state_geo |> select(iso3), join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |> 
      ggplot(aes(geometry = polygon, fill = NumericValue, color = selected_sur, lwd = selected_sur)) +
      geom_sf()+
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_stepsn(n.breaks = 10, na.value = "grey80", 
                        colors = hcl.colors(n = 10, palette = "RdYlBu"))+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(25, "pt"),
        plot.title = ggtext::element_textbox_simple(
          width=grid::unit(1,"npc"),
          halign=0.5,
          margin = margin(t = 10, b = 15, r=0, l=0, unit = "pt")
        )
      )+
      labs(
        title = paste0(input$selected_SUR, ": ", UHC_estimate_2021),
        # caption = "RMNCH: reproductive, maternal, newborn and child health",
        fill = NULL
      )+
      guides(color = "none", lwd = "none")+
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
  })
  
  output$UHC_trend <- renderPlot({
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_UHC_neighbors()) && plot_width_UHC_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    UHC_all |> 
      filter(IndicatorCode %in% c("UHC_SCI_RMNCH",
                                  "UHC_INDEX_REPORTED")) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(country_name %in% c(
        input$selected_SUR,
        nearest_neighbors_list[, input$selected_SUR][1:5]
      )) |>
      filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = str_wrap(country_name, 19)) |> 
      mutate(country_name = fct_relevel(country_name, str_wrap(input$selected_SUR,19))) |>
      mutate(IndicatorName = str_wrap(IndicatorName,40))|> 
      ggplot(aes(x=YEAR, y = NumericValue, color = IndicatorName, shape = IndicatorName))+
      geom_point(size = 3)+
      geom_line(linewidth=1.5)+
      labs(y = "Index value",
           x = NULL,
           title = "UHC Service Coverage", color = NULL, shape = NULL)+
      # facet_wrap(.~country_name, ncol=num_cols)+
      ggh4x::facet_wrap2(~country_name, ncol = num_cols
                         ,strip = ggh4x::strip_themed(
                           text_x = list(element_text(color="white", face = "bold"),
                                         NULL, NULL, NULL, NULL, NULL),
                           background_x = list(element_rect(fill = "grey30"), 
                                               NULL, NULL, NULL, NULL, NULL)
                         )
      )+
      theme_bw()+
      scale_x_continuous(breaks = c(2005, 2020))+
      theme(
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        axis.text = element_text(angle=30, size = 10, hjust=0.5),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank()
      )+
      # scale_y_continuous(sec.axis = dup_axis(name = NULL))+
      guides(color=guide_legend(nrow=2,byrow=TRUE))
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
    
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
  })
  
  output$mmr_time_plot_neighbors <- renderPlot({
    
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_mmr_neighbors()) && plot_width_mmr_neighbors() < 300) {
      num_cols <- 2
    }
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
        nearest_neighbors_list[, input$selected_SUR][1:5]
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
        title = paste0("Trends in Maternal Mortality Ratio (MMR)"),
        x = NULL, y = "MMR estimate (per 100,000 live births)",
        color = NULL,
        fill = NULL
      ) +
      geom_line(lwd = 1, aes(color = selected_sur)) +
      geom_ribbon(aes(ymin = Low, ymax = High, fill = selected_sur), color = NA, alpha = 0.4) +
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
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
      mutate(cause_name = fct_inorder(str_wrap(cause_name,30))) |>
      droplevels() |>
      ggplot(aes(y = cause_name, x = val, fill = country)) +
      scale_fill_manual(values = c("grey40", "tomato3")) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.5, position = position_dodge(width = 0.9)) +
      labs(
        y = "Maternal cause of death", x = "Age-standardized rate (per 100,000)\nin 2023", fill = NULL
        # , title = "Distribution of causes of maternal deaths (2021)"
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
  
  ## Abortion Outputs -------------------------------------------------
  
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
      labs(title = "Abortion laws by State (current as of June 2023)", fill = NULL) +
      theme(
        legend.position = "right",
        legend.key.size = unit(15, "pt"),
        # legend.key.height = unit(1,"cm"),
        legend.text = element_text(size = 11)
      ) +
      coord_sf(
        xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
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
        legend.text = element_text(size=12),
        legend.key.size = unit(20, "pt"),
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
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
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
        # legend.key.height = unit(1,"cm"),
        legend.key.size = unit(20, "pt"),
        legend.text = element_text(size=12),
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
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
  })
  
  ## Family planning outputs -------------------------------------
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
        legend.text = element_text(size=12),
        legend.key.size = unit(25,"pt"),
        legend.background = element_blank(),
        axis.title = element_blank(),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        )
      ) +
      labs(
        title = "Women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (%), latest year",
        fill = NULL,
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none") +
      coord_sf(
        xlim = c(bbox_SUR_region()[[1]],bbox_SUR_region()[[3]]),
        ylim = c(bbox_SUR_region()[[2]], bbox_SUR_region()[[4]])
        # xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
        # ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
      )
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",      # Make the rectangle hollow
      color = "red",             # Set the border color
      linewidth = 0.5            # Set the border thickness
    )}
    
    p3<-p1+
      scale_linewidth_manual(values = c(0.2, 0.1))+
      coord_sf(
        xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
        ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    
    if(sur_area() > 10^11){p2} else{p2+p3}
  })
}

# 4. RUN APP ==================================================================
shinyApp(ui, server)