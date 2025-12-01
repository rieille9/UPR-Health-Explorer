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
  ggnewscale,
  ggtext, # allow dynamically wrapped plot titles
  janitor,
  DT, # interactive tables
  openxlsx, # download as xlsx
  sf, # mapping features
  # necountries,
  patchwork,
  pdftools,
  tinytex,
  plotly
  # quarto
)

# tinytex::install_tinytex()

## Data ---------------------------------------------------------------
source(here("code", "external_data_GBD.R"))

# Read in pre-processed datasets
# sdg_data <- readRDS(here("data", "SDG_data_enhanced.rds")) |> droplevels() |> 
#   mutate(response_upr = fct_recode(response_upr, "Noted" = "Noted/Other"))
sdg_data <- readRDS(here("output", "UHRI_UPR_enhanced.rds")) |> 
  mutate(response_upr = fct_recode(response_upr, 
                                   "Noted" = "Partially supported"),
         response_upr = fct_relevel(response_upr, "Noted")) |> 
  droplevels()
state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))
nearest_neighbors_list <- readRDS(here("output", "nearest_neighbors_list.rds"))
source(here("code", "theme_labels.R"))
theme_labels <- theme_labels |> 
  filter(!variable %in% c(
    "SRHR", "health_related", "SOCED",
    "essential_medicines","TB_malaria", "NTD","vaccinations"
  ))

sdg_data <- sdg_data |> select(-any_of(c("SRHR", "SOCED",
                                         "essential_medicines","TB_malaria", 
                                         "NTD","vaccinations")))

# Loop through API-generated files
for (file_name in list.files(path = here("data", "API_data"), pattern = "\\.rds$")) {
  object_name <- gsub("\\.rds$", "", file_name)
  assign(object_name, readRDS(here("data", "API_data", file_name)))
}

# Make a vertical line that cuts the mapping geometries, for adjusting the maps to allow for shifting the center to the Pacific region
polygon_shift <- st_polygon(x = list(rbind(
  c(-0.000001, 90),
  c(0, 90),
  c(0, -90),
  c(-0.000001, -90),
  c(-0.000001, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# A helper function from utils.R for the app to run standalone
relabel_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "No data"
  factor(x)
}

map_insetting <- function(
    p1, 
    plot_dat,
    bbox_SUR_region_dynamic, bbox_sur, sur_area, 
    p_title_text="Update this text", 
    p_caption_text = "Update this text",
    title_size=14, title_margin=17,
    caption_size=14
) {
  p2 <- p1 + 
    geom_sf(
      # data=plot_dat, 
      color="grey80", fill="transparent")+
    coord_sf(
      # expand = FALSE,
      xlim = c(bbox_SUR_region_dynamic[[1]],bbox_SUR_region_dynamic[[3]]),
      ylim = c(bbox_SUR_region_dynamic[[2]], bbox_SUR_region_dynamic[[4]])
      
      # Alternative mapping centered on the selected State
      # xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
      # ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
    )+
    geom_rect(
      aes(
        xmin = bbox_sur["xmin"] - 0.7,
        xmax = bbox_sur["xmax"] + 0.7,
        ymin = bbox_sur["ymin"] - 0.7,
        ymax = bbox_sur["ymax"] + 0.7
      ),
      fill = "transparent",
      color = "red",
      linewidth = 0.5
    )
  
  p3 <- p1 + guides(fill="none")+
    ggnewscale::new_scale_fill()+ 
    geom_sf(
      # data=plot_dat, 
      aes(geometry=polygon, fill = selected_sur),
      color = "transparent")+ 
    scale_fill_manual(values=c("transparent", "white"))+
    # scale_linewidth_manual(values = c(0.2, 0.1)) +
    coord_sf(
      expand=FALSE,
      xlim = c(bbox_sur[[1]]-.11, bbox_sur[[3]]+.11),
      ylim = c(bbox_sur[[2]]-.11, bbox_sur[[4]]+.11)
    ) +
    theme_void()+
    theme(
      panel.background = element_rect(fill="white"), 
      panel.border = element_rect(color = "red")
    )+
    geom_rect(
      aes(
        xmin = bbox_sur["xmin"]-.1,
        xmax = bbox_sur["xmax"]+.1,
        ymin = bbox_sur["ymin"]-.1,
        ymax = bbox_sur["ymax"]+.1
      ),
      fill = "transparent",
      color = "red",
      linewidth = 0.5
    )+
    guides(fill = "none") +
    labs(
      # subtitle = "(map zoom)", 
      caption = NULL)
  
  p_title <- plot_annotation(
    title = p_title_text,
    caption = p_caption_text,
    theme = theme(
      plot.background = element_rect(color = "#1c164d", fill=NA),
      plot.title = element_textbox_simple(
        size = title_size,
        margin = margin(t = title_margin, b = title_margin, r=0, l=0, unit = "pt")
      ),
      plot.caption = element_textbox_simple(
        size = caption_size, halign = 0.5,
        margin = margin(t = 10, b = 2, r=0, l=0, unit = "pt")
      )
    )
  )
  
  # Calculate Inset dimensions for lower left
  a<-ggplot_build(p2)
  b<-ggplot_build(p3)
  multiplier_a <- abs(a$layout$coord$limits$x[1]-a$layout$coord$limits$x[2])/abs(a$layout$coord$limits$y[1]-a$layout$coord$limits$y[2])
  multiplier_b <- abs(b$layout$coord$limits$x[1]-b$layout$coord$limits$x[2])/abs(b$layout$coord$limits$y[1]-b$layout$coord$limits$y[2])
  
  area_a <- abs(a$layout$coord$limits$x[1]-a$layout$coord$limits$x[2])*abs(a$layout$coord$limits$y[1]-a$layout$coord$limits$y[2])
  area_b <- abs(b$layout$coord$limits$x[1]-b$layout$coord$limits$x[2])*abs(b$layout$coord$limits$y[1]-b$layout$coord$limits$y[2])
  
  key_dim <- 0.3/(multiplier_a/multiplier_b)
  inset_dimensions <- if(multiplier_b<=3){c(0,0,key_dim,key_dim*multiplier_a/multiplier_b)}else{c(0,0,key_dim*multiplier_a/multiplier_b,0.2)}
  
  if (
    # sur_area > 11^11 | 
    area_b > 0.02*area_a
  ) {
    p2 + p_title
  } else {
    p2 + inset_element(p3, inset_dimensions[1], inset_dimensions[2],inset_dimensions[3],inset_dimensions[4]) + p_title
  }
}

## Plot resolutions ------------------------------------------
upr_dpi <- 200
upr_width <- 620
upr_height <- 550

upr_cycle_width <- 550
upr_cycle_height <- 800

# 2. UI: BSLIB USER INTERFACE (with page_navbar) ==============================

# The theme definition remains the same
app_theme <- bs_theme(
  version = 5,
  bg = "#F9F9F6",
  fg = "#1c164d",
  primary = "#1c164d",
  base_font = font_google("Lato", local = FALSE)
)

bs_theme(
  version = 5,
  bg = "#F9F9F6",
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
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_bslib.css"),
    # Custom CSS for the PDF download button.
    tags$style(HTML("
      /* The ID selector '#' must match the downloadButton's outputId */
      #download_report {
        background-color: #4a86e8; /* color */
        color: white;             /* White text */
        border-radius: 5px;       /* Rounded corners */
        border: 2px solid #4a86e8;
        font-weight: bold;        /* Make the text bold */
        transition: background-color 0.3s, border-color 0.3s; /* Smooth transition for hover effect */
      }
      
      /* Style for when you hover over the button */
      #download_report:hover {
        background-color: #ec5557; /*  */
        border-color: #ec5557;
        cursor: pointer;          /* Show a 'hand' cursor on hover */
      }
    ")),
    
    tags$style(HTML("
      .selectize-dropdown {
        width: auto !important; /* Let the content define the width */
        min-width: 200px;       /* Set a minimum for good measure */
      }
    "))
  ),
  
  
  ## Sidebar for Controls ----------------------------------------------------
  # This sidebar is now accessible via a button on the navbar
  sidebar = sidebar(
    width = 400,
    bg = "#1c164d",
    # title = "Controls & Map", # Give the sidebar a title
    
    selectInput("selected_regional_grouping", "Select Regional Grouping:",
                choices = c("Global", "WHO regions", "World Bank regions", 
                            "Sub-regions (UN M49)",
                            # "ECSA-HC Membership", 
                            "CARICOM Membership",
                            # "South Centre Membership", 
                            # "OACPS Membership", "OACPS Member regions", 
                            # "COMESA Membership", 
                            "Fragile and Conflict-affected States (2026)"
                ),
                selected = "Global"),
    
    selectInput("selected_region", "Select Region:",
                choices = c("Global"),
                selected = "Global"),
    
    selectInput("selected_SUR", "Select State:",
                choices = state_geo |> 
                  # Remove non-member states (no data, causes crash)
                  filter(!country %in% c("Western Sahara","Greenland" ,
                                         "Palestine","Vatican",
                                         "Siberian Artifact")) |>  
                  select(country) |> distinct() |> arrange(country) |> 
                  pull(country),
                multiple = FALSE),
    
    # ### PDF downloader ------------------------
    # downloadButton(
    #   outputId = "download_report",
    #   label = "Download Country Profile"
    #   # ,style = "width: 100%;" # Make the button full-width
    # ),
    
    #### qmd --------------------------
    # downloadButton(
    #   outputId = "download_report_qmd",
    #   label = "Download Report QMD (IN DEVELOPMENT)"
    #   # ,style = "width: 100%;" # Make the button full-width
    # ),
    
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
        markdown("This dashboard displays the results of a **preliminary** analysis regarding recommendations from the first four cycles of the Universal Periodic Review (UPR). ***Results are subject to change as the classification methodology continues to be refined and as data sources are updated***.

UPR recommendations were downloaded directly from the [Universal Human Rights Index](https://uhri.ohchr.org/en/our-data-api), maintained by the Office of the High Commissioner for Human Rights.

Data related to various indicators (e.g. maternal mortality ratio and estimated abortion rates) were accessed via the [Global Health Observatory's API](https://www.who.int/data/gho/info/gho-odata-api), and data regarding the causes of maternal death were obtained using the [IHME's GBD Results tool](https://vizhub.healthdata.org/gbd-results/).

Grouping by Fragile/Conflict-affected Situations (**FCS status**) was made according to the [FCS grouping obtained from the World Bank](https://thedocs.worldbank.org/en/doc/5c7e4e268baaafa6ef38d924be9279be-0090082025/original/FCSListFY26.pdf).

**Map disclaimer:** CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown on these maps. The information is for reference only.")
        #         markdown("This dashboard displays the results of a **preliminary** analysis regarding recommendations from the first four cycles of the Universal Periodic Review (UPR). ***Results are subject to change as the classification methodology continues to be refined***.
        # 
        # UPR recommendations were downloaded from a database maintained by the Danish Institute for Human Rights: the ['SDG-Human Rights Data Explorer'](https://www.humanrights.dk/sdg-human-rights-data-explorer). Their database in turn relies partly on UPR Info's [Database of Recommendations](https://upr-info-database.uwazi.io/).
        # 
        # Data related to various indicators (e.g. maternal mortality ratio and estimated abortion rates) were accessed via the [Global Health Observatory's API](https://www.who.int/data/gho/info/gho-odata-api), and data regarding the causes of maternal death were obtained using the [IHME's GBD Results tool](https://vizhub.healthdata.org/gbd-results/).
        # 
        # Grouping by Fragile/Conflict-affected Situations (**FCS status**) was made according to the [FCS grouping obtained from the World Bank](https://thedocs.worldbank.org/en/doc/5c7e4e268baaafa6ef38d924be9279be-0090082025/original/FCSListFY26.pdf).
        # 
        # **Map disclaimer:** CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown on these maps. The information is for reference only.")
      )
    )
  ),
  
  ## Main Content Pages ------------------------------------------------------
  # Each nav_panel is now a separate page accessible from the top navbar
  ### Landing page ------------------
  nav_panel(title = "Health & Rights Observatory", icon = icon("info-circle"),
            # nav_panel(title = "About", icon = icon("info-circle"),
            # card(
            #   fill = FALSE,
            #   card_body(
            markdown(
              "Welcome to the **Health & Rights Observatory**. This platform has been designed and created by the **Global Center for Health Diplomacy and Inclusion (CeHDI)**, to advance and amplify the mainstreaming of the right to health in the Human Rights Council processes, treaty bodies and special procedures as a gateway for universal health coverage and global health equity. The platform is intended to empower diplomats and policymakers across the health, foreign affairs, and related sectors, as well as civil society actors, to advance the Right to Health within global and national human rights discussions."),
            card(
              fill = FALSE,
              card_header("The Right to Health"),
              card_body(markdown(
                "The <a href='https://www.ohchr.org/en/health' target='_blank'>**Right to Health**</a>, as enshrined in <a href='https://www.ohchr.org/en/instruments-mechanisms/instruments/international-covenant-economic-social-and-cultural-rights#article-12' target='_blank'>Article 12 of the International Convenant on Economic, Social and Cultural Rights</a>, is an inclusive human right that extends beyond timely and appropriate health care to encompass the underlying determinants of health. It forms an essential part of States’ obligations under international human rights law and provides a binding normative framework for advancing well-being, equity, and dignity across all sectors of society.  
                           
Under the Right to Health, States have the following obligations:  
-  **Respect**: refrain from directly or indirectly interfering with the enjoyment of the right to health.  
-  **Protect**: take effective measures to prevent third parties from undermining or violating the guarantees of the right to health.  
-  **Fulfill**: adopt appropriate legislative, administrative, budgetary, judicial, promotional, and other measures toward the full realization of the right to health."
                # ))
              ))),
            
            card(
              fill = FALSE,
              card_header("The Right to Health and the Universal Periodic Review"),
              
              card_body(
                
                # 1. The Wrapper: This is a standard div that contains the floating image and text.
                div(
                  style = "overflow: hidden;",
                  
                  # 2. The Floated Clickable Image (using R actionLink)
                  # We put the actionLink here, and use R's tag$div to wrap the image 
                  # and apply the float styles.
                  tags$div(
                    class = "image-float-wrapper",
                    style = "float: left; max-width: 30%; height: auto; margin: 0px 15px 0px 0px;", # Apply float styles here
                    actionLink(
                      inputId = "upr_image_expand", # This ID triggers the modal in the server
                      label = img(
                        src = "WHO_UPR.png",
                        style = "height: auto; width: 100%; object-fit: contain; cursor: pointer;"
                      )
                    ),
                    tags$figcaption(
                      HTML("<small><a href='https://iris.who.int/handle/10665/277114' target='_blank'>Image: WHO</a>  
                                          More than 90,000 recommendations have been issued during the first three cycles of the UPR.</small>"),
                      style = "text-align: center; color: #666; font-size: 0.8em; margin-top: 5px;"
                    )
                  ),
                  
                  # 3. All Text Content (using HTML to ensure it wraps the floated div)
                  HTML(
                    "
        <p>This platform presents data on the Right to Health within the context of the <a href='https://www.ohchr.org/en/hr-bodies/upr/basic-facts' target='_blank'><b>Universal Periodic Review (UPR)</b></a>. This <b>State-led mechanism</b> evaluates each state’s human rights obligations and commitments. The review process is participatory and includes interactive discussions during which any UN Member State may issue recommendations to the State under review, which may then choose to ‘support’ or ‘note’ those recommendations.</p>
        <p>Contact us at info[at]cehdi.org for more information or to give feedback.</p>
        "
                  )
                )
              )
            )
  ),
  
  ### UPR Impact ------------------
  nav_panel(title = "UPR impact", icon = icon("square-poll-vertical"),
            card(
              fill = FALSE,
              card_header("Do UPR recommendations impact health outcomes? (a preliminary analysis)"),
              
              card_body(
                
                # 1. The Wrapper: This is a standard div that contains the floating image and text.
                div(
                  style = "overflow: hidden;",
                  
                  # 2. The Floated Clickable Image (using R actionLink)
                  # We put the actionLink here, and use R's tag$div to wrap the image 
                  # and apply the float styles.
                  tags$div(
                    class = "image-float-wrapper",
                    style = "float: left; max-width: 40%; height: auto; margin: 5px 5px 5px 5px;", # Apply float styles here
                    actionLink(
                      inputId = "upr_analysis", # This ID triggers the modal in the server
                      label = img(
                        src = "full_plot.png",
                        style = "height: auto; width: 100%; object-fit: contain; cursor: pointer;"
                      )
                    )
                  ),
                  
                  # 3. All Text Content (using HTML to ensure it wraps the floated div)
                  HTML(
                    "
        <p>We investigated the potential relationship between <a href='https://www.ohchr.org/en/hr-bodies/upr/basic-facts' target='_blank'>Universal Periodic Review (UPR)</a> recommendations and health outcomes, using maternal mortality as a key indicator. Specifically, we examined whether supporting UPR recommendations on maternal health from the first three cycles was associated with changes in the maternal mortality ratio (MMR) across countries.</p>
        <p>Our preliminary analysis indicates that countries with a higher proportion of accepted recommendations, as well as higher  number of UPR recommendations related to maternal health, show a significant correlation with reductions in MMR over time.</p>
        <br>
        <h3>Summary of methodology</h3>
                <b>Identification of health-related recommendations and thematic classification</b>
        <p>We first developed a rule-based text classification algorithm to identify health-related themes from all available UPR recommendations (obtained from the <a href='https://uhri.ohchr.org/en/our-data-api' target='_blank'>Universal Human Rights Index</a>). We used a <a href='https://iris.who.int/handle/10665/277114' target='_blank'>2019 report from the WHO</a> to further classify the recommendations into thematic groups. For each thematic group, a dictionary of keywords and term combinations was developed and matched against the recommendation text. A single recommendation could match several thematic groups.
        
        <p>The “Maternal health” thematic category including the following  dictionary of keywords: \"obstetric,\" \"prenatal,\" \"postnatal,\" \"miscarriage,\" and \"maternal mortality.\" The algorithm also identified recommendations containing specific combinations of terms linking pregnancy to healthcare access (e.g., \"pregnant\" appearing in conjunction with \"healthcare,\" \"medical care,\" or \"free access\"). Recommendations primarily addressing abortion were conditionally classified as maternal health if they explicitly contextualized abortion access within the framework of saving the woman’s life or preserving her physical health. Linguistic false positives were explicitly excluded from the matches.</p>

        <b>Statistical analysis</b>
        <p>Using a linear mixed-effects model to account for individual country trends, we estimated trends in MMR over time as a function of engagement with UPR recommendations related to maternal health. We specifically tested for a three-way interaction between time, the number of recommendations received, and the proportion of recommendations supported, which allowed us to observe whether engagement with the UPR process was associated with faster rates of MMR reduction.</p>
        <p>We visualized these results by comparing the predicted rates of decline for countries with varying levels of recommendation volume (e.g. 5 vs. 15 recommendations) and with varying levels of recommendation acceptance (e.g., 50% vs. 90% support). Pairwise slope comparisons were calculated to assess the statistical differences. All analyses were performed using R.</p>

        <b>Summary of Results</b>
        <p>We found that a country's support of UPR recommendations on maternal health was associated with a faster decline in MMR. This effect was much stronger when a country received a higher number of the recommendations. When a country received only 5 recommendations (left panel of the figure above), the difference in the rate of MMR reduction between supporting 90% and supporting 50% was minimal but still statistically significant (p = 0.042). However, when a country received 15 recommendations (right panel), supporting 90% of the recommendations was associated with a significantly faster reduction in MMR over time compared to supporting only 50% (<b>p = 0.013</b>).</p>
        <b>Conclusion</b>
        <p>This preliminary analysis suggests that the UPR process may have impact in contributing towards positive health outcomes as demonstrated by the relationship between the UPR recommendations pertaining to maternal health and reduction of MMR over time.</p>
        <p>It is important to note that these results should be interpreted with caution, as this analysis cannot establish causality. Nevertheless, it signifies a potentially important role of engagement with the UPR process and its associated peer review process in enhancing political support and attention for critical health challenges.</p>
        "
                  )
                )
              )
            )
  ),
  ### UPR recommendations ----------------
  nav_menu(title = "UPR recommendations", icon = icon("people-arrows"),
           #### UPR: Regional -----------------------
           nav_panel(title = "By Region", icon = icon("globe-africa"),
                     markdown("Click on a bar chart to view the text of the relevent UPR recommendations"),
                     layout_column_wrap(
                       style = css(grid_template_columns = "1fr 1fr"),
                       navset_card_tab(
                         full_screen = TRUE,
                         # title = "Regional Recommendation Themes",
                         nav_panel("All Recommendations by Region", 
                                   card(
                                     # fill = FALSE,
                                     card_body(
                                       min_height = 450,
                                       plotlyOutput("plotly_UPR_regional")
                                     ),
                                     card_footer(
                                       downloadButton(
                                         outputId = "download_plotly_UPR_regional",
                                         label = "Download as PNG"
                                       )
                                     )
                                     # card(
                                     #   fill = FALSE,
                                     #   card_body(
                                     #     plotOutput("upr_themes_all_global"
                                     #                ,width = paste0(upr_width,"px")
                                     #                ,height =  paste0(upr_height,"px")
                                     #     )),
                                     #   card_footer(
                                     #     downloadButton(
                                     #       outputId = "download_upr_themes_all_global",
                                     #       label = "Download as PNG"
                                     #     )
                                     #   )
                                   )),
                         nav_panel("Per UPR Cycle", 
                                   card(
                                     # fill=FALSE,
                                     # card_body(plotOutput("upr_themes_cycle_global", 
                                     #                      width = paste0(upr_width*1.05,"px"),
                                     #                      height =  paste0(upr_height*1.6,"px")
                                     # ))
                                     card_body(
                                       min_height = 550,
                                       plotlyOutput("plotly_UPR_regional_cycle")
                                     )
                                   )
                         ),
                         nav_panel("Health-Related Recommendations",
                           card(
                                full_screen = TRUE,
                                fill = FALSE,
                                card_body(plotOutput("global_plot"))
                           )
                         ),
                         nav_panel("Recommending states",
                                   card(
                                     # fill = FALSE,
                                     full_screen = TRUE,
                                     # card_header("Recommending States (top 20)"),
                                     card_body(
                                       # markdown("(Themes of maternal health, family planning, and abortion)"),
                                       plotlyOutput("plotly_UPR_regional_recommending")
                                       # plotOutput("recommending_states_REGION")
                                       )
                                   )
                         )
                       ),
                       layout_column_wrap(
                         card(
                           full_screen = TRUE,
                           fill=TRUE,
                           card_body(DTOutput("plotly_table_regional"))
                         )
                       )
                       # layout_column_wrap(
                       #   width=1,
                       #   # This sets a 3:2 height ratio
                       #   style = css(grid_template_rows = "3fr 1fr"),
                       #   card(
                       #     # fill = FALSE,
                       #     full_screen = TRUE,
                       #     card_header("Recommending States (top 20)"),
                       #     card_body(
                       #       # markdown("(Themes of maternal health, family planning, and abortion)"),
                       #       plotOutput("recommending_states_REGION"))
                       #   ),
                       #   # card(
                       #   #   full_screen = TRUE,
                       #   #   card_header("Health-Related Recommendations"),
                       #   #   card_body(plotOutput("global_plot"))
                       #   # ),
                       #   card(
                       #     full_screen = TRUE,
                       #     # card_header("Regional map"),
                       #     card_body(plotOutput("regional_map"))
                       #   )
                       # )
                     )
           ),
           
           ## UPR - SuR2 ----------------------------------------------------------
           nav_panel(title = "By State", icon = icon("flag"),
                     markdown("Click on a bar chart to view the text of the relevent UPR recommendations"),
                     layout_column_wrap(
                       style = css(grid_template_columns = "1fr 1fr"),
                       navset_card_tab(
                         full_screen = TRUE,
                         # title = "Regional Recommendation Themes",
                         nav_panel("All Recommendations by Region", 
                                   card(
                                     # fill = FALSE,
                                     card_body(
                                       min_height = 450,
                                       plotlyOutput("plotly_UPR_SUR")
                                     ),
                                     card_footer(
                                       downloadButton(
                                         outputId = "download_plotly_UPR_SUR",
                                         label = "Download as PNG"
                                       )
                                     )
                                     # card(
                                     #   fill = FALSE,
                                     #   card_body(
                                     #     plotOutput("upr_themes_all_global"
                                     #                ,width = paste0(upr_width,"px")
                                     #                ,height =  paste0(upr_height,"px")
                                     #     )),
                                     #   card_footer(
                                     #     downloadButton(
                                     #       outputId = "download_upr_themes_all_global",
                                     #       label = "Download as PNG"
                                     #     )
                                     #   )
                                   )),
                         nav_panel("Per UPR Cycle", 
                                   card(
                                     # fill=FALSE,
                                     # card_body(plotOutput("upr_themes_cycle_global", 
                                     #                      width = paste0(upr_width*1.05,"px"),
                                     #                      height =  paste0(upr_height*1.6,"px")
                                     # ))
                                     card_body(
                                       min_height = 550,
                                       plotlyOutput("plotly_UPR_SUR_cycle")
                                     )
                                   )
                         ),
                         nav_panel("Health-Related Recommendations",
                                   card(
                                     full_screen = TRUE,
                                     fill = FALSE,
                                     card_body(plotOutput("plot")),
                                     card_footer(
                                           downloadButton(
                                             outputId = "download_rec_plot_object",
                                             label = "Download as PNG"
                                           )
                                         )
                                   )
                         ),
                         nav_panel("Data Table", 
                                   card(fill=TRUE,
                                        card_body(DTOutput("DT_table")),
                                        card_footer(
                                          downloadButton(
                                            outputId = "download_data_csv",
                                            label = "State data (csv)"
                                          ),
                                          downloadButton(
                                            outputId = "download_data_xlsx",
                                            label = "State data (xlsx)"
                                          ),
                                          downloadButton(
                                            outputId = "download_data_csv_all",
                                            label = "All data (csv)"
                                          )
                                        )
                                   )),
                         nav_panel("Recommending states",
                                   card(
                                     fill = FALSE,
                                     full_screen = TRUE,
                                     # card_header("Recommending States (top 20)"),
                                     card_body(
                                       # markdown("(Themes of maternal health, family planning, and abortion)"),
                                       plotlyOutput("plotly_UPR_SUR_recommending")
                                       # plotOutput("recommending_states_REGION")
                                     )
                                   )
                         )
                       ),
                       layout_column_wrap(
                         card(
                           full_screen = TRUE,
                           fill=TRUE,
                           card_body(DTOutput("plotly_table_SUR"))
                         )
                       )
                       # layout_column_wrap(
                       #   width=1,
                       #   # This sets a 3:2 height ratio
                       #   style = css(grid_template_rows = "3fr 1fr"),
                       #   card(
                       #     # fill = FALSE,
                       #     full_screen = TRUE,
                       #     card_header("Recommending States (top 20)"),
                       #     card_body(
                       #       # markdown("(Themes of maternal health, family planning, and abortion)"),
                       #       plotOutput("recommending_states_REGION"))
                       #   ),
                       #   # card(
                       #   #   full_screen = TRUE,
                       #   #   card_header("Health-Related Recommendations"),
                       #   #   card_body(plotOutput("global_plot"))
                       #   # ),
                       #   card(
                       #     full_screen = TRUE,
                       #     # card_header("Regional map"),
                       #     card_body(plotOutput("regional_map"))
                       #   )
                       # )
                     )
           )
  ),
  
  ### UHC ---------------------
  nav_panel(title = "UHC", icon = icon("umbrella"),
            markdown("UHC: Universal Health Coverage"),
            layout_column_wrap(
              layout_column_wrap(
                width=1,
                card(full_screen = TRUE,card_header("UHC Service Coverage Index (2021)"), plotOutput("UHC_map")),
                card(full_screen = TRUE,card_header("UHC sub-index on RMNCH (2021)"), plotOutput("UHC_RMNCH_map"))
              ),
              card(full_screen = TRUE, card_header("UHC indices over time"), plotOutput("UHC_trend"))
            )
  ),
  ### Maternal health ------------------------------
  nav_menu(title = "Maternal health", icon = icon("person-pregnant"),
           #### Maternal mortality -----------------------
           nav_panel(title = "Maternal Mortality", icon = icon("house-medical"),
                     "Maternal Mortality Ratio (MMR): Number of maternal deaths per 100,000 live births.",
                     layout_column_wrap(
                       full_screen = TRUE,
                       style = css(grid_template_columns = "1fr 1fr"),
                       layout_column_wrap(
                         width=1,
                         style = css(grid_template_rows = "6fr 5fr"),
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
           
           #### Skilled birth attendance ------------------------
           nav_panel(title = "Skilled birth attendance", icon=icon("user-nurse"),
                     layout_column_wrap(
                       layout_column_wrap(
                         width=1,
                         card(
                           full_screen = TRUE,
                           card_header("Births attended by skilled health personnel")
                           ,plotOutput("skilled_birth")
                         ),
                         card(
                           full_screen = TRUE,
                           card_header("Trends vs. Neighbors"), 
                           plotOutput("skilled_birth_plot_neighbors")
                         )
                       ),
                       layout_column_wrap(
                         width=1,
                         card(
                           full_screen = TRUE,
                           card_header("Proportion of births delivered in a health facility")
                           ,plotOutput("births_facility")
                         ),
                         card(
                           full_screen = TRUE,
                           card_header("Trends vs. Neighbors"), 
                           plotOutput("births_facility_plot_neighbors")
                         )
                       )
                     )
           ),
           
           #### Abortion ------------------------
           nav_panel(title = "Abortion", icon = icon("prescription-bottle-medical"),
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
           )
  ),
  ## Family planning ------------------
  nav_panel(title = "Family planning", icon = icon("people-group"),
            # "Family planning",
            layout_column_wrap(
              full_screen = TRUE,
              style = css(grid_template_columns = "1fr 1fr"),
              card(
                fill = FALSE,
                full_screen = TRUE,
                card_header("Met Need for Family planning (%)"),
                plotOutput("family_planning")
              )
            )
  ),
  
  ## Constitutions  ------------------
  nav_panel(title = "Constitutions", icon = icon("people-group"),
            # "Family planning",
            layout_column_wrap(
              full_screen = TRUE,
              style = css(grid_template_columns = "1fr"),
              card(
                # fill = FALSE,
                full_screen = TRUE,
                card_header("Does the constitution explicitly guarantee citizens’ right to health? (as of June 2024)"),
                plotOutput("constitution_const_health")
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
  
  ## Plot width expressions -----------
  # This reactive expression captures the real-time width of our plot container.
  # It's debounced to wait 500ms after a resize before updating.
  plot_width_mmr_neighbors <- reactive({
    session$clientData$output_mmr_time_plot_neighbors_width
  }) |> debounce(500)
  
  plot_width_skilled_birth_neighbors <- reactive({
    session$clientData$output_skilled_birth_plot_neighbors_width
  }) |> debounce(500)
  
  plot_width_births_facility_neighbors <- reactive({
    session$clientData$output_births_facility_plot_neighbors_width
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
      # title = "Graphical overview of the UPR process",
      img(src = "WHO_UPR.png", style = "width: 100%"),
      size = "xl",           # Make the modal large
      easyClose = TRUE,     # Allow closing by clicking outside
      footer = NULL         # Remove the default buttons
    ))
  })
  
  observeEvent(input$upr_analysis, {
    showModal(modalDialog(
      # title = "Trend analysis of States’ maternal mortality ratios (MMR) over time compared with engagement with UPR recommendations related to maternal health",
      img(src = "full_plot.png", style = "width: 100%"),
      size = "l",           # Make the modal large
      easyClose = TRUE,     # Allow closing by clicking outside
      footer = NULL         # Remove the default buttons
    ))
  })
  
  ## Reactive Expressions for Data Filtering ---------------------------------
  state_geo_reactive <- reactive({
    if (input$selected_regional_grouping == "Sub-regions (UN M49)") {
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
    choices <- region_selection() |> 
      # Remove non-member states (no data, causes crash)
      filter(!country %in% c("Western Sahara","Greenland" ,
                             # "Palestine",
                             "Vatican",
                             "Siberian Artifact")) |>  
      select(country) |> distinct() |> arrange(country) |> 
      pull(country)
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
  
  SUR_WHOregion <- reactive({
    req(input$selected_SUR)
    state_geo_reactive()[state_geo_reactive()$country == input$selected_SUR, ]$WHO_region
  })
  
  SUR_subregion <- reactive({
    req(input$selected_SUR)
    state_geo_reactive()[state_geo_reactive()$country == input$selected_SUR, ]$subregion
  })
  
  sur_area <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |>
      filter(country %in% c(input$selected_SUR)) |>
      st_area() |> as.numeric()
  })
  
  bbox_selected_SUR <- reactive({
    req(input$selected_SUR)
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo_reactive() |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo_reactive()}) |>
      filter(country %in% c(input$selected_SUR)) |>
      st_bbox()
  })
  
  bbox_SUR_region <- reactive({
    req(SUR_region())
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo_reactive() |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo_reactive()}) |>
      filter(region_dashboard %in% c(SUR_region())) |>
      st_bbox()
  })
  
  bbox_SUR_region_dynamic <- reactive({
    req(SUR_region())
    if(input$selected_regional_grouping == "Global"){
      (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
        state_geo |> 
          st_difference(polygon_shift) |> 
          st_shift_longitude()} else{state_geo}) |>
        filter(WHO_region %in% c(SUR_WHOregion())) |>
        st_bbox()} else{
          (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
            state_geo_reactive() |> 
              st_difference(polygon_shift) |> 
              st_shift_longitude()} else{state_geo_reactive()}) |>
            filter(region_dashboard %in% c(SUR_region())) |>
            st_bbox()}
  })
  
  bbox_SUR_subregion <- reactive({
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo}) |>
      filter(subregion %in% c(SUR_subregion())) |>
      st_bbox()
  })
  
  bbox_SUR_WHOregion <- reactive({
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo}) |>
      filter(WHO_region %in% c(SUR_WHOregion())) |>
      st_bbox()
  })
  
  
  ## PDF Country profile -------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("CeHDI-Profile-", input$selected_SUR, ".pdf")
    },
    
    content = function(file) {
      withProgress(message = 'Generating your report', value = 0, {
        
        # incProgress(0.1, detail = "--> Preparing template")
        
        # Logic to dynamically get flag image. Flags downloaded from:
        # https://stefangabos.github.io/world_countries/#custom-download
        country_iso2 <- state_geo |> 
          filter(country == input$selected_SUR) |> 
          mutate(iso2=tolower(iso2)) |> 
          pull(iso2) |> 
          unique()
        flag_filename <- paste0(country_iso2, ".png")
        source_flag <- here::here("flags", flag_filename)
        
        # Create a temporary directory for Quarto to work in.
        temp_dir <- tempdir()
        
        # Copy Rmd and other relevant files into that directory.
        temp_report_path <- file.path(temp_dir, "report-template.Rmd")
        file.copy("report-template.Rmd", temp_report_path, overwrite = TRUE)
        # temp_report_path <- file.path(temp_dir, "report-template-2.Rmd")
        # file.copy("report-template-2.Rmd", temp_report_path, overwrite = TRUE)
        file.copy("preamble.tex", temp_dir, overwrite = TRUE)
        file.copy("logo.png", temp_dir, overwrite = TRUE)
        file.copy("logo2.png", temp_dir, overwrite = TRUE)
        file.copy(here("www", "WHO_UPR_nobg.png"), temp_dir, overwrite = TRUE)
        file.copy(here("www", "WHO_UPR-removebg3.png"), temp_dir, overwrite = TRUE)
        file.copy(source_flag, temp_dir, overwrite = TRUE)
        file.rename(file.path(temp_dir, flag_filename), 
                    file.path(temp_dir, "countryflag.png"))
        
        incProgress(0.6, detail = "--> Rendering PDF (this may take a moment)")
        # Render the document inside the temporary directory.
        rmarkdown::render(
          input = temp_report_path,
          # output_file = file,
          output_dir = temp_dir,
          output_file = "report.pdf",
          params = list(
            country_name = input$selected_SUR,
            upr_all = upr_themes_all_object(),
            rec_plot = rec_plot_object(),
            mmr_map_plot = mmr_map_object(),
            bbox_selected_SUR = bbox_selected_SUR(),
            sur_area = sur_area(),
            bbox_SUR_region_dynamic = bbox_SUR_region_dynamic()
          ),
          envir = new.env(parent = globalenv())
        )
        
        # Copy the generated PDF from the temporary directory to the final
        #    download path that Shiny expects.
        file.copy(file.path(temp_dir, "report.pdf"), file, overwrite = TRUE)
        
        incProgress(1, detail = "Done!")
      })
    }
  )
  
  ## MAPS (from original sidebar) ------------------------------------------
  output$global_map <- renderPlot({
    req(input$selected_SUR)
    p1 <- state_geo_reactive() |>
      mutate(selected_sur = factor(case_when(country == input$selected_SUR ~ input$selected_SUR,
                                             region_dashboard == input$selected_region ~ "Region",
                                             .default = "Other"),
                                   levels = c(input$selected_SUR,"Region", "Other"))) |>
      ggplot(aes(geometry = polygon, color = selected_sur, fill = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_color_manual(values = c("tomato2", "grey60", "grey85")) +
      scale_linewidth_manual(values = c(0.8,0.3, 0.3)) +
      scale_fill_manual(values = c("tomato2","grey70", "grey95")) +
      scale_alpha_manual(values = c(1,1,0.3))+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
      ) +
      labs(
        title = NULL,
        fill = NULL,
        color = NULL, lwd = NULL, alpha=NULL
      ) +
      guides(
        fill = "none", lwd = "none", color = "none", alpha="none"
      )
    
    if (sur_area() > 10^11) {
      p2 <- p1
    } else {
      p2 <- p1 + geom_rect(
        aes(
          xmin = bbox_selected_SUR()["xmin"] - 2,
          xmax = bbox_selected_SUR()["xmax"] + 2,
          ymin = bbox_selected_SUR()["ymin"] - 2,
          ymax = bbox_selected_SUR()["ymax"] + 2
        ),
        fill = "transparent",
        color = "tomato2",
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
        plot.background = element_rect(fill="transparent", color = NA), 
        panel.background = element_blank(),
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
  
  ### Plotly ---------------------
  
  #### All  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_regional_object <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    
    a_1 <- 
      filtered_upr_region() |>    
      # filter(state_under_review == chosen_country) |> 
      select(text_2, cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- 
      filtered_upr_region() |>    
      # filter(state_under_review == chosen_country) |> 
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n_other"
      )
    
    a <- left_join(a_1, a_2) |>
      mutate(response_upr = fct_relevel(response_upr, "Noted")) |> 
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
        n_sup = case_when(n_tot_theme == 0 ~ "", .default = n_sup)
      ) |>
      ungroup() |>
      filter(!theme %in% c("health_related", "TB_malaria", "NTD")) |>
      left_join(theme_labels, by = c("theme" = "variable")) |>
      arrange(-n_tot_theme) |>
      mutate(
        theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label),
        theme_label = fct_inorder(theme_label)
      ) |> 
      filter(!theme %in% c(
        "SRHR", "health_related", "SOCED",
        "essential_medicines","TB_malaria", "NTD","vaccinations"
      ))
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    
    a |>
      ggplot(aes(
        x = perc, y = fct_rev(theme_label), 
        customdata = paste(theme_label, response_upr, sep = "|"),
        text = paste0(response_upr, ": n = ", n, " ", n_sup,"\n(click to view text of recommendations)")
      )) +
      geom_col(aes(fill = response_upr), alpha = 0.8, width = 0.85) +
      scale_fill_manual(values = c("#ec5557", "#1c164d"))+
      labs(
        x = paste0(
          "% of all recommendations", 
          "\n",
          "(Total N = ", format(nrow(filtered_upr_region()), big.mark = ","), ")"
        ),
        y = NULL,
        fill = NULL
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        # limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(clip = "off")+
      guides(fill=guide_legend(reverse=T))+
      theme(
        # plot.margin = margin(l=0,t=2,b=1, r = 2, unit = "pt"),
        legend.position = c(0.9, 0.1),
        legend.justification = c("right", "bottom"),
        legend.margin = margin(0,0,0,0),
        legend.frame = element_blank(),
        legend.text = element_text(size = 9, color = "#1c164d"),
        legend.title = element_text(size = 11, color = "#1c164d"),
        legend.background = element_blank(),
        legend.key.size = unit(10,"pt"),
        axis.text.y = element_text(size = 9, color = "#1c164d"),
        axis.text.x = element_text(size = 10, color = "#1c164d", angle=30),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#1c164d"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "#1c164d", hjust = 0.5),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        plot.caption = element_text(color = "#1c164d"),
        plot.background = element_rect(color = "#1c164d", fill = NA),
        panel.background = element_blank()
      )
  })
  
  ##### Plot output --------------------
  output$plotly_UPR_regional <- renderPlotly({
    # ggplotly(plotly_UPR_regional_object(),
    #          tooltip = c("text"),
    #          source = "click")
    
    # 1. Get the ggplot object from your reactive
    p <- plotly_UPR_regional_object()
    
    # 2. Extract the title and replace newline "\n" with HTML <br>
    #    This is how Plotly handles multi-line titles.
    title_text <- input$selected_region
    
    # 3. Convert to plotly, then pipe to plotly::layout() to fix formatting
    ggplotly(
      p,
      tooltip = c("text"),
      source = "click"
    ) |>
      plotly::layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        # 4. Fix the legend position
        legend = list(
          traceorder = "reversed",
          x = 0.99,
          y = 0.01,
          xanchor = 'right',
          yanchor = 'bottom',
          bgcolor = 'rgba(0,0,0,0)', # Transparent background
          bordercolor = 'rgba(0,0,0,0)'
        ),
        
        # 5. Fix the title alignment and text
        title = list(
          text = title_text,
          automargin = TRUE,
          x = 0               # Center the title (0 = left, 0.5 = center, 1 = right)
        ),
        
        # 6. X axis
        xaxis = list(
          automargin = TRUE # Automatically creates space for the title
        ),
        
        # 7. Manually set margins
        margin = list(
          l = 0, # Left margin
          r = 40, # Right margin
          b = 0, # Bottom margin (for the long x-axis title)
          t = 30  # Top margin (for the plot title)
        )
      )
  })
  
  ##### Plot downloader -------------------
  output$download_plotly_UPR_regional <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("health-recommendations-", input$selected_region, ".png")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      ggsave(
        file,
        plot = plotly_UPR_regional_object()+
          theme(
            plot.background = element_rect(color = "#1c164d", fill = "#F9F9F6"),
            panel.background = element_rect(color = NA, fill="#F9F9F6")
          )+
          scale_x_continuous(
            # labels = function(x) paste0(x, "%"),
            # limits = c(0, max_a + 2),
            expand = expansion(mult = c(0, 0.2))
          ) +
          labs(
            title = paste0("Health-related recommendations of the UPR"
                           , "\n"
                           , input$selected_region),
            caption = "*Numbers after the bars indicate N (% supported)"
          )+
          geom_text(
            data = plotly_UPR_regional_object()@data |> filter(response_upr == "Supported"),
            aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
            hjust = -0.05,
            size = 3, color = "#1c164d"
            # vjust = 0.25
          )
        ,width = 7,
        height = 5,
        dpi = 400,
        units = "in"
      )
    }
  )
  
  #### Cycles  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_regional_cycle_object <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    
    a_1<- filtered_upr_region() |> 
      # filter(cycle != "Cycle 4") |> 
      # filter(state_under_review == "Nigeria") |> 
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |> 
      # mutate(across(c(health_related:other_health_related), ~ .x != "Other")) |> 
      group_by(cycle, response_upr) |> 
      summarise(across(c(health_related:other_health_related), ~ sum(.x !="Other"))) |> 
      ungroup() |> 
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(cols = health_related:other_health_related, 
                   names_to = "theme", 
                   values_to = "n"
      )
    
    a_2<- filtered_upr_region() |> 
      # filter(cycle != "Cycle 4") |> 
      # filter(state_under_review == "Nigeria") |> 
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |> 
      # mutate(across(c(health_related:other_health_related), ~ .x != "Other")) |> 
      group_by(cycle, response_upr) |> 
      summarise(across(c(health_related:other_health_related), ~ sum(.x =="Other"))) |> 
      ungroup() |> 
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(cols = health_related:other_health_related, 
                   names_to = "theme", 
                   values_to = "n_other"
      )
    
    a_3<- filtered_upr_region() |> 
      # filter(cycle != "Cycle 4") |> 
      # filter(state_under_review == "Nigeria") |> 
      group_by(cycle) |> 
      summarise(health_n = sum(health_related !="Other")) |> 
      ungroup()
    
    a <- left_join(a_1,a_2) |> 
      left_join(a_3) |> 
      mutate(cycle2 = fct_recode(cycle, "1"="Cycle 1", "2"="Cycle 2", "3"="Cycle 3", "4"="Cycle 4")) |> 
      group_by(cycle2, theme) |> 
      mutate(n_tot = sum(n)+sum(n_other)) |> 
      mutate(n_tot_theme = sum(n)) |> 
      mutate(perc = n/n_tot*100,
             perc_theme = n_tot_theme/n_tot*100,
             theme_perc_health = n_tot_theme/health_n*100) |> 
      group_by(cycle2, theme) |> 
      mutate(n_sup = paste0("(", sprintf("%1.0f", n/sum(n)*100), "%)"),
             n_sup = case_when(n_tot_theme == 0 ~ "(NA)", .default = n_sup)) |> 
      # mutate(n_sup = case_when(
      #   response_upr == "Noted/Other" ~ "",
      #   response_upr == "Supported" ~ paste0("(", sprintf("%1.0f", n/sum(n)*100), "%)"),
      #   .default = NA
      # )) |> 
      ungroup() |> 
      # filter(!theme %in% c(
      #   "health_related"
      #   # , "abortion"
      #   , "TB_malaria", "NTD"
      #   # , "TB_malaria_NTD"
      # )) |> 
      filter(!theme %in% c(
        "SRHR", "health_related", "SOCED",
        "essential_medicines","TB_malaria", "NTD","vaccinations"
      )) |> 
      left_join(theme_labels, join_by(theme == variable)) |> 
      group_by(theme) |> 
      mutate(perc_tot = sum(n_tot_theme)) |> 
      ungroup() |> 
      arrange(
        # cycle2, 
        -perc_tot
        # -n_tot_theme
      ) |> 
      mutate(
        theme_label = fct_inorder(theme_label)
      )
    
    max_a <- max(a$perc_theme)
    a |> 
      # mutate(n_tot_theme = case_when(response_upr!="Supported" ~ "", 
      #                                .default = as.character(n_tot_theme))) |> 
      ggplot(aes(x = perc, y = fct_rev(cycle2), 
                 customdata = paste(theme_label, response_upr, cycle, sep = "|"),
                 text = paste0(cycle, " - ", response_upr,  ": n = ", n, " ", n_sup,"\n(click to view text of recommendations)")
                 ))+
      geom_col(aes(fill = response_upr), alpha = 0.8, width = 0.95)+
      facet_grid(
        rows = vars(theme_label), switch = "y"
        # ,labeller = labeller(theme_label = label_wrap_gen(30))
      )+
      labs(x = "% of all recommendations in cycle", y = NULL,
           fill = "State's response",
           # title = "Health-related recommendations of the UPR",
           caption = "*Numbers after the bars indicate N (% supported)")+
      # theme_bw()+
      theme_classic()+
      # scale_y_discrete(expand = c(0.1, 0))+
      scale_fill_manual(values = c("#ec5557", "#1c164d"))+
      scale_x_continuous(labels = function(x) paste0(x, "%"), 
                         # limits = c(-0.35,max_a+1), 
                         limits = c(0,max_a+1), 
                         # sec.axis = dup_axis(name = NULL),
                         expand = expansion(mult = c(0, 0.05)) # 0 exactly on axis
      )+
      # geom_text(
      #   data = distinct(a, theme_label, cycle2), 
      #   aes(label = cycle2, x = -0.25, y = fct_rev(cycle2)), # x=0.5 puts it slightly inside the bar
      #   hjust = 0,                   # Align text to the left
      #   color = "#1c164d",             # White text for contrast against dark bars
      #   size = 2.4,
      #   inherit.aes = FALSE
      # )+
      guides(fill=guide_legend(reverse=T))+
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.75,0.5),
        legend.justification = c("right", "center"),
        legend.frame = element_rect(color = "black"),
        # axis.text.y = element_text(size = 6, margin = margin(l=0, r=-10)), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 0.5
                                         , color = "#1c164d"
                                         #, size = 8
        ),
        # strip.background = element_blank()
        panel.grid = element_blank(),
        panel.spacing = unit(0.01, "lines"),
        # plot.background = element_blank(),
        plot.background = element_rect(color = "#1c164d", fill = NA),
        panel.background = element_blank()
      )
  })
  
  ##### Plot output --------------------
  output$plotly_UPR_regional_cycle <- renderPlotly({
    # ggplotly(plotly_UPR_regional_object(),
    #          tooltip = c("text"),
    #          source = "click")
    
    # 2. Extract the title and replace newline "\n" with HTML <br>
    #    This is how Plotly handles multi-line titles.
    title_text <- input$selected_region
    
    # 3. Convert to plotly, then pipe to plotly::layout() to fix formatting
    # 1. Create the base interactive plot
    fig <- ggplotly(
      plotly_UPR_regional_cycle_object(),
      source = "click",
      tooltip = c("text") # Ensure this matches your aesthetic mapping
    ) 
    
    # 2. The Fix: Manually move the strip labels
    # We iterate through the layout annotations. If we find a vertical label (the strip),
    # we rotate it to 0 (horizontal) and move its X coordinate to the left.
    
    fig$x$layout$annotations <- lapply(fig$x$layout$annotations, function(a) {
      # ggplotly renders side strips with an angle (usually -90 or 90)
      # We catch those and modify them
      if (!is.null(a$textangle) && a$textangle != 0) {
        a$textangle <- 0       # Make text horizontal
        a$x <- -0.01           # Move to Left (Adjust this value based on label length)
        a$xanchor <- "right"   # Align text against the axis
        a$yanchor <- "middle"  # Center vertically
        a$align <- "center"
        # Border
        # a$bordercolor <- "#1c164d" # Match your theme color (or use "black")
        # a$borderwidth <- 0.2         # Thickness of the line
        # a$borderpad   <- 0         # Padding inside the box (crucial for looks)
        # a$bgcolor     <- "white"   # Optional: ensures the line behind doesn't show through
      }
      return(a)
    })
    
    # 3. Final Layout Adjustments
    # We must increase the left margin (l) so the moved labels don't get cut off.
    fig |> 
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 270), # Increase 'l' until your longest label fits
        legend = list(
          traceorder = "reversed",
          x = 0.99,
          y = 0.01,
          xanchor = 'right',
          yanchor = 'bottom',
          bgcolor = 'rgba(0,0,0,0)', # Transparent background
          bordercolor = 'rgba(0,0,0,0)'
        ),
        title = list(
          text = title_text,
          automargin = TRUE,
          x = 0               # Center the title (0 = left, 0.5 = center, 1 = right)
        ),
        xaxis = list(
          automargin = TRUE # Automatically creates space for the title
        )
      )
  })
  
  #### Recommending States  ---------------

  ##### Plot object -------------------
  plotly_UPR_regional_recommending_object <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    
    upr_rec_countries <- filtered_upr_region() |>
      filter(!is.na(recommending_state_upr)) |> 
      filter(response_upr == "Supported") |> 
      filter(if_any(any_of(c(theme_labels$variable)), ~ .x != "Other")) |> 
      # select(cycle, recommending_state_upr) |> 
      separate_longer_delim(cols = c(recommending_state_upr), delim="-") |> 
      mutate(recommending_state_upr = str_trim(recommending_state_upr)) |> 
      group_by(cycle, recommending_state_upr) |> count(across(any_of(theme_labels$variable))) |> 
      pivot_longer(cols = !c(cycle, recommending_state_upr, n),
                   names_to = "variable",
                   values_to = "theme") |> 
      filter(theme!="Other") |> 
      left_join(theme_labels) |> 
      group_by(cycle, recommending_state_upr, variable) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct()
    
    c_plot <- upr_rec_countries |> 
      filter(variable %in% c("abortion", 
                             "maternal_health", 
                             "contraception",
                             "sexual_health",
                             "sexual_education"
                             )) |> 
      select(-cycle) |> 
      group_by(recommending_state_upr, theme) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct() |> 
      arrange(theme, -n) |> 
      mutate(recommending_state_upr = str_wrap(recommending_state_upr, 20)) |> 
      group_by(recommending_state_upr) |> 
      mutate(n_tot = sum(n)) |> 
      ungroup() |> 
      arrange(-n_tot) |> 
      ungroup()
    
    ccp <- c_plot |> select(recommending_state_upr, n_tot) |> distinct() |> 
      arrange(-n_tot) |>
      slice_head(n=20)
    
    c_plot |> 
      filter(recommending_state_upr %in% c(ccp |> pull(recommending_state_upr))) |> 
      ggplot(aes(x= reorder(recommending_state_upr, n_tot), y=n,fill=theme_label
                 ,customdata = paste(theme_label, "Supported", NA, recommending_state_upr, sep = "|"),
                 text = paste0(recommending_state_upr, " - ", theme_label,  ": n = ", n,"\n(click to view text of recommendations)")
                 ))+
      geom_col(alpha = 1, width = 0.8)+
      scale_fill_manual(values = c(
        "Maternal health" = "#8dd3c7",
        "Family planning" = "#fed9a6",
        "Abortion" = "#bebada",
        "Sexual health and wellbeing" = "#fb8072",
        "Sexual education" = "#80b1d3"
      ))+
      scale_y_continuous(expand = c(0, 0.1)) +
      tidytext::scale_x_reordered() +
      coord_flip()+
      theme_minimal() +
      guides(fill=guide_legend(reverse=TRUE))+
      theme(
        strip.text.y = element_text(angle = 270, face = "bold"),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.justification = c("left", "bottom"),
        legend.margin = margin(0,0,0,0), 
        legend.box.margin = margin(0,0,0,0), 
        legend.box.spacing = unit(0,"pt"),
        legend.background = element_blank(),
        plot.background = element_rect(color = "#1c164d", fill = NA),
        panel.background = element_blank()
        )+
      labs(y="Supported recommendations (N)", x=NULL,
           fill=NULL)

  })

  ##### Plot output --------------------
  output$plotly_UPR_regional_recommending <- renderPlotly({
    
    title_text <- input$selected_region
    
    
    p <- ggplotly(
      plotly_UPR_regional_recommending_object(),
      source = "click",
      tooltip = c("text") # Ensure this matches your aesthetic mapping
    ) |> 
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        # margin = list(l = 265), # Increase 'l' until your longest label fits
        legend = list(
          traceorder = "reversed",
          x = 0.99,
          y = 0.01,
          xanchor = 'right',
          yanchor = 'bottom',
          bgcolor = 'rgba(0,0,0,0)', # Transparent background
          bordercolor = 'rgba(0,0,0,0)'
        ),
        title = list(
          text = title_text,
          automargin = TRUE,
          x = 0               # Center the title (0 = left, 0.5 = center, 1 = right)
        ),
        margin = list(
          l = 0, # Left margin
          r = 0, # Right margin
          b = 0, # Bottom margin (for the long x-axis title)
          t = 30  # Top margin (for the plot title)
        )
      )
    p
  })
  
  #### Table -------------------
  
  output$plotly_table_regional <- renderDataTable({
    
    plot_data <- filtered_upr_region() |> 
      select(text_2, state_under_review, response_upr, cycle, health_related:other_health_related, document_code) |> 
      pivot_longer(cols = health_related:other_health_related) |> 
      mutate(
        value = case_when(is.na(value) ~ FALSE,
                          value == "Other" ~ FALSE,
                          value != "Other" ~ TRUE)
      ) |> 
      filter(value, name != "health_related") |>
      left_join(theme_labels, by = c("name" = "variable"))
    
    event.data <- event_data("plotly_click", source = "click")
    
    # if(is.null(event.data) == T) return(NULL)
    req(event.data)
    
    # Filter result via Data
    clicked_data_string <- event.data$customdata
    
    # 2. Split the customdata back into two parts
    clicked_info <- strsplit(clicked_data_string, "|", fixed = TRUE)[[1]]
    clicked_theme <- clicked_info[1]
    clicked_response <- clicked_info[2]
    clicked_cycle <- clicked_info[3]
    clicked_recommending <- clicked_info[4]
    
    if(is.na(clicked_cycle) | clicked_cycle == "NA"){
    res <- plot_data |> 
      filter(
        theme_label == clicked_theme, 
        response_upr == clicked_response
      ) |> 
      select(text_2, state_under_review, cycle, response_upr) |> 
      mutate(state_under_review = factor(state_under_review)) |> 
      rename(
        # !! paste0("Recommendation: ", clicked_theme) := text_2,
        `Recommendation text` = text_2,
        SUR = state_under_review, 
        Cycle = cycle, 
        Response = response_upr
      )
    } else{
      res <- plot_data |> 
        filter(
          theme_label == clicked_theme, 
          response_upr == clicked_response,
          cycle == clicked_cycle
        ) |> 
        select(text_2, state_under_review, cycle, response_upr) |> 
        mutate(state_under_review = factor(state_under_review)) |> 
        rename(
          # !! paste0("Recommendation: ", clicked_theme) := text_2,
          `Recommendation text` = text_2,
          SUR = state_under_review, 
          Cycle = cycle, 
          Response = response_upr
        )
    }
    if(is.na(clicked_recommending)|clicked_recommending == "NA"){
      res2 <- res}else{
        res2 <- res |> filter(str_detect(`Recommendation text`, clicked_recommending))
      }
    
    DT::datatable(res2,
                  filter = "top",
                  extensions = 'FixedHeader',
                  # caption = paste0("Theme: ", clicked_theme),
                  caption = tags$caption(
                    style = "caption-side: top; text-align: left;",
                    paste0("Theme: ", clicked_theme)
                  ),
                  options = list(
                    pageLength = 10
                    , fixedHeader = TRUE
                    , selectize = list(on = 'change')
                  ),
                  rownames = FALSE,
                  class = 'cell-border stripe hover compact'
    )
    # return(event.data)
  })
  ### Cycle themes ---------------------
  output$upr_themes_cycle_global <- renderPlot({
    req(nrow(filtered_upr_region()) > 0)
    a_1 <- filtered_upr_region() |>
      select(cycle, health_related:other_health_related, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr_region() |>
      select(cycle, health_related:other_health_related, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
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
        title = paste0("Health-related recommendations in each cycle of the UPR\n", input$selected_region)
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
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5,
                                  # size = 16,
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
      select(cycle, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr_region() |>
      select(cycle, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
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
        n_sup = case_when(n_tot_theme == 0 ~ "", .default = n_sup)
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
        title = paste0("All health-related recommendations of the UPR\n", input$selected_region),
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
        plot.title = element_text(hjust = 0.5,face = "bold"),
        plot.caption = element_text(size = 11),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.05,
        size = 3, color = "#1c164d"
        # vjust = 0.25
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
  
  ### MH recommending states --------------------------
  output$recommending_states_REGION <- renderPlot({
    req(nrow(filtered_upr()) > 0)
    upr_rec_countries <- filtered_upr_region() |>
      filter(!is.na(recommending_state_upr)) |> 
      filter(response_upr == "Supported") |> 
      filter(if_any(any_of(c(theme_labels$variable)), ~ .x != "Other")) |> 
      # select(cycle, recommending_state_upr) |> 
      separate_longer_delim(cols = c(recommending_state_upr), delim="-") |> 
      mutate(recommending_state_upr = str_trim(recommending_state_upr)) |> 
      group_by(cycle, recommending_state_upr) |> count(across(any_of(theme_labels$variable))) |> 
      pivot_longer(cols = !c(cycle, recommending_state_upr, n),
                   names_to = "variable",
                   values_to = "theme") |> 
      filter(theme!="Other") |> 
      left_join(theme_labels) |> 
      group_by(cycle, recommending_state_upr, variable) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct()
    
    c_plot <- upr_rec_countries |> 
      filter(variable %in% c("abortion", 
                             "maternal_health", 
                             "contraception")) |> 
      select(-cycle) |> 
      group_by(recommending_state_upr, theme) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct() |> 
      arrange(theme, -n) |> 
      mutate(recommending_state_upr = str_wrap(recommending_state_upr, 20)) |> 
      group_by(recommending_state_upr) |> 
      mutate(n_tot = sum(n)) |> 
      ungroup() |> 
      arrange(-n_tot) |> 
      ungroup()
    
    ccp <- c_plot |> select(recommending_state_upr, n_tot) |> distinct() |> 
      arrange(-n_tot) |>
      slice_head(n=20)
    
    c_plot |> 
      filter(recommending_state_upr %in% c(ccp |> pull(recommending_state_upr))) |> 
      ggplot(aes(x= reorder(recommending_state_upr, n_tot), y=n,fill=theme_label))+
      geom_col(alpha = 0.8, width = 0.8)+
      scale_fill_manual(values = c(
        "Maternal health" = "#7570b3",
        "Family planning" = "#1b9e77",
        "Abortion" = "#d95f02"
      ))+
      scale_y_continuous(expand = c(0, 0.1)) +
      tidytext::scale_x_reordered() +
      coord_flip()+
      theme_minimal() +
      guides(fill=guide_legend(reverse=TRUE))+
      theme(
        strip.text.y = element_text(angle = 270, face = "bold"),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.justification = c("left", "bottom"),
        legend.margin = margin(0,0,0,0), 
        legend.box.margin = margin(0,0,0,0), 
        legend.box.spacing = unit(0,"pt"),
        legend.background = element_blank())+
      labs(y="Supported recommendations (N)", x=NULL,
           fill=NULL)
  })
  
  ## UPR: SUR Outputs --------------------------------------------------------
  
  ### Plotly ---------------------
  
  #### All  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_SUR_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    
    a_1 <- 
      filtered_upr() |>    
      # filter(state_under_review == chosen_country) |> 
      select(text_2, cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- 
      filtered_upr() |>    
      # filter(state_under_review == chosen_country) |> 
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n_other"
      )
    
    a <- left_join(a_1, a_2) |>
      mutate(response_upr = fct_relevel(response_upr, "Noted")) |> 
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
        n_sup = case_when(n_tot_theme == 0 ~ "", .default = n_sup)
      ) |>
      ungroup() |>
      filter(!theme %in% c("health_related", "TB_malaria", "NTD")) |>
      left_join(theme_labels, by = c("theme" = "variable")) |>
      arrange(-n_tot_theme) |>
      mutate(
        theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label),
        theme_label = fct_inorder(theme_label)
      ) |> 
      filter(!theme %in% c(
        "SRHR", "health_related", "SOCED",
        "essential_medicines","TB_malaria", "NTD","vaccinations"
      ))
    
    max_a <- max(a$perc_theme, na.rm = TRUE)
    
    a |>
      ggplot(aes(
        x = perc, y = fct_rev(theme_label), 
        customdata = paste(theme_label, response_upr, sep = "|"),
        text = paste0(response_upr, ": n = ", n, " ", n_sup,"\n(click to view text of recommendations)")
      )) +
      geom_col(aes(fill = response_upr), alpha = 0.8, width = 0.85) +
      scale_fill_manual(values = c("#ec5557", "#1c164d"))+
      labs(
        x = paste0(
          "% of all recommendations", 
          "\n",
          "(Total N = ", format(nrow(filtered_upr()), big.mark = ","), ")"
        ),
        y = NULL,
        fill = NULL
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        # limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(clip = "off")+
      guides(fill=guide_legend(reverse=T))+
      theme(
        # plot.margin = margin(l=0,t=2,b=1, r = 2, unit = "pt"),
        legend.position = c(0.9, 0.1),
        legend.justification = c("right", "bottom"),
        legend.margin = margin(0,0,0,0),
        legend.frame = element_blank(),
        legend.text = element_text(size = 9, color = "#1c164d"),
        legend.title = element_text(size = 11, color = "#1c164d"),
        legend.background = element_blank(),
        legend.key.size = unit(10,"pt"),
        axis.text.y = element_text(size = 9, color = "#1c164d"),
        axis.text.x = element_text(size = 10, color = "#1c164d", angle=30),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#1c164d"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "#1c164d", hjust = 0.5),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        plot.caption = element_text(color = "#1c164d"),
        plot.background = element_rect(color = "#1c164d", fill = NA),
        panel.background = element_blank()
      )
  })
  
  ##### Plot output --------------------
  output$plotly_UPR_SUR <- renderPlotly({
    # ggplotly(plotly_UPR_SUR_object(),
    #          tooltip = c("text"),
    #          source = "click")
    
    # 1. Get the ggplot object from your reactive
    p <- plotly_UPR_SUR_object()
    
    # 2. Extract the title and replace newline "\n" with HTML <br>
    #    This is how Plotly handles multi-line titles.
    title_text <- input$selected_SUR
    
    # 3. Convert to plotly, then pipe to plotly::layout() to fix formatting
    ggplotly(
      p,
      tooltip = c("text"),
      source = "click"
    ) |>
      plotly::layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        # 4. Fix the legend position
        legend = list(
          traceorder = "reversed",
          x = 0.99,
          y = 0.01,
          xanchor = 'right',
          yanchor = 'bottom',
          bgcolor = 'rgba(0,0,0,0)', # Transparent background
          bordercolor = 'rgba(0,0,0,0)'
        ),
        
        # 5. Fix the title alignment and text
        title = list(
          text = title_text,
          automargin = TRUE,
          x = 0               # Center the title (0 = left, 0.5 = center, 1 = right)
        ),
        
        # 6. X axis
        xaxis = list(
          automargin = TRUE # Automatically creates space for the title
        ),
        
        # 7. Manually set margins
        margin = list(
          l = 0, # Left margin
          r = 40, # Right margin
          b = 0, # Bottom margin (for the long x-axis title)
          t = 30  # Top margin (for the plot title)
        )
      )
  })
  
  ##### Plot downloader -------------------
  output$download_plotly_UPR_SUR <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("health-recommendations-", input$selected_SUR, ".png")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      ggsave(
        file,
        plot = plotly_UPR_SUR_object()+
          theme(
            plot.background = element_rect(color = "#1c164d", fill = "#F9F9F6"),
            panel.background = element_rect(color = NA, fill="#F9F9F6")
          )+
          scale_x_continuous(
            # labels = function(x) paste0(x, "%"),
            # limits = c(0, max_a + 2),
            expand = expansion(mult = c(0, 0.2))
          ) +
          labs(
            title = paste0("Health-related recommendations of the UPR"
                           , "\n"
                           , input$selected_SUR),
            caption = "*Numbers after the bars indicate N (% supported)"
          )+
          geom_text(
            data = plotly_UPR_SUR_object()@data |> filter(response_upr == "Supported"),
            aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
            hjust = -0.05,
            size = 3, color = "#1c164d"
            # vjust = 0.25
          )
        ,width = 7,
        height = 5,
        dpi = 400,
        units = "in"
      )
    }
  )
  
  #### Cycles  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_SUR_cycle_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    
    a_1<- filtered_upr() |> 
      # filter(cycle != "Cycle 4") |> 
      # filter(state_under_review == "Nigeria") |> 
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |> 
      # mutate(across(c(health_related:other_health_related), ~ .x != "Other")) |> 
      group_by(cycle, response_upr) |> 
      summarise(across(c(health_related:other_health_related), ~ sum(.x !="Other"))) |> 
      ungroup() |> 
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(cols = health_related:other_health_related, 
                   names_to = "theme", 
                   values_to = "n"
      )
    
    a_2<- filtered_upr() |> 
      # filter(cycle != "Cycle 4") |> 
      # filter(state_under_review == "Nigeria") |> 
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |> 
      # mutate(across(c(health_related:other_health_related), ~ .x != "Other")) |> 
      group_by(cycle, response_upr) |> 
      summarise(across(c(health_related:other_health_related), ~ sum(.x =="Other"))) |> 
      ungroup() |> 
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(cols = health_related:other_health_related, 
                   names_to = "theme", 
                   values_to = "n_other"
      )
    
    a_3<- filtered_upr() |> 
      # filter(cycle != "Cycle 4") |> 
      # filter(state_under_review == "Nigeria") |> 
      group_by(cycle) |> 
      summarise(health_n = sum(health_related !="Other")) |> 
      ungroup()
    
    a <- left_join(a_1,a_2) |> 
      left_join(a_3) |> 
      mutate(cycle2 = fct_recode(cycle, "1"="Cycle 1", "2"="Cycle 2", "3"="Cycle 3", "4"="Cycle 4")) |> 
      group_by(cycle2, theme) |> 
      mutate(n_tot = sum(n)+sum(n_other)) |> 
      mutate(n_tot_theme = sum(n)) |> 
      mutate(perc = n/n_tot*100,
             perc_theme = n_tot_theme/n_tot*100,
             theme_perc_health = n_tot_theme/health_n*100) |> 
      group_by(cycle2, theme) |> 
      mutate(n_sup = paste0("(", sprintf("%1.0f", n/sum(n)*100), "%)"),
             n_sup = case_when(n_tot_theme == 0 ~ "(NA)", .default = n_sup)) |> 
      # mutate(n_sup = case_when(
      #   response_upr == "Noted/Other" ~ "",
      #   response_upr == "Supported" ~ paste0("(", sprintf("%1.0f", n/sum(n)*100), "%)"),
      #   .default = NA
      # )) |> 
      ungroup() |> 
      # filter(!theme %in% c(
      #   "health_related"
      #   # , "abortion"
      #   , "TB_malaria", "NTD"
      #   # , "TB_malaria_NTD"
      # )) |> 
      filter(!theme %in% c(
        "SRHR", "health_related", "SOCED",
        "essential_medicines","TB_malaria", "NTD","vaccinations"
      )) |> 
      left_join(theme_labels, join_by(theme == variable)) |> 
      mutate(theme_label = case_when(is.na(theme_label) ~ theme, .default = theme_label)) |> 
      group_by(theme) |> 
      mutate(perc_tot = sum(n_tot_theme)) |> 
      ungroup() |> 
      arrange(
        # cycle2, 
        -perc_tot
        # -n_tot_theme
      ) |> 
      mutate(
        theme_label = fct_inorder(theme_label)
      )
    
    
    max_a <- max(a$perc_theme)
    a |> 
      # mutate(n_tot_theme = case_when(response_upr!="Supported" ~ "", 
      #                                .default = as.character(n_tot_theme))) |> 
      ggplot(aes(x = perc, y = fct_rev(cycle2), 
                 customdata = paste(theme_label, response_upr, cycle, sep = "|"),
                 text = paste0(cycle, " - ", response_upr,  ": n = ", n, " ", n_sup,"\n(click to view text of recommendations)")
      ))+
      geom_col(aes(fill = response_upr), alpha = 0.8, width = 0.95)+
      facet_grid(
        rows = vars(theme_label), switch = "y"
        # ,labeller = labeller(theme_label = label_wrap_gen(30))
      )+
      labs(x = "% of all recommendations in cycle", y = NULL,
           fill = "State's response",
           # title = "Health-related recommendations of the UPR",
           caption = "*Numbers after the bars indicate N (% supported)")+
      # theme_bw()+
      theme_classic()+
      # scale_y_discrete(expand = c(0.1, 0))+
      scale_fill_manual(values = c("#ec5557", "#1c164d"))+
      scale_x_continuous(labels = function(x) paste0(x, "%"), 
                         # limits = c(-0.35,max_a+1), 
                         limits = c(0,max_a+1), 
                         # sec.axis = dup_axis(name = NULL),
                         expand = expansion(mult = c(0, 0.05)) # 0 exactly on axis
      )+
      # geom_text(
      #   data = distinct(a, theme_label, cycle2), 
      #   aes(label = cycle2, x = -0.25, y = fct_rev(cycle2)), # x=0.5 puts it slightly inside the bar
      #   hjust = 0,                   # Align text to the left
      #   color = "#1c164d",             # White text for contrast against dark bars
      #   size = 2.4,
      #   inherit.aes = FALSE
      # )+
      guides(fill=guide_legend(reverse=T))+
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.75,0.5),
        legend.justification = c("right", "center"),
        legend.frame = element_rect(color = "black"),
        # axis.text.y = element_text(size = 6, margin = margin(l=0, r=-10)), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 0.5
                                         , color = "#1c164d"
                                         #, size = 8
        ),
        # strip.background = element_blank()
        panel.grid = element_blank(),
        panel.spacing = unit(0.01, "lines"),
        # plot.background = element_blank(),
        plot.background = element_rect(color = "#1c164d", fill = NA),
        panel.background = element_blank()
      )
  })
  
  ##### Plot output --------------------
  output$plotly_UPR_SUR_cycle <- renderPlotly({
    # ggplotly(plotly_UPR_SUR_object(),
    #          tooltip = c("text"),
    #          source = "click")
    
    # 2. Extract the title and replace newline "\n" with HTML <br>
    #    This is how Plotly handles multi-line titles.
    title_text <- input$selected_SUR
    
    # 3. Convert to plotly, then pipe to plotly::layout() to fix formatting
    # 1. Create the base interactive plot
    fig <- ggplotly(
      plotly_UPR_SUR_cycle_object(),
      source = "click",
      tooltip = c("text") # Ensure this matches your aesthetic mapping
    ) 
    
    # 2. The Fix: Manually move the strip labels
    # We iterate through the layout annotations. If we find a vertical label (the strip),
    # we rotate it to 0 (horizontal) and move its X coordinate to the left.
    
    fig$x$layout$annotations <- lapply(fig$x$layout$annotations, function(a) {
      # ggplotly renders side strips with an angle (usually -90 or 90)
      # We catch those and modify them
      if (!is.null(a$textangle) && a$textangle != 0) {
        a$textangle <- 0       # Make text horizontal
        a$x <- -0.01           # Move to Left (Adjust this value based on label length)
        a$xanchor <- "right"   # Align text against the axis
        a$yanchor <- "middle"  # Center vertically
        a$align <- "center"
        # Border
        # a$bordercolor <- "#1c164d" # Match your theme color (or use "black")
        # a$borderwidth <- 0.2         # Thickness of the line
        # a$borderpad   <- 0         # Padding inside the box (crucial for looks)
        # a$bgcolor     <- "white"   # Optional: ensures the line behind doesn't show through
      }
      return(a)
    })
    
    # 3. Final Layout Adjustments
    # We must increase the left margin (l) so the moved labels don't get cut off.
    fig |> 
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 270), # Increase 'l' until your longest label fits
        legend = list(
          traceorder = "reversed",
          x = 0.99,
          y = 0.01,
          xanchor = 'right',
          yanchor = 'bottom',
          bgcolor = 'rgba(0,0,0,0)', # Transparent background
          bordercolor = 'rgba(0,0,0,0)'
        ),
        title = list(
          text = title_text,
          automargin = TRUE,
          x = 0               # Center the title (0 = left, 0.5 = center, 1 = right)
        ),
        xaxis = list(
          automargin = TRUE # Automatically creates space for the title
        )
      )
  })
  
  #### Recommending States  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_SUR_recommending_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    
    upr_rec_countries <- filtered_upr() |>
      filter(!is.na(recommending_state_upr)) |> 
      filter(response_upr == "Supported") |> 
      filter(if_any(any_of(c(theme_labels$variable)), ~ .x != "Other")) |> 
      # select(cycle, recommending_state_upr) |> 
      separate_longer_delim(cols = c(recommending_state_upr), delim="-") |> 
      mutate(recommending_state_upr = str_trim(recommending_state_upr)) |> 
      group_by(cycle, recommending_state_upr) |> count(across(any_of(theme_labels$variable))) |> 
      pivot_longer(cols = !c(cycle, recommending_state_upr, n),
                   names_to = "variable",
                   values_to = "theme") |> 
      filter(theme!="Other") |> 
      left_join(theme_labels) |> 
      group_by(cycle, recommending_state_upr, variable) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct()
    
    c_plot <- upr_rec_countries |> 
      filter(variable %in% c("abortion", 
                             "maternal_health", 
                             "contraception",
                             "sexual_health",
                             "sexual_education"
                             )) |> 
      select(-cycle) |> 
      group_by(recommending_state_upr, theme) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct() |> 
      arrange(theme, -n) |> 
      mutate(recommending_state_upr = str_wrap(recommending_state_upr, 20)) |> 
      group_by(recommending_state_upr) |> 
      mutate(n_tot = sum(n)) |> 
      ungroup() |> 
      arrange(-n_tot) |> 
      ungroup()
    
    ccp <- c_plot |> select(recommending_state_upr, n_tot) |> distinct() |> 
      arrange(-n_tot) |>
      slice_head(n=20)
    
    c_plot |> 
      filter(recommending_state_upr %in% c(ccp |> pull(recommending_state_upr))) |> 
      ggplot(aes(x= reorder(recommending_state_upr, n_tot), y=n,fill=theme_label
                 ,customdata = paste(theme_label, "Supported", NA, recommending_state_upr, sep = "|"),
                 text = paste0(recommending_state_upr, " - ", theme_label,  ": n = ", n,"\n(click to view text of recommendations)")
      ))+
      geom_col(alpha = 1, width = 0.8)+
      scale_fill_manual(values = c(
        "Maternal health" = "#8dd3c7",
        "Family planning" = "#fed9a6",
        "Abortion" = "#bebada",
        "Sexual health and wellbeing" = "#fb8072",
        "Sexual education" = "#80b1d3"
      ))+
      scale_y_continuous(expand = c(0, 0.1)) +
      tidytext::scale_x_reordered() +
      coord_flip()+
      theme_minimal() +
      guides(fill=guide_legend(reverse=TRUE))+
      theme(
        strip.text.y = element_text(angle = 270, face = "bold"),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        aspect.ratio = 0.09*n_distinct(ccp$recommending_state_upr),
        legend.position = "top",
        legend.justification = c("left", "bottom"),
        legend.margin = margin(0,0,0,0), 
        legend.box.margin = margin(0,0,0,0), 
        legend.box.spacing = unit(0,"pt"),
        legend.background = element_blank(),
        plot.background = element_rect(color = "#1c164d", fill = NA),
        panel.background = element_blank()
      )+
      labs(y="Supported recommendations (N)", x=NULL,
           fill=NULL)
    
  })
  
  ##### Plot output --------------------
  output$plotly_UPR_SUR_recommending <- renderPlotly({
    
    title_text <- input$selected_SUR
    
    
    p <- ggplotly(
      plotly_UPR_SUR_recommending_object(),
      source = "click",
      tooltip = c("text") # Ensure this matches your aesthetic mapping
    ) |> 
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        # margin = list(l = 265), # Increase 'l' until your longest label fits
        legend = list(
          traceorder = "reversed",
          x = 0.99,
          y = 0.01,
          xanchor = 'right',
          yanchor = 'bottom',
          bgcolor = 'rgba(0,0,0,0)', # Transparent background
          bordercolor = 'rgba(0,0,0,0)'
        ),
        title = list(
          text = title_text,
          automargin = TRUE,
          x = 0               # Center the title (0 = left, 0.5 = center, 1 = right)
        ),
        margin = list(
          l = 0, # Left margin
          r = 0, # Right margin
          b = 0, # Bottom margin (for the long x-axis title)
          t = 30  # Top margin (for the plot title)
        )
      )
    p
  })
  
  #### Table -------------------
  
  output$plotly_table_SUR <- renderDataTable({
    
    plot_data <- filtered_upr() |> 
      select(text_2, state_under_review, response_upr, cycle, health_related:other_health_related, document_code) |> 
      pivot_longer(cols = health_related:other_health_related) |> 
      mutate(
        value = case_when(is.na(value) ~ FALSE,
                          value == "Other" ~ FALSE,
                          value != "Other" ~ TRUE)
      ) |> 
      filter(value, name != "health_related") |>
      left_join(theme_labels, by = c("name" = "variable"))
    
    event.data <- event_data("plotly_click", source = "click")
    
    # if(is.null(event.data) == T) return(NULL)
    req(event.data)
    
    # Filter result via Data
    clicked_data_string <- event.data$customdata
    
    # 2. Split the customdata back into two parts
    clicked_info <- strsplit(clicked_data_string, "|", fixed = TRUE)[[1]]
    clicked_theme <- clicked_info[1]
    clicked_response <- clicked_info[2]
    clicked_cycle <- clicked_info[3]
    clicked_recommending <- clicked_info[4]
    
    if(is.na(clicked_cycle) | clicked_cycle == "NA"){
      res <- plot_data |> 
        filter(
          theme_label == clicked_theme, 
          response_upr == clicked_response
        ) |> 
        select(text_2, state_under_review, cycle, response_upr) |> 
        mutate(state_under_review = factor(state_under_review)) |> 
        rename(
          # !! paste0("Recommendation: ", clicked_theme) := text_2,
          `Recommendation text` = text_2,
          SUR = state_under_review, 
          Cycle = cycle, 
          Response = response_upr
        )
    } else{
      res <- plot_data |> 
        filter(
          theme_label == clicked_theme, 
          response_upr == clicked_response,
          cycle == clicked_cycle
        ) |> 
        select(text_2, state_under_review, cycle, response_upr) |> 
        mutate(state_under_review = factor(state_under_review)) |> 
        rename(
          # !! paste0("Recommendation: ", clicked_theme) := text_2,
          `Recommendation text` = text_2,
          SUR = state_under_review, 
          Cycle = cycle, 
          Response = response_upr
        )
    }
    if(is.na(clicked_recommending)|clicked_recommending == "NA"){
      res2 <- res}else{
        res2 <- res |> filter(str_detect(`Recommendation text`, clicked_recommending))
      }
    
    DT::datatable(res2,
                  filter = "top",
                  extensions = 'FixedHeader',
                  # caption = paste0("Theme: ", clicked_theme),
                  caption = tags$caption(
                    style = "caption-side: top; text-align: left;",
                    paste0("Theme: ", clicked_theme)
                  ),
                  options = list(
                    pageLength = 10
                    , fixedHeader = TRUE
                    , selectize = list(on = 'change')
                  ),
                  rownames = FALSE,
                  class = 'cell-border stripe hover compact'
    )
    # return(event.data)
  })
  
  ### General plot --------------------------
  #### Plot object ----------------------------
  rec_plot_object <- reactive({
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
    
    p<-upr_rec_countries |>
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
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.15))
      )+
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
        # plot.title = ggtext::element_textbox_simple(
        #   margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        # ),
        plot.title= element_blank()
      )
    p
  })
  
  #### Output --------------------
  output$plot <- renderPlot({
    rec_plot_object()
  })
  
  #### Plot downloader -------------------
  output$download_rec_plot_object <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("health-recommendations-", input$selected_SUR, ".png")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      ggsave(
        file,
        plot = rec_plot_object()+
          labs(y="Recommendations (N)")+
          geom_text(aes(label = sprintf("%1.0f", n_tot), y = n_tot, vjust = -0.2), size = 5, fontface = "bold", color = "white") +
          scale_fill_manual(values = c("Health-related" = "#ec5557", "Other" = "grey80"))+
          theme(
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 12, color = "white"),
            axis.text.y = element_text(size = 12, color = "white"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14, color = "white"),
            # legend.position = "bottom",
            legend.position = c(0, 1),
            legend.justification = c("left", "top"), 
            legend.text = element_text(size = 11,colour = "white"),
            legend.key.size = unit(15,"pt"),
            # plot.background = element_blank(),
            plot.background = element_rect(color = "white", fill = NA),
            panel.border = element_rect(color = "white"),
            # panel.border = element_blank(),
            axis.ticks = element_line(color = "white"),
            panel.background = element_blank(),
            legend.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank(),
            plot.title = element_blank()
          )
        ,
        width = 5,
        height = 3.3,
        dpi = 300,
        # units = "in", 
        bg="transparent"
      )
    }
  )
  
  ### Cycle themes ------------------
  output$upr_themes_cycle <- renderPlot({
    req(nrow(filtered_upr()) > 0)
    a_1 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(cycle, response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
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
        title = paste("Health-related recommendations in each cycle of the UPR\n", input$selected_SUR)
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
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, 
                                  # size = 16, 
                                  face = "bold"),
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
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x != "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
        names_to = "theme",
        values_to = "n"
      )
    
    a_2 <- filtered_upr() |>
      select(cycle, state_under_review, health_related:other_health_related, response_upr) |>
      group_by(response_upr) |>
      summarise(across(c(health_related:other_health_related), ~ sum(.x == "Other"))) |>
      ungroup() |>
      filter(response_upr %in% c("Supported", "Noted")) |>
      pivot_longer(
        cols = health_related:other_health_related,
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
        n_sup = case_when(n_tot_theme == 0 ~ "", .default = n_sup)
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
    p <- a |>
      ggplot(aes(x = perc, y = fct_rev(theme_label))) +
      geom_col(aes(fill = response_upr)) +
      labs(
        x = "Proportion of all recommendations (%)", y = NULL,
        fill = "State's response",
        title = paste("All health-related recommendations of the UPR\n", input$selected_SUR),
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
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 3, vjust = 0.25, color = "#1c164d"
      )
    p
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
  
  #### Table object ----------------
  DT_table_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    theme_labels_test <- theme_labels |> 
      filter(!variable %in% c("TB_malaria_NTD"))
    
    table_upr <- 
      # filtered_upr() |>
      sdg_data_dashboard() |>
      # filter(state_under_review == input$selected_SUR)
      mutate(state_under_review = factor(state_under_review)) |>
      select(
        # text, cycle, response_upr, health_related:other_health_related,
        # state_under_review,recommending_state_upr_comma, document_code, paragraph
        
        text_2, cycle, response_upr,
        any_of(theme_labels_test$variable), 
        state_under_review, recommending_state_upr_comma, 
        document_code, paragraph #, affected_persons, themes
      ) |>
      rename(
        `Recommending State(s)` = recommending_state_upr_comma,
        `State under Review` = state_under_review,
        Document  = document_code,
        Paragraph = paragraph,
        Recommendation = text_2, 
        Cycle = cycle, 
        `State's response` = response_upr
      )
    
    rename_map <- setNames(theme_labels_test$variable, theme_labels_test$theme_label)
    table_upr |>
      dplyr::rename(any_of(rename_map))
    
    # table_upr |> 
    #   DT::datatable(
    #     # extensions = "Responsive",
    #     filter = "top",
    #     options = list(
    #       pageLength = 100,
    #       deferRender = TRUE,
    #       scrollY = 800,
    #       scrollX = TRUE,
    #       scroller = TRUE,
    #       autoWidth = TRUE,
    #       columnDefs = list(
    #         list(width = '500px', targets = c(0))
    #         # list(width = '200px', targets = c(1))
    #       )
    #     ),
    #     rownames = FALSE,
    #     class = 'cell-border stripe hover compact'
    #   )
  })
  
  
  #### Render table ---------------------
  output$DT_table <- renderDT({
    req(nrow(filtered_upr()) > 0)
    DT_table_object() |> 
      filter(`State under Review` == input$selected_SUR) |> 
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
            list(width = '350px', targets = c(0)),
            list(width = '150px', targets = c(5,8,10)),
            list(width = '100px', targets = c(4,17,26))
            , list(width = '150px', targets = c(21))
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
  })
  
  #### Table downloader -----------------------------
  ##### CSV --------------------
  output$download_data_csv <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("UPR-recommendations-", input$selected_SUR, ".csv")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      write_csv(DT_table_object() |> filter(`State under Review` == input$selected_SUR), file)
    }
  )
  
  ##### CSV - ALL --------------------
  output$download_data_csv_all <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("UPR-recommendations-all", ".csv")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      write_csv(DT_table_object(), file)
    }
  )
  
  ##### XLSX ---------------------
  output$download_data_xlsx <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("UPR-recommendations-", input$selected_SUR, ".xlsx")
    },
    content = function(file) {
      withProgress(message = 'Generating xlsx file', value = 0, {
        
        # incProgress(0.1, detail = "Formatting....")
        
        wb <- createWorkbook()
        addWorksheet(wb, "recommendations")
        writeDataTable(wb, "recommendations", 
                       DT_table_object() |> filter(`State under Review` == input$selected_SUR), 
                       tableStyle = "TableStyleMedium15")
        
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = 1,      # Target the first column
          widths = 70    # Set its width
        )
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = 2:(ncol(DT_table_object())-4),      #rest of the columns
          widths = 12    # Set its width
        )
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = c(5,6,9,11,18,22),      #rest of the columns
          widths = 20    # Set its width
        )
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = ncol(DT_table_object())-2,      #rest of the columns
          widths = 20    # Set its width
        )
        
        wrap_style <- createStyle(wrapText = TRUE)
        addStyle(
          wb,
          sheet = "recommendations",
          style = wrap_style,
          rows = 1:(nrow(DT_table_object()) + 1), # all rows
          cols = 1:ncol(DT_table_object()),          # all columns
          gridExpand = TRUE                 # Ensure style is applied to all specified cells
        )
        
        freezePane(
          wb,
          sheet = "recommendations",
          firstActiveRow = 2, # The second row is the first one that moves
          firstActiveCol = 2  # The second column is the first one that moves
        )
        
        incProgress(0.7, detail = "--> Saving output (this may take a moment)")
        saveWorkbook(wb, file)
        
        incProgress(1, detail = "Done!")
      })
    }
  )
  
  ### MH recommending states --------------------------
  output$recommending_states_SUR <- renderPlot({
    req(nrow(filtered_upr()) > 0)
    upr_rec_countries <- filtered_upr() |>
      filter(response_upr == "Supported") |> 
      filter(if_any(any_of(c(theme_labels$variable)), ~ .x != "Other")) |> 
      # select(cycle, recommending_state_upr) |> 
      separate_longer_delim(cols = c(recommending_state_upr), delim="-") |> 
      mutate(recommending_state_upr = str_trim(recommending_state_upr)) |> 
      group_by(state_under_review, cycle, recommending_state_upr) |> count(across(any_of(theme_labels$variable))) |> 
      pivot_longer(cols = !c(state_under_review,cycle, recommending_state_upr, n),
                   names_to = "variable",
                   values_to = "theme") |> 
      filter(theme!="Other") |> 
      left_join(theme_labels) |> 
      group_by(state_under_review, cycle, recommending_state_upr, variable) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct()
    
    c_plot <- upr_rec_countries |> 
      filter(variable %in% c("abortion", 
                             "maternal_health", 
                             "contraception")) |> 
      select(-cycle) |> 
      group_by(recommending_state_upr, theme) |> 
      mutate(n=sum(n)) |> 
      ungroup() |> 
      distinct() |> 
      arrange(theme, -n) |> 
      mutate(recommending_state_upr = str_wrap(recommending_state_upr, 20)) |> 
      group_by(recommending_state_upr) |> 
      mutate(n_tot = sum(n)) |> 
      ungroup() |> 
      arrange(-n_tot) |> 
      ungroup()
    
    ccp <- c_plot |> select(recommending_state_upr, n_tot) |> distinct() |> 
      arrange(-n_tot) #|>
    # slice_head(n=15)
    
    c_plot |> 
      filter(recommending_state_upr %in% c(ccp |> pull(recommending_state_upr))) |> 
      ggplot(aes(x= reorder(recommending_state_upr, n_tot), y=n,fill=theme_label))+
      geom_col(alpha = 0.8, width = 0.85)+
      scale_fill_manual(values = c(
        "Maternal health" = "#7570b3",
        "Family planning" = "#1b9e77",
        "Abortion" = "#d95f02"
      ))+
      scale_y_continuous(
        expand = c(0, 0.1),
        breaks = c(0:max(c_plot$n_tot))
      ) +
      tidytext::scale_x_reordered() +
      coord_flip()+
      theme_minimal() +
      guides(fill=guide_legend(reverse=TRUE))+
      theme(
        strip.text.y = element_text(angle = 270, face = "bold"),
        strip.placement = "outside",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=13),
        axis.text = element_text(size=13),
        aspect.ratio = 0.09*n_distinct(ccp$recommending_state_upr),
        plot.margin = margin(0,0,0,0),
        legend.text = element_text(size=11),
        legend.position = "top",
        legend.justification = c("left", "bottom"),
        legend.margin = margin(0,0,0,0), 
        legend.box.margin = margin(0,0,0,0), 
        legend.box.spacing = unit(0,"pt"),
        legend.background = element_blank())+
      labs(y="Supported recommendations (N)", x=NULL,
           fill=NULL)
  })
  
  ## UHC Outputs ----------------------------------------------------------
  output$UHC_map <- renderPlot({
    UHC_estimate_2021 = UHC_all |>
      filter(country_name == input$selected_SUR, YEAR == 2021, 
             IndicatorCode == "UHC_INDEX_REPORTED") |>
      pull(NumericValue) |>
      round(0)
    
    uhc_estimate_data <- UHC_all |> 
      filter(YEAR == 2021) |> 
      # group_by(country_name, IndicatorCode) |> slice_max(order_by = YEAR, n=1) |> ungroup() |> 
      filter(SpatialDimType == "COUNTRY") |> 
      filter(IndicatorCode == "UHC_INDEX_REPORTED") |> 
      right_join(state_geo |> select(iso3), join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    p1 <- uhc_estimate_data |> 
      ggplot(aes(geometry = polygon, fill = NumericValue)) +
      geom_sf(color="transparent")+
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
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
        # title = paste0(input$selected_SUR, ": ", UHC_estimate_2021),
        fill = NULL
      )+
      guides(color = "none", lwd = "none")
    
    map_insetting(
      p1, plot_dat = uhc_estimate_data,
      p_caption_text = paste0(input$selected_SUR, ": ", UHC_estimate_2021),
      p_title_text = "UHC Service Coverage Index (2021)",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  output$UHC_RMNCH_map <- renderPlot({
    UHC_estimate_2021 = UHC_all |>
      filter(country_name == input$selected_SUR, YEAR == 2021, 
             IndicatorCode == "UHC_SCI_RMNCH") |>
      pull(NumericValue) |>
      round(0)
    
    uhc_rmnch_data <- UHC_all |> 
      filter(YEAR == 2021) |> 
      # group_by(country_name, IndicatorCode) |> slice_max(order_by = YEAR, n=1) |> ungroup() |> 
      filter(SpatialDimType == "COUNTRY") |> 
      filter(IndicatorCode == "UHC_SCI_RMNCH") |> 
      right_join(state_geo |> select(iso3), join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    p1 <- uhc_rmnch_data |> 
      ggplot(aes(geometry = polygon, fill = NumericValue)) +
      geom_sf(color="transparent")+
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
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
        # title = paste0(input$selected_SUR, ": ", UHC_estimate_2021),
        # caption = "RMNCH: reproductive, maternal, newborn and child health",
        fill = NULL
      )+
      guides(color = "none", lwd = "none")
    
    map_insetting(
      p1, plot_dat = uhc_rmnch_data,
      p_caption_text = paste0(input$selected_SUR, ": ", UHC_estimate_2021),
      p_title_text = "UHC sub-index on reproductive, maternal, newborn, and child health (2021)",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
    
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
      scale_x_continuous(breaks = c(2005,2010, 2015, 2020))+
      theme(
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        axis.text = element_text(angle=30, size = 10, hjust=0.5),
        axis.title = element_text(size = 12),
        panel.grid.minor.y = element_blank()
      )+
      # scale_y_continuous(sec.axis = dup_axis(name = NULL))+
      guides(color=guide_legend(nrow=2,byrow=TRUE))
  })
  
  ## Maternal health outputs --------------------------------
  
  ### MMR Outputs -------------------------------------------------------------
  #### Map ----------------------------------
  output$mmr_map <- renderPlot({
    
    mmr_estimate_2023 = mmr_map_object()@data |>
      filter(country_name == input$selected_SUR, 
             YEAR == "2023") |>
      pull(NumericValue) |>
      round(0)
    
    map_insetting(
      p1 = mmr_map_object(), 
      p_caption_text = paste0(input$selected_SUR, ": ", mmr_estimate_2023, " per 100,000 live births"),
      p_title_text = "Maternal mortality ratio (MMR) estimates in 2023",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  mmr_map_object <- reactive({
    mmr_estimate_2023 = MMR |>
      filter(country_name == input$selected_SUR, YEAR == "2023") |>
      pull(NumericValue) |>
      round(0)
    
    mmr_dat <- MMR |>
      filter(TimeDimensionValue == 2023, !is.na(country_name)) |>
      right_join(state_geo_reactive(), 
                 by = c("COUNTRY" = "iso3")) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) # |> filter(!is.na(selected_sur))
    
    p1<-mmr_dat |> 
      ggplot(aes(geometry = polygon, fill = mmr_cat)) +
      geom_sf(color="transparent") +
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_brewer(palette = "YlOrRd", na.value = "grey80", labels = relabel_na) +
      theme_void() +
      theme(
        # panel.grid = element_blank(),
        panel.background = element_rect(color="#1c164d"),
        plot.background = element_rect(color = "#1c164d"),
        # axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.background = element_blank()
      ) +
      labs(
        # title = paste0("Maternal mortality ratio (MMR) estimates in 2023\n", input$selected_SUR, ": ", mmr_estimate_2023, " per 100,000 live births"),
        fill = NULL,
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none")
    # coord_sf(
    #   xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
    #   ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
    # )
    
    
    # if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
    #   aes(
    #     xmin = bbox_selected_SUR()["xmin"]-1,
    #     xmax = bbox_selected_SUR()["xmax"]+1,
    #     ymin = bbox_selected_SUR()["ymin"]-1,
    #     ymax = bbox_selected_SUR()["ymax"]+1
    #   ),
    #   fill = "transparent",      # Make the rectangle hollow
    #   color = "red",             # Set the border color
    #   linewidth = 0.5            # Set the border thickness
    # )}
    # 
    # p3<-p1+
    #   scale_linewidth_manual(values = c(0.2, 0.1))+
    #   coord_sf(
    #     xlim = c(bbox_selected_SUR()[[1]], bbox_selected_SUR()[[3]]), 
    #     ylim = c(bbox_selected_SUR()[[2]], bbox_selected_SUR()[[4]]))+guides(fill = "none")+labs(title = NULL)
    # 
    # if(sur_area() > 10^11){p2} else{p2+p3}
  })
  
  #### Neighbors comparison over time -------------------------
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
      ggplot(aes(x = YEAR, y = NumericValue)) +
      labs(
        title = paste0("Trends in Maternal Mortality Ratio (MMR)"),
        x = NULL, y = "MMR estimate (per 100,000 live births)",
        color = NULL,
        fill = NULL
      ) +
      scale_x_continuous(breaks = c(2005, 2020))+
      geom_line(lwd = 1, aes(color = selected_sur)) +
      geom_ribbon(aes(ymin = Low, ymax = High, fill = selected_sur), color = NA, alpha = 0.4) +
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()
  })
  
  #### Causes --------------------------------
  ##### 2023 -------------------------
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
        y = "Maternal cause of death", x = "Age-standardized rate, 2023\n(per 100,000)", fill = NULL
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
  
  ##### Over time --------------------------
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
        lwd = "Rate\n(per 100,000)", color = NULL,
        title = "Longitudinal trends in the causes of maternal deaths\n(Caution: y-axes are variable)"
      ) +
      theme_bw() +
      scale_x_continuous(breaks = c(2005, 2020))+
      theme(
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 1, size = 11),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  ### Skilled birth outputs --------------------------------------------
  
  #### Skilled birth attendance ----------------
  output$skilled_birth <- renderPlot({
    skilled_birth_dat <- skilled_birth |>
      filter(!is.na(COUNTRY)) |>
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo, by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    country_estimate <- skilled_birth_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(Value)
    country_year <- skilled_birth_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(YEAR)
    
    p1<-skilled_birth_dat |> 
      ggplot(aes(geometry = polygon, fill = NumericValue)) +
      geom_sf(color="transparent") +
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
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
        fill = NULL,
        color = NULL, lwd = NULL
      )+guides(color="none", lwd="none")
    
    map_insetting(
      p1,plot_dat = skilled_birth_dat,
      p_caption_text = if(is.na(country_estimate)){paste0(input$selected_SUR, ": No available data")} 
      else{paste0(input$selected_SUR, ": ",country_estimate, "% in ", country_year)},
      p_title_text = "Births attended by skilled health personnel (%), latest year",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(),
      bbox_sur = bbox_selected_SUR(),
      sur_area =sur_area()
    )
  })
  
  ##### Neighbors --------------
  output$skilled_birth_plot_neighbors <- renderPlot({
    
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_skilled_birth_neighbors()) && plot_width_skilled_birth_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    dat_plot <- skilled_birth |>
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
      # filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR))
    
    hline_data <- dat_plot |>
      group_by(country_name) |> 
      filter(YEAR == min(YEAR)) |> ungroup()
    
    dat_plot |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      ggplot(aes(x = YEAR, y = NumericValue)) +
      labs(
        title = paste0("Trends in births attended by skilled health personnel"),
        x = NULL, y = "Births attended by skilled health personnel (%)",
        color = NULL,
        fill = NULL
      ) +
      geom_line(lwd = 1.5, aes(color = selected_sur)) +
      geom_point(aes(color=selected_sur), size=2.5)+
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      scale_x_continuous(breaks = c(2005, 2020))+
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()+
      theme(
        panel.grid.minor.y = element_blank(),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, unit = "pt")
        )
      )
  })
  
  #### Proportion of births delivered in a health facility ----------------
  output$births_facility <- renderPlot({
    institutional_birth_dat <- institutional_birth |>
      filter(!is.na(COUNTRY)) |>
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo, by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    country_estimate <- institutional_birth_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(Value)
    country_year <- institutional_birth_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(YEAR)
    
    p1<-institutional_birth_dat |> 
      ggplot(aes(geometry = polygon, fill = NumericValue)) +
      geom_sf(color="transparent") +
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
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
        fill = NULL,
        color = NULL, lwd = NULL
      )+guides(color="none", lwd="none")
    
    map_insetting(
      p1, plot_dat = institutional_birth_dat,
      p_caption_text = if(is.na(country_estimate)){paste0(input$selected_SUR, ": No available data")} 
      else{paste0(input$selected_SUR, ": ",country_estimate, "% in ", country_year)},
      p_title_text = "Proportion of births delivered in a health facility (%), latest year",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(),
      bbox_sur = bbox_selected_SUR(),
      sur_area =sur_area()
    )
  })
  
  ##### Neighbors --------------
  output$births_facility_plot_neighbors <- renderPlot({
    
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_births_facility_neighbors()) && plot_width_births_facility_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    dat_plot <- institutional_birth |>
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
      # filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR))
    
    hline_data <- dat_plot |>
      group_by(country_name) |> 
      filter(YEAR == min(YEAR)) |> ungroup()
    
    dat_plot |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      ggplot(aes(x = YEAR, y = NumericValue)) +
      labs(
        title = paste0("Trends in births delivered in a health facility"),
        x = NULL, y = "Births delivered in a health facility (%)",
        color = NULL,
        fill = NULL
      ) +
      geom_line(lwd = 1.5, aes(color = selected_sur)) +
      geom_point(aes(color=selected_sur), size=2.5)+
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      scale_x_continuous(breaks = c(2005, 2020))+
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()+
      theme(
        panel.grid.minor.y = element_blank(),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, unit = "pt")
        )
      )
  })
  
  ### Abortion Outputs -------------------------------------------------
  
  output$abortion_map_sur <- renderPlot({
    
    world_abortion_laws_data <- world_abortion_laws |>
      right_join(state_geo_reactive()) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    p1<-world_abortion_laws_data |> 
      ggplot(aes(geometry = polygon, fill = category)) +
      geom_sf(color="transparent") +
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_manual(
        values = c("chartreuse4", "cyan3", "gold", "chocolate1", "red3", "purple"),
        na.value = "grey90", labels = relabel_na
      ) +
      labs(
        # title = "Abortion laws by State (current as of June 2023)", 
        fill = NULL) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.key.size = unit(15, "pt"),
        # legend.key.height = unit(1,"cm"),
        legend.text = element_text(size = 11)
      ) +
      guides(color = "none", lwd = "none", label = "none")
    
    map_insetting(
      p1, plot_dat = world_abortion_laws_data,
      p_caption_text = paste0(input$selected_SUR),
      p_title_text = "Abortion laws by State (current as of June 2023)",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  output$abortion_rate <- renderPlot({
    abortion_rate_data <- abortion_rate |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    country_estimate <- abortion_rate_data |> 
      filter(country == input$selected_SUR) |> 
      pull(Value)
    
    p1<- abortion_rate_data |> 
      ggplot(aes(geometry = polygon, fill = NumericValue)) +
      geom_sf(color="transparent") +
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
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
        # title = "Abortion rate (model-estimated), 2015-2019",
        fill = "Annual estimate\n(per 1,000)",
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none")
    
    map_insetting(
      p1, plot_dat = abortion_rate_data,
      p_caption_text = paste0(input$selected_SUR, if(is.na(country_estimate)){": No available data"} 
                              else{paste0(": ",country_estimate, " per 1,000")}),
      p_title_text = "Abortion rate (model-estimated), 2015-2019",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  output$unintended_pregnancy <- renderPlot({
    unintended_pregnancy_data <- unintended_pregnancy |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    country_estimate <- unintended_pregnancy_data |> 
      filter(country == input$selected_SUR) |> 
      pull(Value)
    
    p1<- unintended_pregnancy_data |> 
      ggplot(aes(geometry = polygon, fill = NumericValue)) +
      geom_sf(color="transparent") +
      # scale_linewidth_manual(values = c(0.8, 0.3)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
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
        # title = "Unintended pregnancy (model-estimated), 2015-2019",
        fill = "Annual estimate\n(per 1,000)",
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none")
    
    map_insetting(
      p1, plot_dat = unintended_pregnancy_data,
      p_caption_text = paste0(input$selected_SUR, if(is.na(country_estimate)){": No available data"} 
                              else{paste0(": ",country_estimate, " per 1,000")}),
      p_title_text = "Unintended pregnancy (model-estimated), 2015-2019",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  ## Family planning outputs -------------------------------------
  output$family_planning <- renderPlot({
    family_planning_dat <- family_planning |>
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
      ))
    
    country_estimate <- family_planning_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(Value)
    country_year <- family_planning_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(YEAR)
    
    p1 <- family_planning_dat |> 
      ggplot(aes(geometry = polygon, fill = NumericValue, color = NumericValue)) +
      geom_sf(
        color = "transparent"
      ) +
      # scale_linewidth_manual(values = c(1, 0)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
      scale_fill_fermenter(
        n.breaks = 10,
        palette = "RdYlBu", direction = 1,
        na.value = "grey80",
        labels = relabel_na
      ) +
      # theme_void()+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key.size = unit(25,"pt"),
        legend.background = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(size=16),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        )
      ) +
      labs(
        # title = p_title,
        fill = NULL,
        # caption = paste0(input$selected_SUR, ": ",country_estimate, "% in ", country_year),
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none")
    
    map_insetting(
      p1, plot_dat=family_planning_dat,
      p_caption_text = if(is.na(country_estimate)){paste0(input$selected_SUR, ": No available data")} 
      else{paste0(input$selected_SUR, ": ",country_estimate, "% in ", country_year)},
      p_title_text = "Women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (%), latest year",
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  ## Constitutions ---------
  ## Right to health ----------
  output$constitution_const_health <- renderPlot({
    constitution_dat <- constitutions |>
      select(-country) |> 
      right_join(state_geo_reactive(), by = c("iso3" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    country_estimate <- constitution_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(const_health)
    country_year <- "20202020"
    
    p1 <- constitution_dat |> 
      ggplot(aes(geometry = polygon, fill = const_health, color = const_health)) +
      geom_sf(
        color = "transparent"
      ) +
      # scale_linewidth_manual(values = c(1, 0)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
      # scale_fill_fermenter(
      #   n.breaks = 10,
      #   palette = "RdYlBu", direction = 1,
      #   na.value = "grey80",
      #   labels = relabel_na
      # ) +
      # theme_void()+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key.size = unit(25,"pt"),
        legend.background = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(size=16),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        )
      ) +
      labs(
        # title = p_title,
        fill = NULL,
        # caption = paste0(input$selected_SUR, ": ",country_estimate, "% in ", country_year),
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none")
    
    map_insetting(
      p1, plot_dat=constitution_dat,
      p_caption_text = if(is.na(country_estimate)){paste0(input$selected_SUR, ": No available data")} 
      else{paste0(input$selected_SUR, ": ",country_estimate)},
      p_title_text = NULL,
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
  
  ## Right to medical care ----------
  output$constitution_const_medcare <- renderPlot({
    constitution_dat <- constitutions |>
      select(-country) |> 
      right_join(state_geo_reactive(), by = c("iso3" = "iso3")) |>
      mutate(selected_sur = factor(case_when(
        country == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      ))
    
    country_estimate <- constitution_dat |> 
      filter(country == input$selected_SUR) |> 
      pull(const_medcare)
    country_year <- "20202020"
    
    p1 <- constitution_dat |> 
      ggplot(aes(geometry = polygon, fill = const_medcare, color = const_medcare)) +
      geom_sf(
        color = "transparent"
      ) +
      # scale_linewidth_manual(values = c(1, 0)) +
      # scale_color_manual(values = c("blue3", "grey90")) +
      # scale_fill_fermenter(
      #   n.breaks = 10,
      #   palette = "RdYlBu", direction = 1,
      #   na.value = "grey80",
      #   labels = relabel_na
      # ) +
      # theme_void()+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size=12),
        legend.key.size = unit(25,"pt"),
        legend.background = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(size=16),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        )
      ) +
      labs(
        # title = p_title,
        fill = NULL,
        # caption = paste0(input$selected_SUR, ": ",country_estimate, "% in ", country_year),
        color = NULL, lwd = NULL
      ) +
      guides(color = "none", lwd = "none", label = "none")
    
    map_insetting(
      p1, plot_dat=constitution_dat,
      p_caption_text = if(is.na(country_estimate)){paste0(input$selected_SUR, ": No available data")} 
      else{paste0(input$selected_SUR, ": ",country_estimate)},
      p_title_text = NULL,
      bbox_SUR_region_dynamic = bbox_SUR_region_dynamic(), 
      bbox_sur = bbox_selected_SUR(), 
      sur_area =sur_area()
    )
  })
}

# 4. RUN APP ==================================================================
shinyApp(ui, server)