## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ##   IMPORTANT - READ FIRST   ## ## ## ## ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# First make sure to clear all objects from the workspace, then source the whole file.
# All objects that exist in the workspace will be saved as rds to the API_data folder !

# Setup ####
pacman::p_load(
  here,
  tidyverse,
  dplyr,forcats,ggplot2,magrittr,readr,readxl,stringr,tibble,tidyr,lubridate,
  # plotly,
  janitor,
  sf,
  ODataQuery # More general API use of OData protocol
)

# Load or install packages from GitHub:
pacman::p_load_gh(
  "PPgp/wpp2024" 
  # "DrMattG/SDGsR", # Uses API to get SDGs data
  # "ODataQuery" # Development version from github
  # "aphp/rgho" # Uses API to get data from Global Health Observatory
  # "PPgp/wpp2024" # United Nations World Population Prospects 2024
  # "m-muecke/isocountry" # Get ISO codes for countries
)

state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))

# Load in custom functions
# source(here("utils.R"))

theme_labels <- tribble(
  ~"variable", ~"theme_label",
  "health_systems", "Health systems and services",
  "emergencies", "Health security, emergencies, and disaster relief",
  "ncd", "Non-communicable diseases",
  "communicable", "Communicable diseases",
  "SRHR", "Sexual and reproductive health and rights",
  "mental_health", "Mental health",
  "SOCED", "Social and economic determinants of health",
  "GBV", "Gender-based violence",
  "women", "Women's health",
  "MCAH", "Maternal, child, and adolescent health",
  "essential_medicines", "Essential medicines and health products",
  "disabilities", "Disabilities and health",
  "LGBTI", "Health of LGBTI persons",
  "HIV", "HIV/AIDS and STIs",
  "TB_malaria", "TB and malaria",
  "NTD", "Neglected tropical diseases",
  "TB_malaria_NTD", "TB, malaria, and neglected tropical diseases",
  "vaccinations", "Vaccinations",
  "WASH", "Water and Sanitation",
  "nutrition", "Nutrition",
  "maternal_health", "Maternal health",
  "abortion", "Abortion",
  "incarcerated", "Health of incarcerated persons"
)

# Population data --------------------------------------
# from the wpp2024 package
data(e01dt);data(pop1dt);data(popAge5dt);data(tfr1dt)

# UHRI dataset --------------------------------------------------------
# Download the full UHRI dataset from https://uhri.ohchr.org/en/our-data-api
# The direct download of the excel file is: https://dataex.ohchr.org/uhri/export-results/export-full-en.xlsx

# httr::GET("https://dataex.ohchr.org/uhri/export-results/export-full-en.xlsx", httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
# df_0 <-  read_xlsx(tf,
#                    # Specify the column types, otherwise some show up as NA in R
#                    col_types = c(
#                      rep("text", 7), "date", rep("text", 7), "date")) |>
#   janitor::clean_names();unlink(tf);rm(tf)
# df<-df_0 |> janitor::clean_names() |>
#   # Add the date that the dataset was accessed from the UHRI website
#   mutate(date_accessed = ymd(Sys.Date())) |>
#   mutate(countries_concerned = str_remove(countries_concerned, "- "),
#          reccomending_body = str_remove(reccomending_body, "- ")
#   ) |>
#   mutate(
#     text = str_remove_all(text, "&nbsp;"),
#   ) |>
#   arrange(countries_concerned, reccomending_body, document_publication_date) |>
#   rename(document = document_symbol, body = reccomending_body) |>
#   mutate(
#     paragraph = case_when(
#       # CASE 1: The text starts with "Recommendation No."
#       # This detects the first pattern you described.
#       str_detect(text, "^Recommendation No\\.") ~ {
#         # str_match() lets us capture parts of the text.
#         # We look for "para. [number]: [number]"
#         matches <- str_match(text, "para\\.\\s*(\\d+):\\s*(\\d+)")
#         # We then take the captured numbers (the 2nd and 3rd elements from the match)
#         # and paste them together with a dot, creating the "96.21" format.
#         paste(matches[, 2], matches[, 3], sep = ".")
#       },
# 
#       # CASE 2: The text starts with "para" or "paragraph"
#       # This is the new case to handle paragraph-only identifiers.
#       str_detect(text, "^[Pp]ara") ~ str_match(text, "[Pp]ara(?:graph)?\\.?\\s*(\\d+)")[, 2],
# 
#       # CASE 3: The text starts with a "number.number" format (e.g., "136.1 ...")
#       # This must come before the single-number check to be matched correctly.
#       str_detect(text, "^\\d+\\.\\d+") ~ str_extract(text, "^\\d+\\.\\d+"),
# 
#       # CASE 4: The text starts with just one number (e.g., "1 ...")
#       # This is the new case to handle single leading numbers.
#       str_detect(text, "^\\d+") ~ str_extract(text, "^\\d+"),
# 
#       # If none of the above patterns match, return NA
#       TRUE ~ NA_character_
#     ),
#     upr_session = str_remove(upr_session, "- "),
#     upr_session = str_split_i(upr_session, " - ", 1),
#     upr_session_number = as.numeric(str_extract(upr_session, "\\d+")),
#     upr_cycle = case_when(
#       upr_session_number <= 12 ~ "Cycle 1",
#       upr_session_number <= 26 ~ "Cycle 2",
#       upr_session_number <= 40 ~ "Cycle 3",
#       upr_session_number <= 54 ~ "Cycle 4",
#     ),
#     type = str_remove(type, "- "),
#     upr_position = str_remove(upr_position, "- "),
#     title_a = str_split_i(paragraph,"\\.",1),
#     title_b = str_pad(str_split_i(paragraph,"\\.",2), , width = 3, side = "left", pad = "0"),
#     title_2 = paste(title_a, title_b,sep = ".")
#   )|>
#   arrange(upr_session_number) |>
#   mutate(
#     upr_session = fct_inorder(upr_session),
#     upr_cycle = fct_inorder(upr_cycle)
#     ) |>
#   select(-title_a, -title_b) |>
#   relocate(paragraph, .after=document) |>
#   relocate(title_2, upr_position, type, .after = paragraph) |>
#   arrange(countries_concerned, body, document_publication_date, title_2) |>
#   rename(
#     state_under_review = countries_concerned,
#     document_code = document
#   )
# 
# saveRDS(df, here("data", "UHRI_full.rds"))
# saveRDS(df, here("data", paste0("UHRI_full_", Sys.Date(), ".rds")))
# 
# uhri_full <- readRDS(here("data", "UHRI_full.rds"))

# Human Development Index (HDI) ---------------------------------------
# https://hdr.undp.org/data-center/documentation-and-downloads
HDI <- vroom::vroom(here("data", "HDR25_Composite_indices_complete_time_series.csv")) |> 
  select(iso3, hdi_1990:hdi_2023) |> 
  pivot_longer(cols = hdi_1990:hdi_2023, names_to = "year", values_to = "HDI") |> 
  mutate(year = as.numeric(str_remove(year, "hdi_")))

# World abortion laws -----------------------------------------------------
# https://reproductiverights.org/maps/worlds-abortion-laws/

gestational_text <- "W5|W8|W10|W12|W13|W14|W16|W17|W18|W20|W22|W24|D90|D120|†|°|º"
world_abortion_laws <- readxl::read_xlsx(here("data", "world_abortion_laws.xlsx")) |> 
  janitor::clean_names() |> 
  mutate(country = str_remove(country, gestational_text)) |> 
  arrange(country) |> 
  mutate(category = factor(case_match(
    category,
    "I" ~ "I. On Request",
    "II" ~ "II. Socioeconomic Grounds",
    "III" ~ "III. To Preserve Health",
    "IV" ~ "IV. To Save the Mother's Life",
    "V" ~ "V. Prohibited Altogether",
    "Varies" ~ "Varies at State level"
  ))) |> 
  mutate(country = case_match(
    country,
    "Antigua & Barbuda" ~ "Antigua and Barbuda",
    "Bolivia" ~ "Bolivia (Plurinational State of)",
    "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    "Cape Verde" ~ "Cabo Verde",
    "Central African Rep." ~ "Central African Republic",
    "Czech Rep." ~ "Czechia",
    "Dem. People’s Rep. of Korea" ~ "Democratic People's Republic of Korea",
    "Dem. Rep. of Congo" ~ "Democratic Republic of the Congo",
    "Eswatini (formerly Swaziland)" ~ "Eswatini",
    "Guinea Bissau" ~ "Guinea-Bissau",
    "Iran" ~ "Iran (Islamic Republic of)" ,
    "Ivory Coast" ~ "C\u00f4te d'Ivoire",
    "Laos" ~ "Lao People's Democratic Republic",
    "Micronesia" ~ "Micronesia (Federated States of)",
    "Moldova" ~ "Republic of Moldova",
    "Dem. People’s Rep. of Korea" ~ "Democratic People's Republic of Korea",
    "Russian Fed." ~ "Russian Federation",
    "Saint Kitts & Nevis" ~ "Saint Kitts and Nevis",
    "Saint Vincent & the Grenadines" ~ "Saint Vincent and the Grenadines",
    "São Tomé & Príncipe" ~ "Sao Tome and Principe",
    "Slovak Rep." ~ "Slovakia",
    "South Korea" ~ "Republic of Korea",
    "Syria" ~ "Syrian Arab Republic",
    "Trinidad & Tobago" ~ "Trinidad and Tobago",
    "Tanzania" ~ "United Republic of Tanzania",
    "Turkey" ~ "T\u00fcrkiye",
    "Great Britain" ~ "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
    "Vietnam" ~ "Viet Nam",
    .default = country
  ))

# Constitutional rights -----------------------------
# Downloaded from https://www.worldpolicycenter.org/constitutional-approaches-to-the-right-to-health
# Data download > Constitutions Data Download
constitutions <- read_xls(here("data", "constitutions", "WORLD_constitutions_2024_6Oct25.xls"))
#
constitutions_variables <- tribble(
  ~varname, ~description, ~details,
  "const_health_20", "Does the constitution explicitly guarantee an approach to non-citizens’ right to health?", "•	The right to health is considered to be protected for non-citizens when the following are explicitly granted to non-citizens or are granted in general and the constitution states that foreign citizens enjoy rights on an equal basis as citizens: a less broad protection of public health, the right to health, the right to healthcare services, the right to public health, the right to medical care, the right to free preventive health services, the right to free medical care, and the right to free health care services.
•	Health rights are explicitly reserved for citizens or restrictions permitted means the constitution explicitly reserves health rights for citizens or permits restrictions on the right to health for non-citizens.  For example, the constitution may state that foreign citizens enjoy all rights except those reserved for citizens and separately state that citizens enjoy the right to health.    
•	No specific provision means that the constitution does not explicitly mention the right to health. This does not mean that the constitution denies this right, but that it does not explicitly include it.
•	Right guaranteed using citizenship language means that the constitution protects the right to health in authoritative language, but uses the word “citizen” instead of “person” to guarantee the right.  For example, constitutions in this category might guarantee that citizens have the right to health.  This language may be interpreted to exclude non-citizens from the right to health. 
•	Guaranteed right, not specific for non-citizens means that the constitution guarantees the work rights to “everyone”, but does not specifically protect non-citizens’ right to health. 
•	Guaranteed for non-citizens means that the constitution guarantees the right to health for foreign citizens in authoritative language.  For example, constitutions in this category might guarantee free medical care regardless of citizenship or guarantee that foreign citizens enjoy the same rights as citizens, including the right to health.",
  "const_podeorebl_sogi", "Does the constitution explicitly guarantee equality or non-discrimination across sexual orientation and gender identity?", "•	Explicit guarantees of equality or non-discrimination across sexual orientation and gender identity include prohibitions of discrimination against sexual minorities, and guarantees of equal rights, guarantees of equality before the law, and guarantees of overall equality regardless of sexual orientation. 
•	No specific provision means that the constitution does not explicitly mention the right to equality or non-discrimination based on sexual orientation or gender identity for all citizens. This does not mean that the constitution denies this right, but that it does not explicitly include it.
•	Equality guaranteed, not specific to SOGI means the right to equality and/or non-discrimination is guaranteed for all citizens, but not specifically on the basis of sexual orientation or gender identity.
•	Aspirational for SOGI means that the constitution protects the general right to equality and/or non-discrimination based on sexual orientation and/or gender identity but does not use language strong enough to be considered a guarantee. For example, constitutions in this category might state that the country aims to protect or promote equality based on sexual orientation.
•	Guaranteed for sexual orientation only means that the constitution protects the right to equality and/or non-discrimination based on sexual orientation in authoritative language, but does not explicitly mention gender identity. For example, constitutions in this category might guarantee citizens’ right to equality based on sexual orientation or make it the State’s responsibility to ensure this right.
•	Guaranteed for sexual orientation and gender identity means that the constitution protects the right to equality and/or non-discrimination based on sexual orientation and gender identity in authoritative language. No country guarantees equality across gender identity, but not sexual orientation.  For example, constitutions in this category might guarantee citizens’ right to equality based on sexual orientation and gender identity or make it the State’s responsibility to ensure this right.",
  "const_samesex_marr", "What is the constitutional status of same-sex marriage?", "•	Denied or may be denied means that the constitution explicitly denies the right to marry to same-sex couples or allows for legislation to do so.    
•	Marriage is defined as between a man and a woman means that the constitution explicitly defines marriage as a union between a man and a woman.
•	Constitution does not address same-sex marriage means that the constitution does not explicitly address the right of same sex couples to marry. The constitution may guarantee the right of men and women to marry and found a family, but it does not use exclusionary or inclusive language to address the right for same-sex couples. 
•	Not explicit, but right to marry is ungendered means that the guarantees the right to marry to “everyone” using ungendered language.
•	Explicitly guaranteed means that the constitution explicitly states that individuals can marry a person of their same sex.",
  "const_podeorebl_23", "Does the constitution explicitly guarantee equality or non-discrimination for persons with disabilities?", "•	Explicit guarantees of equality or non-discrimination for persons with disabilities include prohibitions of discrimination against persons with disabilities, and guarantees of equal rights, guarantees of equality before the law, and guarantees of overall equality or equal opportunity for persons with disabilities. 
•	The term ‘disability’ includes both general references to disabilities and specific mentions of mental or physical disabilities.
•	No specific provision means that the constitution does not explicitly mention the right to equality or non-discrimination for persons with disabilities. This does not mean that the constitution denies this right, but that it does not explicitly include it. 
•	Equality guaranteed, not specific to persons with disabilities means that while the constitution broadly guarantees the right to equality and/or non-discrimination, but it does not specifically do so regardless of disability.
•	Aspirational provision means that the constitution protects the general right to equality and/or non-discrimination for persons with disabilities but does not use language strong enough to be considered a guarantee. For example, constitutions in this category might state that the country aims to protect or promote equality regardless of disability.
•	Guaranteed right means that the constitution protects the right to equality and/or non-discrimination for persons with disabilities in authoritative language. For example, constitutions in this category might guarantee persons with disabilities’ right to equality or make it the State’s responsibility to ensure equality regardless of disability.",
  "const_health_23", "Does the constitution explicitly guarantee the right to health for persons with disabilities?", "•	The right to health is considered to be guaranteed for persons with disabilities if the right to health or the right to medical services is guaranteed. The right to health includes the right of persons with disabilities to physical or overall wellbeing, health protection, health security, or a life free of illness or disease. The right to medical services captures references to the state’s commitment to cure, restore or rehabilitate health; to ensure adequate health facilities for the population; or to provide access to healthcare services, curative services, medical aid, medical assistance or treatment to persons with disabilities.
•	The term ‘disability’ includes both general references to disabilities and specific mentions of mental or physical disabilities.
•	No specific provision means that the constitution does not explicitly protect the right to health for persons with disabilities. This does not mean that the constitution denies this right, but that it does not explicitly include it for persons with disabilities or all persons.
•	Right guaranteed, not disability-specific means that the constitution broadly guarantees the right to health or the right to medical services, but does not specifically guarantee any of these rights to persons with disabilities or broadly protect persons with disabilities from discrimination.
•	Aspirational provision means that the constitution protects the right to health or medical services for persons with disabilities but does not use language strong enough to be considered a guarantee. For example, the country endeavors to provide the right to health for persons with disabilities or intends to provide medical services broadly and persons with disabilities enjoy equal rights.
•	Health rights generally guaranteed and disability discrimination prohibited means that the constitution broadly guarantees the right to health or to medical services and also provides general protection against discrimination based on disability, but does not guarantee any approach to health specifically for persons with disabilities.
•	Guaranteed right means that the constitution explicitly guarantees health rights to persons with disabilities.",
  "const_anyhealth", "Does the constitution explicitly guarantee an approach to the right to health?", "•	Approaches to health include the right to health, public health, or medical care.
•	No specific provision means that the constitution does not explicitly mention health protections. This does not mean that the constitution denies these protections, but that it does not explicitly include them.
•	Guaranteed for some groups, not universally means the constitution explicitly guarantees the right to health, public health, or medical care to specific groups, but not to all citizens. Specific groups that are named in constitutions include children, the elderly, the poor, persons with disabilities, women, and ethnic minorities.
•	Aspirational or subject to progressive realization means that the constitution protects the right to health, public health or medical care but does not use language strong enough to be considered a guarantee, or states that these rights will be implemented progressively or within a certain time period. For example, the nation will endeavor to provide the right to health or will provide medical care within three years.
•	Guaranteed right means that the constitution explicitly guarantees the right to health, medical care, or public health to citizens in authoritative language. For example, constitutions in this category might guarantee citizens’ right to health or make it the State’s responsibility to ensure the protection of the right to health.",
  "const_publichealth", "Does the constitution explicitly guarantee citizens’ right to public health?", "•	The right to public health includes language such as the “defense of public health,” “access to preventive services,” “illness prevention,” “creation of favorable conditions for good health,” etc. Each of these can be guaranteed in broad terms, such as the statement of a right to public health, and/or can be phrased more specifically, such as access to immunizations and health education. 
•	No specific provision means that the constitution does not explicitly mention the right to public health. This does not mean that the constitution denies these protections, but that it does not explicitly include them.
•	Aspirational or subject to progressive realization means that the constitution protects the right to public health but does not use language strong enough to be considered a guarantee, or states that these rights will be implemented progressively or within a certain time period. For example, the nation will endeavor to provide the right to public health or will provide preventive health services within three years.
•	Some aspects guaranteed means that the constitutional explicitly guarantees aspects of the right to public health, such as access to immunization, but only mentions 1 or 2 areas and does not mention them within the context of protection of public health or disease prevention.
•	Guaranteed right means that the constitution explicitly guarantees the right to public health to citizens in authoritative language. Constitutions in this category either broadly guarantee the protection of public health or disease prevention, or enumerate 3 or more areas of health protection that typically constitute public health.  For example, constitutions in this category might guarantee citizens’ right to preventive health care or make it the State’s responsibility to ensure the protection of population health and prevention of disease.",
  "const_enviro", "Does the constitution explicitly guarantee citizens’ right to a healthy environment?", "•	The right to a healthy environment is the right to live in a healthy environment, the right to a pollution-free environment, the right to a clean environment, or provisions to protect the environment to preserve people’s health.  
•	No specific provision means that the constitution does not explicitly mention the right to a healthy environment. This does not mean that the constitution denies this right, but that it does not explicitly include it.  Provisions related to preserving or protecting the environment without referencing health are included in this category.
•	Aspirational or subject to progressive realization means that the constitution protects the right to a healthy environment but does not use language strong enough to be considered a guarantee, or states that this right will be implemented progressively or within a certain time period. For example, the nation will endeavor to provide the right to a healthy environment or will provide environmental protection within three years.
•	Guaranteed right means that the constitution explicitly guarantees the right to a healthy environment to citizens in authoritative language. For example, constitutions in this category might guarantee citizens’ right to a healthy environment or make it the State’s responsibility to ensure protection of the environment.",
  "const_medcare", "Does the constitution explicitly guarantee citizens’ right to medical care?", "•	The right to medical care or services includes language such as “curative services,” “health-care services,” or “disease treatment,” or discussion of the state’s responsibility to restore/rehabilitate health.
•	No specific provision means that the constitution does not explicitly mention the right to medical care. This does not mean that the constitution denies this right, but that it does not explicitly include it.
•	Granted for some groups, not universally means the constitution explicitly guarantees the right to medical care to some groups, but not to all citizens. Specific groups that are named in constitutions include children, the elderly, the poor, persons with disabilities, women, and ethnic minorities.
•	Aspirational or subject to progressive realization means that the constitution protects the right to medical care but does not use language strong enough to be considered a guarantee, or states that these rights will be implemented progressively or within a certain time period. For example, the nation will endeavor to provide access to medical care or will provide medical care within three years.
•	Guaranteed right means that the constitution explicitly guarantees the right to medical care to citizens in authoritative language. For example, constitutions in this category might guarantee citizens’ right to medical services or make it the State’s responsibility to ensure the protection of the right to medical care.
•	Guaranteed free means that the constitution explicitly guarantees the right to free medical care to citizens in authoritative language.",
  "const_health", "Does the constitution explicitly guarantee citizens’ right to health?", "•	The right to health includes language such as the right to “health,” “health security,” and overall well-being”.
•	No specific provision means that the constitution does not explicitly mention the right to health. This does not mean that the constitution denies these protections, but that it does not explicitly include them.
•	Aspirational or subject to progressive realization means that the constitution protects the right to health but does not use language strong enough to be considered a guarantee, or states that these rights will be implemented progressively or within a certain time period. For example, the nation will endeavor to ensure the right to health or will promote the right to health within three years.
•	Guaranteed right means that the constitution explicitly guarantees the right to health to citizens in authoritative language. For example, constitutions in this category might guarantee citizens’ right to health or make it the State’s responsibility to ensure the protection of health."
)
constitutions <- constitutions |> 
  select(country:const_yr_adopt, any_of(constitutions_variables$varname))

# WHO ---------------------------------------------
# https://platform.who.int/data/maternal-newborn-child-adolescent-ageing/data-export
live_births <- readxl::read_xlsx(here("data", "WHO_data_export_250917.xlsx"), 
                                 sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Number of births (thousands)") |> 
  select(year:country_iso_3_code, value_numeric) |> 
  mutate(year = as.numeric(year)) |> 
  rename(livebirths = value_numeric)

live_births_wide <- live_births |> 
  pivot_wider(
    names_from = year, values_from = livebirths, names_prefix = "livebirths_"
  )

maternal_deaths <- readxl::read_xlsx(here("data", "WHO_data_export_250917.xlsx"), 
                                     sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Number of maternal deaths") |> 
  select(year:country_iso_3_code, value_numeric) |> 
  mutate(year = as.numeric(year)) |> 
  rename(maternal_deaths = value_numeric)

maternal_deaths_wide <- maternal_deaths |> 
  pivot_wider(
    names_from = year, values_from = maternal_deaths, names_prefix = "maternal_deaths_"
  )

mmr_WHO <- readxl::read_xlsx(here("data", "WHO_data_export_250917.xlsx"), 
                             sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Maternal mortality ratio (per 100 000 live births) (SDG 3.1.1)") |> 
  select(year:country_iso_3_code, value_numeric) |> 
  mutate(year = as.numeric(year)) |> 
  rename(mmr_WHO = value_numeric)

mmr_WHO_wide <- mmr_WHO |> 
  pivot_wider(
    names_from = year, values_from = mmr_WHO, names_prefix = "livebirths_"
  )

mmr_data_WHO <- mmr_WHO |> left_join(maternal_deaths) |> left_join(live_births) |> 
  mutate(mmr_calc = maternal_deaths/(livebirths/100))

## Antenatal care coverage ------------------------
antenatal_WHO <- read_xlsx(here("data", "antenatal_care_coverage_251009.xlsx"), 
                           sheet = "Data") |> 
  janitor::clean_names() |> 
  filter(indicator=="Antenatal care coverage - at least four visits (%)") |> 
  select(indicator,year:country_iso_3_code, value_numeric) |>
  mutate(year = as.numeric(year)) |> 
  rename(antenatal_WHO = value_numeric)

# GHO ####
## API queries ####
gho_api <- ODataQuery::ODataQuery$new("https://ghoapi.azureedge.net/api")

## Metadata ####
# Indicators list
gho_indicators <- gho_api$path("Indicator")$retrieve()$value |> select(-Language)

# Dimensions list
gho_dimensions <- gho_api$path("Dimension")$retrieve()$value
# Countries list
country_codes <- gho_api$path("Dimension", "COUNTRY", "DimensionValues")$retrieve()$value |> 
  select(Code) |> 
  rename(COUNTRY = Code) |> 
  left_join(state_geo |> select(iso3, country) |> sf::st_drop_geometry(), join_by(COUNTRY==iso3)) |> 
  rename(country_name = country)

# Region list
region_codes <- gho_api$path("Dimension", "REGION", "DimensionValues")$retrieve()$value |> 
  select(Code, Title) |> 
  rename(REGION = Code, region_name = Title)
UN_region_codes <- gho_api$path("Dimension", "UNREGION", "DimensionValues")$retrieve()$value |> 
  select(Code, Title) |> 
  rename(UNREGION = Code, UN_region_name = Title)
WB_income_codes <- gho_api$path("Dimension", "WORLDBANKINCOMEGROUP", "DimensionValues")$retrieve()$value |> 
  select(Code, Title) |> 
  rename(WORLDBANKINCOMEGROUP = Code, WB_income_group = Title)


## Search GHO codes ####
search_term <- "expenditure|spending"
search_term <- "(?=.*expenditure|spending)(?=.*poverty)"
search_term <- "women"
search_term_results <- gho_indicators |> filter(str_detect(IndicatorName, regex(search_term, ignore_case = TRUE))|
                                                  str_detect(IndicatorCode, regex(search_term, ignore_case = TRUE)))

## Indicators ####

### GHE ####
# GHECAUSES <- gho_api$path("Dimension", "GHECAUSES", "DimensionValues")$retrieve()$value |>  tibble()
# GHECAUSE <- gho_api$path("Dimension", "GHECAUSE", "DimensionValues")$retrieve()$value |>  tibble()


### UHC ####
#### Get datasets ####
UHC_AVAILABILITY_SCORE <- gho_api$path("UHC_AVAILABILITY_SCORE")$retrieve()$value |>  tibble()
UHC_INDEX_REPORTED <- gho_api$path("UHC_INDEX_REPORTED")$retrieve()$value|> tibble()
UHC_SCI_CAPACITY <- gho_api$path("UHC_SCI_CAPACITY")$retrieve()$value |> tibble()
UHC_SCI_INFECT <- gho_api$path("UHC_SCI_INFECT")$retrieve()$value |> tibble()
UHC_SCI_NCD <- gho_api$path("UHC_SCI_NCD")$retrieve()$value |> tibble()
UHC_SCI_RMNCH <- gho_api$path("UHC_SCI_RMNCH")$retrieve()$value |> tibble()
#### Combine into one ####
UHC_all <- bind_rows(UHC_INDEX_REPORTED, 
                     UHC_SCI_CAPACITY, UHC_SCI_INFECT,
                     UHC_SCI_NCD, UHC_SCI_RMNCH) |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Health Expenditure ####

# SDGOOP Out-of-pocket expenditure as a percentage of total expenditure on health


### Maternal mortality ratio ####
MMR <- gho_api$path("MDG_0000000026")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  add_row(COUNTRY="GRL", country_name = "Greenland", YEAR = 2023) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  mutate(
    mmr_cat = factor(case_when(
      NumericValue < 10 ~ "<10",
      NumericValue < 20 ~ "10-19",
      NumericValue < 100 ~ "20-99",
      NumericValue < 300 ~ "100-299",
      NumericValue < 500 ~ "300-499",
      NumericValue >= 500 ~ "500+",
      .default = NA), 
      levels = c("<10","10-19", "20-99","100-299", "300-499", "500+" )
    )
  )

### Antenatal care ####
anc4 <- gho_api$path("anc4")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  # group_by(SpatialDim) |> 
  # slice_max(order_by = as.numeric(YEAR), n=1) |> 
  # ungroup() |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Health check after delivery ####
postnatal_care <- gho_api$path("UNICEF_PNCMOTHER")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  group_by(SpatialDim) |> 
  slice_max(order_by = as.numeric(YEAR), n=1) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

postnatal_care_5 <- gho_api$path("pncall5")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  filter(Dim1 == "SEX_FMLE")

postnatal_care_3 <- gho_api$path("pncall3")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

# See: https://maternalhealthatlas.org/factsheets

### Births in health facility ####
institutional_birth <- gho_api$path("SRHINSTITUTIONALBIRTH")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> arrange(YEAR) |> group_by(COUNTRY, YEAR) |> slice_head(n=1)

### HIV death ####
HIV_death <- gho_api$path("HIV_0000000006")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

### Family planning ####

#### Fertility rate ####
fertility_rate <- gho_api$path("tfr")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

#### Need for family planning met ####
family_planning <- gho_api$path("SDGFPALL")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  mutate(
    value_cat = factor(case_when(
      NumericValue< 10 ~ "<10",
      NumericValue< 30 ~ "<30",
      NumericValue< 60 ~ "<60",
      NumericValue< 90 ~ "<90",
      NumericValue>= 90 ~ "90+",
      .default = NA), 
      levels = c("10", "<30", "<60", "<90", "90+")
    ))

#### Modern contraceptive prevalence ####
contraceptive_prevalence <- gho_api$path("cpmowho")$retrieve()$value |>
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

#### Unintended pregnancy ####
unintended_pregnancy <- gho_api$path("SRH_PREGNANCY_UNINTENDED_RATE")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )
#### Abortion rate ####
abortion_rate <- gho_api$path("SRH_ABORTION_RATE")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

#### Adolescent birth rate ####
adolescent_birth_rate <- gho_api$path("MDG_0000000003")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

#### Own informed decisions ####
informed_decisions <- gho_api$path("SG_DMK_SRCR_FN_ZS")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Skilled birth ####
skilled_birth <- gho_api$path("MDG_0000000025")$retrieve()$value |> tibble() |> 
mutate(
  COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
  REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |>
  mutate(
    value_cat = factor(case_when(
      NumericValue< 60 ~ "<60",
      NumericValue< 70 ~ "60-69",
      NumericValue< 80 ~ "70-79",
      NumericValue< 90 ~ "80-89",
      NumericValue< 98 ~ "90-97",
      NumericValue>= 98 ~ "98+",
      .default = NA), 
      levels = c("<60", "60-69", "70-79", "80-89", "90-97", "98+")
    ))

### Density of nursing and midwifery personnel ####
nursing_density <- gho_api$path("HWF_0006")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )



### HPV ####
#### National program ####
HPV_national <- gho_api$path("NCD_CCS_hpv")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

#### Coverage estimates ####
HPV_coverage <- gho_api$path("SDGHPVRECEIVED")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### DPT3 ---------------------
DTP3_coverage <- gho_api$path("WHS4_100")$retrieve()$value |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

# NMIRF status -------------------------------
# Data extracted from Annex 02 of URG's report https://www.universal-rights.org/urg-policy-reports/the-emergence-and-evolution-of-national-mechanisms-for-implementation-reporting-and-follow-up/

nmirf_X <- read_csv(here("data", "NMIRF_status.csv")) |> 
  janitor::clean_names() |> 
  rename(iso3 = iso_3166_1, nmirf_classification = state_classification_in_report)

nmirf_Y <- read_csv(here("data", "NMIRF_Y.csv")) |> 
  janitor::clean_names() |> 
  rename(iso3 = iso_3166_1)

NMIRF <- left_join(nmirf_X, nmirf_Y) |> 
  mutate(
    nmirf_classification = factor(
      nmirf_classification,
      levels = c("NMIRF", "Single inter-ministerial", "Ad hoc inter-ministerial", "Single ministerial", "Hybrid")
    ))
rm(nmirf_X, nmirf_Y)

# Remove non-indicator objects -----------------------
rm(
  country_codes, gho_dimensions, gho_indicators, region_codes, 
  state_geo, theme_labels, gestational_text, UN_region_codes, WB_income_codes, 
  search_term, search_term_results
)

# Save all objects to .rds files
lapply(ls(), function(obj_name) {
  saveRDS(get(obj_name), file = here("data", "API_data", paste0(obj_name, ".rds")))
})
