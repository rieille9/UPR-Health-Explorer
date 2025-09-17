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
  pdftools,
  lme4,
  lmerTest,
  # lspline,
  optimx,
  effects,
  broom.mixed,
  marginaleffects,
  emmeans,
  modelsummary,
  ggeffects
)
# Read data ---------------------------------------
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

# Prep model data ------------------------------------------
start_year = 2005
dat_prep <- MMR |> 
  filter(!is.na(country_name)) |> 
  filter(country_name!="Cook Islands") |> 
  select(ParentLocation, COUNTRY, country_name, YEAR, NumericValue, mmr_cat) |> 
  # mutate(
  #   mmr_cat = factor(case_when(
  #     NumericValue < 100 ~ "<100",
  #     NumericValue < 300 ~ "<200",
  #     NumericValue >= 200 ~ "300+",
  #     .default = NA),
  #     levels = c("<100", "<200", "300+")
  #   )
  # ) |>
  mutate(NumericValue = round(NumericValue)) |> 
  # filter(YEAR %in% c(2005, 2010, 2015, 2018, 2023)) |> 
  filter(YEAR >= start_year) |>
  left_join(mmr_data_WHO, join_by(COUNTRY == country_iso_3_code, YEAR == year)) |> 
  rename(MMR = NumericValue) |> 
  left_join(institutional_birth |> 
              select(COUNTRY, YEAR, NumericValue) |> 
              rename(institutional_birth = NumericValue)) |> 
  left_join(skilled_birth |> 
              select(COUNTRY, YEAR, NumericValue) |> 
              rename(skilled_birth = NumericValue)) |> 
  left_join(family_planning |> 
              select(COUNTRY, YEAR, NumericValue) |> 
              rename(family_planning = NumericValue)) |> 
  left_join(NMIRF |> select(iso3, nmirf_classification), join_by(COUNTRY == iso3)) |> 
  left_join(HDI, join_by(COUNTRY == iso3, YEAR == year)) |> 
  mutate(nmirf_classification = fct_na_value_to_level(nmirf_classification, level = "Unknown"))

n_sup_mh <- sdg_data |> 
  # filter(state_under_review %in% c(mmr_most$country_name)) |> 
  mutate(state_under_review = factor(state_under_review)) |> 
  filter(response_upr %in% c("Supported", "Noted/Other")) |> 
  mutate(response_upr = fct_recode(response_upr, "Noted"="Noted/Other")) |> 
  group_by(state_under_review, response_upr) |> 
  summarise(nsup=sum(maternal_health != "Other")) |> 
  ungroup() |> 
  pivot_wider(names_from = response_upr,values_from = nsup) |> 
  mutate(
    support_ratio = case_when(Noted == 0 & Supported == 0 ~ NA,
                              Supported ==0 ~ 0,
                              Noted == 0 ~ Supported,
                              .default = Supported/Noted),
    perc_dec=case_when(Noted==0 & Supported==0 ~ NA,
                       .default= Supported/(Supported+Noted)),
    n_mh_recs = Noted+Supported,
    log_mh_recs = log(n_mh_recs+1),
    alt = support_ratio/perc_dec,
    perc=perc_dec*100,
    cat_mh_recs = factor(case_when(
      n_mh_recs < 5 ~ "<5",
      n_mh_recs < 10 ~ "5-9",
      n_mh_recs < 15 ~ "10-15",
      n_mh_recs >=15 ~ "15+"
    ))
  ) |> 
  # group_by(state_under_review) |> 
  #   mutate(perc = nsup/sum(nsup)*100,
  #          nsup_tot = sum(nsup)) #|> 
  # filter(response_upr == "Supported") |>
  left_join(state_geo, join_by(state_under_review == country)) |> 
  left_join(world_abortion_laws |> select(country, category), join_by(state_under_review==country)) |> 
  # select(iso3, nsup, perc, nsup_tot, pop, income, category, subregion, FCS_status) |> 
  ungroup() |> 
  mutate(
    perc_cat_50 = fct_relevel(case_when(
      perc >= 50 ~ "\u2265 50%",
      perc < 50 ~ "< 50%",
      .default = NA), "\u2265 50%"),
    
    perc_cat_60 = fct_relevel(case_when(
      perc >= 60 ~ "\u2265 60%",
      perc < 60 ~ "< 60%",
      .default = NA), "\u2265 60%"),
    
    perc_cat_65 = fct_relevel(case_when(
      perc >= 65 ~ "\u2265 65%",
      perc < 65 ~ "< 65%",
      .default = NA), "\u2265 65%"),
    
    perc_cat_70 = fct_relevel(case_when(
      perc >= 70 ~ "\u2265 70%",
      perc < 70 ~ "< 70%",
      .default = NA), "\u2265 70%"),
    
    perc_cat_80 = fct_relevel(case_when(
      perc >= 80 ~ "\u2265 80%",
      perc < 80 ~ "< 80%",
      .default = NA), "\u2265 80%"),
    
    perc_cat_90 = fct_relevel(case_when(
      perc >= 90 ~ "\u2265 90%",
      perc < 90 ~ "< 90%",
      .default = NA), "\u2265 90%"),
    
    perc_cat2 = factor(case_when(
      perc > 90 ~ "> 90%",
      perc >= 70 ~ "70% to 90%",
      perc < 70 ~ "< 70%",
      .default = NA
    ), levels = c("> 90%",
                  "70% to 90%",
                  "< 70%"))
  ) |>  select(iso3, Noted:cat_mh_recs, perc_cat_60:perc_cat2, income, category, region, subregion, wbregion, FCS_status)

dat_model <- left_join(dat_prep, n_sup_mh, join_by(COUNTRY == iso3)) |> 
  filter(COUNTRY!="GRL") |> 
  ungroup() |> 
  # filter(!is.na(perc_cat_60)) |>
  filter(country_name !="Georgia") |> 
  # filter(ParentLocation == "Africa") |>
  mutate(year = ymd(paste0(YEAR,"-01-01"))) |> 
  mutate(
    YEAR=YEAR-min(YEAR),
    KNOT = case_when(year<=ymd("2010-01-01") ~ 0, .default = 1)
  ) |> 
  arrange(country_name, year) |> 
  mutate(MMR_scaled=MMR, 
         livebirths_scaled = livebirths/500
  ) |> 
  group_by(COUNTRY) |> 
  mutate(
    mmr_baseline = MMR[YEAR==0],
    mmr_cat_baseline = mmr_cat[YEAR==0]
    ) |> 
  ungroup() |> 
  mutate(
    mmr_baseline_centered = mmr_baseline-mean(mmr_baseline, na.rm=TRUE),
    mmr_baseline_z = (mmr_baseline-mean(mmr_baseline, na.rm=TRUE))/sd(mmr_baseline, na.rm = TRUE)
  )

# Model parameters ----
dat_model_alt <- dat_model |> filter(!is.na(perc_dec), !is.na(n_mh_recs), !is.na(HDI))
# outcomes <- tribble(~outcome, ~text,
#                     "MMR", "Predicted estimates of MMR",
#                     "institutional_birth")


# Right now I'm using a linear mixed effects model, but the trends are not always really linear. May need to consider splines for the time or even 
m_MMR_0 <- lmer(MMR ~ 1+YEAR 
                      + (1+YEAR |country_name),
                      data= dat_model_alt
                      , weights = livebirths_scaled
                      , REML = FALSE
                      ,control = lmerControl(optimizer ='optimx',
                                             optCtrl=list(method='L-BFGS-B', maxit=2e6))
)
summary(m_MMR_0)
plot(predictorEffects(m_MMR_0))
sigma(m_MMR_0)

m_MMR_1 <- update(m_MMR_0, .~. + mmr_baseline_z)
summary(m_MMR_1)
anova(m_MMR_0, m_MMR_1)

m_MMR_2 <- update(m_MMR_1, .~. + YEAR*perc_dec)
summary(m_MMR_2)
anova(m_MMR_1, m_MMR_2)

m_MMR_3 <- update(m_MMR_1, .~. + YEAR*n_mh_recs)
summary(m_MMR_3)
anova(m_MMR_1, m_MMR_3)

m_MMR_4 <- update(m_MMR_1, .~. + YEAR*perc_dec*n_mh_recs)
summary(m_MMR_4)
anova(m_MMR_1, m_MMR_4)

m_MMR_5 <- update(m_MMR_4, .~. + HDI)
summary(m_MMR_5)
anova(m_MMR_4, m_MMR_5)

# Predictions ----

chosen_model <- m_MMR_5
predm1 <- marginaleffects::plot_predictions(
  chosen_model, 
  condition = list(
    YEAR = unique(chosen_model@frame$YEAR)
    , perc_dec = c(0.5, 1)
    , n_mh_recs = c(5, 10,15)
    # ,mmr_baseline = c(20, 100, 300)
  ),
  draw = FALSE, re.form = NA) |> mutate(
    YEAR = ymd(paste0(YEAR+start_year, "-01-01"))
    ,perc_dec = fct_relabel(
      perc_dec,
      ~ paste0(as.numeric(.x)*100, "%"))
    ,n_mh_recs = fct_relabel(
      n_mh_recs, 
      ~ paste("N =", .x))
  )

## Plot predictions ----
legend_text = str_wrap("% support of maternal health recommendations",20); full_plot <- predm1 |> 
  ggplot(aes(x = YEAR, y = estimate
             , color = perc_dec, fill = perc_dec
             ))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  labs(y="Modelled estimates of MMR", x = "Year",color = legend_text, fill = legend_text,
       title = "Number of issued recommendations related to maternal health")+
  theme_bw()+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.01,0.01),
    legend.justification = c(0,0),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 16),
    title = element_text(size = 15),
    legend.key.size = unit(0.5, "cm"),
    plot.title = element_text(hjust=0.5, size = 16),
    legend.text = element_text(size = 13), 
    legend.title = element_text(size = 13)
  )+ 
  facet_grid(.~n_mh_recs)+scale_y_continuous(limits = c(0,NA)); full_plot

ggsave(here("www", "full_plot.png"), width = 8, height = 4)

## Test pairwise differences of perc_dec ----
my_summary<-summary(
  contrast(
  emtrends(
    chosen_model,
    specs = ~ perc_dec | n_mh_recs,
    var = "YEAR",
    at = list(perc_dec = c(1, 0.5), n_mh_recs = c(5, 10, 15)),
    pbkrtest.limit = 4000,
    lmerTest.limit = 4000
  ), 
  method = "pairwise"
), 
infer=TRUE);my_summary

my_summary |> as_tibble() |> 
  select(-c(SE,df,t.ratio)) |> 
  mutate(across(estimate:upper.CL, ~round(.x,2)),
         p.value=round(p.value,3)) |> 
  mutate(full_estimate = paste0(estimate, " [",lower.CL,",",upper.CL, "]")) |> 
  select(-c(estimate:upper.CL)) |> 
  relocate(full_estimate, .before = p.value) |> 
  gt::gt() |> 
  gt::cols_label(
    "contrast" ~ "Group comparison",
    "n_mh_recs" ~ md("Maternal health<br>recommendations"),
    "p.value" ~ "p-value",
    "full_estimate" ~ "Rate difference"
  )
