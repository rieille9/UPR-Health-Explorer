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


# Plot of proportion health-related for each cycle ----
by_health <- sdg_data |> 
  filter(state_under_review == "Togo") |> 
  group_by(cycle) |> count(health_related) |> 
  mutate(
    perc = round(n/sum(n)*100,0),
    perc = case_when(
      health_related == "Other" ~ paste0(n),
      .default = paste0(n, " (", sprintf("%1.0f", perc), "%)")
    )
    ) |> 
  ungroup() |> 
  mutate(health_related_fr = fct_recode(
    health_related, "Autres" = "Other", "Liées à la santé" = "Health-related"
  ))

by_health |> 
  ggplot(aes(x = cycle, y = n, fill = health_related_fr)) +
  # scale_fill_manual(values = c("Health-related" = "#E69F00", "Other" = "grey80")) +
  geom_bar(stat = "identity") +
  labs(
    y = "Nombre de recommandations", x = "Cycle de l'Examen périodique universel",
    title = "Number of recommendations received by States",
    fill = NULL
  )+
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5), size = 5) +
  theme_bw()+
  # geom_text(aes(label = sprintf("%1.0f", n_tot), y = n_tot, vjust = -0.2), size = 5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Liées à la santé" = "#ec5557", "Autres" = "grey80"))+
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, color = "#1c164d"),
    axis.text.y = element_text(size = 12, color = "#1c164d"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, color = "#1c164d"),
    # legend.position = "bottom",
    legend.position = c(0, 1),
    legend.justification = c("left", "top"), 
    legend.text = element_text(size = 11,colour = "#1c164d"),
    legend.key.size = unit(15,"pt"),
    # plot.background = element_blank(),
    plot.background = element_rect(color = "#1c164d", fill = NA),
    panel.border = element_rect(color = "#1c164d"),
    # panel.border = element_blank(),
    axis.ticks = element_line(color = "#1c164d"),
    panel.background = element_blank(),
    legend.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.title = element_blank()
  )

ggsave(
  "togo_health.png",
  width = 5,
  height = 3.3,
  dpi = 300,
  # units = "in", 
  bg="transparent"
)

# Theme plots ----
a1 <- sdg_data |> 
  filter(state_under_review == "Togo") |> 
  mutate(
  across(c(health_related:other_health_related), ~ .x != "Other")
  ) |> 
  pivot_longer(health_related:other_health_related, names_to = "theme", values_to = "value") |> 
  mutate(n_tot = n_distinct(rowid)) |> 
  group_by(theme, value, n_tot) |> count(response_upr) |> 
  mutate(perc_supported = n/sum(n)*100) |> 
  filter(value) |> 
  group_by(theme) |> mutate(n_tot_theme = sum(n)) |> 
  ungroup() |> 
  mutate(label_n = paste0(n_tot_theme, " (", sprintf("%1.0f", perc_supported), "%)")) |> 
    
  left_join(theme_labels, join_by(theme == variable)) |> 
  arrange(n_tot_theme) |> 
  filter(theme != "health_related") |> 
  mutate(theme_label_fr = fct_inorder(theme_label_fr),
         response_upr = fct_relevel(response_upr, "Noted"),
         response_upr_fr = fct_recode(response_upr, "Acceptées" = "Supported", "Notées" = "Noted"))

a1 |> 
  droplevels() |> 
  ggplot(aes(x = n, y = theme_label_fr, fill = response_upr_fr))+geom_col(alpha = 0.8, width = 0.85)+
  scale_fill_manual(values = c("#ec5557", "#1c164d", "grey"))+
  labs(
    x = paste0(
      "Nombre de recommandations"
      # "\n",
      # "(Total rec = ", unique(a1$n_tot), ")"
    ),
    title = "Nombre de recommandations liées à la santé",
    y = NULL,
    fill = NULL
  ) +
  geom_text(
    data = a1 |> filter(response_upr_fr == "Acceptées"),
    aes(label = label_n, x = n_tot_theme),
    hjust = -0.05,
    size = 3, color = "#1c164d"
    # vjust = 0.25
  )+
  theme_classic() +
  scale_x_continuous(
    # labels = function(x) paste0(x, "%"),
    # limits = c(0, max_a + 2),
    expand = expansion(mult = c(0, 0.15))
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

ggsave("togo_by_theme.png", 
       width = 7,
       height = 5,
       dpi = 400,
       units = "in"
       )
