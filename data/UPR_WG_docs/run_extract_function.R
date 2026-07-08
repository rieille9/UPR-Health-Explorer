pacman::p_load(
  here,
  tidyverse
)

# Run the extract function for each country as needed
extract_upr_recs(
  input = "https://www.ohchr.org/sites/default/files/lib-docs/HRBodies/UPR/Documents/session23/LC/UPR23_SaintLucia_recommendations.docx",
  state_under_review = "Saint Lucia", 
  upr_session = 23, 
  document_symbol = "A/HRC/31/10",
  provisional = FALSE, mode = "auto"
)

# Inspect the extracted file for any issues
recs2 <- readRDS(here("data", "UPR_WG_docs", "extracted_recs", "Saint Lucia_23.rds"))


# Uncomment below code for first run
recs_combined <-
  # List the filenames of each rds file in the folder
  list.files(path = here("data", "UPR_WG_docs", "extracted_recs"),
             pattern = "\\.rds$") |> 
  # Read each file into a list
  map(~readRDS(here("data", "UPR_WG_docs", "extracted_recs", .))) |>
  # row-bind each dataframe into a single dataframe
  list_rbind() |> 
  # rename the variables to be consistent with `df_0` object, so that it can easily be appended
  rename(
    text = recommendation, 
    countries_concerned = state_under_review,
    upr_reccomending_states = recommending_states,
    upr_position = position
  ) |> 
  # Format variables as character (to allow row-binding with df_0)
  mutate(
    across(!provisional, as.character)
  ) |> 
  # Add in variables that will be needed for downstream filtering
  mutate(
    upr_reccomending_states = str_replace_all(upr_reccomending_states, ";", " -"), # for consistency with df_0 file
    reccomending_body = "- UPR",
    type = "- Recommendations",
    # Add in an identifier so we know which recommendations were manually added to the dataset
    manual_upload = TRUE
  ) |> 
  select(-c(recommendation_clean, paragraph))

saveRDS(recs_combined, here("data", "UPR_WG_docs", "recs_combined.rds"))