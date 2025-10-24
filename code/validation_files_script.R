pacman::p_load(
  openxlsx,
  here,
  tidyverse
)

source(here("code", "theme_labels.R"))
theme_labels_test <- theme_labels |> 
  filter(!variable %in% c("TB_malaria_NTD"))
new_colnames <- theme_labels_test$theme_label
sdg_data <- readRDS(here("output", "UHRI_UPR_enhanced.rds"))

# Create test files -------------------------------------------------
# Create the master sample that will be compared to the manually classified test sample
doc_id <- paste0(format(Sys.Date(), format = "%y%m%d"), sample(1000:9999,1));UPR_validation_master <- sdg_data %>% 
  # Take a random sample of n recommendations from health-related and unrelated
  group_by(health_related) %>%
  slice_sample(n=50) %>%
  ungroup() %>%
  # Shuffle the ordering of the recommendations
  slice_sample(n=nrow(.)) |>
  select(
    text_2, document_code, paragraph, rowid,
         any_of(theme_labels_test$variable)
  )

# Strip the test sample down to the bare values
UPR_validation <- UPR_validation_master |> 
  select(
    text_2, document_code, paragraph, rowid
  ) |> 
  # mutate(across(health_related:maternal_health, ~ NA))
  mutate(
    # `Health-related` = NA,
         `Classification keyterms (separated by ";")` = NA
  )
UPR_validation[new_colnames] <- NA
UPR_validation <- UPR_validation |> relocate(`Classification keyterms (separated by ";")`, rowid, document_code, paragraph, .after = last_col())


# Save to XLSX -------------------------------------------------
# Create a workbook and add data
wb <- createWorkbook()
addWorksheet(wb, "user_details")
addWorksheet(wb, "recommendations")
writeDataTable(wb, "recommendations", UPR_validation, tableStyle = "TableStyleMedium15")
writeData(wb, "user_details", "Name (select one):")

## Data validation ---------------------------
# Define the allowed values for the dropdowns
status_options <- c("Yes", "No")
names_options <- sort(c("Anshu", "Mattia", "Jesse", "Haile"))

# Format the options as a single, comma-separated string inside quotes
# (This is the format Excel requires):
validation_list <- paste0('"', paste(status_options, collapse = ","), '"')
names_list <- paste0('"', paste(names_options, collapse = ","), '"')

dataValidation(
  wb,
  sheet = "recommendations",
  cols = 2:(ncol(UPR_validation)-4),                        
  rows = 2:(nrow(UPR_validation) + 1),     
  type = "list",
  value = validation_list,
  showErrorMsg = TRUE             # Show an error if a user enters an invalid value
)
dataValidation(
  wb,
  sheet = "user_details",
  cols = 2,                     
  rows = 1,         
  type = "list",
  value = names_list,
  showErrorMsg = TRUE             # Show an error if a user enters an invalid value
)

## Conditional formatting ----------------------
empty_style <- createStyle(bgFill = "#FFC7CE")
notempty_style <- createStyle(bgFill = "#C6EFCE")

# 10. Apply conditional formatting to highlight empty cells
#     This rule will apply to all data rows (starting from row 2)
#     and all columns in our data frame.
conditionalFormatting(
  wb,
  sheet = "user_details",
  cols = 2,
  rows = 1,
  type = "blanks", # This is the specific rule for empty cells
  style = empty_style
)
conditionalFormatting(
  wb,
  sheet = "user_details",
  cols = 2,
  rows = 1,
  type = "notBlanks", # This is the specific rule for empty cells
  style = notempty_style
)
conditionalFormatting(
  wb,
  sheet = "recommendations",
  cols = 2,
  rows = 2:(nrow(UPR_validation) + 1),
  type = "blanks", # This is the specific rule for empty cells
  style = empty_style
)
conditionalFormatting(
  wb,
  sheet = "recommendations",
  cols = 2,
  rows = 2:(nrow(UPR_validation) + 1),
  type = "notBlanks", # This is the specific rule for empty cells
  style = notempty_style
)

### Empty rows when "Yes" to health-related ------------------------
num_other_cols <- ncol(UPR_validation) - 6
last_col_letter <- int2col(ncol(UPR_validation)-4)
# Formula: =AND($A2="Yes", COUNTBLANK($B2:$C2)=2)
rule_formula_blank <- paste0(
  'AND($B2="Yes", COUNTBLANK($C2:$',
  last_col_letter,
  '2)=',
  num_other_cols,
  ')'
)
rule_formula_notblank <- paste0(
  "AND($B2='Yes', COUNTBLANK($C2:",
  last_col_letter,
  "2)<",
  num_other_cols,
  ")"
)
conditionalFormatting(
  wb,
  sheet = "recommendations",
  cols = 3:(ncol(UPR_validation)-4),       # Apply formatting to the whole row
  rows = 2:(nrow(UPR_validation) + 1), # For all data rows
  # type = "expression",
  rule = rule_formula_blank,
  style = empty_style
)

## Wrapping and other formatting -------------------------
setColWidths(
  wb,
  sheet = "recommendations",
  cols = 1,      # Target the first column
  widths = 50    # Set its width
)
setColWidths(
  wb,
  sheet = "recommendations",
  cols = 2:(ncol(UPR_validation)-3),      #rest of the columns
  widths = 10    # Set its width
)
setColWidths(
  wb,
  sheet = "recommendations",
  cols = (ncol(UPR_validation)-2):ncol(UPR_validation),
  hidden = TRUE   
)
setColWidths(
  wb,
  sheet = "user_details",
  cols = 1:2, 
  widths = 17      # Set width to autofit
)
s_bold <- createStyle(textDecoration = c("bold"))
addStyle(
  wb,
  sheet = "user_details", 
  style = s_bold, 
  rows = 1,
  cols = 1
)

wrap_style <- createStyle(wrapText = TRUE)
addStyle(
  wb,
  sheet = "recommendations",
  style = wrap_style,
  rows = 1:(nrow(UPR_validation) + 1), # all rows
  cols = 1:ncol(UPR_validation),          # all columns
  gridExpand = TRUE                 # Ensure style is applied to all specified cells
)

freezePane(
  wb,
  sheet = "recommendations",
  firstActiveRow = 2, # The second row is the first one that moves
  firstActiveCol = 2  # The second column is the first one that moves
)

# Protect columns
unlocked_style <- createStyle(locked = FALSE)
addStyle(
  wb,
  sheet = "recommendations",
  style = unlocked_style,
  cols = 2:(ncol(UPR_validation)-3),      # Target columns 2 and 3
  rows = 1:(nrow(UPR_validation) + 1),
  gridExpand = TRUE,
  stack = TRUE                      # Stack this style with existing styles
)
protectWorksheet(wb, sheet = "recommendations")

saveWorkbook(wb, here("output", "validation", paste0("UPR_validation", "_", doc_id,".xlsx")), overwrite = TRUE)
saveRDS(UPR_validation_master, here("output", "validation", paste0("UPR_validation_master", "_", doc_id,".rds")))
saveRDS(UPR_validation, here("output", "validation", paste0("UPR_validation", "_", doc_id,".rds")))
