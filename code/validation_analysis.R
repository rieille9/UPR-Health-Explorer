pacman::p_load(
  openxlsx,
  here,
  tidyverse,
  readxl
)

source(here("code", "theme_labels.R"))
doc_id <- "2510239516"
UPR_validation_master <- readRDS(here("output", "validation", paste0("UPR_validation_master", "_", doc_id,".rds")))
UPR_validation_master_long <- UPR_validation_master |> 
  pivot_longer(cols = 5:ncol(UPR_validation_master)) |> 
  mutate(value = case_when(
    is.na(value) ~ NA,
    value == "Other" ~ FALSE,
    value != "Other" ~ TRUE)) |> 
  rename(value_automated = value) |> 
  left_join(theme_labels, by = join_by(name == variable)) |> 
  mutate(theme_label = case_when(name == "Other health-related" ~ "Other health-related",
                                 .default = theme_label)) |> 
  select(-name)


UPR_validation_input <- function(docid = doc_id, name){
  UPR_validation <- read_xlsx(here("data", "validation", paste0("UPR_validation_", doc_id, "_", name, ".xlsx")),
                              sheet = "recommendations")
  
  UPR_validation_long <- UPR_validation |> 
    rowid_to_column(var = "excel_id") |>
    mutate(excel_id = excel_id+1) |> 
    pivot_longer(cols = `Health-related`:`Other health-related`) |> 
    mutate(value = case_when(is.na(value) ~ FALSE,
                             value == "No" ~ FALSE,
                             value == "Yes" ~ TRUE)) |> 
    rename(
      !! paste0("value_manual", "_", name) := value # use !! and walrus operator so R knows to first evaluate the paste0() function
      ) |> 
    left_join(UPR_validation_master_long |> 
                select(rowid, value_automated, theme_label),
              by=join_by(rowid ==rowid, name == theme_label))
  return(UPR_validation_long)
}

UPR_validation_anshu <- UPR_validation_input(name="anshu")

UPR_validation_anshu |> 
  filter(name ==  "Health-related") |>
  count(value_manual_anshu ==value_automated)

a<-UPR_validation_anshu |> 
  filter(name ==  "Health-related") |>
  filter(
    value_manual_anshu!=value_automated|is.na(value_manual_anshu!=value_automated)
           )

UPR_validation_mattia <- UPR_validation_input(name="mattia")

UPR_validation_mattia |> 
  filter(name ==  "Health-related") |>
  count(value_manual_mattia==value_automated)

b<-UPR_validation_mattia |> 
  # filter(name ==  "Health-related") |>
  filter(value_manual_mattia!=value_automated)

DT::datatable(b)

c<-UPR_validation_anshu |> left_join(UPR_validation_mattia |> 
                                       select(rowid, name,value_manual_mattia))
  filter(name == "Health-related") |>
  filter(value_manual != value_mattia)


