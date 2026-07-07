country_cycles <- sdg_data |> 
  filter(cycle != "Cycle 4") |> 
  droplevels() |> 
  group_by(state_under_review) |> count(cycle, .drop = FALSE) |> 
  arrange(n, cycle)



extract_upr_recs(
  input = "https://www.ohchr.org/sites/default/files/lib-docs/HRBodies/UPR/Documents/session23/LC/UPR23_SaintLucia_recommendations.docx",
  state_under_review = "Saint Lucia", 
  upr_session = 23, 
  document_symbol = "A/HRC/31/10",
  provisional = FALSE, mode = "auto"
  )


recs2 <- readRDS(here("data", "UPR_WG_docs", "extracted_recs", "Saint Lucia_23.rds"))
