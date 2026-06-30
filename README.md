# Health & Rights Observatory (HaRO)

An R/Shiny dashboard that visualises the **Right to Health** within the **Universal Periodic Review (UPR)** and links UPR engagement to maternal, reproductive, and universal-health-coverage outcomes.

🔗 **Live app:** <https://cehdi-har.share.connect.posit.cloud/>  
🏛️ **Built by:** [Global Center for Health Diplomacy and Inclusion (CeHDI)](https://www.cehdi.org/)  
✉️ **Contact:** info[at]cehdi.org

---

## Overview

The Health & Rights Observatory helps diplomats, policymakers, and civil-society actors explore how the right to health is reflected in UN human-rights processes and how it relates to population health. It combines:

- **UPR recommendations** (from the OHCHR Universal Human Rights Index), classified into health themes with a transparent rule-based algorithm;
- **Health indicators** from the WHO Global Health Observatory, the WHO MNCAH platform, IHME's Global Burden of Disease study, and UN World Population Prospects;
- **Legal/normative context** on constitutional right-to-health provisions and the world's abortion laws.

The signature analysis ("UPR impact") uses a linear mixed-effects model to test whether countries that engaged more with maternal-health UPR recommendations saw faster declines in the maternal mortality ratio (MMR). Results are **associational, not causal**.

## Dashboard features

The app is a `bslib` `page_navbar` with a shared sidebar (regional grouping, region, and country selectors + a global locator map). Pages:

| Page | What it shows |
| --- | --- |
| **Health & Rights Observatory** | Landing page: the right to health (ICESCR Art. 12) and how the UPR works. |
| **UPR impact** | Preliminary mixed-effects analysis of UPR maternal-health engagement vs. MMR. |
| **UPR recommendations** | Health-related recommendations *By Region* and *By State*: volume per UPR cycle, proportion health-related, top recommending states, and a data table. |
| **Universal Health Coverage** | UHC Service Coverage Index (and RMNCH sub-index) maps and trends. |
| **Maternal health** | *Maternal Mortality* (MMR maps, trends vs. nearest neighbours, causes of death), *Skilled birth attendance*, *Abortion* (laws, rates, abortion/miscarriage deaths). |
| **Family planning** | Met need for family planning (modern methods); unintended-pregnancy rate. |
| **Constitutions** | Constitutional approaches to the right to health (interactive Leaflet maps). |

Regional groupings available: Global, WHO regions, World Bank regions, UN M49 sub-regions, CARICOM, COMESA, and Fragile & Conflict-affected States (2026).

## Tech stack

- **R 4.5.0** (the version pinned in `manifest.json`)
- **Shiny** + **bslib** for the UI; **plotly** and **leaflet** for interactive charts/maps; **sf** for spatial data
- **tidyverse** (dplyr, ggplot2, tidyr, stringr, forcats, readr, lubridate …), plus **DT**, **patchwork**, **ggtext**, **ggnewscale**, **janitor**, **openxlsx**
- **Modelling:** lme4, lmerTest, marginaleffects, emmeans, effects, ggeffects, broom.mixed, modelsummary
- **Reporting:** rmarkdown, tinytex, pdftools, officer, flextable, gt, kableExtra (xelatex country-profile PDFs)
- **Deployment:** rsconnect + `manifest.json` to **Posit Connect Cloud**
- Package loading is handled by **`pacman`** (`pacman::p_load(...)`), so missing CRAN packages install automatically on first run.

The full, version-locked dependency set lives in `manifest.json`.

## Repository structure

```
UPR-Health-Explorer/
├── app.R                       # Main Shiny app — Setup → UI (page_navbar) → Server → shinyApp()
├── manifest.json               # Posit Connect lockfile (R version, 262 pkgs, tracked files)
│
├── code/                       # Data-prep & analysis scripts (run order matters — see Data pipeline)
│   ├── geo_code.R                          # Country geometries → output/state_geo_enhanced.rds (+ neighbours)
│   ├── external_data_OData.R               # WHO GHO / WPP / manual sources → data/API_data/*.rds
│   ├── external_data_GBD.R                 # Process IHME GBD 2021 extracts (also sourced at app startup)
│   ├── theme_labels.R                      # Thematic variable → human-readable label lookup (sourced at startup)
│   ├── UHRI_recommendation_definitions.qmd # CURRENT rule-based classifier → output/UHRI_UPR_enhanced.rds
│   ├── 01_recommendation_definitions.qmd   # LEGACY SDG-based classifier → output/SDG_data_enhanced__.rds
│   ├── MLM_alt.R                           # Mixed-effects model: UPR engagement vs MMR (standalone)
│   ├── validation_files_script.R           # Generate manual-validation sample spreadsheets
│   └── validation_analysis.R               # Compare automated vs. manual classification
│
├── output/                     # Processed datasets the APP reads at startup
│   ├── UHRI_UPR_enhanced.rds               # Main classified UPR dataset (loaded as `sdg_data`)
│   ├── state_geo_enhanced.rds              # Country geometries
│   └── nearest_neighbors_list.rds          # Per-country nearest neighbours (for "trends vs neighbours")
│
├── data/                       # Inputs & intermediates
│   ├── API_data/                           # ~55 RDS indicator files, loaded in a loop at startup
│   ├── GBD/                                # IHME GBD 2021 extracts (deaths, etiology, maternal disorders)
│   ├── constitutions/                      # WORLD Policy Analysis Center constitutions data + dictionary
│   ├── *.rds                               # UHRI extracts + legacy SDG datasets
│   ├── *_status.csv                        # Regional-grouping membership (CARICOM, COMESA, NMIRF, …)
│   └── *.xlsx                              # HDI, abortion laws, country lookups, custom text, WHO export
│
├── report-template.Rmd        # Parameterised country-profile PDF (xelatex)
├── report-template-2.Rmd      # Alternative report layout
├── preamble.tex               # LaTeX preamble for the PDF reports
├── report_pdfs/               # Output directory for generated PDFs
│
├── flags/                     # Country flag PNGs, named by ISO-2 code
├── www/                       # App static assets (banner images, custom_bslib.css)
└── logo.png, logo2.png, logos/
```

## Data sources

All data are from public sources. Please retain attribution and check each provider's licence/terms before re-using the underlying data.

| Source | Used for | Access |
| --- | --- | --- |
| **OHCHR — Universal Human Rights Index (UHRI)** | UPR recommendations (text + metadata) | API: <https://uhri.ohchr.org/en/our-data-api> |
| **WHO Global Health Observatory (GHO)** | MMR, ANC, postnatal care, UHC indices, HIV, fertility, abortion/unintended-pregnancy rates, etc. | OData API: `https://ghoapi.azureedge.net/api` |
| **WHO MNCAH data platform** | Births, maternal deaths, MMR, ANC4 | <https://platform.who.int/data/maternal-newborn-child-adolescent-ageing/data-export> |
| **IHME — Global Burden of Disease 2021** | Causes of maternal death; deaths/DALYs | <https://vizhub.healthdata.org/gbd-results/> |
| **UN World Population Prospects 2024** | Population, life expectancy, TFR | `wpp2024` R package |
| **UNDP — Human Development Report 2025** | Human Development Index | <https://hdr.undp.org/data-center/documentation-and-downloads> |
| **Center for Reproductive Rights** | World's Abortion Laws (evaluated June 2023) | <https://reproductiverights.org/maps/worlds-abortion-laws/> |
| **WORLD Policy Analysis Center** | Constitutional right-to-health provisions | <https://www.worldpolicycenter.org/constitutional-approaches-to-the-right-to-health> |
| **WHO 2019 report (IRIS)** | Thematic classification scheme for recommendations | <https://iris.who.int/handle/10665/277114> |

**Map disclaimer:** CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown in the app. Boundaries are for reference only.

## Getting started (run locally)

**Prerequisites**

- R **4.5.x** and (recommended) RStudio
- A LaTeX engine **only if you build the PDF country profiles** — install via `tinytex::install_tinytex()`
- Some spatial packages (`sf`, `terra`, `units`) need system libraries (GDAL, GEOS, PROJ). On Debian/Ubuntu: `sudo apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev`.

**Steps**

```r
# 1. Clone the repo (note: it is large — see Maintenance notes)
# git clone https://github.com/CeHDI-Foundation/UPR-Health-Explorer.git

# 2. Open the project in RStudio, then install the bootstrap packages
install.packages("pacman")   # pacman installs everything else on first run

# 3. Run the app from the repo root
shiny::runApp("app.R")
```

The app loads pre-processed `.rds` files from `output/` and `data/API_data/` at startup, so **it runs without re-fetching any APIs**. You only need the data-prep scripts when refreshing the data (below).

## Data pipeline (refresh workflow)

The scripts in `code/` have **implicit dependencies and must be run in order**. Each step writes the files consumed by later steps and by the app. Run from the repo root.

1. **`code/geo_code.R`** — builds country geometries (`necountries` + UN WPP locations) and nearest-neighbour lists.
   → writes `output/state_geo_enhanced.rds` and `output/nearest_neighbors_list.rds`.
   *Run this first: almost everything else depends on `state_geo_enhanced.rds`.*

2. **`code/external_data_OData.R`** — pulls live WHO GHO data (via `ODataQuery`), WHO MNCAH, UN WPP 2024, and reads the manual downloads (HDI CSV, abortion-laws XLSX, constitutions).
   → saves **every object in the workspace** to `data/API_data/*.rds`.
   ⚠️ **Read the header note:** clear your R environment *before* sourcing the whole file, because the final loop serialises all workspace objects to `API_data/`. Stray objects will be written as junk files that the app then tries to load.

3. **`code/UHRI_recommendation_definitions.qmd`** — downloads UPR recommendations from UHRI and runs the rule-based health-theme classifier (keyword/term dictionaries mapped to WHO thematic groups).
   → writes intermediates `data/UHRI_full.rds`, `data/UHRI_UPR.rds`, and the **main app dataset** `output/UHRI_UPR_enhanced.rds` (loaded as `sdg_data`).

4. **`code/external_data_GBD.R`** and **`code/theme_labels.R`** — sourced **live by `app.R` at startup**, so no manual step is needed for normal runs. The first-run GBD ingestion block (combining the raw GBD CSVs) is commented out; uncomment it only when adding new GBD extracts to `data/GBD/`.

Optional / supporting:

- **`code/MLM_alt.R`** — the standalone mixed-effects model (lme4/lmerTest) behind the "UPR impact" page. Run it to reproduce or update the model and the `full_plot.png` figure.
- **`code/validation_files_script.R`** → **`code/validation_analysis.R`** — generate manual-coding samples and compute agreement between the automated classifier and human coders. Use these whenever the classification dictionaries change.

> **Tip for maintainers:** after any data refresh, do a clean local run of the app before deploying, and check the "UPR recommendations" data table and a few country profiles for obviously missing or duplicated rows.

## Deployment (Posit Connect Cloud)

The app is deployed to Posit Connect Cloud from this Git repository.

- **Data must be committed to Git.** The deployed app reads the `.rds` files from `output/` and `data/API_data/`, so any refreshed data files have to be committed and pushed for the live app to pick them up.
- **Regenerate the manifest whenever dependencies change** (new package, version bump, R upgrade):

  ```r
  rsconnect::writeManifest()
  ```

  Then commit and push the updated `manifest.json`. Connect rebuilds the environment from this file, so a stale manifest is the most common cause of a deploy that works locally but fails (or silently lags) in production.

## Maintenance notes & known issues  

- **The repository is heavy.** The working tree is ~170 MB and Git history is ~530 MB. `data/UHRI_full.rds` is **68 MB** (above GitHub's 50 MB warning threshold), with several other 20–30 MB `.rds` files. Cloning is slow. Consider **Git LFS** for the large binaries, and/or pruning intermediates that aren't needed at runtime.
- **PDF country-profile download is built but disabled.** `report-template.Rmd`, `report-template-2.Rmd`, and `preamble.tex` generate parameterised country profiles, but the download buttons in the sidebar are currently commented out in `app.R`. Re-enable them there if you want the feature live (and ensure `tinytex` is installed on the server).
- **Live data dependency.** `external_data_OData.R` depends on the WHO GHO OData API and WHO/UN download endpoints being reachable and stable; indicator codes occasionally change upstream. Re-run and spot-check after each refresh.
- **No automated tests.** Validation of the classifier is manual (the `validation_*` scripts). Treat changes to the keyword dictionaries with care and re-run the validation comparison.

## Licence & attribution

The app and CeHDI's analyses are © CeHDI. The underlying datasets remain the property of their respective providers (see **Data sources**); consult each provider's terms before reusing or redistributing data.

**Author:** Anshu Uppal · **Organisation:** CeHDI
