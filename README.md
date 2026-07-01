# Health & Rights Observatory (HaRO)

An R/Shiny dashboard that visualizes the right to health within the Universal Periodic Review (UPR) and displays indicators related to maternal health, family planning, and Universal Health Coverage.

Live app: <https://cehdi-har.share.connect.posit.cloud/>  
Built by: [Global Center for Health Diplomacy and Inclusion (CeHDI)](https://www.cehdi.org/)  
Contact: info[at]cehdi.org

---

## Overview

The Health & Rights Observatory helps diplomats, policymakers, and civil-society actors see how the right to health is reflected in UN human-rights processes and how it relates to population health. It combines three kinds of data:

- UPR recommendations from the OHCHR Universal Human Rights Index, classified into health themes with a rule-based keyword algorithm.
- Health indicators from the WHO Global Health Observatory, the WHO MNCAH platform, IHME's Global Burden of Disease study, UN World Population Prospects, the World Bank, OECD, and UNFPA.
- Legal and normative context on constitutional right-to-health provisions and the world's abortion laws.

The app also generates a downloadable per-country PDF profile, and a separate offline workflow produces standalone country-briefing figures (see Country briefing workflow below).

One page ("UPR impact") presents a linear mixed-effects analysis of whether countries that engaged more with maternal-health UPR recommendations saw faster declines in the maternal mortality ratio (MMR). Those results are associational, not causal, and the app says so. Note that the script behind that figure is no longer in the repository (see Maintenance notes).

## Dashboard features

The app is a `bslib` `page_navbar` with a shared sidebar (regional grouping, region, and country selectors, a global locator map, and a "Download Country Profile" button). Pages:

| Page | What it shows |
| --- | --- |
| Health & Rights Observatory | Landing page: the right to health (ICESCR Art. 12) and how the UPR works. |
| UPR impact | Mixed-effects analysis of UPR maternal-health engagement against MMR. |
| UPR recommendations | Health-related recommendations *By Region* and *By State*: volume per UPR cycle, proportion health-related, top recommending states, and a data table. |
| Universal Health Coverage | UHC Service Coverage Index and RMNCH sub-index maps and trends. |
| Maternal health | *Maternal Mortality* (MMR maps, trends against nearest neighbors, causes of death), *Skilled birth attendance*, and *Abortion* (laws, rates, abortion and miscarriage deaths). |
| Family planning | Met need for family planning with modern methods; unintended-pregnancy rate. |
| Constitutions | Constitutional approaches to the right to health (interactive Leaflet maps). |

Regional groupings available: Global, WHO regions, World Bank regions, UN M49 sub-regions, CARICOM, COMESA, and Fragile and Conflict-affected States (2026).

## Tech stack

- R 4.5.0 (the version pinned in `manifest.json`).
- Shiny and bslib for the UI; plotly and leaflet for interactive charts and maps; sf for spatial data.
- tidyverse packages (dplyr, ggplot2, tidyr, stringr, forcats, readr, lubridate), plus DT, patchwork, ggtext, ggnewscale, janitor, and openxlsx.
- rmarkdown, tinytex, and pdftools for the xelatex country-profile PDF.
- rsconnect and `manifest.json` for deployment to Posit Connect Cloud.
- Package loading uses `pacman` (`pacman::p_load(...)`), so missing CRAN packages install on first run.

The full pinned dependency set (currently 177 packages) lives in `manifest.json`. Note that the modeling packages used for the "UPR impact" analysis (lme4, lmerTest, emmeans, and similar) are no longer in the manifest, because that analysis is not run at app startup.

## Repository structure

```
UPR-Health-Explorer/
├── app.R                       # Main Shiny app: Setup, UI (page_navbar), Server, shinyApp()
├── manifest.json               # Posit Connect lockfile (R version, packages, tracked files)
│
├── code/                       # Data-prep and analysis scripts (run order matters, see Data pipeline)
│   ├── geo_code.R                          # Country geometries -> output/state_geo_enhanced.rds (+ neighbors)
│   ├── external_data_OData.R               # WHO GHO API + World Bank/OECD/UNFPA/UCDP files -> data/API_data/*.rds
│   ├── external_data_GBD.R                 # Process IHME GBD 2021 extracts (also sourced at app startup)
│   ├── theme_labels.R                      # Thematic variable -> label lookup, English and French (sourced at startup)
│   ├── UHRI_recommendation_definitions.qmd # Rule-based classifier -> output/UHRI_UPR_enhanced.rds
│   └── manual_plots_themes_profiles.R      # Offline generation of per-country profile figures (EN + FR)
│
├── output/                     # Processed datasets the app reads at startup
│   ├── UHRI_UPR_enhanced.rds               # Main classified UPR dataset (loaded as `sdg_data`)
│   ├── state_geo_enhanced.rds              # Country geometries
│   └── nearest_neighbors_list.rds          # Per-country nearest neighbors (for "trends vs neighbors")
│
├── data/                       # Inputs and intermediates
│   ├── API_data/                           # ~56 RDS indicator files, loaded in a loop at startup
│   ├── GBD/                                # IHME GBD 2021 extracts (deaths, etiology, maternal disorders)
│   ├── constitutions/                      # WORLD Policy Analysis Center constitutions data + dictionary
│   ├── UPR_WG_docs/                        # Country-briefing workflow: draft reports, extractor, figures
│   ├── UHRI_full.rds, UHRI_UPR.rds         # Large UPR extracts (see Maintenance notes on repo size)
│   ├── *_status.csv, FCS_status_*.csv      # Regional-grouping and fragile-state membership
│   └── *.csv, *.xlsx, *.xls                # World Bank, OECD, UNFPA, HDI, abortion laws, ICESCR, lookups
│
├── report-template.Rmd        # Parameterized country-profile PDF (xelatex)
├── report-template-2.Rmd      # Alternative report layout
├── report-template-3.Rmd      # Alternative report layout
├── preamble.tex               # LaTeX preamble for the PDF reports
├── report_pdfs/               # Output directory for generated PDFs
│
├── flags/                     # Country flag PNGs, named by ISO-2 code
├── countryflag.png            # Fallback flag used when testing profile generation
├── logos/                     # CeHDI logos used in the app
└── www/                       # App static assets (banner images, full_plot.png, custom_bslib.css)
```

## Data sources

All data come from public sources. Retain attribution and check each provider's terms before redistributing the underlying data.

| Source | Used for | Access |
| --- | --- | --- |
| OHCHR, Universal Human Rights Index (UHRI) | UPR recommendations (text and metadata) | API: <https://uhri.ohchr.org/en/our-data-api> |
| WHO Global Health Observatory (GHO) | MMR, ANC, postnatal care, UHC indices, HIV, fertility, abortion and unintended-pregnancy rates | OData API: `https://ghoapi.azureedge.net/api` |
| WHO MNCAH data platform | Births, maternal deaths, MMR, ANC4 | <https://platform.who.int/data/maternal-newborn-child-adolescent-ageing/data-export> |
| IHME, Global Burden of Disease 2021 | Causes of maternal death; deaths and DALYs | <https://vizhub.healthdata.org/gbd-results/> |
| UN World Population Prospects 2024 | Population, life expectancy, TFR | `wpp2024` R package |
| World Bank Open Data (WDI) | GDP per capita (PPP), adult and female literacy, contraceptive prevalence | <https://data.worldbank.org/> |
| World Bank Fragile and Conflict-affected Situations (FY26) | Fragile-state grouping | FCS list FY26 |
| World Bank Worldwide Governance Indicators | Government effectiveness | <https://www.worldbank.org/en/publication/worldwide-governance-indicators> |
| OECD (DAC2a) | Official development assistance disbursements | <https://data-explorer.oecd.org/> |
| UNFPA | Contraceptive prevalence (any method) | UNFPA data portal |
| UCDP (Uppsala Conflict Data Program) | Armed conflict | <https://ucdp.uu.se/downloads/> |
| UNDP, Human Development Report 2025 | Human Development Index | <https://hdr.undp.org/data-center/documentation-and-downloads> |
| Center for Reproductive Rights | World's Abortion Laws (evaluated June 2023) | <https://reproductiverights.org/maps/worlds-abortion-laws/> |
| WORLD Policy Analysis Center | Constitutional right-to-health provisions | <https://www.worldpolicycenter.org/constitutional-approaches-to-the-right-to-health> |
| WHO 2019 report (IRIS) | Thematic classification scheme for recommendations | <https://iris.who.int/handle/10665/277114> |

Map disclaimer: CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown in the app. Boundaries are for reference only.

## Getting started (run locally)

Prerequisites:

- R 4.5.x and, recommended, RStudio.
- A LaTeX engine for the country-profile PDF, installed with `tinytex::install_tinytex()`.
- System libraries for the spatial packages (`sf`, `terra`, `units`): GDAL, GEOS, and PROJ. On Debian or Ubuntu: `sudo apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev`.

Steps:

```r
# 1. Clone the repo (note: it is large, see Maintenance notes)
# git clone https://github.com/CeHDI-Foundation/UPR-Health-Explorer.git

# 2. Open the project in RStudio, then install the bootstrap package
install.packages("pacman")   # pacman installs everything else on first run

# 3. Run the app from the repo root
shiny::runApp("app.R")
```

The app loads pre-processed `.rds` files from `output/` and `data/API_data/` at startup, so it runs without re-fetching any APIs. You only need the data-prep scripts when refreshing the data.

## Data pipeline (refresh workflow)

The scripts in `code/` have implicit dependencies and should be run in order. Each step writes the files consumed by later steps and by the app. Run from the repo root.

1. `code/geo_code.R` builds country geometries (`necountries` plus UN WPP locations), the nearest-neighbor lists, and the fragile-state grouping (from the World Bank FCS FY26 list). It writes `output/state_geo_enhanced.rds` and `output/nearest_neighbors_list.rds`. Run this first, because most other scripts read `state_geo_enhanced.rds`.

2. `code/external_data_OData.R` pulls live WHO GHO data (via `ODataQuery`) and reads the manually downloaded files (World Bank WDI CSVs, OECD DAC2, UNFPA, UCDP, HDI, abortion laws, constitutions). It then saves every object in the workspace to `data/API_data/*.rds`. Read the header note: clear your R environment before sourcing the whole file, because the final loop serializes all workspace objects to `API_data/`. Any stray object becomes a junk file that the app then tries to load.

3. `code/UHRI_recommendation_definitions.qmd` downloads UPR recommendations from UHRI and runs the rule-based classifier (keyword and term dictionaries mapped to WHO thematic groups). It writes the intermediates `data/UHRI_full.rds` and `data/UHRI_UPR.rds`, and the main app dataset `output/UHRI_UPR_enhanced.rds` (loaded as `sdg_data`).

4. `code/external_data_GBD.R` and `code/theme_labels.R` are sourced by `app.R` at startup, so no manual step is needed for a normal run. The first-run GBD ingestion block (combining the raw GBD CSVs) is commented out; uncomment it only when adding new GBD extracts to `data/GBD/`.

Supporting, run as needed:

- `code/manual_plots_themes_profiles.R` generates the per-country profile figures (recommendation and theme breakdowns) in both English and French, using the `theme_label` and `theme_label_fr` columns from `theme_labels.R`. This produces the standalone figures, not the in-app plots.

After any data refresh, do a clean local run of the app before deploying, and check the "UPR recommendations" data table and a few country profiles for missing or duplicated rows.

## Country briefing workflow (`data/UPR_WG_docs/`)

This folder holds a separate, document-driven workflow for producing per-country UPR health briefings from Working Group draft reports:

1. `extract_recs.R` reads a Word draft report and extracts the recommendations into a tidy data frame. It handles two draft formats automatically: Word auto-numbered lists, and paragraphs that begin with a literal "6.1" style number as plain text.
2. `classify_provisional.qmd` applies the same rule-based health classifier to those extracted recommendations before they appear in UHRI.
3. `manual_plots_themes_profiles.R` (in `code/`) renders the profile figure, and the resulting `*_profile.png` files are saved alongside the source documents.

See the Maintenance notes about the draft reports in this folder.

## Deployment (Posit Connect Cloud)

The app is deployed to Posit Connect Cloud from this Git repository.

- Data must be committed to Git. The deployed app reads the `.rds` files from `output/` and `data/API_data/`, so refreshed data files have to be committed and pushed for the live app to pick them up.
- Regenerate the manifest whenever dependencies change (new package, version bump, R upgrade):

  ```r
  rsconnect::writeManifest()
  ```

  Then commit and push the updated `manifest.json`. Connect rebuilds the environment from this file, so a stale manifest is the most common reason a deploy works locally but fails or lags in production.

## Maintenance notes and known issues

Notes for whoever maintains this next:

- The repository is large. Large files can be untracked with `git rm --cached filepath`.
- The "UPR impact" analysis is not reproducible from the repository as it stands. The page shows a static image (`www/full_plot.png`), but the mixed-effects script that produced it (`MLM_alt.R`) has been removed. To update or reproduce that analysis, recover the script from Git history (`git log --diff-filter=D --name-only`) or rewrite it.
- Draft UPR reports are committed to a public repository. `data/UPR_WG_docs/` contains draft Working Group reports, including several marked "circulated on [date]" and one marked "ad referendum" (Latvia). This repository is public. Confirm that these pre-adoption documents are cleared for public distribution, and move them elsewhere if not.
- Bilingual labels must stay in sync. `theme_labels.R` now carries English and French labels, and `manual_plots_themes_profiles.R` uses both. If you add a theme, add both labels, or the French figures will break.
- There is no test or validation code in the repository. The earlier `validation_*` scripts, which compared the automated classifier against manual coding, were removed. Keep a copy elsewhere and re-run it whenever the keyword dictionaries change.
- Live and manual data dependencies. `external_data_OData.R` depends on the WHO GHO OData API and on several manually downloaded files (World Bank, OECD, UNFPA, UCDP, UNDP). Indicator codes and file formats change upstream, so re-run and spot-check after each refresh.

## License and attribution

The app and CeHDI's analyses are © CeHDI. The underlying datasets remain the property of their respective providers (see Data sources); consult each provider's terms before reusing or redistributing data.

Author: Anshu Uppal. Organization: CeHDI.