# Peru Disease and Climate Dashboard

R Shiny dashboard for visualizing dengue, malaria, and climate-related threshold information in Peru.

## Overview

This repository contains an interactive Shiny application focused on environmental health surveillance. The dashboard connects disease information for dengue and malaria with climate-related variables from PISCO and ERA5 datasets, organized by climatic sectors in Peru.

## Study Context

Climate variability can influence the distribution and intensity of vector-borne diseases. This dashboard presents sector-level threshold messages for dengue and malaria using climate indicators associated with temperature, precipitation, evaporation, pressure, and related environmental variables.

## Objective

Provide an interactive tool to explore disease and climate threshold information by climatic sector in Peru.

## Data Sources

- Disease data: dengue and malaria threshold information for Peru, included as processed text tables.
- PISCO climate dataset: gridded climate information for Peru.
- ERA5 climate dataset: reanalysis climate variables used for environmental interpretation.
- Climatic sectors: spatial sector boundaries stored in `SECTORES/`.

The current repository includes partial shapefile components and a download reference for the complete `SECTORES` spatial data. The complete sector data can be downloaded from:

https://onedrive.live.com/?redeem=aHR0cHM6Ly8xZHJ2Lm1zL3UvcyFBcUpXdEZGSTJpanloM0c5X1hzVEN0amx3bVFaP2U9VDlENlRP&id=F228DA4851B456A2%211009&cid=F228DA4851B456A2

## Dashboard Features

- Interactive Leaflet maps of climatic sectors in Peru.
- Separate views for dengue, malaria falciparum, and malaria vivax.
- PISCO and ERA5 buttons for switching climate-threshold messages.
- Sector-level popups and dynamic explanatory text.
- CDC logo assets stored in `www/`.

## Repository Structure

```text
.
├── app.R
├── DENGUE_v3.txt
├── MALARIA_v2.txt
├── r_text.md
├── SECTORES/
├── www/
├── LICENSE
└── README.md
```

## How to Run

1. Open this repository in RStudio or another R environment.
2. Make sure the complete `SECTORES` shapefile is available inside `SECTORES/`, including `SECTORES.shp` and its companion files.
3. Install the required R packages.
4. Run the app:

```r
shiny::runApp(".")
```

## Required R Packages

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "leaflet",
  "sf",
  "htmltools",
  "tidyverse",
  "rmapshaper",
  "data.table",
  "recipes",
  "rsconnect"
))
```

## Notes on Data

The disease files in this repository appear to contain processed sector-level messages rather than raw surveillance records. The repository does not include sensitive personal data.

The complete shapefile is required for the map to render. If `SECTORES/SECTORES.shp` is missing, the app will not start successfully.

## Skills Demonstrated

- R Shiny dashboard development
- Interactive geospatial visualization with Leaflet
- Environmental health data communication
- Disease surveillance visualization
- Climate data interpretation with PISCO and ERA5
- Peru-focused geospatial data science

## Limitations

- The app depends on external spatial data that may need to be downloaded separately.
- The included disease tables are processed and do not document the full raw-data workflow.
- Package versions are not currently locked with `renv`.

## Author

xxguisseppe

## License

This project is licensed under the MIT License.
