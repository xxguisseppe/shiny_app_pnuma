# Peru Disease and Climate Dashboard

R Shiny dashboard for visualizing dengue, malaria, and climate-related threshold information in Peru.

## Overview

This repository contains an interactive Shiny application focused on environmental health surveillance. The dashboard connects disease information for dengue and malaria with climate-related variables from PISCO and ERA5 datasets, organized by climatic sectors in Peru.

## Study Context

Climate variability can influence the distribution and intensity of vector-borne diseases. This dashboard presents sector-level threshold messages for dengue and malaria using climate indicators associated with temperature, precipitation, evaporation, pressure, and related environmental variables.

## Objective

Provide an interactive tool to explore disease and climate threshold information by climatic sector in Peru.

## Data Sources

- Disease data: processed dengue and malaria threshold tables for Peru in `data/disease/`.
- PISCO climate dataset: gridded climate information for Peru.
- ERA5 climate dataset: reanalysis climate variables used for environmental interpretation.
- Climatic sectors: spatial sector boundaries stored as a shapefile in `data/spatial/`.

A backup download reference for the complete sector data is documented in `docs/spatial_data_download_link.txt`.

## Dashboard Features

- Interactive Leaflet maps of climatic sectors in Peru.
- Separate views for dengue, malaria falciparum, and malaria vivax.
- PISCO and ERA5 buttons for switching climate-threshold messages.
- Sector-level popups and dynamic explanatory text.
- CDC logo assets served from `www/images/`.

## Repository Structure

```text
.
|-- app.R
|-- data/
|   |-- disease/
|   |   |-- dengue_thresholds_peru.txt
|   |   `-- malaria_thresholds_peru.txt
|   `-- spatial/
|       |-- climate_sectors_peru.cpg
|       |-- climate_sectors_peru.dbf
|       |-- climate_sectors_peru.prj
|       |-- climate_sectors_peru.shp
|       `-- climate_sectors_peru.shx
|-- docs/
|   |-- climate_threshold_context.md
|   `-- spatial_data_download_link.txt
|-- www/
|   `-- images/
|       |-- cdc_logo.jpg
|       `-- cdc_logo.png
|-- LICENSE
`-- README.md
```

## How to Run

1. Open this repository in RStudio or another R environment.
2. Install the required R packages.
3. Run the app:

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
  "rsconnect",
  "markdown"
))
```

## Notes on Data

The disease files in this repository appear to contain processed sector-level messages rather than raw surveillance records. The repository does not include sensitive personal data.

The map depends on the shapefile stored in `data/spatial/`. The app reads it from:

```r
./data/spatial/climate_sectors_peru.shp
```

## Skills Demonstrated

- R Shiny dashboard development
- Interactive geospatial visualization with Leaflet
- Environmental health data communication
- Disease surveillance visualization
- Climate data interpretation with PISCO and ERA5
- Peru-focused geospatial data science

## Limitations

- The included disease tables are processed and do not document the full raw-data workflow.
- Package versions are not currently locked with `renv`.

## Author

Guisseppe Vasquez

## License

This project is licensed under the MIT License.
