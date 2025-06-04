# Emissions Pipeline

### Authors
Carmen Hoyt [@ceh58](https://github.com/ceh58)

Nicole Pepper [@nicolelpepper](https://github.com/nicolelpepper)

Stephen Carroll [@stephenccodes](https://github.com/stephenccodes)

Josh Mull [@llumj](https://github.com/llumj)

### About

This repository hosts the code for assigning *broadcasting* and *non-broadcasting* emissions estimates (in metric tons) for 9 pollutants (CO2, CH4, N2O, NOX, SOX, CO, VOCs, PM2.5, PM10) to FAO catch (in metric tons). More about the analysis can be found [here](https://seamissions.github.io/emissions-pipeline/).

### Data

Data must be downloaded separately and be locally accessible in the corresponding folders below. All files are .csv files with the exception of the FAO Region Shapefile (which is a compilation of .shp, .shx, .sbn, .prj, and .dbf files). 

[Emissions]

- Emissions data is available from 2016 onward in near-real-time.

[FAO Seafood Production](https://www.fao.org/fishery/en/collection/capture?lang=en)

- FAO catch data is available up through 2022.

[FAO Region Shapefile](https://www.marineregions.org/gazetteer.php?p=details&id=22541)

[Sea Around Us](https://www.seaaroundus.org/data/#/fao)

### Repository Structure
```
├── data/
│   ├── raw/ # download data separately
│   │    ├── emissions/ 
│   │    │   ├── broadcasting.csv 
│   │    │   └── non-broadcasting.csv 
│   │    ├── fao-seafood-produciton/ 
│   │    │   ├── Capture_Quantity.csv 
│   │    │   ├── CL_FI_SPECIES_GROUPS.csv
│   │    │   └── CL_FI_COUNTRY_GROUPS.csv
│   │    ├── fao-region-shapefile/ 
│   │    │   ├── World_Fao_Zones.shp
│   │    │   ├── World_Fao_Zones.shx
│   │    │   ├── World_Fao_Zones.sbn
│   │    │   ├── World_Fao_Zones.prj
│   │    │   └── World_Fao_Zones.dbf
│   │    ├── sea-around-us/ 
│   │    │   ├── SAU FAO 27 v50-1.csv
│   │    │   ├── SAU FAO 88 v50-1.csv
│   │    │   ├── SAU FAO 71 v50-1.csv
│   │    │   ├── SAU FAO 48 v50-1.csv
│   │    │   ├── SAU FAO 67 v50-1.csv
│   │    │   ├── SAU FAO 51 v50-1.csv
│   │    │   ├── SAU FAO 61 v50-1.csv
│   │    │   ├── SAU FAO 57 v50-1.csv
│   │    │   ├── SAU FAO 27 v50-1.csv
│   │    │   ├── SAU FAO 58 v50-1.csv
│   │    │   ├── SAU FAO 34 v50-1.csv
│   │    │   ├── SAU FAO 47 v50-1.csv
│   │    │   ├── SAU FAO 18 v50-1.csv
│   │    │   ├── SAU FAO 21 v50-1.csv
│   │    │   ├── SAU FAO 87 v50-1.csv
│   │    │   ├── SAU FAO 31 v50-1.csv
│   │    │   ├── SAU FAO 81 v50-1.csv
│   │    │   └── SAU FAO 77 v50-1.csv
│   │    └── master_species_key.csv
│   └── processed/
│        ├── full_emissions_fao.csv 
│        └── full_emissions_sau.csv 
├── notebooks/
│   ├── data-assembly.qmd
│   ├── data-pipeline.qmd
│   ├── fao-sau-comparison.qmd
│   └── visualizations.qmd
└── README.md
```
