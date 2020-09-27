# NYenviroScreen
Integrating environmental and social indicators in to further protect vulnerable communities in New York State

## Hazardous Air Pollutant Exposure as a Contributing Factor to COVID-19 Mortality in the United States

This repository contains code and data to reproduce the analysis from our paper "Hazardous Air Pollutant Exposure as a Contributing Factor to COVID-19 Mortality in the United States."

### Code:

[`Data Synthesis nyes.R`](https://github.com/mdpetron/NYenviroScreen/blob/master/Data%20Synthesis%20nyes.R) contains all code for pre-processing the data for the analysis. 

[`Model Development.R`](https://github.com/mdpetron/NYenviroScreen/blob/master/Model%20Development.R) contains the code to develop the NYenviroScreen Models and Decision Tree. 

[`GINI Figure.R`](https://github.com/mdpetron/NYenviroScreen/blob/master/GINI%20Figure.R) contains code to reproduce the NYenviroScreen GINI figure as well as explore other GINI figures.

[`Spider Plots.R`](https://github.com/mdpetron/NYenviroScreen/blob/master/Spider%20Plots.R) contains code to reproduce the Spider Plots displaying each component of the NYenviroScreen score.

[`State Maps.R`](https://github.com/mdpetron/NYenviroScreen/blob/master/State%20Maps.R) contains code to reproduce maps displaying the NYenviroScreen score across the state of New York.

[`NYenviroScreen-app`](https://github.com/mdpetron/NYenviroScreen/tree/master/NYenviroScreen-app) contains the code used to develop the interactive data viewer.

### Data:

| Data Type  | Source | Description |
| ------------- | ------------- | ------------- |
| Environmental Exposures  | [NYS Department of Health - Drinking Water Contaminants Data](https://www.health.ny.gov/statistics/environmental/public_health_tracking/about_pages/drinking_water/export)  | Drinking water contamination |
| Environmental Exposures and Hazards  | [Environmental Justice Indicators from EPA EJSCREEN](https://www.epa.gov/ejscreen)  | Air pollution, hazard proximity, and lead exposure indicators |
| Sensative Populations  | [NYS Department of Health (DOH) Heat Vulnerability Index](https://www.health.ny.gov/environmental/weather/vulnerability_index/)  | 2006-2012 census tract composite score of health vulnerability indicators derived from the American Community Survey and National Land Cover Database |
| Sensative Populations  | [NYU Furman Center’s Flooding Risk](Floodzonedata.us)  |  Percentage of housing units in the 100 and 500 year floodplains in 2018 Census tracts  |
| Sensative Populations | [Food Insecurity via USDA Food Access Research Atlas](https://www.ers.usda.gov/data-products/food-access-research-atlas/) | 2015 census tracts classified as a low income and low access tract measured at 1 mile for urban areas and 10 miles for rural areas  |
| Sensative Populations  | [Environmental Justice Indicators from EPA EJSCREEN](https://www.epa.gov/ejscreen) | Population age characteristics |
| Socioeconomic Vulnerability  | [Environmental Justice Indicators from EPA EJSCREEN](https://www.epa.gov/ejscreen)  | Linguistic isolation, low income, educational attainment, and minority percentage |
| Socioeconomic Vulnerability and Health | [NYS Department of Health (DOH) Heat Vulnerability Index](https://www.health.ny.gov/environmental/weather/vulnerability_index/)  | Census tract unemployment and disability rate |
| Socioeconomic Vulnerability  | [American Community Survey](https://data.census.gov/cedsci/)  | 2018 block group average rent as percentage of average income  |
| Health  | [NYDOH Prevention Agenda 2019-2014 Tracking Indicators: County Trend Data](https://health.data.ny.gov/Health/Prevention-Agenda-2019-2024-Tracking-Indicators-Co/7j59-48xy)  | Asthma, heart attacks, preterm births, premature deaths,  |
| Health  | [NYDOH Cancer Mapping Database](https://www.health.ny.gov/statistics/cancer/environmental_facilities/mapping/)  | 2011-2015 different than expected cancer incidence  |

### Contact Us:

* Email: mdpetron@syr.edu

### Terms of Use:

* Authors/funders retain copyright (where applicable) of code on this Github repo.

