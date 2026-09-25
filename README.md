# Unseen-Borders
This respository contains scripts used for data cleaning, data analysis, and visualization creation of my undergraduate Honors Thesis ***Unseen Borders: Sprawl's Role in Fueling Las Vegas Inequality***. 

This README file provides an overview of the research questions, statistical methodologies, and data used to create this research project. It also details how to navigate this repository to view the cleaned data, key findings, and figures of my research.

## Thesis Links
* **Abbreviated Version**
* **Full Version**

## Research Questions
This project explores how urban sprawl shapes everyday life in the Las Vegas Valley. It specifically analyzes whether a census tract's level of sprawl affects residents' commute lengths, primary mode of transportation, access to employment opportunities, and exposure to air pollution. It also explores whether these effects are disproportionately experienced by people of color and lower-income residents, introducing a novel Sprawl Burden Index to quantify that disparity. Las Vegas makes a compelling case study for this project, as it is one of the most racially diverse and fastest-growing cities in the United States, yet the consequences of its rapid outward expansion remain understudied.

## Methodologies
To test sprawl inequality, I used the following statistical methodologies: 
* **Ordinary Least Squares (OLS) Regression**: OLS models are used to quantify the directional relationships between sprawl and each outcome variable (commute length, transportation mode, employment composition, and air pollution exposure) while controlling for race (% of people of color) and median income as cofounding variables.
* **Principle Component Analysis (PCA)**: PCA is used to construct a Sprawl Burden Index, which quantifies the cumulative burden that urban sprawl places on racially and financially marginalized populations. PCA reduces the multi-dimensional set of sprawl outcomes into a single composite score, captured by the first principal component, allowing for geographic comparison of burden across census tracts.
* **LASSO Regression**: LASSO regression is used alongside the combined OLS model to identify which sprawl outcomes have the strongest preditive relationships with the sprawl index.  
* **Two-Sample T-Test**: The two-sample T-test is used to assess whether the average sprawl index differs significantly between census tracts where the majority of residents commute by private vehicle versus those who use other means of transportation.

## Data
To measure these research questions, I used the following data: 

* [<u>**Urban Sprawl Index**</u>](https://gis.cancer.gov/tools/urban-sprawl/) - *Reid Ewing & Shima Hamidi (2010)*
* [<u>**Predicted PM<sub>2.5</sub> Concentrations**</u>](https://data.cdc.gov/Environmental-Health-Toxicology/Daily-Census-Tract-Level-PM2-5-Concentrations-2006/ujra-cbx5/about_data) - *Centers for Disease Control and Prevention (2010)*.
* [<u>**U.S. Census Bureau**</u>](https://data.census.gov/table) - *American Community Survey (2010)*. 
  * DP05 - Demographic and Housing Estimates (race, population)
  * DP03 - Selected Economic Characteristics (mean commute time, occupation)
  * B08301 - Means of Transportation to Work
  * B08303 - Travel Time to Work
  * B08141 - Means of Transportation to Work by Vehicles Available
  * B24011 - Occupation by Median Earnings

The tables listed under the US Census Bureau are available to view via the Census data explorer. Select: Year = 2010, Geography = Census Tract -> Nevada => Clark County => All Census Tracts

## Code Structure

This analysis is organized into two stages: data preparation and analysis. All scripts are located in the `r_scripts/` folder and are designed to be run in order. 

**<u>Data Preparation</u>**
* `01_data_cleaning.R`: Imports and cleans all raw data sources, standardizes census tract identifiers, reverses sprawl index scale, and exports a single Excel workbook (`data/processed/thesis_data.xlsx`), with one sheet per dataset.

**<u>Data Analysis</u>**
* `02_hypothesis_1.R`: OLS regressions examining the relationship between urban sprawl, race, and income
* `03_hypothesis_2.R`: OLS regressions examining the relationship between sprawl and comute length, including summary statistics by transportation method
* `04_hypothesis_3.R`: T-test and OLS regressions examining the relationship between sprawl and private vehicle usage
* `05_hypothesis_4.R`: OLS regressions examining the relationship between sprawl and employment composition (specifically management vs. service occupations)
* `06_hypothesis_5.R`: OLS regressions examining the relationship between sprawl and PM<sub>2.5</sub> air pollution exposure
* `07_combined_analysis.R`: Combined OLS and LASSO regression models across all sprawl outcomes
* `08_summary_stats.R`: Summary statistics for all analysis variables (Table 2 in the thesis)
* `09_maps.R`: Choropleth maps of sprawl, median income, and percent people of color across the Las Vegas Valley. 
* `10_burden_maps.R`: Map of the Sprawl Burden Index across the Las Vegas Valley. 

**<u>Exploratory</u>** (not used in final thesis)
* `exploratory/`: Scripts from earlier stages of analysis that were not incorporated into the final paper, including alternative model specifications and race-group breakdowns




