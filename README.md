# Air pollution and cognitive productivity: Evidence from court hearings

This repository contains all the data sets and codes necessary to replicate the tables and graphs in the paper:

Air pollution and the productivity of high-skill labor: Evidence from court hearings published in the Scandinavian Journal of Economics

The complete replication code is in the R programming language:

a) All packages are loaded and installed automatically by opening the project file `pollution_prod.Rproj.` 

b) They are embedded in the ".Rprofile" file. If you want to add or take away any package, you can do it there.

c) If you have issues with any package, here is the Rsession summary:

Attached base packages:

stats | graphics | grDevices | data-sets | utils | methods | base     

Other attached packages:

  | bannerCommenter_1.0.0 | geosphere_1.5-10 | ggmap_3.0.0 | mgsub_1.7.2 | readxl_1.3.1 | tmap_3.3-1 | sf_0.9-8 |scales_1.1.1 | xtable_1.8-4 |  texreg_1.37.5 | Hmisc_4.5-0         | Formula_1.2-4  | survival_3.2-10 | lattice_0.20-41 | lfe_2.8-6 | Matrix_1.2-18 | forcats_0.5.1 | stringr_1.4.0 | dplyr_1.0.5 | purrr_0.3.4 | readr_1.4.0  | tidyr_1.1.3         | tibble_3.1.0 | ggplot2_3.3.3 | tidyverse_1.3.0 | data.table_1.14.0    

## Construct the raw data sets for hearings and pollution

The raw data files I use to construct the panels for the regression design are in the folders under the folder `RawData` of  `01_ConstructHearingsData/` and `02_ConstructPollutionData`.

### Construct hearings panel

`01_ConstructHearingsData` contains the Rscript `01_HearingsGenCode.R` The script manipulates the raw hearing information in the folder 01_RawData and transforms 
it to Rdata files necessary to integrate the data frame delitos.RData. 

This data frame is the clean version of the hearings raw datasets and contains the following variables:

bonding_hearing_start | bonding_hearing_end |   time_minutes | judge_number | hearing_type | felony | resolution | work_adress  |  longitude| latitude |  city | year | month |day |hour   

Variables definition:

a) bonding_hearing_start: Timestamp for the start of the hearing

b) bonding_hearing_end: Timestamp for the end of the hearing

c) time_minutes: The length of the hearing in minutes

d) hearing_type: The type of hearing

e) felony: The prosecuted felony

f) work_adress: The address of the courthouse

* Notes: You need to return to the project's working directory (line 203) at the end of the script. Please change the path to fit your computer

### Construct air pollution panel

`02_ConstructPollutionData` contains the Rscript `01_PolGenCode.R`. The script manipulates the raw pollution information in the folder `01_RawData` and transforms it to 
Rdata files that are necessary to integrate the data frame pol.RData. 

This data frame is the clean version of the air pollution raw datasets and contains the following variables:


city |cve_station | longitud | latitud | date | year | month | day | hour | CO | NO2 | O3 | PM2.5 | PBa RH |TMP | WDR | WSP | RAINF

Variables definition:
a) cve_station: Station identifier

b) CO: Concentration of carbon monoxide in PPM

c) NO2: Concentration of nitrogen dioxide in PPB

d) O3: Concentration of ozone in PPB

e) PM2.5: Concentration of fine particulate matter in micrograms per cubic meter

f) PBa: Atmospheric pressure in millimeters of mercury

g) RH: Relative Humidity in percentages

h) TMP: Temperature in degrees Celsius

i) WDR: Wind direction in degrees

j) WSP: Wind Speed in Kilometers per hour

k) RAINF: Rainfall in millimeters per hour

* Notes: You need to return to the project's working directory (line 129). Please change the path to fit your computer

## Construct the data for the regressions

### Regressions Data set 
`07_scripts/01_ConstructionRegressionData` geographically merges the hearing and pollution information through inverse distance weighting. 

The script creates the file "data_reg.rds." and is the data set I use for the regressions. 

This file contains the following columns:

bonding_hearing_start | bonding_hearing_end | time_minutes | judge_number | hearing_type | felony | resolution | work_adress |  longitude | latitude |city |courthouse | date | CO | NO2 | O3 | PM2.5 | PBa | RH | TMP | WDR | WSP |RAINF  | year |month  | day | hour | day_time | daily_hearings | hourly_hearings | weekday | week |  wdr_iv | TMP_q | RH_q | WSP_q | PBa_q | wdr_iv2


New variables definition:
a) day_time: Subsets of the day; Night, morning, midday, evening

b) daily_hearings: Count of hearings per courthouse per day

c) hourly_hearings: Count of hearings per courthouse per hour

d) wdr_iv: Wind direction value for each hearing in degrees

e) TMP_q:PBa_q: This is a flexible decile specification of weather variables

f) wdr_iv2: Instrumental variable: Factor variable of wind direction in the four different cardinal directions

### Construt the instrumental variable (wind direction)

`07_scripts/01a_InstrumentConstruction.R/ creates a data frame of hourly wind direction values for all courthouses. It does it by using the closest
measuring station's wind direction as the predominant wind direction in the courthouse. 

This file creates the data files "wdr_iv.rds" and "wdr_iv_daily.rds"

The only difference between both files is that one has hourly and the other daily wind direction values for each courthouse.

### Construct alternative data-sets for placebo tests and robustness

`07_scripts/01b_AlternativeRegressionData` constructs alternative data sets that I use in the appendix for robustness 

Only run if you are interested in appendix tables.


## Regression Analysis

All econometric regressions are located in` 07_scripts/02_Regressions.R/`. Researchers can execute them in any order. 

You can identify each regression with its respective table/figure in the study. 

For instance, the regression code chunk with the title "Table 4: Single pollutant log-linear IV with wind direction as inst." Runs the regression that fills 
table 4 of the article. 

Researchers can run each code chunk independently if they are interested in a specific regression.

## Tables

`07_scripts/03_PaperTables`  creates the tables in the article. For instance, the regression code chunk with the title "Figure 2: Plot the average length of the hearing by city" Runs the code that constructs figure 2 of the article. 

Researchers can run each code chunk independently if they are interested in generating a specific figure.

## Figures

`07_scripts/04_PaperFigures`  creates the tables in the article. For instance, the regression code chunk with the title "Figure 2: Plot the average length of the hearing by city" Runs the code that constructs figure 2 of the article. 

Researchers can run each code chunk independently if they are interested in generating a specific figure.











