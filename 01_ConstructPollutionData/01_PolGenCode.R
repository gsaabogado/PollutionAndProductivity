#### #################################################################### ####
####                  Construct pollution data                            #### 
#### #################################################################### ####
#### Set the working directory of the pollution folder ####
setwd("02_ConstructPollutionData")
#### ____________________________________________________________________ ####
#### Load and manipulate pollution data from Monterrey ####
#### ____________________________________________________________________ ####
#### Load the raw data sets ####
est_2016 <- read_excel("02_RawData/01_zmm/02_estaciones_unidades_parametros/stations.xlsx")
pol_2015 = rio::import_list("02_RawData/01_zmm/01_contaminantes/monterrey_2015_mod.xlsx")
pol_2016 = rio::import_list("02_RawData/01_zmm/01_contaminantes/monterrey_2016_mod.xlsx")
#### Join the data sets on pollution of Monterrey ####
pol_mty  = list(rbindlist(pol_2015), rbindlist(pol_2016))
pol_mty = rbindlist(pol_mty, fill = TRUE)
#### Create the date variable ####
pol_mty  = dplyr::mutate(pol_mty, date = as.POSIXct(date, format  = "%Y-%m-%d-%H-%M"))
#### Determine the time variables ####
pol_mty = select(pol_mty, -time) %>% 
  mutate(time = substr(date, 12,13), year = as.POSIXlt(pol_mty$date)$year+1900, 
         month = as.POSIXlt(pol_mty$date)$mon+1, day = as.POSIXlt(pol_mty$date)$mday)
#### left join the data sets on stations ####
pol_mty = left_join(pol_mty, est_2016)
#### Save the pollution data-set of Monterrey ####
save(pol_mty, file = "03_GenData/pol_mty.RData")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Load and manipulate pollution data for CDMX ####
#### ____________________________________________________________________ ####
#### Read files ####
setwd("02_ConstructPollutionData")
files = list.files(path = "02_RawData/02_cdmx/02_resolucion_hora/01_contaminantes", full.names = TRUE, pattern = ".csv")
data <- rbindlist(lapply(files, fread, sep = ","))
files = list.files(path = "02_RawData/02_cdmx/02_resolucion_hora/03_metereologia", full.names = TRUE, pattern = ".csv")
weather <- rbindlist(lapply(files, fread, sep = ","))
#### Modify the data set to wide format ####
data <- dcast(data, date + cve_station ~ cve_parameter, value.var = "value")
data <- arrange(data, cve_station, date)
#### Read files on stations location and characteristics ####
estaciones <- read.csv("02_RawData/02_cdmx/03_estaciones_parametros_unidades/estaciones.csv",  sep = ",")
#### Eliminate all stations that have closed before 2015 ####
estaciones <- estaciones %>% subset(select = -c(obs_estac, id_station))
#### Merge stations and pollution data ####
data <- merge(data, estaciones, by.x = c("cve_station"), by.y = c("cve_estac"))
#### Create the date variable ####
data$date <- as.POSIXct(data$date, format = "%d/%m/%Y %H:%M")
#### Take away all values smaller than the 01 of January 2015 (First data of the judicial data) ####
data <- dplyr::filter(data, date > "2014-12-31 24:00:00")
#### Create the subset of time variables ####
data <- mutate(data, year = as.POSIXlt(data$date)$year + 1900, 
               month = as.POSIXlt(data$date)$mon + 1, 
               day = as.POSIXlt(data$date)$mday)
#### Modify the data set to longitudinal way ####
weather <- dcast(weather, date + id_station ~ id_parameter, value.var = "value")
#### Merge stations and weather data ####
weather <- merge(weather, estaciones, by.x = c("id_station"), by.y = c("cve_estac"))
#### create a date variable ####
weather$date <- as.POSIXct(weather$date, format = "%d/%m/%Y %H:%M")
#### Take away all values larger than the 01 of January 2015 ####
weather <- dplyr::filter(weather, date >= "2015-01-01 23:00:00")
#### Left join with the pollution data ####
weather <- rename(weather, cve_station = id_station)
pol_mex <- left_join(data, weather)
#### Give a look to the data set ####
#### Save ####
save(pol_mex, file = "03_GenData/pol_mex.RData")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Load and manipulate pollution data for Guanajuato ####
#### ____________________________________________________________________ ####
#### Load the data sets ####
files = list.files(path = "02_RawData/03_gto/01_contaminacion", full.names = TRUE, pattern = ".xlsx")
data = lapply(files, rio::import_list)
estaciones <- read_excel("02_RawData/03_gto/02_estaciones/estaciones.xlsx",  col_types = c("text", "numeric", "numeric", "skip", "skip"))
#### Row bind each city ####
data = lapply(data, rbindlist, fill =TRUE)
data = rbindlist(data, fill =TRUE)
#### Create a date object ####
data$MES <- mgsub(data$MES,  pattern = c("ENE", "FEB", "MZO", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC"), 
                 replacement = c(01, 02, 03,04,05,06,07,08,09,10,11,12)) 
data$MES = as.numeric(data$MES)
data$date = as.POSIXct(paste(data$DIA, data$MES ,data$year, data$HORA, 00, sep = "/"),format = "%d/%m/%Y/%H/%M")
#### Left join stations and pollution ####
data = left_join(data, estaciones)
#### Drop inconsistent data (less than 0.01%) ####
data = data[!duplicated(select(data, date, station)), ] 
#### Create the subset of time variables ####
pol_gto = mutate(as.data.frame(data), 
                 year = as.POSIXlt(date)$year+1900,
                 month = as.POSIXlt(date)$mon+1,
                 day = as.POSIXlt(date)$mday, 
                 hour = HORA)
#### See the data set ####
pol_gto = select(pol_gto, -MES, -HORA, -DIA)
head(pol_gto)
#### filter the unnecessary columns #####
save(pol_gto, file = "03_GenData/pol_gto.RData")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Merge all pollution data sets ####
#### ____________________________________________________________________ ####
#### Load data sets ####
load("03_GenData/pol_mex.RData")
load("03_GenData/pol_mty.RData")
load("03_GenData/pol_gto.RData")
#### homologize names ####
pol_mex = pol_mex %>% mutate(city = "mexico city") %>% dplyr::select(-nom_estac, -PMCO, -alt)
pol_mty = rename(pol_mty, PBa = PRS, TMP = TOUT, WSP = WSR, WDR = WDV, cve_station = station) %>% select(-SR, -time)
pol_gto = rename(pol_gto, TMP = TEMP, PBa = PBAR, WDR = WD, WSP = WS, RH = HR, cve_station = station) %>% select(-RADSOL)
#### Join all pollution measures together ####
pol = rbindlist(list(pol_mex, pol_mty, pol_gto), fill = TRUE)
#### Filter the small cities of Guanajuato (No judicial data) ####
pol = dplyr::filter(pol, !(city %in% c("san miguel", "purisima", "silao", "guanajuato", "san luis de la paz")))
#### Extract the hour of measure ####
pol = pol %>% mutate(hour = as.numeric(substr(as.character(pol$date), 12, 13))) %>%
  mutate_at(vars(month, year, hour, day), as.numeric)
#### Reorganize the data ####
pol = select(pol, c(city, cve_station, longitud, latitud, date, year, month, day, hour, 
                    CO, NO2, O3, PM2.5, PBa, RH, TMP, WDR, WSP, RAINF))
#### Save the final pollution data set ####
save(pol, file = "03_GenData/pol.RData")
#### Come back to the project's directory ###
setwd("C:/Users/Sarmiento/Dropbox (Personal)/DIW/pollution_productivity/CodeSJE") # Change it to your computer's project directory
#### Clear the space ####
rm(list = ls())
gc()



