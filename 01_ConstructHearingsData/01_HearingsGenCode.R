#### #################################################################### ####
####                    Construct the hearing data                        #### 
#### #################################################################### ####
#### Set the working directory with the hearings folder ####
setwd("01_ConstructHearingsData")
#### ____________________________________________________________________ ####
#### Load the hearing data of CDMX ####
#### ____________________________________________________________________ ####
#### Read curricular data of Judges ####
delitos <- read_delim("01_RawData/01_cdmx/02_datos_audiencias/Info_delitos.csv", ";",escape_double = FALSE, 
                      col_types = cols(bonding_hearing_end = col_datetime(format = "%d.%m.%Y %H:%M"),
                                       bonding_hearing_start = col_datetime(format = "%d.%m.%Y %H:%M"), 
                                       hearing_time_request = col_datetime(format = "%d.%m.%Y %H:%M")), 
                      na = "NA", trim_ws = TRUE)
coords = read_csv("01_RawData/HearingsCoordinates.csv")
crimeCategories = read_excel("01_RawData/04_extras/delitos_agrupacion.xlsx", sheet = "cdmx")
#### Left join longitude and latitude with original data set ####
delitos = left_join(delitos, coords)
#### filter the data set to exclude incomplete data, also create the length variable of each hearing ####
delitos = delitos %>% 
  dplyr::filter(is.na(bonding_hearing_start) == FALSE, is.na(bonding_hearing_end)==FALSE) %>% 
  arrange(judge_number,bonding_hearing_start) %>% 
  mutate(time_minutes = (bonding_hearing_end - bonding_hearing_start))
#### Group the felonys ####
for(i in 1:nrow(delitos)){for (j in 1:nrow(crimeCategories)){
    if(delitos$felony[i] %in% crimeCategories$felony[j] == TRUE){
      delitos$felony[i] = crimeCategories$group[j]}}}
#### Create the variables of time ####
delitos = mutate(delitos, year = as.POSIXlt(delitos$bonding_hearing_start)$year+1900,
                 month = as.POSIXlt(delitos$bonding_hearing_start)$mon + 1,
                 day = as.POSIXlt(delitos$bonding_hearing_start)$mday,
                 hour = as.POSIXlt(delitos$bonding_hearing_start)$hour)
#### save ####
delitos_mx = delitos
save(delitos_mx, file = "02_GenData/delitos_mx.RData")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Load the hearing data of MTY ####
#### ____________________________________________________________________ ####
#### Load data sets ####
del_mty <- read_excel("01_RawData/02_zmm/ArchivoSPIHibrido.xlsx")
datos_web <- read_excel("01_RawData/02_zmm/direccion_delito_data.xlsx")
coords = read_csv("01_RawData/HearingsCoordinates.csv")
CrimeCategories = read_excel("01_RawData/04_extras/delitos_agrupacion.xlsx", sheet = "mty")
#### Extract hour and case code from both files for merging ####
datos_web$Hora = substr(as.character(datos_web$Hora),11,16)
datos_web$Fecha = as.character(datos_web$Fecha)
#### Determine the hour and date of the hearing in the government provided data ####
del_mty = mutate(del_mty, hour = substr(as.character(del_mty$FecInicio), 11, 16 ),
                 date = substr(as.character(del_mty$FecInicio), 1, 10 ))
#### Extract the case_year values ####
del_mty = mutate(del_mty, hearing = paste(`process`, `year`, sep = "/"))
#### Left join both data sets ####
del_mty = left_join(del_mty, datos_web, by = c("hour" = "Hora", "hearing" = "Proceso", "date" = "Fecha"))
#### Take out all the hearings were the location was not recorded ####
del_mty = dplyr::filter(del_mty, is.na(Ubicacion) == FALSE)
#### rename the variables on the mty data set ####
del_mty = select(del_mty, -`process`, -`Juzgado.x`, -`Juzgado.y`, 
                 - date, -`Delito/Juicio`, -`Juzgado.y`, -hour, - Audiencia, - Naturaleza )
colnames(del_mty) = c("year", "start_date", "end_date", "judge_id", "hearing_type", "felony", 
                      "hearing_outcome", "hearing_id", "adress")
#### Extract all of the courthouses outside the Monterrey metropolitan area ####
del_mty = dplyr::filter(del_mty, adress !="Industria Alimenticia , Parque Industrial , Linares, Sala: Sala 1 Ed. Linares", 
                 adress != "Cuauhtemoc , Galeana Centro , Galeana, Sala: Sala 1 Ed. Galeana" ,
                 adress != "Lerdo De Tejada , Doctor Arroyo Centro , Doctor Arroyo, Sala: Sala 1 Ed. Dr. Arroyo" , 
                 adress != "Zaragoza , Villaldama , Villaldama, Sala: Sala 1 Ed. Villaldama",
                 adress != "Ave. CapitAn Alonso De LeOn Km. 3 , Montemorelos Centro , Montemorelos, Sala: Sala Oral Ed. Montemorelos", 
                 adress != "Ave. Capitan Alonso De Leon Km. 3 , Montemorelos Centro , Montemorelos, Sala: Sala Oral Ed. Montemorelos")
#### modify the adresses for the left_join with the coordinates data set ####
del_mty$adress <-mgsub(del_mty$adress, c("Avenida Rodrigo Gomez.*", "Avenida Plutarco Elias Calles.*","Jorge Gonzalez Camarena.*",
                                         "Calle Mariano Matamoros.*", "Carretera Monterrey-Cadereyta Jimenez.*","Avenida Corregidora N.*", 
                                         "Av. Constituyentes.*","  Esq. Jose Garibaldi.*"), 
                       c("esq. Rodrigo Gomez, Calle Av. Aztlan s/n, Sin Nombre de Col 15, 64180 Monterrey, N.L., Mexico", 
                         "Gral. Plutarco Elias Calles S/N, Ignacio Zaragoza, Sin Nombre de Col 13, 67160 Guadalupe, N.L., Mexico", 
                         "Joaquin A. Mora y, Jorge Gonzalez Camarena, Residencial Roble 4o Sector, Residencial Roble 4to Sector, 66418 San Nicolas de los Garza, N.L., Mexico", 
                         "Esq. Jose Garibaldi, Calle Mariano Matamoros 180, Centro, 64000 Monterrey, N.L., Mexico",
                         "Calle Vicente Guerrero SN-S, Sin Nombre de Col 2, 67450 Cadereyta Jimenez, N.L., Mexico",
                         "Av. Corregidora 507, Casco Urbano, 66230 San Pedro Garza Garcia, N.L., Mexico", 
                         "Av. Constituyentes de Nuevo Leon 204 Sin Nombre de Col 21 Monterrey, N.L. Mexico", 
                         "Esq. Jose Garibaldi, Calle Mariano Matamoros 180, Centro, 64000 Monterrey, N.L., Mexico")) 

#### Left join the data set with the courthouse coordinates ####
del_mty = left_join(del_mty, coords, by = c("adress" = "work_adress"))
#### Create ID variables ####
del_mty = del_mty %>% mutate(time_minutes = (end_date - start_date)/60, 
         year = as.POSIXlt(del_mty$start_date)$year+1900,
         month = as.POSIXlt(del_mty$start_date)$mon + 1,
         day = as.POSIXlt(del_mty$start_date)$mday,
         hour = as.POSIXlt(del_mty$start_date)$hour, 
         judge_id = as.numeric(judge_id) + 7000) 
#### Eliminate negative time values due to missrecording #### 
del_mty = dplyr::filter(del_mty, time_minutes > 0)
#### Group the felonys ####
for(i in 1:nrow(del_mty)){
  for (j in 1:nrow(CrimeCategories)){
    if(del_mty$felony[i] %in% CrimeCategories$felonys[j] == TRUE){
      del_mty$felony[i] = CrimeCategories$group[j]}}}
#### Save the crimes in Monterrey ####
save(del_mty, file = "02_GenData/del_mty.RData")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Load the hearing data of GTO ####
#### ____________________________________________________________________ ####
#### load data ####
delitos <- rio::import_list("01_RawData/03_gto/delitos_gto.xlsx")
coords = read_csv("01_RawData/HearingsCoordinates.csv")
CrimeCategories = read_excel("01_RawData/04_extras/delitos_agrupacion.xlsx", sheet = "gto")
#### Take away the last sheet (not necessary) ####
delitos = delitos[-6]
#### Create a vector of city names ###
for(k in 1:length(delitos)){for(i in 1:length(delitos))
    delitos[[k]]$city = names(delitos[k])}
head(delitos)
#### Create unique Judge ID's per city ####
delitos= lapply(delitos, function(x) x = mutate(x, ID_JUEZ = as.numeric(ID_JUEZ)))
delitos= lapply(delitos, function(x) 
  mutate(x, ID_JUEZ = ifelse(city == "GTO", ID_JUEZ + 1000, 
                             ifelse(city == "CELAYA", ID_JUEZ + 2000,
                                    ifelse(city == "LEÓN", ID_JUEZ + 3000, 
                                           ifelse(city == "SALAMANCA", ID_JUEZ + 4000,
                                                  ifelse(city == "IRAPUATO", ID_JUEZ + 5000)))))))
#### Rbindlist all data frames ####
delitos = rbindlist(delitos)
#### Join with main data frame ####
delitos = left_join(delitos, select(coords, -city), by = c("JUZGADO" = "work_adress"))
#### Change the column names ####
delitos = rename(delitos, consecutive_id = CONSECUTIVO, crime_id = CAUSA, 
                 bonding_hearing_start = FECHA.HORA.INICIO.REAL, bonding_hearing_end = FECHA.HORA.FIN.REAL,
                 work_adress = JUZGADO, judge_number = ID_JUEZ, hearing_type = TIPO.AUDIENCIA,
                 felony = DELITOS, resolution = resolucion)
#### Modify the date variable: Increase 12 hours if the meridian is pm ####
for(i in 1:nrow(delitos)){
  if(delitos$meridian_start[i] == "p. m." & as.POSIXlt(delitos$bonding_hearing_start[i])$hour != 12)
    delitos$bonding_hearing_start[i] = delitos$bonding_hearing_start[i] + 12*60*60}

for(i in 1:nrow(delitos)){
  if(delitos$meridian_finish[i] == "p. m." & as.POSIXlt(delitos$bonding_hearing_end[i])$hour != 12)
    delitos$bonding_hearing_end[i] = delitos$bonding_hearing_end[i] + 12*60*60}
#### hearing time in minutes ####
delitos$time_minutes = delitos$bonding_hearing_end - delitos$bonding_hearing_start
#### Subset the date and eliminate negative values due to miss-recording ####
delitos = mutate(delitos, year = as.POSIXlt(delitos$bonding_hearing_start)$year+1900,
                 month = as.POSIXlt(delitos$bonding_hearing_start)$mon + 1,
                 day = as.POSIXlt(delitos$bonding_hearing_start)$mday,
                 hour = as.POSIXlt(delitos$bonding_hearing_start)$hour)
#### Group the felonies into categories####
for(i in 1:nrow(delitos)){
  for (j in 1:nrow(CrimeCategories)){
    if(delitos$felony[i] %in% CrimeCategories$felony[j] == TRUE){
      delitos$felony[i] = CrimeCategories$group[j]}}}
#### Save Guanajuato Felonies ####
delitos_gto = delitos
head(delitos)
save(delitos_gto, file = "02_GenData/delitos_gto.RData")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Merge the hearing data of all three sets ####
#### ____________________________________________________________________ ####
#### load data sets ####
load("02_GenData/del_mty.RData")
load("02_GenData/delitos_mx.RData")
load("02_GenData/delitos_gto.RData")
SentenceType = read_excel("01_RawData/04_extras/delitos_agrupacion.xlsx", sheet = "Clasificacion de resolucion")
#### Take away the unnecessary columns of the data for CDMX ####
delitos_mx = delitos_mx %>%dplyr::select(-hearing_time_request)
#### Take away and rename the unnecessary columns of the data for Monterrey ####
del_mty = del_mty %>%select(-hearing_id) %>% 
  rename(judge_number = judge_id, work_adress = adress, bonding_hearing_start = start_date,
         bonding_hearing_end = end_date, resolution = hearing_outcome)
#### Take away and rename the unnecessary columns of the data for Guanajuato ####
delitos_gto = delitos_gto %>%select(-consecutive_id, -crime_id, -meridian_start, -meridian_finish)
#### Homologize the name of the cities ####
del_mty$city = "ZMM"
delitos_mx$city = "CDMX"
#### Aggregate manually the type of hearing shared by CDMX ####
delitos_mx$hearing_type = "AUDIENCIA DE CONTROL DE IMPUTACION"
#### Rbindlist all of the felonies ####
delitos = rbindlist(list(del_mty, delitos_mx, delitos_gto), use.names = TRUE, fill = TRUE)
#### Reduce hearings to only those under 240 minutes ####
delitos = dplyr::filter(delitos, time_minutes < 240)
#### Classify all the hearing types ####
delitos = mutate(delitos, hearing_type = ifelse(hearing_type == "Formulacion de imputacion. Sin detenido", "Non Attendance",
                                                ifelse(hearing_type %in% c("Audiencia de control de detencion.", "ORDEN DE APREHENSION", "CONTROL DE DETENCION"), "Arrest Warrant",
                                                       ifelse(hearing_type %in% c("Formulacion de imputacion con detenido en caso de aprehension.", "FORMULACION DE IMPUTACION"),"In Fraganti", 
                                                              ifelse(hearing_type %in% c("AUDIENCIA DE CONTROL DE IMPUTACION","VINCULACION A PROCESO") , "Voluntary Presence", NA)))))
#### Classify per type of resolution ####
for(i in 1:nrow(delitos)){for (j in 1:nrow(SentenceType)){
    if(delitos$resolution[i] %in% SentenceType$resolucion[j] == TRUE){
      delitos$resolution[i] = SentenceType$grouping[j]}}}
delitos = mutate(delitos, resolution = ifelse(resolution == "NA" | is.na(resolution) == TRUE, "Data Unavailable", resolution))
#### Organize columns ####
delitos = mutate(delitos, time_minutes = as.numeric(time_minutes))
delitos = select(delitos, c(bonding_hearing_start, bonding_hearing_end, time_minutes, judge_number:city, year, month:hour))
#### Save the data set of judicial hearings ####
save(delitos, file = "02_GenData/delitos.RData")
#### Come back to the project's directory ###
setwd("C:/Users/Sarmiento/Dropbox (Personal)/DIW/pollution_productivity/CodeSJE") # Change it to your computer's project directory
#### Clear the space ####
rm(list = ls())
gc()
