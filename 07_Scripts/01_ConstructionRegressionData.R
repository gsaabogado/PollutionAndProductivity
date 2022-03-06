#### #################################################################### ####
####                Construct the Regression data set                     #### 
#### #################################################################### ####
#### ____________________________________________________________________ ####
#### Determine the distance between pollution stations and courthouses####
#### ____________________________________________________________________ ####
#### Load data ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
load("01_ConstructHearingsData/02_GenData/delitos.RData")
#### Select the spatial variables relevant in the polution data set ###
pol = pol %>% dplyr::rename(longitude_st = longitud, latitude_st = latitud)
coord_pol = unique(dplyr::select(pol, cve_station, longitude_st, latitude_st, city))
#### Transform to numeric the latitude and longitude #####
delitos = mutate_at(delitos, vars(longitude, latitude), as.numeric)
#### Select the coordinates of courthouses #####
coord_del = unique(dplyr::select(delitos, work_adress, longitude, latitude, city))
coord_del = mutate(coord_del, id1 = c(1:nrow(coord_del)))
#### Distance function ####
distance_meters <- function(df1, df2, limit){ 
  distance <- distHaversine(dplyr::select(df1, longitude, latitude), dplyr::select(df2, longitude_st, latitude_st))
  df_distance <- data.table(station = dplyr::select(df2, cve_station), courthouse = dplyr::select(df1, id1),
                            distance = distance, city =dplyr::select(df2, city), work_adress =dplyr::select(df1, work_adress) )
  return(dplyr::filter(df_distance, distance < limit))}
#### Calculate distances for at most 20 Km ####
counter = 0 
distance_list = list()
for(i in 1:nrow(coord_pol)){
  distance_list[[i]] = distance_meters(coord_del, coord_pol[i,], 20000)
  counter <<- counter + 1; 
  if(counter %in% seq(0, nrow(coord_pol), 1)){ 
    print(paste(counter, "% has been processed")) }}
#### RBIND the distance list ####
distance_stations = rbindlist(distance_list)
#### Change the column names ####
colnames(distance_stations) = c("cve_station", "courthouse","distance","city", "work_adress")
#### Determine the average distance to measuring stations ####
data = distance_stations %>% group_by(city) %>% dplyr::filter(distance == min(distance))
#### Left join ####
delitos = mutate_at(delitos, vars(longitude:latitude), function(x) x = as.numeric(as.character(x)))
delitos_long = left_join(delitos, select(distance_stations, -city), by = "work_adress")
#### Take away station-hearing pairs further away than 25 K ####
delitos_long = dplyr::filter(delitos_long, is.na(distance) == FALSE)
#### Clear unused data ####
rm(distance_list, distance_stations, coord_del, coord_pol, data)
#### Merge the long hearing data and the pollution data ####
data_full = left_join(delitos_long, select(pol, -city))
rm(delitos_long)
#### Create the variables list to input into the inverse distance weighting function ####
as_quosure <- function(strs) rlang::parse_quosures(paste(strs, collapse=";"))
variables = as.data.frame(names(select(data_full, CO:PM2.5, PBa:RAINF))) %>% mutate_all(as.character)
variables = apply(variables, 1, sym)
#### Make all variables numeric ####
data_full = mutate_at(data_full, vars(CO:PM2.5, PBa:RAINF), as.numeric)
#### Create the function to store the IDW values in a list ####
list_idw = list()
for(i in 1:length(variables)){
  list_idw[[i]] = data_full %>% dplyr::filter(is.na(!!variables[[i]]) == FALSE) %>%
    group_by(courthouse, date) %>% 
    summarise(temp = sum((1/distance^2) * !!variables[[i]])/sum(1/distance^2)) 
  colnames(list_idw[[i]]) = c("courthouse", "date", as.character(variables[[i]]))
  print(i)}
#### data set of unique courthouse identifiers ####
head(data_full)
data_short = unique(dplyr::select(data_full, bonding_hearing_start:city, courthouse, date))
#### Merge the IDW values with each courthouse ####
for(k in 1:length(list_idw)){data_short = left_join(data_short, list_idw[[k]])}
#### create the data short set where we left join the IDW values with the additional data ####
data_short = left_join(data_short, unique(dplyr::select(data_full, city, courthouse)))
#### Subset the date and eliminate negative values due to miss-recording ####
data_short = mutate(data_short, year = as.POSIXlt(date)$year+1900,
                 month = as.POSIXlt(date)$mon + 1,day = as.POSIXlt(date)$mday,
                 hour = as.POSIXlt(date)$hour)
#### Clear unnecessary data sets ####
rm(data_full, delitos, list_idw, pol, variables)
#### Add the dummies for time of the day ####
data_short = data_short %>% 
  mutate(day_time = ifelse(hour >= 0 & hour <= 5, "Night",
                           ifelse(hour >= 7 & hour <= 12, "Morning",
                                  ifelse(hour >=13 & hour <= 16, "Midday",
                                         ifelse(hour >= 17 & hour <= 23, "Evening", NA )))))
#### Make the hearing length numeric ####
data_short$time_minutes = data_short$bonding_hearing_end - data_short$bonding_hearing_start
data_short = mutate(data_short, time_minutes = as.numeric(time_minutes)/60)
mean(data_short$time_minutes)
#### Re-level factors ####
data_short = mutate_at(data_short, vars(hearing_type, felony, city, resolution), as.factor)
data_short$hearing_type <- relevel(data_short$hearing_type, "Non Attendance")
data_short$felony <- relevel(data_short$felony, "7")
data_short$resolution = relevel(data_short$resolution, "Rescheduled" )
#### Determine the number of daily and hourly cases in the courthouse ####
data_short = data_short %>% group_by(courthouse, year, month, day) %>% mutate(daily_hearings = n())
data_short = data_short %>% group_by(courthouse, year, month, day,hour) %>% mutate(hourly_hearings = n())
#### Create time varying variables ####
data_short = mutate(data_short, weekday = weekdays(bonding_hearing_start, abbreviate = TRUE), 
                    week = lubridate::isoweek(bonding_hearing_start))
#### Filter negative hearing lengths (less than 0.01% of the data) ###
data_short = dplyr::filter(data_short, time_minutes > 0)
data = unique(data_short)
rm(data_short)
#### Load the weighted data on wind direction ####
wdr = read_rds("03_GenData/wdr_iv.rds")
#### Create the Wind Direction Instrument ####
wdr = mutate(as.data.frame(wdr), courthouse = as.numeric(courthouse))
data = left_join(data, wdr)
#### Filter the hours without data on wind direction ####
data = dplyr::filter(data, is.na(wdr_iv) == FALSE)
#### Create the decile of weather variables ####
data = mutate(as.data.frame(data), TMP_q= cut(TMP, 10, na.rm = TRUE))
data = mutate(as.data.frame(data), RH_q = cut(RH, 10, na.rm = TRUE))
data = mutate(as.data.frame(data), WSP_q= cut(WSP, 10, na.rm = TRUE))
data = mutate(as.data.frame(data), PBa_q= cut(PBa, 10, na.rm = TRUE))
data = mutate(as.data.frame(data), wdr_iv2= cut2(wdr_iv, seq(0,360,90), na.rm = TRUE))
#### Transform all dependent variables to log ####
data = mutate_at(data, vars(time_minutes), function(x) x = log(x))
data = mutate_at(as.data.frame(data), vars(year, week, weekday, courthouse, judge_number, city, TMP_q:wdr_iv2), as.factor)
#### save the regression data ####
write_rds(data, file = "03_GenData/data_reg.rds")
#### Clear the space ####
rm(list = ls())
gc()

#########################################################################################################################
#########################################################################################################################