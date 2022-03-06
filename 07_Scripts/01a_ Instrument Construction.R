#### #################################################################### ####
####               Construct the Instrumental variable                    #### 
#### #################################################################### ####
#### ____________________________________________________________________ ####
#### Construct the data set of hourly wind direction per court #### 
#### ____________________________________________________________________ ####
#### Load data ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
load("01_ConstructHearingsData/02_GenData/delitos.RData")
#### Select the spatial variables relevant in the pollution data set ###
pol = pol %>% dplyr::rename(longitude_st = longitud, latitude_st = latitud)
coord_pol = unique(dplyr::select(pol, cve_station, longitude_st, latitude_st, city))
#### Transform to numeric the latitude and longitude #####
delitos = mutate_at(delitos, vars(longitude, latitude), as.numeric)
#### Select the coordinates of courthouses #####
coord_del = unique(dplyr::select(delitos, work_adress, longitude, latitude, city))
coord_del = mutate(coord_del, id1 = c(1:nrow(coord_del)))
#### Distance function ####
distance_meters <- function(df1, df2, limit){ 
  distance <- distHaversine(dplyr::select(df1, longitude, latitude), 
                            dplyr::select(df2, longitude_st, latitude_st))
  df_distance <- data.table(station = dplyr::select(df2, cve_station), 
                            courthouse = dplyr::select(df1, id1),
                            distance = distance, city =dplyr::select(df2, city), work_adress =dplyr::select(df1, work_adress) )
  return(dplyr::filter(df_distance, distance < limit))}
#### Calculate distances for at most 25 Km ####
counter = 0 
distance_list = list()
for(i in 1:nrow(coord_pol)){
  distance_list[[i]] = distance_meters(coord_del, coord_pol[i,], 25000)
  counter <<- counter + 1; 
  if(counter %in% seq(0, nrow(coord_pol), 1)){ 
    print(paste(counter, "% has been processed")) }}
#### Bind the distance list ####
distance_stations = rbindlist(distance_list)
#### Change the column names ####
colnames(distance_stations) = c("cve_station", "courthouse","distance","city", "work_adress")
#### determine the closest station to each courthouse ####
dist = distance_stations
rm(coord_del, coord_pol, distance_list, distance_stations, delitos)
#### Take away courthouses and hours without wind direction values ####
pol = dplyr::filter(pol, is.na(WDR) == FALSE)
dist = dplyr::filter(dist, cve_station %in% unique(pol$cve_station))
#### Determine the wind direction in each hour of the day ####
wdr = select(pol, cve_station, city, date, WDR)
wdr = left_join(wdr, dist)
wdr = dplyr::filter(wdr, is.na(courthouse) == FALSE)
#### Select only the wind direction of the closest station ####
wdr = wdr %>% group_by(courthouse, date) %>% dplyr::filter(distance == min(distance))
#### Change the names of the columns for the final data set ####
wdr = select(wdr, courthouse, city,date, wdr_iv = WDR)
#### Change the city names ####
wdr = mutate(wdr, city = mgsub::mgsub(city, c("monterrey", "mexico city","guadalajara","celaya","leon", "salamanca", "irapuato"), 
                                      c("ZMM","CDMX","ZMG","CELAYA","LEÓN", "SALAMANCA", "IRAPUATO" )))
#### Round the wind direction variable to integers ####
wdr = mutate(wdr, wdr_iv = as.numeric(wdr_iv))
wdr = mutate(wdr, wdr_iv = round(wdr_iv, 0))
#### Save the data set ####
write_rds(wdr,file = "03_GenData/wdr_iv.rds")
#### Clear the data space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Aggregate hourly to daily wind direction #### 
#### ____________________________________________________________________ ####
#### Load the data set ####
wdr = read_rds("03_GenData/wdr_iv.rds")
#### Aggregate to the wind direction on the day ####
wdr = mutate(wdr, date = substr(date, 1,10))
wdr = as.data.frame(wdr) %>% group_by(courthouse, city, date) %>% summarise(wdr_iv = mean(wdr_iv))
wdr = unique(wdr)
#### Save the data set ####
wdr = mutate(as.data.frame(wdr), courthouse = as.numeric(courthouse))
write_rds(wdr,file = "03_GenData/wdr_iv_daily.rds")
#### Clear the data space ####
rm(list = ls())
gc()
