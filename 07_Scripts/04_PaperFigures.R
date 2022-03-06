#### ###################################################################################################### ####
####                            Script to create all images in the paper                                    #### 
#### ###################################################################################################### ####
#### ______________________________________________________________________________________________________ ####
#### Figure 1: Spatial location of courthouses and monitoring stations ####
##### ______________________________________________________________________________________________________ ####
#### Select the stations in Mexico City ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
mun = st_read("05_ExtraData/01_shp/04_municipios/municipios_2010_5A.shp", options = "ENCODING=WINDOWS-1252")
data = read_rds("03_GenData/data_reg.rds")
#### Filter the shape-file to the relevant counties ####
mun = dplyr::filter(mun, CVE_ENT %in% c("09", "11", "19"))
mun = dplyr::filter(mun, NOM_MUN %in% c("León", "Monterrey", "Irapuato", "Celaya", "Salamanca", "San Pedro Garza García", 
                                        "San Nicolás de los Garza", "Guadalupe","Gral. Escobedo")| CVE_ENT == "09")
#### Homologize the name of counties part of the same city ####
mun = mutate(mun, NOM_MUN = ifelse(CVE_ENT == "09", "Mexico City", NOM_MUN))
mun = mutate(mun, NOM_MUN = ifelse(NOM_MUN %in% c("Celaya", "Irapuato", "Salamanca"), "Bajio", NOM_MUN))
mun = mutate(mun, NOM_MUN = ifelse(NOM_MUN %in% c("Monterrey", "San Pedro Garza García", 
                                                  "San Nicolás de los Garza", "Guadalupe", 
                                                  "Gral. Escobedo"), "Monterrey", NOM_MUN))
#### Transform the projection of the shapefile ####
mun = st_transform(mun, crs = 4326)
#### Extract the longitude and latitude of courthouses ####
courthouse = unique(select(data, longitude, latitude, city))
courthouse = st_as_sf(courthouse, coords = c("longitude", "latitude"), crs = st_crs(mun))
courthouse$city = as.character(courthouse$city)
courthouse= mutate(courthouse, city = ifelse(city %in% c("CELAYA", "IRAPUATO", "SALAMANCA"),"Bajio", city))
#### Extract the longitude and latitude of pollution monitoring stations ####
stations = unique(select(as.data.frame(pol), cve_station, longitud, latitud, city))
stations = st_as_sf(stations, coords = c("longitud", "latitud"), crs = st_crs(mun))
stations = rename(stations, NOM_MUN = city)
stations = mutate(stations, NOM_MUN = stringr::str_to_title(NOM_MUN))
stations = mutate(stations, NOM_MUN = ifelse(NOM_MUN %in% c("Celaya", "Irapuato", "Salamanca"),"Bajio", NOM_MUN))
class(stations)
#### Only keep the stations that fall within the cities in the analysis ####
overlay = as.data.frame(st_join(stations, select(mun, -NOM_MUN), join = st_intersects))
overlay = dplyr::filter(overlay, is.na(CVE_MUN) == FALSE)
stations = dplyr::filter(stations, cve_station %in% overlay$cve_station)
#### Split the data set into cities####
stations = split(stations, f = stations$NOM_MUN)
courthouse = split(courthouse, f = courthouse$city)
mun = split(mun, f = mun$NOM_MUN)
#### Put mexico city and monterrey together ####
shapes = do.call("rbind", list(mun$`Mexico City`, mun$Monterrey))
ch = do.call("rbind", list(courthouse$CDMX, courthouse$ZMM))
st = do.call("rbind", list(stations$`Mexico City`, stations$Monterrey))
head(st)
merged = tm_grid(alpha = 0.3,labels.size = 0.5,
                 labels.format = list(format = "f", digits = 1)) +
  tm_shape(shapes) +
  tm_polygons() +
  tm_facets(by = "NOM_MUN", ncol = 2, drop.units = TRUE) +
  tm_shape(st) +
  tm_dots(col = "black", size = 0.2) +
  tm_shape(ch) +
  tm_dots(col = "darkgrey", size = 0.2, shape = 22) +
  tm_compass(position = c("left", "top"), text.size = 0.7) +
  tm_scale_bar(breaks = c(0, 7.5,15), position = c("left", "bottom"), text.size = 0.7) +
  tm_layout(bg.color = "white", legend.position = c("right", "top"),
            legend.frame = "white", legend.just = "center",
            panel.label.bg.color = "transparent",
            inner.margins = c(0.01,0.15,0.01,0.15), outer.margins = c(0.01,0.01,0.01,0.01),
            panel.label.size = 0.8, panel.label.height = 2) 
tmap_save(merged, "06_figures/MapShapes.png",  width = 7, height = 3.5)
#### Map of courthouses and stations in CDMX ####
cdmx = tm_grid(alpha = 0.3,labels.size = 0.5, labels.format = list(format = "f", digits = 1)) + 
  tm_shape(mun$`Mexico City`) + 
  tm_fill(col = rgb(204,218,227, maxColorValue = 255), alpha = 0.2) +
  tm_borders(col = "black") +
  tm_shape(stations$`Mexico City`) + 
  tm_dots(col = "black", size = 0.2) +
  tm_shape(courthouse$CDMX) + 
  tm_dots(col = "darkgrey", size = 0.2, shape = 22) +
  tm_compass(position = c("left", "top"), text.size = 0.5) +
  tm_scale_bar(width = 0.20, position = c("left", "bottom")) +
  tm_layout(inner.margins = c(0.01,0.15,0.01,0.15), outer.margins = c(0.01,0.01,0.01,0.01))
cdmx
tmap_save(cdmx, "06_figures/MapCdmx.png",  width = 2.5, height = 2.5, scale = 0.8)
#### Map of courthouses and stations in Monterrey ####
mty = tm_grid(alpha = 0.3,labels.size = 0.5, labels.format = list(format = "f", digits = 1)) + 
  tm_shape(mun$Monterrey) + 
  tm_fill(col = rgb(204,218,227, maxColorValue = 255), alpha = 0.2) +
  tm_borders(col = "black") +
  tm_shape(stations$Monterrey) + 
  tm_dots(col = "black", size = 0.2) +
  tm_shape(courthouse$ZMM) + 
  tm_dots(col = "darkgrey", size = 0.2, shape = 22) +
  tm_compass(position = c("left", "top"), text.size = 0.5) +
  tm_scale_bar(width = 0.25, position = c("left", "bottom")) +
  tm_layout(inner.margins = c(0.01,0.15,0.01,0.15), outer.margins = c(0.01,0.01,0.01,0.01))
tmap_save(mty, "06_figures/MapMty.png", width = 2.5, height =2.5, scale = 0.8)
#### Map of courthouses and stations in Leon (appendix) ####
leon = tm_grid(alpha = 0.3,labels.size = 0.5, labels.format = list(format = "f", digits = 1)) +
  tm_shape(mun$León) + 
  tm_fill(col = rgb(204,218,227, maxColorValue = 255), alpha = 0.2) +
  tm_borders(col = "black") +
  tm_shape(stations$Leon) + 
  tm_dots(col = "black", size = 0.2) +
  tm_shape(courthouse$LEÓN) + 
  tm_dots(col = "darkgrey", size = 0.2, shape = 22) +
  tm_compass(position = c("left", "top"), text.size = 0.5) +
  tm_scale_bar(width = 0.20, position = c("left", "bottom"))  +
  tm_layout(inner.margins = c(0.05,0.15,0.05,0.10), outer.margins = c(0.01,0.01,0.01,0.01))
tmap_save(leon, "06_figures/MapLeon.png", width = 2.5, height =2.5)
#### Map of courthouses and stations in Bajío cities (appendix) ####
bjx = tm_grid(alpha = 0.3,labels.size = 0.5, labels.format = list(format = "f", digits = 1)) +
  tm_shape(mun$Bajio) + 
  tm_fill(col = rgb(204,218,227, maxColorValue = 255), alpha = 0.2) +
  tm_borders(col = "black") +
  tm_shape(stations$Bajio) + 
  tm_dots(col = "black", size = 0.1) +
  tm_shape(courthouse$Bajio) + 
  tm_dots(col = "darkgrey", size = 0.2, shape = 22) +
  tm_compass(position = c("left", "top"), text.size = 0.5) +
  tm_scale_bar(width = 0.25, position = c("left", "bottom")) +
  tm_layout(inner.margins = c(0.25,0.10,0.25,0.10), outer.margins = c(0.01,0.01,0.01,0.01))
tmap_save(bjx, "06_figures/MapBjx.png", width = 2.5, height =2.5)
#### Clean the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Figure 2: Plot the average length of the hearing by city ####
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Aggregate ####
hour = data %>% group_by(hour)%>% 
  summarise(`A) Duration of the hearing in minutes` = mean(exp(time_minutes), na.rm = TRUE), `B) Number of hearings` = n())
#### Transform the data to long format ####
hour = tidyr::gather(hour, variable, value, -hour)
#### Plot it ####
ggplot(data = dplyr::filter(hour, hour > 6)) +
  geom_bar(aes(y = value, x = hour), stat = "identity", fill = "black") +
  geom_point(aes(y = value, x = hour), color = "grey") +
  facet_wrap(~variable, scales = "free") +
  theme(panel.background = element_rect(fill = "transparent", color = "black"),
        axis.line = element_line(),
        strip.background = element_rect(fill = "transparent", color = "black"),
        strip.text = element_text(hjust = 0)) +
  ggpubr::grids() + labs(x = "Time of the Day", y = "")
#### Save it ####
ggsave("06_figures/hearing.png", width = 7, height = 2.5)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Figure 3a: Plot monthly and hourly variation in pollution #### 
#### ____________________________________________________________________ ####
#### Load the data set ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
#### Select only Mexico City, Monterrey, and Leon ####
pol = dplyr::filter(as.data.frame(pol), city %in% c("mexico city", "monterrey", "leon"))
pol = mutate(pol, city = ifelse(city == "mexico city", "Mexico City", ifelse(city == "leon", "Leon", "Monterrey")))
#### Transform pollution values to numeric ####
pol = mutate_at(pol, vars(PM2.5, NO2, O3, CO), as.numeric)
#### Determine the average value of each particle by month ####
month = pol %>% group_by(city,  time = month) %>%
  summarise_at(vars(`Fine PM` =PM2.5, Ozone =  O3, 
                    `Nitrogen dioxide` = NO2, `Carbon monoxide` = CO), mean, na.rm = TRUE)
month = gather(month, pollutant, value, -time, -city) %>% mutate(window = "Month")

#### Determine the average value of each particle by hour ####
hour = pol %>% group_by(city, time = hour) %>%
  summarise_at(vars(`Fine PM` =PM2.5, Ozone =  O3, 
                    `Nitrogen dioxide` = NO2, `Carbon monoxide` = CO), mean, na.rm = TRUE)
hour = gather(hour, pollutant, value, -time, -city) %>% mutate(window = "Hrs.")
#### Bind both together ####
plot = rbindlist(list(month, hour))
plot = mutate(plot, column = paste0(pollutant, " (", window, ")"))
plot = plot %>% group_by(city) %>% arrange(paste0(window, pollutant))
levels = c("Carbon monoxide (Hrs.)", "Nitrogen dioxide (Hrs.)", 
           "Carbon monoxide (Month)", "Nitrogen dioxide (Month)",
           "Ozone (Hrs.)", "Fine PM (Hrs.)", 
           "Ozone (Month)", "Fine PM (Month)")
plot$column = factor(plot$column, levels = levels)
#### Plot the wind direction ####
ggplot(plot) +
  geom_line(aes(x = time, y =  value, group = city, color = city, linetype = city), lwd = 1) + 
  facet_wrap(~column, scales = "free", ncol = 4) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom", legend.title = element_blank()) +
  ggpubr::grids() + labs(x = "Time-value", y = "Concentration") + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_color_manual(values = c("black", "grey40", "gray60")) +
  scale_linetype_manual(values=c("longdash","solid", "dashed"))
#### Save the image ####
ggsave("06_figures/PolTs.png",  height = 4.5, width = 7.5)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Figure 3b: Plot hourly variation in pollution #### 
#### ____________________________________________________________________ ####
#### Load the data set ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
#### Select only Mexico City, Monterrey, and Leon ####
pol = dplyr::filter(as.data.frame(pol), city %in% c("mexico city", "monterrey", "leon"))
pol = mutate(pol, city = ifelse(city == "mexico city", "Mexico City",ifelse(city == "leon", "Leon", "Monterrey")))
#### Transform pollution values to numeric ####
pol = mutate_at(pol, vars(PM2.5, NO2, O3, CO), as.numeric)
#### Determine the average value of each particle by hour if the day ####
plot = pol %>% group_by(city, hour) %>%summarise_at(vars(`Fine particulate matter` =PM2.5, Ozone =  O3, 
                    `Nitrogen dioxide` = NO2, `Carbon Monoxide` = CO), mean, na.rm = TRUE)
#### Transform the data set to long format ####
plot = gather(plot, pollutant, value, -hour, -city)
#### Plot the wind direction ####
ggplot(plot) +
  geom_line(aes(x = hour, y =  value, group = city, color = city, linetype = city), lwd = 1) + 
  facet_wrap(~pollutant, scales = "free") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom", legend.title = element_blank()) +
  ggpubr::grids() + labs(x = "Month", y = "Concentration") + 
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_color_manual(values = c("black", "grey40", "gray60")) +
  scale_linetype_manual(values=c("dotted","solid", "dashed"))
#### Save the image ####
ggsave("06_figures/pol_hourly.png",  height = 4.5, width = 4)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Figure 4: Plot pollution as a function of wind direction #### 
#### ____________________________________________________________________ ####
#### Load the data set ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
#### Select only the cities of Mexico City and Monterrey ####
pol = dplyr::filter(as.data.frame(pol), city %in% c("mexico city", "monterrey"))
pol = mutate(pol, city = ifelse(city == "mexico city", "Mexico City", "Monterrey"))
#### Create the wind direction value ####
pol = mutate(pol, WDR = as.numeric(WDR))
pol = mutate(as.data.frame(pol), wdr_iv2= cut2(WDR, seq(0,360,10), na.rm = TRUE))
pol = mutate(pol, wdr_iv2 = as.numeric(wdr_iv2))
#### Transform the pollutants to numeric ####
pol = mutate_at(pol, vars(PM2.5, NO2, O3), as.numeric)
#### Determine the average value per city and wind direction value ####
plot = pol %>% group_by(city, wdr_iv2) %>%
  summarise_at(vars(`Fine particle matter` =PM2.5, Ozone =  O3, `Nitrogen dioxide` = NO2), mean, na.rm = TRUE)
##### Transform the data to long format ####
plot = gather(plot, pollutant, value, -wdr_iv2, -city)
#### Calculate the deviation from the mean in the exposure value ####
plot = plot %>% group_by(city, pollutant) %>% mutate(dev = ((value -mean(value))/value)*100)
#### Plot the wind direction ####
ggplot(plot) +
  geom_line(aes(x = wdr_iv2, y =  dev, group = pollutant, 
                color = pollutant, linetype = pollutant), lwd = 1) + 
  facet_wrap(~city) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.line = element_line()) +
  ggpubr::grids() + labs(x = "Wind Direction (10 deg.)", y = "% Deviation from Avg.") +
  geom_hline(aes(yintercept = 0), color = "black") + 
  scale_color_manual(values = c("black", "grey40", "gray60")) +
  scale_linetype_manual(values=c("dotted","solid", "dashed"))
#### Save the image ####
ggsave("06_figures/wdr.png",  height = 3, width = 7.0)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Figure 5: Nonlinear parametric estimates on the effect of air pollution on hearings length ####
#### ____________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
summary = read_rds("04_RegResults/SingleIVQuad.rds")
#### Create the quadratic value of each air pollutant ####
square = function(x){x = x^2}
data = mutate_at(data, vars(CO, NO2, O3, PM2.5), .funs = list(sq = ~square(.))) 
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100, est = (est-1)*100)
#### Create a data set of exposure values for each pollutant from zero to the 999 percentile ####
desc = data%>% summarise_at(vars(NO2, PM2.5), list(quantile = quantile), 0.999, na.rm =TRUE)
#### Make the data set long format ####
desc = tidyr::gather(desc, variable, value)
#### Change the name of the pollutant ####
desc = mutate(desc,pol = gsub("_quantile", "", variable)) %>% select(-variable)
#### Split by air contaminant ####
desc = split(desc, f = desc$pol)
#### Add a value of the concentration of pollution (x-axis) ####
desc = lapply(desc, function(x) x = data.frame(x, concentration = seq(0, x$value, x$value/50)))
#### Bind the list elements ####
desc = rbindlist(desc)
#### Add the linear and quadratic estimates ####
plot = split(summary, f = summary$estimate)
plot = lapply(plot, function(x) x = x %>% select(est, se, variable))
desc = left_join(desc, select(plot$Linear, linear_est = est, linear_se = se, pol = variable))
desc = left_join(desc, select(plot$Square, quadratic_est = est, quadratic_se = se, pol = variable))
#### determine the value of the function for each concentration level (90% confidence intervals) ####
desc = mutate(desc, estimate = (concentration*linear_est) + (concentration^2*quadratic_est))
desc = mutate(desc, upper_estimate = concentration*(linear_est+(linear_se*1.645)) + (concentration^2*(quadratic_est)))
desc = mutate(desc, lower_estimate = concentration*(linear_est-(linear_se*1.645)) + (concentration^2*(quadratic_est)))
#### Change the pollutants names ####
prev = c("NO2", "PM2.5")
names = c("Nitrogen Dioxide","Particle Matter")
desc = mutate(desc, pol = mgsub::mgsub(pol, prev, names))
#### Select the shift point ####
desc = desc %>% group_by(pol) %>% mutate(max = ifelse(estimate == max(estimate), concentration, 0))
desc = desc %>% group_by(pol) %>% mutate(max = max(max))
#### Plot the quadratic model estimates ####
ggplot(desc) +
  geom_line(aes(x = concentration, y = estimate, group =1), size = 0.5) + 
  facet_wrap(~pol, scales = "free", nrow = 1) +
  geom_line(aes(x = concentration, y = upper_estimate), linetype = "dotted") +
  geom_line(aes(x = concentration, y = lower_estimate), linetype = "dotted") +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_vline(aes(xintercept = max), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank(), text = element_text(size = 10)) +
  ggpubr::grids() + scale_x_continuous(breaks = pretty_breaks(5)) +
  labs(y = "Percentage Increase", x = "Concentration") + guides(alpha = FALSE, fill = FALSE) 
#### Save the figure ####
ggsave("06_figures/NonLinearPlot.png",  height = 2.5, width = 5.0)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Figure 6: Non parametric plot of Non linearities ####
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/SingleIVNP.rds")
#### Transform the point estimates from log to exp ####
summary = mutate(summary, se = ifelse(est < 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1))
summary = mutate_at(summary, vars(est), function(x) x = (x-1)*100)
summary = mutate_at(summary, vars(se), function(x) x = (x)*100)
#### Add the ascending bin number ####
summary = summary %>% group_by(pollutant) %>% mutate(numeric_bin = seq(1, n(), 1))
#### Subtract the maximum and minimum bin value ####
summary = mutate(summary, min = as.character(mgsub::mgsub(bin, c(",.*", "\\["), c("", ""))))
summary = mutate(summary, max = as.character(mgsub::mgsub(bin, c(".*,", "\\)", "\\]"), c("", "", ""))))
#### Round the max and min bin values to one digit ####
summary = mutate_at(summary, vars(min, max), as.numeric)
summary = mutate_at(summary, vars(min, max), round, 0)
#### Create a new bin variable with the rounded min and max values ####
summary = mutate(summary, bin = paste(min, max, sep = "-"))
summary = mutate(summary, bin = paste0("(", bin, ")"))
#### Change the name of the pollutants ####
summary = mutate(summary, pollutant = mgsub::mgsub(pollutant, c("bin_co", "bin_no2", "bin_o3","bin_pm25"), 
                                                   c("Carbon Monoxide", "Nitrogen Dioxide", "Ozone", "Fine Particle Matter")))
#### Plot the relationship in the full model ####
ggplot(summary) +
  geom_point(aes(x = reorder(bin, numeric_bin), y = est, group = 1)) + 
  geom_errorbar(aes(ymax = est+se*1.645, ymin = est-se*1.645, x= reorder(bin, numeric_bin))) +
  facet_wrap(~as.character(pollutant), scales = "free", nrow = 2) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") + 
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), axis.text = element_text(angle = 0, size = 9.5),
        legend.title = element_blank()) + 
  ggpubr::grids() + 
  labs(x = "Interval of fitted values", y = "Percentage Increase") 
#### Save the plot ####
ggsave("06_figures/NonParametricPlot.png",  height = 5, width = 9.0)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Figure 8: Time series of average hearings length per month and year ####
#### ____________________________________________________________________ ####
#### Load the data set ####
data = read_rds("03_GenData/data_reg.rds")
data = mutate(data, time_minutes = exp(time_minutes))
##### Aggregate to the monthly length of the hearing ####
plot = data %>% group_by(month) %>% 
  summarise(mean = mean(time_minutes, na.rm = TRUE), sd = sd(time_minutes, na.rm = TRUE))
ggplot(plot) +
  geom_line(aes(x = month, y = mean)) +
  geom_line(aes(x = month, y = mean+sd), linetype = "dashed") +
  geom_line(aes(x = month, y = mean-sd), linetype = "dashed") +
  geom_point(aes(x = month, y = mean)) +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Average hearing length", x = "month") + scale_x_continuous(breaks = seq(1,12,1))
ggsave("06_figures/MonthlyVariation.png",  height = 3, width = 4)
##### Aggregate to the monthly length of the hearing ####
plot = data %>% group_by(year) %>% 
  summarise(mean = mean(time_minutes, na.rm = TRUE), sd = sd(time_minutes, na.rm = TRUE))
ggplot(plot) +
  geom_bar(aes(x = year, y = mean), stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Average hearing length", x = "year") 
ggsave("06_figures/YearlyVariation.png",  height = 3, width = 4)
#### ____________________________________________________________________ ####
#### Figure 9: Descriptive statistics by type of felony, hearing, and final resolution ####
#### ____________________________________________________________________ ####
#### Load the data set ####
data = read_rds("03_GenData/data_reg.rds")
#### distribution of cases by hearing time ####
plot = data %>% group_by(hearing_type) %>% summarise(time = sum(exp(time_minutes))/(60*24))
plot = plot %>% mutate(share = (time/sum(time))*100)
a1 = ggplot(plot) +
  geom_bar(aes(y = share, x = hearing_type, group = hearing_type, fill = hearing_type), color = "black", alpha = 0.5, stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank(), 
        axis.text.x = element_blank(), axis.title = element_blank()) +
  ggpubr::grids() + labs(y = "Share of total time", x = "Type of Hearing") +
  ggtitle("Share of total time by hearing type") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80", "black"))
#### Determine the share of cases by hearing type ####
b1 = ggplot(data) +
  geom_density(aes(time_minutes, group = hearing_type, fill = hearing_type), alpha = 0.5, color = "black") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Density", x = "Time in Minutes (log)") +
  scale_fill_grey(start =0.1, end = 1)+
  ggtitle("Density of hearing length by hearing type") 
#### Determine the share of cases by hearing hour ####
plot = data %>% group_by(hour, hearing_type) %>% summarise(count = n()) 
plot = plot %>% group_by(hour) %>% mutate(share = count/sum(count))
c1 = ggplot(plot) +
  geom_bar(aes(y = count, x =hour, group = hearing_type, fill = hearing_type), col = "black", alpha = 0.5, stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Number of Cases", x = "Hour of the Day") +
  scale_x_continuous(breaks = seq(0,23,1))+
  ggtitle("Number of cases by hour and hearing type") +
  scale_fill_manual(values = c("grey60", "grey80", "grey100", "black"))
#### Change the names of the type of felony ####
data = mutate(as.data.frame(data), felony = as.character(felony))
data = mutate(data, felony = ifelse(!(felony %in% c("2", "7", "8", "12", "4", "1")), "Other", felony))
data = mutate(data, felony = mgsub::mgsub(felony, c("2", "7", "8", "12", "4", "1"), 
                                          c("Assaults", "Theft", "Family", "Drugs", "Sexual", "Murder")))
#### distribution of cases by hearing time ####
head(data)
plot = data %>% group_by(felony) %>% summarise(time = sum(exp(time_minutes))/(60*24))
plot = plot %>% mutate(share = (time/sum(time))*100)
a2 = ggplot(plot) +
  geom_bar(aes(y = share, x = reorder(felony, share), group = felony, fill = felony),
           color = "black", alpha = 0.5, stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank(), 
        axis.text.x = element_blank(), axis.title = element_blank()) +
  ggpubr::grids() + labs(y = "Share of total time", x = "Felony group") +
  ggtitle("Share of total time by felony group") +
  scale_fill_manual(values = c("grey10","grey20", "grey40", "grey60", "grey80", "grey100", "black"))

#### Determine the share of cases by hearing type ####
b2 = ggplot(data) +
  geom_density(aes(time_minutes, group = felony, fill = felony), alpha = 0.5, color = "black") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Density", x = "Time in Minutes (log)") +
  scale_fill_grey(start =0.1, end = 1)+
  ggtitle("Density of hearing length by felony group") 
#### Determine the share of cases by hearing hour ####
plot = data %>% group_by(hour, felony) %>% summarise(count = n()) 
plot = plot %>% group_by(hour) %>% mutate(share = count/sum(count))
c2 = ggplot(plot) +
  geom_bar(aes(y = count, x =hour, group = felony, fill = felony), col = "black", alpha = 0.5, stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Number of Cases", x = "Hour of the Day") +
  scale_x_continuous(breaks = seq(0,23,1))+
  ggtitle("Number of cases by hour and felony group") +
  scale_fill_manual(values = c("grey10","grey20", "grey40", "grey60", "grey80", "grey100", "black"))
#### Change the name of the main resolution types ####

data = mutate(data, resolution = mgsub::mgsub(as.character(resolution),
                                              c("Data Unavailable", "Non Authorized"), c("Unknown", "Unauthorized")))
#### distribution of cases by hearing time ####
plot = data %>% group_by(resolution) %>% summarise(time = sum(exp(time_minutes))/(60*24))
plot = plot %>% mutate(share = (time/sum(time))*100)
a3 = ggplot(plot) +
  geom_bar(aes(y = share, x = reorder(resolution, share), group = resolution, fill = resolution), color = "black", alpha = 0.5, stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank(), 
        axis.text.x = element_blank(), axis.title = element_blank()) +
  ggpubr::grids() + labs(y = "Share of total time", x = "Type of Hearing") +
  ggtitle("Share of total time by resolution") +
  scale_fill_manual(values = c("grey20", "grey40", "grey60", "grey80", "grey100", "black"))
#### Determine the share of cases by hearing type ####
b3 = ggplot(data) +
  geom_density(aes(time_minutes, group = resolution, fill = resolution), alpha = 0.5, color = "black") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Density", x = "Time in Minutes (log)") +
  scale_fill_grey(start =0.1, end = 1) +
  ggtitle("Density of hearing length by resolution") 
#### Determine the share of cases by hearing hour ####
plot = data %>% group_by(hour, resolution) %>% summarise(count = n()) 
plot = plot %>% group_by(hour) %>% mutate(share = count/sum(count))

c3 = ggplot(plot) +
  geom_bar(aes(y = count, x =hour, group = resolution, fill = resolution), col = "black", alpha = 0.5, stat = "identity") +
  theme(panel.background = element_rect(fill = "white", color = "black"), 
        strip.background = element_rect(fill = "white", color = "black"),
        axis.line = element_line(), legend.title = element_blank()) +
  ggpubr::grids() + labs(y = "Number of Cases", x = "Hour of the Day") +
  scale_x_continuous(breaks = seq(0,23,1))+
  ggtitle("Number of cases by hour and resolution") +
  scale_fill_manual(values = c("grey20", "grey40", "grey60", "grey80", "grey100", "black"))
c = gridExtra::grid.arrange(c1,c2,c3, ncol = 1)
a = gridExtra::grid.arrange(a1,a2,a3, ncol = 1)
b = gridExtra::grid.arrange(b1,b2,b3, ncol = 1)
#### CREATE THE COWPLOT ####
resolution = gridExtra::grid.arrange(a,b,c, ncol = 3)
ggsave("06_figures/Pathwork.png", 
       gridExtra::arrangeGrob(a,b,c, ncol = 3), width = 14, height = 9)

