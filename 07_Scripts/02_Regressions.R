#### ###################################################################################################### ####
####                                 SCRIPT TO RUN ALL REGRESSION MODELS                                    #### 
#### ###################################################################################################### ####
#### ______________________________________________________________________________________________________ ####
#### Table 4: Single pollutant log-linear IV with wind direction as inst. #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
colnames(data)
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse","judge_number"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### Run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### Extract the first stage F-values ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SingleIVSJE.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 5: Single pollutant log-linear IV with wind speed as inst. #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c("CO", "NO2", "O3", "PM2.5")
outcome = "time_minutes"
controls = list(c("daily_hearings","hourly_hearings"))
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q","wdr_iv2", "PBa_q" ,"courthouse","week:year", "judge_number", "hour", "weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls[[i]], collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "WSP_q", sep = "~"), ")",  "| courthouse"))) }}
#### Run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SingleIvWSP.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()


#### ______________________________________________________________________________________________________ ####
#### Table 6: Single pollutant log-linear IV with wind direction as inst, across different crime categories ####
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Summarize to the number of hourly hearings ####
data = data %>% group_by(year, month, day, week, weekday, hour, courthouse, felony, 
                         TMP_q, WSP_q, RH_q, PBa_q,CO, NO2, O3, PM2.5, wdr_iv2) %>% summarise(hourly_hearings = n())
data = mutate(as.data.frame(data), hourly_hearings = log(hourly_hearings))
#### Split the data set by crime category ####
data = mutate(as.data.frame(data), felony = as.numeric(as.character(felony)))
data = mutate(data, felony = ifelse(!(felony %in% c(7,2)), "Other", felony))
data = mutate(data, felony = mgsub::mgsub(as.character(felony), c("7", "2", "Other"), c("Larceny, theft, and robbery", "Assaults", "Other")))
data = split(data, f = data$felony, drop = TRUE)
#### Define the model ####
dependent = c("CO", "NO2", "O3", "PM2.5")
outcome = "hourly_hearings"
controls = c("1")
fe = c("TMP_q","RH_q", "WSP_q", "PBa_q","courthouse", "week:year","hour","weekday")
unique(data$courthouse)
#### Define the specification ####
variables = vector("list", length(dependent))
variables = lapply(variables, function(x) x = vector("list", length(controls)))
for(k in 1:length(dependent)){for(i in 1:length(controls)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe, collapse = "+"), "|", "(",paste(paste(dependent[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### Run all the estimates ####
estimates = lapply(data, function(x) lapply(variables, function(y) x = felm(y[[1]], data = x)))
estimates = lapply(estimates, setNames, dependent)
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "pollutant")
summary = rbindlist(summary, idcol = "crime")
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "pollutant")
fstat = rbindlist(fstat, idcol = "crime")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/FelonyNcases.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 7: Single pollutant log-linear IV with wind direction as inst for scheduled hearings ####
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Filter to keep only pre-scheduled hearings ####
data = dplyr::filter(data, hearing_type != "In Fraganti")
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
unique(data$courthouse)
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
head(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SjeUnschedueled.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()

#### ______________________________________________________________________________________________________ ####
#### Table 8: Multipollutant Log-linear IV with wind direction as inst #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Create the regression variables ####
dependent = list(c("PM2.5", "NO2", "O3"), c("CO", "NO2", "O3"), c("PM2.5", "CO", "O3"), c("PM2.5", "CO", "NO2"))
outcome = "time_minutes"
controls = list(c("daily_hearings","hourly_hearings"))
fe = list(c("resolution", "hearing_type", "felony", "WSP_q","TMP_q","RH_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(dependent))
variables = lapply(variables, function(x) x = vector("list", length(controls)))
for(k in 1:length(dependent)){for(i in 1:length(controls)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls[[i]], collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), "|", 
                                               "(",paste(paste(dependent[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse")))}}

#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = c("1) Without Carbon Monoxide", "2) Without Particle Matter", "3) Without Nitrogen Dixoide", "4) Without Ozone")
#### Summarize the results ####
estimates_summary= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(estimates_summary, function(x) lapply(x, function(z) z = data.frame(z$coefficients)))
#### Add the row-names as a new column ####
summary  = lapply(summary, function(x) lapply(x, function(z)  z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(x) lapply(x, function(z)  z = dplyr::filter(z, grepl(c("fit"), variable ))))
#### Determine the model####
summary = lapply(summary, function(x) rbindlist(x, idcol = "model"))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("specification","est", "se", "tvalue","pvalue",  "variable"))
#### Row bind the different models #####
summary = rbindlist(summary, idcol = "Model")
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(x)))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = tidyr::gather(x, "pollutant", "f_stat")))
fstat = lapply(fstat, rbindlist)
fstat = rbindlist(fstat, idcol = "Model", fill = TRUE, use.names = TRUE)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/MultiIV.rds", compress = "gz")
write_rds(fstat, file = "04_RegResults/MultiFstat.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 9: Single pollutant log-linear quadratic IV with wind direction as inst. #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Square the key pollutants ####
square = function(x){x = x^2}
data = mutate_at(data, vars(CO, NO2, O3, PM2.5), .funs = list(sq = ~square(.))) 
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q","courthouse", "week:year", "judge_number","hour" ,"weekday")
#### Define the specification ####
variables = lapply(causal, function(x) 
  x = as.formula(paste(outcome, paste(paste(paste(controls, collapse = "+")), 
                                      paste0("|", paste(fe, collapse = "+")), "|", 
                                      paste0("(", paste(paste(c(x, paste0(x, "_sq")), collapse = "|"),"wdr_iv2:courthouse", sep ="~"),")"),
                                      "|courthouse", sep = ""), sep = "~")))
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) x = felm(x, data = data))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, summary)
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) z = data.frame(z$coefficients, z$r.squared, z$N))
#### Add row names ####
summary  = lapply(summary, function(z)  z = mutate(z, variable = rownames(z)))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable )))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable"))
#### Row bind the list elements and plot the results #####
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### Determine if it is the square or linear value ####
summary = mutate(summary, estimate = ifelse(grepl("sq", variable), "Square", "Linear"))
summary = mutate(summary, variable = gsub("_sq", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, condfstat)
fstat = lapply(fstat, function(x) x = data.frame(f_stat = x[1]))
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SingleIVQuad.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 10: Single pollutant Poisson IV of  Unattended hearings # with wind direction as inst. #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Keep only non-attended hearings ####
data = dplyr::filter(data, hearing_type == "Non Attendance")
#### Select the relevant variables ####
data = select(data, year, month, day, week, weekday,hour,courthouse, judge_number, hearing_type, PM2.5, NO2, 
              CO, O3, TMP_q, RH_q, hourly_hearings, WSP_q)
data = data %>% group_by(year, month, day, hour, courthouse) %>% mutate(HrNA = n())
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data),NO2, PM2.5, CO, O3)))
outcome = "HrNA"
controls = c()
fe = c("TMP_q","RH_q", "WSP_q","courthouse", "week^year","hour" ,"weekday", "judge_number")
#### Define the specification ####
variables = lapply(causal, function(x) 
  x = as.formula(paste(outcome, paste(paste(paste(x, collapse = "+")), 
                                      paste0("|", paste(fe, collapse = "+")), sep = ""), sep = "~")))
#### Define the specification ####
estimates = lapply(variables, function(x) x = fixest::feglm(x, data = data, cluster = "courthouse", family = "poisson"))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, summary)
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) z = data.frame(z$coeftable, z$pseudo_r2, z$nobs))
#### Add row names ####
summary  = lapply(summary, function(z)  z = mutate(z, variable = rownames(z)))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable )))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("est", "se", "zvalue","pvalue",  "r2","N.obs","variable"))
#### Row bind the list elements #####
summary = rbindlist(summary)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/NonAttendedPoisson.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Figure 6: Divide the linear effect into exposure intervals (Non parametric) #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/Data2sls.rds")
#### Divide the intervals #### 
data = mutate(data, bin_co = cut2(CO_IV, seq(min(CO_IV, na.rm = TRUE),3.5, 0.5), onlycuts = TRUE), 
              bin_no2 = cut2(NO2_IV, seq(min(NO2_IV, na.rm = TRUE), 54, 8), onlycuts = TRUE),
              bin_o3 = cut2(O3_IV, seq(min(O3_IV, na.rm = TRUE), 60,12), onlycuts = TRUE),
              bin_pm25 = cut2(PM2.5_IV, seq(min(PM2.5_IV, na.rm = TRUE), 60,10), onlycuts = TRUE))
#### Create the regression variables ####
causal = list("bin_o3", "bin_co", "bin_no2", "bin_pm25")
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = c("resolution", "hearing_type", "felony","courthouse^week^year", "judge_number","hour" ,"weekday", "TMP_q", "RH_q")
#### Define the specification ####
variables = lapply(causal, function(x) 
  x = as.formula(paste(outcome, paste(paste(paste(c(x, controls), collapse = "+")), paste0("|", paste(fe, collapse = "+"))), sep = "~")))
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) x = fixest::feols(x, data = data))
names(estimates) = causal
#### Summarize the results ####
est_sum = lapply(estimates,summary)
#### extract the coefficients ####
summary = lapply(est_sum, function(x) x = data.frame(est = x$coefficients, se= x$se, p_value = x$coeftable[,4],Nobs = x$nobs))
#### make the row-names the variable names ####
summary = lapply(summary, function(x) x =  mutate(x, variable = rownames(x)))
#### Rbindlist ####
summary = lapply(summary, function(x) x  = dplyr::filter(x, grepl("co|o3|pm25|so2|no2", variable)))
summary = rbindlist(summary)
#### select only the coefficient of the interesting variable ####
summary = mutate(summary, pollutant= mgsub::mgsub(variable, c("\\[.*", "fit_bin_"), c("", "")))
summary =  mutate(summary, bin = mgsub::mgsub(variable, c("bin_co","bin_o3","bin_pm25","bin_no2"), c("","","","")))
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SingleIVNP.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 11: Single pollutant Log-linear IV with daily aggregates #### 
#### ______________________________________________________________________________________________________ ####
#### load data ####
data = read_rds("03_GenData/data_reg.rds")
wdr = read_rds("03_GenData/wdr_iv_daily.rds")
#### Aggregate all relevant variables to the daily value ####
data = mutate(data, date = substr(date, 1,10))
DailyData = data %>% group_by(city, courthouse, year, month, day, week, weekday, date, daily_hearings) %>% 
  summarise(PM2.5 = mean(PM2.5, na.rm = TRUE), 
            WSP = mean(WSP, na.rm = TRUE),
            TMP= mean(TMP, na.rm = TRUE), 
            RH= mean(RH, na.rm = TRUE),
            time_minutes = log(sum(exp(time_minutes), na.rm =TRUE)), 
            NO2 = mean(NO2, na.rm = TRUE),
            O3 = mean(O3, na.rm = TRUE), 
            CO = mean(CO, na.rm = TRUE)) 
#### Transform CO to particles per billion ####
DailyData = mutate(DailyData, CO = CO*1000)
#### Create the Wind Direction Instrument ####
DailyData = mutate(DailyData, courthouse = as.numeric(as.character(courthouse)))
DailyData = left_join(DailyData, wdr)
DailyData = dplyr::filter(DailyData, is.na(wdr_iv) == FALSE)
DailyData = mutate(as.data.frame(DailyData), wdr_iv2= cut2(wdr_iv, seq(0,360,90), na.rm = TRUE))
#### Create the weather specification of weather covariates ####
DailyData = mutate_at(as.data.frame(DailyData), vars(TMP, RH, WSP), function(x) x= cut(x, 10, na.rm = TRUE))
#### Transform all dependent variables to log ####
DailyData = mutate_at(as.data.frame(DailyData), vars(year, week, weekday, courthouse, city, month), as.factor)
#### Define the model elements ####
dependent = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = list(c("daily_hearings"))
fe = list(c("TMP","RH", "WSP","courthouse:year", "week" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(dependent))
variables = lapply(variables, function(x) x = vector("list", length(controls)))
for(k in 1:length(dependent)){for(i in 1:length(controls)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls[[i]], collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), "|",
                                               "(",paste(paste(dependent[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse")))}}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = DailyData)))
names(estimates) = dependent
#### Summarize the results ####
estimates_summary= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(estimates_summary, function(x) lapply(x, function(z) z = data.frame(z$coefficients, z$r.squared,z$N)))
#### Add the name of the rows as a variable id ####
summary  = lapply(summary, function(x) lapply(x, function(z)  z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(x) lapply(x, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Determine the model####
summary = lapply(summary, function(x) rbindlist(x, idcol = "model"))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("specification","est", "se", "tvalue","pvalue", "r2","N", "variable"))
#### Bind all the list elements together #####
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/DailySingleIV.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 12: Single pollutant Log-linear IV with city aggregates #### 
#### ______________________________________________________________________________________________________ ####
#### load data ####
data = read_rds("03_GenData/data_reg.rds")
wdr = read_rds("03_GenData/wdr_iv.rds")
#### Aggregate all relevant variables to the daily value ####
CityData = as.data.frame(data) %>% group_by(city, year, month, day, week, weekday, hour, date) %>% 
  summarise(PM2.5 = mean(PM2.5, na.rm = TRUE), 
            WSP = mean(WSP, na.rm = TRUE),
            TMP= mean(TMP, na.rm = TRUE),
            RH= mean(RH, na.rm = TRUE),
            time_minutes = log(sum(exp(time_minutes), na.rm =TRUE)),
            hourly_hearings = sum(hourly_hearings), 
            daily_hearings = sum(daily_hearings),
            NO2 = mean(NO2, na.rm = TRUE), 
            O3 = mean(O3, na.rm = TRUE), 
            CO = mean(CO, na.rm = TRUE)) 
#### Transform CO to PPB ####
CityData  = mutate(CityData, CO = CO*1000)
#### Create the Wind Direction Instrument ####
wdr = wdr %>% group_by(date, city) %>% summarise(wdr_iv = mean(wdr_iv))
CityData = left_join(CityData, wdr)
CityData = dplyr::filter(CityData, is.na(wdr_iv) == FALSE)
#### Create the non-parametric weather variables ####
CityData = mutate_at(as.data.frame(CityData), vars(TMP, RH, WSP), function(x) x= cut(x, 10, na.rm = TRUE))
CityData = mutate(as.data.frame(CityData), wdr_iv2= cut2(wdr_iv, seq(0,360,90), na.rm = TRUE))
#### Transform all dependent variables to log ####
CityData = mutate_at(as.data.frame(CityData), vars(year, week, weekday, hour, city, month), as.factor)
#### Define the model elements ####
dependent = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = list(c("daily_hearings", "hourly_hearings"))
fe = list(c("TMP","RH","WSP", "city:year:week" ,"weekday", "hour"))
#### Define the specification ####
variables = vector("list", length(dependent))
variables = lapply(variables, function(x) x = vector("list", length(controls)))
for(k in 1:length(dependent)){for(i in 1:length(controls)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls[[i]], collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), "|", "(",paste(paste(dependent[[k]], collapse = "|"), 
                                                                                                   "wdr_iv2:city", sep = "~"), ")",  "| city")))}}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = CityData)))
names(estimates) = dependent
#### Summarize the results ####
estimates_summary= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(estimates_summary, function(x) lapply(x, function(z) z = data.frame(z$coefficients, z$r.squared,z$N)))
#### Add the row-names as a column of variable names ####
summary  = lapply(summary, function(x) lapply(x, function(z)  z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(x) lapply(x, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Determine the model####
summary = lapply(summary, function(x) rbindlist(x, idcol = "model"))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("specification","est", "se", "tvalue","pvalue", "r2","N", "variable"))
#### Bind all the list elements #####
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
head(summary)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/CitySingleIV.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 13 (upper panel): Temporal placebo #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = list("CO", "NO2", "O3", "PM2.5")
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour" ,"weekday")
#### Define the specification ####
variables = lapply(causal, function(x) 
  x = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                       paste("|", paste(fe, collapse = "+"), "|", "(",
                             paste(paste(x, collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse")))) 
#### randomize the value of pollution measurement across all time periods ####
set.seed(10000000)
data1 = split(data, f = data$courthouse, drop = TRUE)
data1  = lapply(data1, function(x)  x =  as.data.frame(x) %>% mutate_at(vars(CO, NO2, PM2.5, O3), funs(x[sample(nrow(x), nrow(x)),]$.)))
data1 = rbindlist(data1)

data2 = split(data, f = list(data$courthouse, data$month, data$year, drop = TRUE))
data2  = lapply(data2, function(x)  x =  as.data.frame(x) %>% mutate_at(vars(CO, NO2, PM2.5, O3), funs(x[sample(nrow(x), nrow(x)),]$.)))
data2 = rbindlist(data2)

data = list(data1, data2)
#### run the regressions for all the different sets ####
estimates = lapply(data, function(y) y = lapply(variables, function(x)  x = felm(x, data = y)))
names(estimates) = c("Courthouse randomization", "Month and courthouse randomization")
#### Summarize the regression results ####
estimates_summary= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(estimates_summary, function(x) lapply(x, function(z) z = data.frame(z$coefficients)))
#### Add the rownames as a column of variable names ####
summary  = lapply(summary, function(x) lapply(x, function(z)  z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(x) lapply(x, function(z)  z = dplyr::filter(z, grepl(c("fit"), variable ))))
#### Determine the model####
summary = lapply(summary, function(x) rbindlist(x))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("est", "se", "tvalue","pvalue",  "variable"))
#### Bind all the list elements #####
summary = rbindlist(summary, idcol = "Model")
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### Save the data set ####
write_rds(summary, file = "04_RegResults/TemporalPlaceboIV.rds", compress = "gz" )
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 13 Lower panel: Spatial placebo #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = list("CO", "NO2", "O3", "PM2.5")
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour" ,"weekday")
#### Define the specification ####
variables = lapply(causal, function(x) 
  x = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                       paste("|", paste(fe, collapse = "+"), "|", "(",
                             paste(paste(x, collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse")))) 
variables
#### randomize the value of pollution measurement across all time periods ####
set.seed(10000000)
data1 = split(data, f = data$month, data$year, drop = TRUE)
data1  = lapply(data1, function(x)  x =  as.data.frame(x) %>% mutate_at(vars(CO, NO2, PM2.5, O3), funs(x[sample(nrow(x), nrow(x)),]$.)))
data1 = rbindlist(data1)

data2 = split(data, f = list(data$year, drop = TRUE))
data2  = lapply(data2, function(x)  x =  as.data.frame(x) %>% mutate_at(vars(CO, NO2, PM2.5, O3), funs(x[sample(nrow(x), nrow(x)),]$.)))
data2 = rbindlist(data2)

data = list(data1, data2)
#### run the regressions for all the different sets ####
estimates = lapply(data, function(y) y = lapply(variables, function(x)  x = felm(x, data = y)))
names(estimates) = c("Monthly randomization", "Yearly randomization")
#### Summarize the regression results ####
estimates_summary= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(estimates_summary, function(x) lapply(x, function(z) z = data.frame(z$coefficients)))
#### Add the row-names as a column of variable names ####
summary  = lapply(summary, function(x) lapply(x, function(z)  z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(x) lapply(x, function(z)  z = dplyr::filter(z, grepl(c("fit"), variable ))))
#### Determine the model####
summary = lapply(summary, function(x) rbindlist(x))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("est", "se", "tvalue","pvalue",  "variable"))
#### Bind all the list elements #####
summary = rbindlist(summary, idcol = "Model")
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SpatialPlaceboIV.rds", compress = "gz" )
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 14: Divide the sample by time of the day #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Filter to only the morning hours ####
data = list(Afternoon = dplyr::filter(data, hour %in% seq(13,19,1)), 
            Morning = dplyr::filter(data, hour %in% seq(6,12,1)))
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data$Afternoon), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour", "weekday")
#### Define the specification ####
variables = lapply(causal, function(x) 
  x = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                       paste("|", paste(fe, collapse = "+"), "|", "(",
                             paste(paste(x, collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse")))) 
#### run the regressions for all the different sets ####
estimates = lapply(data, function(y) y = lapply(variables, function(x) x = felm(x, data = y)))
estimates = lapply(estimates, setNames, causal)
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z) z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z) 
  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) 
  lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist)
summary = rbindlist(summary, idcol = "sample")
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat))
fstat = lapply(fstat, function(x) lapply(x, function(x)  x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "variable")
fstat = rbindlist(fstat, idcol = "sample")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/MorningIV.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 15: Main regression with OLS #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Create the regression variables ####
dependent = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = list(c("1"),
                c("daily_hearings","hourly_hearings"),
                c("daily_hearings","hourly_hearings"),
                c("daily_hearings","hourly_hearings"),
                c("daily_hearings","hourly_hearings"))

fe = list(c("0"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))



#### Make the variables a formula to run them in a loop ####
variables = vector("list", length(dependent))
variables = lapply(variables, function(x) x = vector("list", length(controls)))
for(k in 1:length(dependent)){for(i in 1:length(controls)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(c(dependent[[k]], controls[[i]]), collapse = " + "), sep = "~"),
                                         paste("|", paste(fe[[i]], collapse = "+"), "| 0 | courthouse"))) }}
#### Run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
#### Summarize the results ####
estimates_summary= lapply(estimates, function(x) lapply(x, summary))
#### Save the data set ####
write_rds(estimates_summary, file = "04_RegResults/SingleOLS.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 16: Regression without excluding lengthy hearings #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg2.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/SingleNoLengthRest.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 17: Regression by excluding days-judge combination when a judge had a long hearing #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg3.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/JudgeLongHearing.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 18: Main regression with IV and Wind Direction (Different weather covariates) #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg3.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = list(c("1"),
                c("TMP_q","RH_q", "WSP_q"),
                c("resolution", "hearing_type", "felony", "daily_hearings","hourly_hearings"),
                c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q"),
                c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "daily_hearings","hourly_hearings"))
fe = c("courthouse", "week:year", "judge_number","hour", "weekday")
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(controls)))
for(k in 1:length(causal)){for(i in 1:length(controls)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls[[i]], collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe, collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/CovsRobustnessIV.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 19: Main regression with IV and clustering SE at the city level #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg3.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| city"))) }}
#### Run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/CityRobustSE.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 20: Main regression with IV and clustering SE at the hour level #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg3.rds")
#### Transform CO to Particles per billion ####
data  = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week", "year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"),
          c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse:week:year", "judge_number","hour" ,"weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| hour"))) }}
#### Run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Summarize the results ####
est_sum= lapply(estimates, function(x) lapply(x, summary))
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) lapply(z, function(z) z = data.frame(z$coefficients, z$r.squared, z$N)))
#### Add row names ####
summary  = lapply(summary, function(z) lapply(z, function(z)   z = mutate(z, variable = rownames(z))))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) lapply(z, function(z)  z = dplyr::filter(z, grepl(c("CO|NO2|PM2.5|O3"), variable ))))
#### Change the names of the coefficients ####
summary = lapply(summary, function(z) lapply(z, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable")))
#### Row bind the list elements and plot the results #####
summary = lapply(summary, rbindlist, idcol = "model")
summary = rbindlist(summary)
#### Adjust the name of the variable ID ####
summary = mutate(summary, variable = gsub("\\(fit\\)", "",variable))
summary = mutate(summary, variable = gsub("`", "",variable))
#### #### Extract the first stage F-values #### ####
fstat = lapply(estimates, function(x) lapply(x, condfstat, quantile = 0, bN = 100L))
fstat = lapply(fstat, function(x) lapply(x, function(x) x = data.frame(f_stat = x[1])))
fstat = lapply(fstat, rbindlist, idcol = "model")
fstat = rbindlist(fstat, idcol = "variable")
#### Left join with the F-value with the summary of coefficients ####
summary = left_join(summary, fstat)
#### Save the data set ####
write_rds(summary, file = "04_RegResults/HourRobustSE.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()
#### ______________________________________________________________________________________________________ ####
#### Table 21: First stage coefficients of the preferred specification. #### 
#### ______________________________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform CO to Particles per billion ####
data = mutate(data, CO = CO*1000)
#### Create the regression variables ####
causal = c(colnames(select(as.data.frame(data), CO, NO2, O3, PM2.5)))
outcome = "time_minutes"
controls = c("daily_hearings","hourly_hearings")
fe = list(c("resolution", "hearing_type", "felony","TMP_q","RH_q", "WSP_q", "courthouse", "week:year", "judge_number","hour", "weekday"))
#### Define the specification ####
variables = vector("list", length(causal))
variables = lapply(variables, function(x) x = vector("list", length(fe)))
for(k in 1:length(causal)){for(i in 1:length(fe)){
  variables[[k]][[i]] = as.formula(paste(paste(outcome, paste(controls, collapse = " + "), sep = "~"), 
                                         paste("|", paste(fe[[i]], collapse = "+"), 
                                               "|", "(",
                                               paste(paste(causal[[k]], collapse = "|"), "wdr_iv2:courthouse", sep = "~"), ")",  "| courthouse"))) }}
#### run the regressions for all the different sets ####
estimates = lapply(variables, function(x) lapply(x, function(x) x = felm(x, data = data)))
names(estimates) = causal
#### Only select the fourth specification ####
estimates = lapply(estimates, function(x) x = x[[1]])
estimates = lapply(estimates, function(x) x = x$stage1)
#### Summarize the results ####
est_sum= lapply(estimates, summary)
#### Extract the coefficients ####
summary = lapply(est_sum, function(z) z = data.frame(z$coefficients, z$r.squared, z$N))
#### Add row names ####
summary  = lapply(summary, function(z)  z = mutate(z, variable = rownames(z)))
#### Extract the coefficients of interest ####
summary = lapply(summary, function(z) z = dplyr::filter(z, grepl(c("wdr_iv2"), variable )))
#### Create columns with the courthouse and the wind direction #### ####
summary = lapply(summary, function(z) z = mutate(z, courthouse = gsub(".*:courthouse", "", variable)))
summary = lapply(summary, function(z) z = mutate(z, wdr = gsub(":courthouse.*", "", variable)))
summary = lapply(summary, function(z) z = mutate(z, wdr = gsub(".*wdr_iv2", "", wdr)))
#### Change the names of the coefficients ####
summary = lapply(summary, setNames, c("est", "se", "tvalue","pvalue",  "r2","N.obs","variable", "courthouse", "wdr"))
#### Row bind the list elements and plot the results #####
summary = rbindlist(summary, idcol = "pollutant")
#### Save the data set ####
write_rds(summary, file = "04_RegResults/FirstStage.rds", compress = "gz")
#### Clear the space ####
rm(list = ls())
gc()



