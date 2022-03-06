#### ###################################################################################################### ####
####                            SCRIPT TO create all Latex tables in the paper                              #### 
#### ###################################################################################################### ####
#### _______________________________________________________________________________________ ####
#### Table 1: Impact of air pollution on the length of judicial hearings (IV Model) ####
#### _______________________________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
#### Transform the length of the hearing from logs to levels ####
data = mutate(data, time_minutes = exp(time_minutes))
#### Upper panel (Descriptives on type of crime) ####
data = mutate(as.data.frame(data), felony = as.character(felony))
data = mutate(data, felony = ifelse(!(felony %in% c("7","8","12","2")), "Other crime categories", felony))
data = mutate(data, felony = mgsub::mgsub(felony, c("7","8","12","2"), 
                                          c("Drug dealing, posession and use", "Assault and associated crimes", 
                                            "Larceny, theft, and robbery",  "Alimony and associated family crimes")))
desc_felony = data %>% group_by(felony) %>% summarise_at(vars(time_minutes), list(mean = mean, sd = sd, max = max, min = min), na.rm =TRUE)
xtable::print.xtable(xtable(desc_felony, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Middle Panel (Descriptives on type of hearing) ####
desc_felony = data %>% group_by(hearing_type) %>% summarise_at(vars(time_minutes), list(mean = mean, sd = sd, max = max, min = min), na.rm =TRUE)
xtable::print.xtable(xtable(desc_felony, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Lower panel (Descriptives by final resolution) ####
desc_felony = data %>% group_by(resolution) %>% summarise_at(vars(time_minutes), list(mean = mean, sd = sd, max = max, min = min), na.rm =TRUE)
xtable::print.xtable(xtable(desc_felony, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 2: Table of number and length of hearings per judge #### 
#### ____________________________________________________________________ ####
#### Load data set ####
data = read_rds("03_GenData/data_reg.rds")
data = mutate(data, time_minutes = exp(time_minutes))
data = dplyr::filter(data, is.na(judge_number) == FALSE)
#### Upper panel: descriptive on number of hearings per judge ####
count = data %>% group_by(year, month, day, courthouse, city, judge_number) %>% summarise(count = n())
count = as.data.frame(count) %>% summarise(avg = mean(count), sd = sd(count), max = max(count), min = min(count))
xtable::print.xtable(xtable(count, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Descriptive on number of hearings per courthouse ####
count = data %>% group_by(year, month, day, courthouse, city) %>% summarise(count = n())
count = as.data.frame(count) %>% summarise(avg = mean(count), sd = sd(count), max = max(count), min = min(count))
xtable::print.xtable(xtable(count, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Descriptive on time spent on hearings per judge
count = data %>% group_by(year, month, day, courthouse, city, judge_number) %>% summarise(length = mean(time_minutes))
count = as.data.frame(count) %>% summarise(avg = mean(length), sd = sd(length), max = max(length), min = min(length))
xtable::print.xtable(xtable(count, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Descriptive on number of hearings per courthouse ####
count = data %>% group_by(year, month, day, courthouse, city, judge_number) %>% summarise(length = sum(time_minutes))
count = as.data.frame(count) %>% summarise(avg = mean(length), sd = sd(length), max = max(length), min = min(length))
xtable::print.xtable(xtable(count, summary = FALSE, digits = 2), include.rownames = FALSE)
#### Descriptive on total hearing length per courthouse ####
count = data %>% group_by(year, month, day, courthouse, city) %>% summarise(length = sum(time_minutes))
count = as.data.frame(count) %>% summarise(avg = mean(length), sd = sd(length), max = max(length), min = min(length))
xtable::print.xtable(xtable(count, summary = FALSE, digits = 2), include.rownames = FALSE)
#### _____________________________________________________________________________________ ####
#### Table3: Create summary table of pollution related variables ####
#### _____________________________________________________________________________________ ####
#### Load data ####
load("02_ConstructPollutionData/03_GenData/pol.RData")
#### Transform pollution values to numeric ####
pol = mutate_at(pol, vars(PM2.5, NO2, O3, CO), as.numeric)
#### Mean ####
avg_pollutants = as.data.frame(pol) %>% summarise_at(vars(c(CO, NO2, O3, PM2.5)), funs(mean, sd, max, min),  na.rm = TRUE)
mean <- gather(select(avg_pollutants, c(CO_mean:PM2.5_mean)) , mean_variable, mean, CO_mean:PM2.5_mean, factor_key=TRUE)
sd <- gather(select(avg_pollutants, c(CO_sd:PM2.5_sd)), sd_variable, standard_deviation, CO_sd:PM2.5_sd, factor_key=TRUE)
max <- gather(select(avg_pollutants, c(CO_max:PM2.5_max)), max_variable, max, CO_max:PM2.5_max, factor_key=TRUE)
min <- gather(select(avg_pollutants, c(CO_min:PM2.5_min)), min_variable, min, CO_min:PM2.5_min, factor_key=TRUE)
#### create the cbindlist function ####
table = list(mean, sd, max, min) %>% purrr::reduce(cbind)
table = table[,-c(1,3,5,7)]



table$pollutant = c("Carbon Monoxide", "Nitrogen Dioxides", "Ozone", "Fine Particle Matter")
head(table)
table = select(table, c(pollutant, mean:min))
colnames(table) = c("Pollutant", "Mean", "Standard Deviation","Maxima", "Minima")
#### Create the table of coefficients ####
stargazer(table, summary = FALSE, rownames = FALSE, notes.align = "c", digits = 2)
#### _______________________________________________________________________________________ ####
#### Table 4: Impact of air pollution on the length of judicial hearings (IV Model) ####
#### _______________________________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/SingleIVSJE.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
#### _______________________________________________________________________________________ ####
#### Table 5: Impact of air pollution on the length of judicial hearings (IV with wind speed) ####
#### _______________________________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/SingleIvWSP.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object ####
tab = summary
tab = split(tab, f = tab$variable)
latex = lapply(tab, function(x) createTexreg("", x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### _______________________________________________________________________________________ ####
#### Table 6: Results on the number of hearings across different crime categories (IV model) ####
#### _______________________________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/FelonyNcases.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$crime)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$crime)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$crime)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$crime)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### clear the space ####
rm(list = ls())
gc()




#### _______________________________________________________________________________________ ####
#### Table 7: Impact on the length of previously scheduled judicial hearings (IV model) ####
#### _______________________________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/SjeUnschedueled.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
#### _______________________________________________________________________________________ ####
#### Table 8: Impact of air pollution on the length of judicial hearings (Multipollutant IV) #### 
#### _______________________________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/MultiIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### Add the long name of the particles ####
names = data.frame(variable = c("imeca", "CO", "O3", "NO2", "PM10", "PM2.5", "SO2"), 
                   full_name = c("Air Quality Index", "Carbon Monoxide", "Ozone", 
                                 "Nitrogen Dioxide", "Coarse Particle Matter", 
                                 "Fine Particle Matter", "Sulfur Dioxide"))
summary = left_join(summary, names)
#### create the latex table of the MP model (Upper panel) ####
summary = arrange(summary,  Model)
summary = mutate(summary, Model = as.factor(Model))
tab = split(summary, f = list(summary$full_name), drop = TRUE)
reg = lapply(tab, function(x) createTexreg(as.character(x$Model), x$est,x$se, x$pvalue))
texreg(reg, stars = c(0.01, 0.05, 0.1), digits = 1,
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize" )
#### Create the latex table of F-test values (lower panel) ####
summary = read_rds("04_RegResults/MultiFstat.rds")
summary$pollutant = factor(summary$pollutant, levels = c("CO", "PM2.5", "NO2", "O3"))
tab = split(summary, f = list(summary$pollutant), drop = TRUE)
reg = lapply(tab, function(x) createTexreg(as.character(x$Model), x$f_stat))
texreg(reg, stars = c(0.001, 0.01, 0.05, 0.1), digits = 0, symbol = "+",
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize" )

#### Clear the space ####
rm(list = ls())
gc()
#### _______________________________________________________________________________________ ####
#### Table 9: Impact of air pollution on the length of judicial hearings (Quadratic IV) ####
#### _______________________________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/SingleIVQuad.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object ####
tab = split(summary, f = summary$variable)
latex = lapply(tab, function(x) createTexreg(as.character(x$estimate),x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)),
                                                           rep("R2", nrow(x)),
                                                           rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)),
                                                             rep(T, nrow(x)),
                                                             rep(T, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 3)
#### clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 10: Poisson of Non-Attended Hearings ####
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/NonAttendedPoisson.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100, est = (est-1)*100)
#### Create the texreg object ####
tab = split(summary, f = summary$variable)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)),
                                                           rep("R2", nrow(x))),
                                             gof = c(x$N.obs, x$r2),
                                             gof.decimal = c(rep(F, nrow(x)),
                                                             rep(T, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 3, booktabs = TRUE)
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 11: IV regression results (Daily aggregates) #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/DailySingleIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#summary = mutate(summary, est = est*standard_deviation, se = se*standard_deviation)
names = data.frame(variable = c("imeca", "CO", "O3", "NO2", "PM10", "PM2.5", "SO2"), 
                   full_name = c("Air Quality Index", "Carbon Monoxide", "Ozone", "Nitrogen Dioxide", "Coarse Particle Matter", 
                                 "Fine Particle Matter", "Sulfur Dioxide"))
summary = left_join(summary, names)
head(summary)
#### create the tex variable ####
tab = split(summary, f = list(summary$full_name), drop = TRUE)
reg= lapply(tab, function(x) createTexreg("", x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(reg, stars = c(0.01,0.05,0.1), digits = 2,
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize" )
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 12: IV regression results (City aggregates) #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/CitySingleIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#summary = mutate(summary, est = est*standard_deviation, se = se*standard_deviation)
names = data.frame(variable = c("CO", "O3", "NO2", "PM2.5"), 
                   full_name = c("Carbon Monoxide", "Ozone", "Nitrogen Dioxide","Fine Particle Matter"))
summary = left_join(summary, names)
#### create the tex variable ####
head(summary)
tab = split(summary, f = list(summary$full_name), drop = TRUE)
reg= lapply(tab, function(x) createTexreg("", x$est,x$se, x$pvalue, 
                                          gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                          gof = c(x$N, x$r2, x$f_stat),
                                          gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(reg, stars = c(0.01,0.05,0.1), digits = 2,
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize", booktabs = TRUE )
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 13 (Upper panel): Temporal Placebo #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/TemporalPlaceboIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#summary = mutate(summary, est = est*standard_deviation, se = se*standard_deviation)
names = data.frame(variable = c("imeca", "CO", "O3", "NO2", "PM10", "PM2.5", "SO2"), 
                   full_name = c("Air Quality Index", "Carbon Monoxide", "Ozone", "Nitrogen Dioxide", "Coarse Particle Matter", 
                                 "Fine Particle Matter", "Sulfur Dioxide"))
summary = left_join(summary, names)
#### create the tex variable ####
tab = split(summary, f = list(summary$full_name), drop = TRUE)
reg = lapply(tab, function(x) createTexreg(as.character(x$Model), x$est,x$se, x$pvalue))
texreg(reg, stars = c(0.01, 0.05, 0.1), digits = 1,
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize" )
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 13 (Lower panel): Temporal Placebo #### 
#### ____________________________________________________________________ ####
summary = read_rds("04_RegResults/SpatialPlaceboIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#summary = mutate(summary, est = est*standard_deviation, se = se*standard_deviation)
names = data.frame(variable = c("imeca", "CO", "O3", "NO2", "PM10", "PM2.5", "SO2"), 
                   full_name = c("Air Quality Index", "Carbon Monoxide", "Ozone", "Nitrogen Dioxide", "Coarse Particle Matter", 
                                 "Fine Particle Matter", "Sulfur Dioxide"))
summary = left_join(summary, names)
#### create the tex variable ####
tab = split(summary, f = list(summary$full_name), drop = TRUE)
reg = lapply(tab, function(x) createTexreg(as.character(x$Model), x$est,x$se, x$pvalue))
texreg(reg, stars = c(0.01, 0.05, 0.1), digits = 1,
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize" )
#### Clear the space ####
#### ____________________________________________________________________ ####
#### Table 14: Effects for mornings and afternoons ####
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/MorningIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for moring hearings ####
tab = dplyr::filter(summary, sample == "Morning")
tab = split(tab, f = tab$variable)
latex = lapply(tab, function(x) createTexreg(x$sample,x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for CO####
tab = dplyr::filter(summary, sample == "Afternoon")
tab = split(tab, f = tab$variable)
latex = lapply(tab, function(x) createTexreg(x$sample,x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### ____________________________________________________________________ ####
#### Table 15 (appendix): OLS regression results #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
est = read_rds("04_RegResults/SingleOLS.rds")
#### Extract the coefficients ####
summary = lapply(est, function(x) lapply(x, function(z) z = data.frame(z$coefficients)))
#### Add rownames ####
summary  = lapply(summary, function(x) lapply(x, function(z)  z = mutate(z, variable = rownames(z))))
lapply(summary, head)
#### Extract the master_word ####
summary = lapply(summary, function(x) lapply(x, function(z)  z = dplyr::filter(z, variable %in%c("CO", "NO2", "PM10", "PM2.5", "O3", "SO2", "imeca") )))
#### Determine the model####
summary = lapply(summary, function(x) rbindlist(x, idcol = "model"))
#### change the name of the coefficients ####
summary = lapply(summary, setNames, c("specification","est", "se", "tvalue","pvalue",  "variable"))
#### Rbind-list and plot the results #####
summary = rbindlist(summary)
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se)) %>% 
  mutate_at(vars(est, se), function(x) x = exp(x)) %>% mutate(se = abs(se-1)*100*10, est = (est-1)*100*10)
#summary = mutate(summary, est = est*standard_deviation, se = se*standard_deviation)
summary = mutate(summary, variable = mgsub::mgsub(variable, c("imeca", "CO", "O3", "NO2", "PM10", "PM2.5", "SO2"), 
                                                  c("Air Quality Index", "Carbon Monoxide", "Ozone", "Nitrogen Dioxide", "Coarse Particle Matter", 
                                                    "Fine Particle Matter", "Sulfur Dioxide")))

#### create the tex variable ####
tab = split(summary, f = list(summary$specification))
reg = lapply(tab, function(x) createTexreg(as.character(x$variable), x$est,x$se, x$pvalue))
texreg(reg, stars = c(0.05), digits = 1, symbol = "+",
       caption.above = TRUE, caption = "Effect of Air pollution on hearings length", 
       label = "tab: labor_mort",fontsize = "scriptsize")
#### Clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 16: Regression without excluding lengthy hearings #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/SingleNoLengthRest.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 17 (appendix): Regression by excluding days-judge combination when a judge had a long hearing #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/JudgeLongHearing.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 18: Main regression with IV and Wind Direction (Different weather covs) #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/CovsRobustnessIV.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 19: new table of IV results (cluster at the city level) ####
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/CityRobustSE.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
### ____________________________________________________________________ ####
#### Table 20: new table of IV results (cluster at the hour level) ####
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/HourRobustSE.rds")
#### Transform the estimates to percentage changes ####
summary = mutate(summary, se = ifelse(est< 0, se*-1, se))
summary = summary %>% mutate_at(vars(est, se), function(x) x = exp(x))
summary = mutate(summary, se = abs(se-1)*100*10, est = (est-1)*100*10)
#### create the texreg object for CO####
tab = dplyr::filter(summary, variable == "CO")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, variable == "NO2")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, variable == "O3")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N", nrow(x)), rep("R2", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
unique(summary$variable)
tab = dplyr::filter(summary, variable == "PM2.5")
tab = split(tab, f = tab$model)
latex = lapply(tab, function(x) createTexreg("",x$est,x$se, x$pvalue, 
                                             gof.names = c(rep("N.obs", nrow(x)), rep("R squared", nrow(x)),rep("F-stat", nrow(x))),
                                             gof = c(x$N.obs, x$r2, x$f_stat),
                                             gof.decimal = c(rep(F, nrow(x)), rep(T, nrow(x)),rep(F, nrow(x)))))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)

#### clear the space ####
rm(list = ls())
gc()
#### ____________________________________________________________________ ####
#### Table 21: First stage coefficients of the preferred specification. #### 
#### ____________________________________________________________________ ####
#### Load the coefficients ####
summary = read_rds("04_RegResults/FirstStage.rds")
#### create the texreg object for NO2 ####
tab = dplyr::filter(summary, pollutant == "NO2")
tab = split(tab, f = tab$wdr)
latex = lapply(tab, function(x) createTexreg(x$courthouse,x$est,x$se, x$pvalue))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for O3 ####
tab = dplyr::filter(summary, pollutant == "O3")
tab = split(tab, f = tab$wdr)
latex = lapply(tab, function(x) createTexreg(x$courthouse,x$est,x$se, x$pvalue))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for CO ####
tab = dplyr::filter(summary, pollutant == "CO")
tab = split(tab, f = tab$wdr)
latex = lapply(tab, function(x) createTexreg(x$courthouse,x$est,x$se, x$pvalue))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### create the texreg object for PM ####
tab = dplyr::filter(summary, pollutant == "PM2.5")
tab = split(tab, f = tab$wdr)
latex = lapply(tab, function(x) createTexreg(x$courthouse,x$est,x$se, x$pvalue))
texreg(latex, stars = c(0.01,0.05, 0.1), digits = 1, booktabs = TRUE)
#### Clear the space ####
rm(list = ls())
gc()
