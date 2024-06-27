# Code for climate change NTM analysis using Medicare data 
# 4/2/2024
#_____________________________________________________________________________________________________
setwd("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate")
# load packages 
library(dplyr)
library(tidyverse)
library(zoo)
library(tidyr)
library(tibble)
library(glmnet)
library(car)
library(MASS)
library(dLagM)
# ___________________________________________________________________________________________________
# PREPARE DATASET

# Read in latest version of Weather Source data. 
climate_historical = read.csv("S:/WeatherSource Data/monthly_data_NewNWSupdate_2010-2019.csv",header = TRUE)

# Read in evapotranspiration data separately and format monthly time variable. 
climate_evapo = read.csv("S:/WeatherSource Data/monthly_evap_data.csv",header = TRUE) %>%
  mutate(month.year = month_year)


# Join evapotranspiration and other climate variables into one dataset. 
# Remove min and max variables as we are only looking at averages. 
climate_dat = left_join(climate_historical, climate_evapo, by  = c("month.year", "fips")) %>%
  mutate_at(vars(evapotranspiration_mm), ~replace(., is.na(.), 0)) %>%
  select(-(contains("max") | contains("min") | contains("longitude")| contains("latitude"))) %>%
  separate(month.year, c('month', 'year')) %>%
  mutate(month = match(month, month.abb)) %>%
  mutate(year = as.integer(year) + 2000) %>%
  dplyr::select(-X, -NAME, -month_year) # remove no climate variables 
  
# Read in and format FIPS to IECC climate zone crosswalk. 
climate_zone_crosswalk = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/clim_zones_crosswalk_2021_Dec_2023.csv") %>%
  mutate(fips = GEOID) %>%
  mutate(zone_moisture = as.character(zone_moisture))
head(climate_zone_crosswalk)

# Join and summarize climate variables by climate zone 
# We decided to average across climate zone for both meteorological variables and severe weather events.
climate_zone_dat = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(avg_temperature_air_2m_f:evapotranspiration_mm, ~ mean(.x, na.rm = TRUE)))
head(climate_zone_dat)


####### UPDATED DATA ##############################

# Read in most recent CMS data (downloaded 4/23/2024). 
cms_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/exposure_zone_month_04232024.csv")


# Join CMS data with climate data
#ntm_climate_dat = left_join(climate_zone_dat, cms_dat, by = c("month", "year", "zone_moisture"))
#write.csv(ntm_climate_dat, "ntm_cms_climate_dat_03_20_2024.csv")


#______________________________________________________________________________________________
# VARIABLE SELECTION
# read in old climate data and join with new disease data

ntm_climate_dat = read.csv("ntm_cms_climate_dat_03_20_2024.csv") %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg) %>%
  dplyr::select(-denom_count, -inc_count ) %>%
  left_join( cms_dat, by = c("zone_moisture", "year", "month")) 
head(ntm_climate_dat)

# 1. Calculate pairwise correlations between all variables and filter 
# out the correlations that are > .5. 
pairwise_cor = ntm_climate_dat %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) # 106 variable pairs are highly correlated. 

# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")

lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)

# print variables that are highly correlated with literature variables 
print(lit$var1)
print(lit$var2)

# remove variables that are highly correlated with variables found to be significant in 
#prior analyses 
remove = c("urban", "avg_temperature_air_2m_f", "avg_temperature_windchill_2m_f",
           "avg_temperature_heatindex_2m_f", "avg_humidity_relative_2m_pct", 
           "avg_humidity_specific_2m_gpkg", "avg_humidity_relative_2m_pct" ,
           "avg_radiation_solar_total_wpm2", "avg_pressure_2m_mb", 
           "tot_radiation_solar_total_wpm2", "FA", "SV", "bl_eg_co", 
           "shrb_hrb_f", "shrb_hrb_f", "avg_temperature_wetbulb_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_feelslike_2m_f", 
           "nl_dc_co", "avg_pressure_tendency_2m_mb")
# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from above 
pairwise_cor_r2 = ntm_climate_dat[, !names(ntm_climate_dat) %in% remove] %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-(contains("deg"))) %>%  # we are not interested in the degree of wind direction but only in the speed so removing this 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 68 pairs of variables still highly correlated

# Old method of variable selection
# randomly select one of each highly correlated pair of variables to keep in the model
# Assign 0 1 status to each pair and if 0, keep var 1, if 1, keep var 2
#var = pairwise_cor_r2  %>%
#  mutate(var_select =  sample(x = 0:1, size  = 65, replace = TRUE)) %>%
#  mutate(retain_variable = ifelse(var_select == 0, var1, var2))
print(pairwise_cor_r2$var1)


# Manually remove variables with high correlation, retaining the fewest number of 
#variables that do not have high correlation with any other variables.
remove_r2 = c("avg_wind_speed_100m_mph","avg_wind_speed_10m_mph", "herbaceous", 
              "cons_bare", "bl_dc_cl", "crop_veg_m", "HU", "TR", "bl_dc_co", 
              "herbaceous", "lich_moss", "sprs_herb", "sprs_veg_1", 
              "prm_snw_ic", "sprs_shrub", "nl_dc_co", "shrubland", "nl_eg_cl", 
              "tot_snowdepth_in", "veg_crop_m" , "tr_hb_mx", "tr_hb_mx", 
              "hb_tr_mx", "nl_eg_co", "shrubland", "tr_mx_lf"  )


# Last run, check to make sure no variables are highly correlated

pairwise_cor_r3 = ntm_climate_dat %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-c(remove, remove_r2 )) %>%
  dplyr::select(-(contains("deg"))) %>% # we are not interested in the degree of wind direction
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE)
head(pairwise_cor_r3)

# Done! The only remaining correlated pair is water body and brackish flooding (both shown to be significant in the literature). 



## 2. Use Lasso regression to test variable selection further. 
# Make incidence variable and remove variables with no relevance. 
ntm_climate_inc = ntm_climate_dat %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-(contains("deg"))) %>% # not interested in direction of wind.
  dplyr::select(-tree_shrub, -WI, -BW, -nl_eg_op, -crop_irg, 
         -shrub_eg, -shrub_dc ) %>% # these variables are "0" the entire dataset so cannot contribute to the model
  drop_na(incidence)
head(ntm_climate_inc)


# Define response
y = ntm_climate_inc$incidence
# Define predictors 
x = data.matrix(ntm_climate_inc[, 5:25])


# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 

best_lambda <- lamda_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# avg_wind_speed_80m_mph not significant

ntm_climate_inc = ntm_climate_inc %>% 
  dplyr::select(-avg_wind_speed_80m_mph )


## 3. Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
ntm_climate_inc$month = as.character(ntm_climate_inc$month)
head(ntm_climate_inc)


vif_model <- lm(incidence ~ avg_pressure_mean_sea_level_mb  + tot_precipitation_in  + DS +
            DU + EW + FF + FL + HF + HW + TO + bare + crop_rain +
            grassland + tr_fld_slt + tr_fld_fr + water_bdy + evapotranspiration_mm + vapor_pressure + month, data = ntm_climate_inc)
vif(vif_model)
# High GVIF (> 5) indicates that the variables "bare" and "tr_fld_slt" are potentially problematic.  

# Remove "bare" to test the impact on multicollinearity. 
vif_model2 <- lm(incidence ~ avg_pressure_mean_sea_level_mb + tot_precipitation_in  + DS +
                  DU + EW + FF + FL + HF + HW + TO  + crop_rain +
                  grassland + tr_fld_slt + tr_fld_fr + water_bdy + evapotranspiration_mm + month, data = ntm_climate_inc)
vif(vif_model2)
# Nothing looks problematic here, so retaining "tr_fld_slt" but dropping "bare." 


# Final variable alterations, based on discussions with team:
# - remove other pressure variables (avg_pressure_tendency_2m_mb, avg_pressure_mean_sea_level_mb )
# - remove "tot_snowfall_in" and "avg_cloud_cover_tot_pct" because of lack of biological plausibility
# - remove "tr_mx_lf" since it is related to leaf species and this is not a plausible biological driver. 
# - remove grassland, this does not tell us much 
# ___________________________________________________________________________________________
# RUN THE MODEL

# Final dataset used for modeling 
ntm_climate_model = ntm_climate_dat %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-(contains("deg"))) %>% 
  dplyr::select(-tree_shrub, -WI, -BW, -nl_eg_op, -crop_irg, 
         -shrub_eg, -shrub_dc ) %>% # variables that are all 0 
  dplyr::select( -grassland,
         -avg_cloud_cover_tot_pct, -tot_snowfall_in, -X ) %>%
  drop_na(incidence) 

head(ntm_climate_model)
# Quasi Poisson glm on all climate zones simultaneouly 
# make month a cateogrical variable 
ntm_climate_model$month = as.character(ntm_climate_model$month)

glm_model = glm(exp_count ~ avg_pressure_mean_sea_level_mb + tot_precipitation_in +
                  DS + DU + EW + FF + FL + HF + HW + TO  + 
                  crop_rain   + tr_fld_fr + 
                  tr_fld_slt + water_bdy +
                  evapotranspiration_mm + vapor_pressure + month, family = poisson(link = "log"), 
                  offset = log(denom_count), data = ntm_climate_model )
summary(glm_model)

# lag variables by 1 year and rerun model
##########################################################################
head(ntm_climate_model)
weather_var  <- colnames(ntm_climate_model[4:21])
print(weather_var)

lag_1year = ntm_climate_model %>% 
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    across(weather_var, ~lag(., 12L))
  ) %>% 
  ungroup() %>%
  drop_na() %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))

glm_model_lag1yr = glm(exp_count ~ avg_pressure_mean_sea_level_mb  + tot_precipitation_in +
                  DS + DU + EW + FF + FL + HF + HW + TO +
                   crop_rain  + tr_fld_fr + 
                  tr_fld_slt  + water_bdy +
                  evapotranspiration_mm + month + vapor_pressure, family = poisson(link = "log"), 
                offset = log(denom_count), data = lag_1year )
summary(glm_model_lag1yr)



# Loop to run model on each climate zone individually assuming lag  = 0. 
####################################################################33

zones = print(unique(ntm_climate_model$zone_moisture))

for(i in zones){
  
  zone_subset = subset(ntm_climate_model, ntm_climate_model$zone_moisture == i)
  
  glm_model = glm(exp_count ~ avg_pressure_mean_sea_level_mb  + tot_precipitation_in +
                    DS + DU + EW + FF + FL + HF + HW + TO +
                    crop_rain  + tr_fld_fr + 
                    tr_fld_slt  + water_bdy +
                    evapotranspiration_mm + month + vapor_pressure, family = poisson(link = "log"), 
                  offset = log(denom_count), data = zone_subset )
  
  
  summary(glm_model)
  
  print(i)
  print(summary(glm_model))
  
}


# Loop to run model on each climate zone individually assuming lag  = 1 year. 
#######################################################################
zones = print(unique(ntm_climate_model$zone_moisture))
print(zones)

for(i in zones){
  
  print(i)
  zone_subset = subset(ntm_climate_model, ntm_climate_model$zone_moisture == i)
  
  inc_zone = sum(zone_subset$exp_count)/sum(zone_subset$denom_count)*100000
  print(inc_zone)
  
  lag_zone_subset = zone_subset %>% 
    mutate(vapor_pressure = vapor_pressure/100) %>%
    as_tibble() %>%
    group_by(zone_moisture) %>% 
    mutate(month = as.numeric(month)) %>%
    arrange(year, month) %>%
    mutate(
      across(weather_var, ~lag(., 9L))
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    mutate(lag = 1) %>%
    mutate(month = as.character(month))
  
  glm_model_lag = glm(exp_count ~ avg_pressure_mean_sea_level_mb  + tot_precipitation_in +
                        DS + DU + EW + FF + FL + HF + HW + TO +
                        crop_rain  + tr_fld_fr + 
                        tr_fld_slt  + water_bdy +
                        evapotranspiration_mm + month + vapor_pressure, family = poisson(link = "log"), 
                      offset = log(denom_count), data = zone_subset )

    summary = (summary(glm_model_lag))
    print(summary)
 
    
   CI = confint(glm_model_lag)
   print(CI)
  
}



#########################################################################
########### DLM 
# zone 3A

zone_3A = filter(ntm_climate_model %>% filter(zone_moisture == "3 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month)
head(zone_3A)
# significant variables: FF and DS 
model.dlm = dlm(x = zone_3A$DS,  
                y = zone_3A$incidence , q = 24)
summary(model.dlm)

# Run model with lags 
lag_3A = zone_3A %>% 
  as_tibble() %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    across(FF, ~lag(., 5L))
  ) %>% 
  mutate(
    across(DS, ~lag(., 4L))
  ) %>% 
  ungroup() %>%
  drop_na() %>%
  mutate(month = as.character(month))

glm_3a_lag = glm(exp_count ~ DS + FF, family = poisson(link = "log"), 
                 offset = log(denom_count), data = lag_3A )
summary(glm_3a_lag)

##########################################################
# here q = defined lag, model finds how that is distributed monthly up until 24 months
# zone 4A
zone_4A = filter(ntm_climate_model %>% filter(zone_moisture == "4 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month)
head(zone_4A)
# significant variables: tr_fld_fr, vapor_pressure, and water_bdy

model.dlm = dlm(x = zone_4A$vapor_pressure,  
                y = zone_4A$incidence , q = 24)
summary(model.dlm)


#####################################################
# 1a 
zone_1A = filter(ntm_climate_model %>% filter(zone_moisture == "1 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month)
head(zone_1A)
# significant variable: vapor_pressure
model.dlm = dlm(x = zone_1A$vapor_pressure,  
                y = zone_1A$incidence , q = 24)
summary(model.dlm)

######################################################################

########## May 2, 2024 Poster plots ##################################

############################################
## plot map of climate zones in continental US
library(tigris)
library(cowplot)

us_counties <- counties(state = NULL, cb = TRUE) %>%
  mutate(GEOID = str_remove(GEOID, "^0+")) # need to drop the leading 0

climate_zone_crosswalk = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/clim_zones_crosswalk_2021_Dec_2023.csv") %>%
  mutate(fips = as.character(GEOID)) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(zone_moisture = as.character(zone_moisture)) 

cz_map = left_join(us_counties,climate_zone_crosswalk,  by = c("GEOID")) %>%
  filter( STATE_NAME != "Alaska"  &
           STATE_NAME != "American Samoa" & 
           STATE_NAME != "Commonwealth of the Northern Mariana Islands" & 
           STATE_NAME != "Puerto Rico"& 
           STATE_NAME != "Guam")

cz_map = left_join(us_counties,climate_zone_crosswalk,  by = c("GEOID")) %>%
  filter( STATE_NAME == "Hawaii")

us_map = ggplot(data = cz_map %>% drop_na(zone_moisture)) + geom_sf(aes(geometry = geometry, fill = as.character(zone_moisture))) +
  theme_bw() + 
  scale_fill_viridis_d(option = "H") +
  theme_void() +
  labs(fill="Climate Zone")  + 
  theme (legend.position = "right")
us_map

plot_grid(us_map, label = "C")



#################################################################
###########################
# May 4 2024
## Rrun models using the CF dataset 
cz_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/CF_Climate/cf_cz_dat.csv")

cfdat = cz_dat %>% dplyr::select(zone_moisture, month, year, cases, population) %>%
  mutate(month = as.character(month)) %>%
  left_join(ntm_climate_model, cfdat, by = c("year", "month", "zone_moisture")) %>%
  dplyr::select(-incidence, -denom_count, -exp_count ) %>%
  mutate(incidence = cases/population) 

# Loop to run model on each climate zone individually assuming lag  = 1 year. 

zones = print(unique(cfdat$zone_moisture))

for(i in zones){
  
  zone_subset = subset(cfdat, cfdat$zone_moisture == i)
  
  lag_zone_subset = zone_subset %>% 
    mutate(vapor_pressure = vapor_pressure/100) %>%
    as_tibble() %>%
    group_by(zone_moisture) %>% 
    mutate(month = as.numeric(month)) %>%
    arrange(year, month) %>%
    mutate(
      across(weather_var, ~lag(., 12L))
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    mutate(lag = 1) %>%
    mutate(month = as.character(month))
  
  glm_model_lag = glm(cases ~ avg_pressure_mean_sea_level_mb  + tot_precipitation_in +
                        DS + DU + EW + FF + FL + HF + HW + TO +
                        crop_rain  + tr_fld_fr + 
                        tr_fld_slt  + water_bdy +
                        evapotranspiration_mm + month +  vapor_pressure, family = poisson(link = "log"), 
                      offset = log(population), data = zone_subset )
  
  
  
  print(i)
  print(summary(glm_model_lag))
 CI  = confint(glm_model_lag) 
   print(CI)
  
}

# average annua incidence my climate zone

for(i in zones){
  
  print(i)
  zone_subset = subset(cfdat,cfdat$zone_moisture == i) %>%
    drop_na()
  
  inc_zone = sum(zone_subset$cases)/sum(zone_subset$population)*100000
  print(inc_zone)

  }


#########################################################################
# DLM for cf data #########################################################

# zone 4A

zone_4A = filter(cfdat %>% filter(zone_moisture == "4 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month)
head(zone_4A)
#significant variables: dust storms, tr_fld_fr, avg_pressure_mean_sea_level_mb, water_bdy
model.dlm = dlm(x = zone_4A$DS,  
                y = zone_4A$incidence , q = 24)
summary(model.dlm)


#####################################
# zone 3A

zone_3A = filter(cfdat %>% filter(zone_moisture == "3 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month)
head(zone_3A)
# significant varibles: tornadoes 
model.dlm = dlm(x = zone_3A$TO,  
                y = zone_3A$incidence , q = 24)
summary(model.dlm)


##############################################################
#####################################################
# rolling mean plot for both populations by three broad climate groups

# CMS
g1 = c("1 A", "2 A")
g2 = c("2 B", "3 A", "3 B", "3 C", "4 A")
g3 = c("4 A", "5 A", "5 B", "5 C", "6 A")

peaky = ntm_climate_model %>%
  mutate(Climate_Group = ifelse(zone_moisture %in% g1, "Southeast",
                                ifelse(zone_moisture %in% g2, "Mid-Southern", "Northwest"))) %>%
  group_by(Climate_Group, month, year) %>%
  dplyr::summarize(across(exp_count:denom_count, ~sum(.x, na.rm = TRUE))) %>%
  mutate( incidence = 100000*exp_count/denom_count)

pcms = ggplot(data = peaky %>% group_by(Climate_Group)) + 
  geom_line(aes(x = as.numeric(month)/12 + year, y = rollmean(incidence, 2, fill = 4), col = Climate_Group), lwd = 2) +
  theme_bw() +
  ylab("Incidence per 100k") +
  xlab("Time") +
  ylim(c(1, 6)) +
  scale_color_manual(values = c( "chartreuse1", "darkred",  "royalblue2")) +
  guides(col = guide_legend(title = "Climate zone group")) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))  +
  theme(legend.position="none")
pcms 

### CF
peakycf = cfdat %>%
  mutate(Climate_Group = ifelse(zone_moisture %in% g1, "Southeast",
                                ifelse(zone_moisture %in% g2, "Mid-Southern", "Northwest"))) %>%
  group_by(Climate_Group, month, year) %>%
  dplyr::summarize(across(cases:population, ~sum(.x, na.rm = TRUE))) %>%
  mutate( incidence = 100000*cases/population)

pcf = ggplot(data = peakycf %>% group_by(Climate_Group)) + 
  geom_line(aes(x = as.numeric(month)/12 + year, y = rollmean(incidence, 2, fill =400), col = Climate_Group), lwd = 2) +
  theme_bw() +
  ylab("Incidence per 100k") +
  xlab("Time") +
  ylim(c(0, 600)) +
  scale_color_manual(values = c("chartreuse1","darkred",  "royalblue2")) +
  guides(col = guide_legend(title = "Climate zone group")) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme(legend.position="bottom")
 pcf

plot_grid(pcms, pcf, labels = c("A", 'B'),
          ncol = 1, rel_heights = c(4, 5))

#######################################
#####################################
# annual incidence barplot 

yearly_cases_cf = cfdat %>%
  group_by(year) %>%
  dplyr::summarize(across(cases:population, ~sum(.x, na.rm = TRUE)))

ycf = ggplot(data = yearly_cases_cf) + 
  geom_col(aes(x = year, y = cases/population*100000), fill = "darkred") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  ylab("Incidence per 100k") + 
  theme_bw() +
  xlab('Time') +
  theme(axis.text = element_text(size = 7))
ycf

yearly_cases_cms = ntm_climate_model %>%
  group_by(year) %>%
  dplyr::summarize(across(exp_count:denom_count, ~sum(.x, na.rm = TRUE)))

ycms = ggplot(data = yearly_cases_cms ) + 
  geom_col(aes(x = year, y = exp_count/denom_count*100000), fill = "darkblue") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  ylab("Incidence per 100k") + 
  theme_bw() +
  xlab('Time')  +
  theme(axis.text = element_text(size = 7))
ycms

plot_grid(ycms, ycf, labels = c("A", "B"), nrow =2)


###############################################################

### plot CMS and CF incidence by climate zone

cms = ggplot(data = ntm_climate_model %>% filter(zone_moisture != "4 B"), aes(x = (as.numeric(month)/12 + as.numeric(year)), y = rollmean(incidence*100000, k = 1,  fill=NA), col = zone_moisture) )+
  scale_color_viridis_d(option = "H") +
  geom_line(lwd = 2)+
  labs(fill="Climate Zone")   +
  xlab("Time") +
  ylab("Incidence per 100k") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  labs(col="Climate Zone")  +
  theme(legend.position="bottom")
cms

cf = ggplot(data = c %>% filter(zone_moisture != "5 C"), aes(x = (as.numeric(month)/12 + as.numeric(year)), y = rollmean(incidence*100000, k = 12,  fill=NA), col = zone_moisture) )+
  scale_color_viridis_d(option = "H") +
  geom_line(lwd = 2)+
  # theme_void() +
  labs(fill="Climate Zone")   +
  xlab("Time") +
  ylab("Incidence per 100k") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  labs(col="Climate Zone")  + 
  theme(legend.position="bottom")


plot_grid(cms, cf, labels = c("A", "B"), nrow = 1)


############################# moving average #####################
head(cfdat)
ma_cf = cfdat %>%
  group_by(year, month) %>%
  mutate(month = as.numeric(month)) %>%
  dplyr::summarize(across(cases:population, ~sum(.x, na.rm = TRUE))) %>%
  filter(year > 2011) %>%
  arrange(year, month) %>%
  ungroup() %>%
  mutate(ma3 = rollmean(cases, k = 6, fill = NA, align = "right")) %>% 
  drop_na(ma3) %>%
  mutate(seasonal_var = cases -  ma3) %>%
  mutate(diff= ma3 - seasonal_var)

ggplot(data = ma_cf, aes(x = as.numeric(month)/12 + year, y = diff/population)) +
  geom_line(lwd = 1.4) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(2012, 2019, by = 1))


ggplot(data = ma_cf, aes(x = factor(month, levels = 
                                      c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")), y = seasonal_var )) +
  geom_violin(fill = "cornflowerblue") +
 # geom_point(aes(x = month, y = mean(seasonal_var))) +
  geom_hline(yintercept = 0, col = "red" ) +
  theme_bw() +xlab("Month") +ylab("Seasonal variation") +
  ggtitle("4 wk moving average")
 
#################

ma_cms = ntm_climate_model %>%
  group_by(year, month) %>%
  mutate(month = as.numeric(month)) %>%
  dplyr::summarize(across(exp_count:denom_count, ~sum(.x, na.rm = TRUE))) %>%
 # filter(year > 201) %>%
  arrange(year, month) %>%
  ungroup() %>%
  mutate(ma3 = rollmean(exp_count, k = 6, fill = NA, align = "right")) %>% 
  drop_na(ma3) %>%
  mutate(seasonal_var = exp_count -  ma3) %>%
  mutate(diff= ma3 - seasonal_var)

ggplot(data = ma_cms, aes(x = as.numeric(month)/12 + year, y = diff/population)) +
  geom_line(lwd = 1.4) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(2012, 2019, by = 1))


ggplot(data = ma_cms, aes(x = factor(month, levels = 
                                      c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")), y = seasonal_var )) +
  geom_violin(fill = "cornflowerblue") +
  # geom_point(aes(x = month, y = mean(seasonal_var))) +
  geom_hline(yintercept = 0, col = "red" ) +
  theme_bw() +xlab("Month") +ylab("Seasonal variation") +
  ggtitle("4 wk moving average: CMS") +
  ylim(c(-200, 200))


#####################################################################
# june 3 2024 
## redo with % of land cover for climate variables 

library(dplyr)
# Climate variables 
#######################################################################################
#######################################################################################
#######################################################################################


# Read in latest version of Weather Source data. 
setwd("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate")
climate_historical = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/hist_clim_6_4.csv",header = TRUE)


# Read in evapotranspiration data separately and format monthly time variable. 
climate_evapo = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/monthly_evap_data.csv",header = TRUE) %>%
  dplyr::mutate(month.year = month_year)

# Join evapotranspiration and other climate variables into one dataset. 
# Remove min and max variables as we are only looking at averages. 
climate_dat = left_join(climate_historical, climate_evapo, by  = c("month.year", "fips")) %>%
  mutate_at(vars(evapotranspiration_mm), ~replace(., is.na(.), 0)) %>%
  dplyr::select(-(contains("max") | contains("min") | contains("longitude")| contains("latitude"))) %>%
  separate(month.year, c('month', 'year')) %>%
  mutate(month = match(month, month.abb)) %>%
  mutate(year = as.integer(year) + 2000) %>%
  dplyr::select(-X, -NAME, -month_year) 

print(colnames(climate_dat))

# Read in and format FIPS to IECC climate zone crosswalk. 
climate_zone_crosswalk = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/clim_zones_crosswalk_2021_Dec_2023.csv") %>%
  mutate(fips = GEOID) %>%
  mutate(zone_moisture = as.character(zone_moisture))
head(climate_zone_crosswalk)

# Join and summarize climate variables by climate zone 
# We decided to average across climate zone for both meteorological variables and severe weather events.

# meterological variables, we want a mean here 
meteor = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(avg_temperature_air_2m_f:tot_radiation_solar_total_wpm2, ~ mean(.x, na.rm = TRUE)))

evapo = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(evapotranspiration_mm, ~ mean(.x, na.rm = TRUE)))


severe = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(BW:SV, ~ sum(.x, na.rm = TRUE)))
head(severe)

weather_var = left_join(meteor, evapo, by  = c("zone_moisture", "month", "year"))
w_s_var =  left_join(weather_var, severe, by  = c("zone_moisture", "month", "year"))

county <- read.delim("2023_Gaz_counties_national.txt") %>%
  mutate(fips = GEOID) %>%
  mutate(size = ALAND_SQMI + AWATER_SQMI) %>%
  dplyr::select(fips, size)

lu = left_join(climate_dat, county , by = "fips") 

landuse = left_join(lu , climate_zone_crosswalk, by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  mutate(cz_size = sum(size), prop_size = size/cz_size) 
head(landuse)

new = landuse[41:72]*landuse$prop_size
landuse_new = data.frame(landuse[1:3], new, landuse[85]) %>%
  group_by(zone_moisture, month, year)  %>%
  summarize(across(bare:water_bdy, ~ sum(.x, na.rm = TRUE)))
  
##### new variable ensemble 

climate_var = left_join(w_s_var, landuse_new, by = c("zone_moisture", "month", "year"))
head(climate_var)
  

########################################################################################3
########################################################################################
#########################################################################################

# add in disease data 

cms_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/exposure_zone_month_04232024.csv")
cms_clim_dat = left_join(cms_dat, climate_var, by = c("month", "year", "zone_moisture")) %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg) 
head(cms_clim_dat)



#cf_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/CF_Climate/cf_cz_dat.csv")
#cf_clim_dat = cf_dat %>% dplyr::select(zone_moisture, month, year, cases, population) %>%
#     mutate(month = as.integer(month)) %>% left_join(cf_clim_dat, climate_var, by = c("month", "year", "zone_moisture"))
#head(cf_clim_dat)

####################################
#______________________________________________________________________________________________
# VARIABLE SELECTION
# read in old climate data and join with new disease data

# remove variables we know we will not care about 
sense = cms_clim_dat %>%
       dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
              -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
              -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
              -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
              -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf, -avg_cloud_cover_tot_pct, 
              -cons_bare, -bl_eg_co, -nl_dc_co, -shrb_hrb_f, -veg_crop_m, 
              - crop_veg_m)  %>%
  dplyr::select(-(contains("deg")))

print(colnames(sense))

# 1. Calculate pairwise correlations between all variables and filter 
# out the correlations that are > .5. 
pairwise_cor = sense %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) # 106 variable pairs are highly correlated. 

# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")

lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)

# print variables that are highly correlated with literature variables 
print(unique(lit$var1))
print(unique(lit$var2))

# remove variables that are highly correlated with variables found to be significant in 
#prior analyses 
remove = c("TO", "urban", "avg_temperature_air_2m_f", "avg_temperature_windchill_2m_f",
           "avg_temperature_heatindex_2m_f", 
           "avg_humidity_specific_2m_gpkg", "avg_humidity_relative_2m_pct" ,
           "avg_radiation_solar_total_wpm2", "avg_pressure_2m_mb", 
           "tot_radiation_solar_total_wpm2", "FA", "SV", "avg_temperature_wetbulb_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_feelslike_2m_f")
# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from above 
pairwise_cor_r2 = sense[, !names(sense) %in% remove] %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 68 pairs of variables still highly correlated

# Old method of variable selection
# randomly select one of each highly correlated pair of variables to keep in the model
# Assign 0 1 status to each pair and if 0, keep var 1, if 1, keep var 2
#var = pairwise_cor_r2  %>%
#  mutate(var_select =  sample(x = 0:1, size  = 65, replace = TRUE)) %>%
#  mutate(retain_variable = ifelse(var_select == 0, var1, var2))
print(unique(pairwise_cor_r2$var1))


# Manually remove variables with high correlation, retaining the fewest number of 
#variables that do not have high correlation with any other variables.
remove_r2 = c("avg_wind_speed_100m_mph", "avg_wind_speed_80m_mph", "tot_snowdepth_in", "TR" )


# Last run, check to make sure no variables are highly correlated

pairwise_cor_r3 = sense %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-c(remove, remove_r2 )) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE)
head(pairwise_cor_r3)

# Done! The only remaining correlated pair is water body and brackish flooding (both shown to be significant in the literature). 


## 2.  Make incidence variable and remove variables with no relevance. 
ntm_climate_inc = sense %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-BW, -WI, -crop_irg) %>%
  drop_na(incidence)
head(ntm_climate_inc)
sd(ntm_climate_inc$HF)


## 3. Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
ntm_climate_inc$month = as.character(ntm_climate_inc$month)
head(ntm_climate_inc)


vif_model <- lm(incidence ~ avg_pressure_tendency_2m_mb +  avg_pressure_mean_sea_level_mb  + tot_precipitation_in  + DS +
                  DU + EW + FF + FL + HF + HW + HU  + crop_rain + tot_snowfall_in + prm_snw_ic +
                  water_bdy + month + tr_fld_fr  + evapotranspiration_mm + vapor_pressure, data = ntm_climate_inc)
vif(vif_model)
# High GVIF (> 5) indicates that the variables "bare" and "tr_fld_slt" are potentially problematic.  

# Remove "bare" to test the impact on multicollinearity. 
vif_model2 <- lm(incidence ~ avg_pressure_mean_sea_level_mb + tot_precipitation_in  + DS +
                   DU + EW + FF + FL + HF + HW + TO  + crop_rain +
                   grassland + tr_fld_slt + tr_fld_fr + water_bdy + evapotranspiration_mm + month, data = ntm_climate_inc)
vif(vif_model2)
# Nothing looks problematic here, so retaining "tr_fld_slt" but dropping "bare." 

# remove tr_fld_slt


ntm_climate_inc = sense %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-BW, -WI, -crop_irg, -tr_fld_slt) %>%
   drop_na(incidence)
head(ntm_climate_inc)
# Define response
y = ntm_climate_inc$incidence
# Define predictors 
x = data.matrix(ntm_climate_inc[, 6:24])

# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 

best_lambda <- lamda_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# avg_pressure_mean_sea_level_mb  and HF 


### final data 
model_dat  = sense %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-BW, -WI, -crop_irg, -avg_pressure_mean_sea_level_mb, -HF, -prm_snw_ic,
                -avg_wind_speed_10m_mph) %>%
  drop_na(incidence)%>% 
  mutate(month = as.character(month))
head(model_dat)
weather_var  = colnames(model_dat[6:21])
print(weather_var)



### make it for cf 

## Rrun models using the CF dataset 
cz_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/CF_Climate/cf_cz_dat.csv")

cfdat = cz_dat %>% dplyr::select(zone_moisture, month, year, cases, population) %>%
  mutate(month = as.character(month)) %>%
  left_join(model_dat, cfdat, by = c("year", "month", "zone_moisture")) %>%
  dplyr::select(-incidence, -denom_count, -exp_count ) %>%
  mutate(incidence = cases/population) 

zones = print(unique(model_dat$zone_moisture))

for(i in zones){
  
 zone_subset = subset(model_dat, model_dat$zone_moisture == i)
 # zone_subset = subset(cfdat, cfdat$zone_moisture == i)
  
  lag_zone_subset = zone_subset %>% 
    mutate(vapor_pressure = vapor_pressure/100) %>%
    as_tibble() %>%
    group_by(zone_moisture) %>% 
    mutate(month = as.numeric(month)) %>%
    arrange(year, month) %>%
    mutate(
      across(weather_var, ~lag(., 6L))
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    mutate(lag = 1) %>%
    mutate(month = as.character(month))
  
 glm_model_lag = glm(exp_count ~ avg_pressure_tendency_2m_mb  + tot_precipitation_in + 
                       DS + DU + EW + FF + FL + HU + HW  +
                      crop_rain  + 
                       tr_fld_fr  + water_bdy +
                      evapotranspiration_mm  +  vapor_pressure + month, family = poisson(link = "log"), 
                     offset = log(denom_count), data = lag_zone_subset)
  
 # glm_model_lag = glm(cases ~ avg_pressure_tendency_2m_mb  + tot_precipitation_in + 
  #                      DS + DU + EW + FF + FL + HU + HW  +
   #                     crop_rain  + 
    #                    tr_fld_fr  + water_bdy +
    #                    evapotranspiration_mm  +  vapor_pressure + month, family = poisson(link = "log"), 
    #                  offset = log(population), data = zone_subset )
  
  
  
 print(i)
 print(summary(glm_model_lag))
#  CI  = confint(glm_model_lag) 
 # print(CI)
  
}

###### zone 3a
zone_3A = filter(cms_clim_dat %>% filter(zone_moisture == "3 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(incidence = exp_count/denom_count) 

#significant variables: 
model.dlm = dlm(x = zone_3A$vapor_pressure,  
                y = zone_3A$incidence , q = 18)
summary(model.dlm)



############### zone 3a lags 


lag_zone_subset = model_dat %>% 
  filter(zone_moisture == "3 A") %>%
  mutate(vapor_pressure = vapor_pressure/100) %>%
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
#    across(weather_var, ~lag(., 5L))
#  ) %>% 
#  mutate(
    across(FF, ~lag(., 5L))
  ) %>% 
  ungroup() %>%
  drop_na(FF) %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))



glm_model_lag = glm(exp_count ~ avg_pressure_tendency_2m_mb  + tot_precipitation_in + 
                      DS + DU + EW + FF + FL + HU + HW  +
                      crop_rain  + 
                      tr_fld_fr  + water_bdy +
                      evapotranspiration_mm  +  vapor_pressure + month, family = poisson(link = "log"), 
                    offset = log(denom_count), data = zone_subset)

summary(glm_model_lag)

#####################################################


lag_zone_subset1 = model_dat %>% 
  filter(zone_moisture == "3 A") %>%
  mutate(vapor_pressure = vapor_pressure/100) %>%
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    #    across(weather_var, ~lag(., 5L))
    #  ) %>% 
    #  mutate(
    across(FF, ~lag(., 0L))
  ) %>% 
  ungroup() %>%
  drop_na(FF) %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))

lag_zone_subset2 = model_dat %>% 
  filter(zone_moisture == "3 A") %>%
  mutate(vapor_pressure = vapor_pressure/100) %>%
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    #    across(weather_var, ~lag(., 5L))
    #  ) %>% 
    #  mutate(
    across(FF, ~lag(., 6L))
  ) %>% 
  ungroup() %>%
  drop_na(FF) %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))
colnames(lag_zone_subset2) <- paste(colnames(lag_zone_subset2), '2', sep = '_')
lag_zone_subset2 = lag_zone_subset2 %>%
  mutate(year = year_2,month = month_2, zone_moisture = zone_moisture_2)

lag_zone_subset3 = model_dat %>% 
  filter(zone_moisture == "3 A") %>%
  mutate(vapor_pressure = vapor_pressure/100) %>%
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    #    across(weather_var, ~lag(., 5L))
    #  ) %>% 
    #  mutate(
    across(FF, ~lag(., 12L))
  ) %>% 
  ungroup() %>%
  drop_na(FF) %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))

colnames(lag_zone_subset3) <- paste(colnames(lag_zone_subset3), '3', sep = '_')
lag_zone_subset3 = lag_zone_subset3 %>%
  mutate(year = year_3,month = month_3, zone_moisture = zone_moisture_3)


## join 

almost = left_join(lag_zone_subset1, lag_zone_subset2, by = c("month", "year", "zone_moisture"))
head(almost)

lags_tres  = left_join(almost, lag_zone_subset3,  by = c("month", "year", "zone_moisture")) %>%
  drop_na()


glm_model_lag = glm(exp_count ~ avg_pressure_tendency_2m_mb  + tot_precipitation_in + 
                        DU + EW  + FL + HU + HW  + FF + DS +
                      crop_rain  + 
                      tr_fld_fr  + water_bdy +
                      evapotranspiration_mm   + month + vapor_pressure + 
                      avg_pressure_tendency_2m_mb_2  + tot_precipitation_in_2  + 
                      DU_2  + EW_2  + FF_2  + FL_2  + HU_2  + HW_2   +
                      crop_rain_2   +  DS_2  +   vapor_pressure_2 +
                      tr_fld_fr_2   + water_bdy_2  +
                      evapotranspiration_mm_2    + 
                      avg_pressure_tendency_2m_mb_3  + tot_precipitation_in_3 + 
                     DU_3 + EW_3  + FL_3 + HU_3 + HW_3  + FF_3 +
                      crop_rain_3  + DS_3 +
                      tr_fld_fr_3  + water_bdy_3 +
                      evapotranspiration_mm_3 , family = poisson(link = "log"), 
                    offset = log(denom_count), data = lags_tres )

summary(glm_model_lag)

# remove vp 1: 968


# REMOVE ff_2 = 981.9
# REMOVE FF_1 = 973.5
# REMOVE ff_3 = 970.2

# REMOVE DS_1 = 968
# REMOVE DS_2 = 968
# REMOVE DS_3 = 968





###### zone 4a cf
zone_4A = filter(cfdat %>% filter(zone_moisture == "4 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(incidence = cases/population) 

#significant variables: 
model.dlm = dlm(x = zone_4A$DS,  
                y = zone_4A$incidence , q = 18)
summary(model.dlm)



# DLNM

#https://cran.r-project.org/web/packages/dlnm/vignettes/dlnmTS.pdf
library(dlnm)
library(splines)
summary(zone_3A$DS)

cb1.ff <- crossbasis(zone_3A$FF, lag= 12, argvar=list(fun="lin"),
                     arglag=list(fun="poly",degree=3)) # max = 5 years, assumes linear relationship between predictor and response
cb1.ds <- crossbasis(zone_3A$DS, lag= 12, argvar=list(df=1),
                     arglag=list(fun="strata",breaks=1))
cb1.vp <- crossbasis(zone_3A$vapor_pressure, lag= 12, argvar=list(df=1),
                     arglag=list(fun="strata",breaks=1))


model1 <- glm(exp_count ~ cb1.ff + cb1.ds + cb1.vp, 
              family= poisson(), offset = log(denom_count), zone_3A)

pred1.FF <- crosspred(cb1.ff, model1, at=0:20, bylag=0.2, cumul=TRUE)
pred1.DS <- crosspred(cb1.ds, model1, at=0:20, bylag=0.2, cumul=TRUE)
pred1.VP <- crosspred(cb1.vp, model1, at=0:20, bylag=0.2, cumul=TRUE)

plot(pred1.FF, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=15,lwd=2),
     main="Zone 3A: Flash flood")
plot(pred1.DS, "slices", var=10, col=2, cumul=TRUE, ylab="RR",
     main="Zone 3A: Dust storm")
plot(pred1.VP, "slices", var=10, col=2, cumul=TRUE, ylab="RR",
     main="Zone 3A: Vapor pressure")


################## STARMA 
install.packages("starma")
library(starma)

library(spdep)

# Create a 5x5 regular grid which will be our lattice
sites <- matrix(0, 25, 2)
for (i in 1:5) {
  for (j in 1:5)
    sites[(i-1)*5 + j, ] <- c(i, j) - .5
}
plot(sites)

# Create a uniform first order neighbourhood
knb <- dnearneigh(sites, 0, 1)
plot(knb, sites)

# Lag the neighbourhood to create other order matrices
knb <- nblag(knb, 4)
klist <- list(order0=diag(25),
              order1=nb2mat(knb[[1]]),
              order2=nb2mat(knb[[2]]),
              order3=nb2mat(knb[[3]]),
              order4=nb2mat(knb[[4]]))

# Simulate a STARMA(2;1) process
eps <- matrix(rnorm(200*25), 200, 25)
star <- eps
for (t in 3:200) {
  star[t,] <- (.4*klist[[1]] + .25*klist[[2]]) %*% star[t-1,] +
    (.25*klist[[1]]                ) %*% star[t-2,] +
    (            - .3*klist[[2]]) %*% eps[t-1,] +
    eps[t, ]
}

star <- star[101:200,]	# Remove first observations
star <- stcenter(star)	# Center and scale the dataset

# Identify the process
stacf(star, klist)
stpacf(star, klist)

# Estimate the process
ar <- matrix(c(1, 1, 1, 0), 2, 2)
ma <- matrix(c(0, 1), 1, 2)
model <- starma(star, klist, ar, ma)
model
summary(model)

# Diagnose the process
stcor.test(model$residuals, klist, fitdf=4)
stacf(model$residuals, klist)
stpacf(model$residuals, klist)










### Zone 4A 
#########################################################

Zone_4A = model_dat %>%
  filter(zone_moisture == "3 A") %>%
  mutate(incidence = exp_count/denom_count)

ggplot(data = Zone_4A ) + 
 # geom_line(aes(x = as.numeric(month)/12 + year, y = (cases/population)*100000), cex  = 2)+
  geom_line(aes(x = as.numeric(month)/12 + year, y = DS, cex  = 1))+
  xlab("Time") +
  ylab("Incidence per 100k") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  ggtitle("CF: Zone 4A")

ggplot(data = Zone_4A ) + 
  geom_line(aes(x = as.numeric(month)/12 + year, y = incidence/(max(incidence))),lwd = 1.2)+
 geom_line(aes(x = as.numeric(month)/12 + year, y = DS/max(DS), col = "dust storms"), lwd = 1.2)+
 geom_line(aes(x = as.numeric(month)/12 + year, y = FF/max(FF), col = "floods"), lwd = 1.2)+
 # geom_line(aes(x = as.numeric(month)/12 + year, y = vapor_pressure/max(vapor_pressure), col = "vapor pressure"), lwd = 1.2)+
  xlab("Time") +
  scale_color_manual(values = c("tomato", "cornflowerblue")) +
  ylab("Variable scaled") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  ggtitle("CMS: Zone 3A")


  xlab("Time") +
  ylab("Incidence per 100k") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  ggtitle("CMS, Zone 3A, Dust storms")


print(colnames(Zone_4A))


pairwise_cor = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% # random plants? 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) #



# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")

lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)

# print variables that are highly correlated with literature variables 
print(unique(lit$var1))
print(unique(lit$var2))

# remove variables that are highly correlated with variables found to be significant in 
#prior analyses 
remove = c("SV", "FA", "FF", "TO", "avg_temperature_air_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_windchill_2m_f",
           "avg_humidity_specific_2m_gpkg", "tot_precipitation_in", "tot_radiation_solar_total_wpm2",
           "crop_veg_m" , "avg_temperature_wetbulb_2m_f" , "avg_temperature_feelslike_2m_f",
           "avg_temperature_heatindex_2m_f", "avg_pressure_2m_mb" , "avg_radiation_solar_total_wpm2",
           "crop_rain", "veg_crop_m")
  
# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from above 
pairwise_cor_r2 = Zone_4A[, !names(Zone_4A) %in% remove] %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-(contains("deg"))) %>%  # we are not interested in the degree of wind direction but only in the speed so removing this 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 68 pairs of variables still highly correlated

# Old method of variable selection
# randomly select one of each highly correlated pair of variables to keep in the model
# Assign 0 1 status to each pair and if 0, keep var 1, if 1, keep var 2
#var = pairwise_cor_r2  %>%
#  mutate(var_select =  sample(x = 0:1, size  = 65, replace = TRUE)) %>%
#  mutate(retain_variable = ifelse(var_select == 0, var1, var2))
print(unique(pairwise_cor_r2$var1))


# Manually remove variables with high correlation, retaining the fewest number of 
#variables that do not have high correlation with any other variables.
remove_r2 = c( "avg_wind_speed_80m_mph" , "urban", "TR", 
               "avg_wind_speed_100m_mph", "avg_cloud_cover_tot_pct", "tot_snowdepth_in" )


# Last run, check to make sure no variables are highly correlated

pairwise_cor_r3 = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-c(remove, remove_r2 )) %>%
  dplyr::select(-(contains("deg"))) %>% # we are not interested in the degree of wind direction
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE)
head(pairwise_cor_r3)

# Done! The only remaining correlated pair is vp and evapo (both shown to be significant in the literature). 


## 2.  Make incidence variable and remove variables with no relevance. 
z4a_climate_inc = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-(contains("deg"))) %>% # not interested in direction of wind.
  dplyr::select( -WI, -BW, -crop_irg, -EW, -DU, -prm_snw_ic,
                 -cons_bare, -tr_fld_slt) %>% # these variables are "0" the entire dataset so cannot contribute to the model
  drop_na(incidence)
head(z4a_climate_inc)


## 3. Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
z4a_climate_inc$month = as.character(z4a_climate_inc$month)
head(z4a_climate_inc)
print(colnames(z4a_climate_inc))

cor(z4a_climate_inc)
summary(z4a_climate_inc$tr_fld_slt)

# EW all 
vif_model <- lm(incidence ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
                  avg_wind_speed_10m_mph + FL + HU +
                  tr_fld_fr + water_bdy + avg_pressure_mean_sea_level_mb +
                  tot_snowfall_in + DS  + HF + HW  + vapor_pressure + month, data = z4a_climate_inc)

vif(vif_model)
# High GVIF (> 5) "tr_fld_slt" are potentially problematic.  

# Remove "bare" to test the impact on multicollinearity. 
vif_model2 <- lm(incidence ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
                   avg_wind_speed_10m_mph + FL + HU +
                   water_bdy + avg_pressure_mean_sea_level_mb +
                   tot_snowfall_in + DS  + HF + HW  + vapor_pressure, data = z4a_climate_inc)
vif(vif_model2)
# Nothing looks problematic here, so dropping "tr_fld_slt" 


# Final variable alterations, based on discussions with team:
# - remove other pressure variables (avg_pressure_tendency_2m_mb, avg_pressure_mean_sea_level_mb )
# - remove "tot_snowfall_in" and "avg_cloud_cover_tot_pct" because of lack of biological plausibility
# - remove "tr_mx_lf" since it is related to leaf species and this is not a plausible biological driver. 
# - remove grassland, this does not tell us much 
head(z4a_climate_inc)


# Define response
y = z4a_climate_inc$incidence
# Define predictors 
x = data.matrix(z4a_climate_inc[, 6:19])

# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 

best_lambda <- lamda_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


## run a model

head(z4a_climate_inc)
weather_var  <- colnames(z4a_climate_inc[6:19])
print(weather_var)

lag_1year = z4a_climate_inc %>% 
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    across(weather_var, ~lag(., 12L))
  ) %>% 
  ungroup() %>%
  drop_na() %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))

glm_model_lag1yr = glm(exp_count ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
                         avg_wind_speed_10m_mph + FL + HU +
                         tr_fld_fr + water_bdy + avg_pressure_mean_sea_level_mb +
                         tot_snowfall_in + DS  + HF + HW  + vapor_pressure, family = poisson(link = "log"), 
                       offset = log(denom_count), data = lag_1year )
summary(glm_model_lag1yr)





################### zone 2a 


Zone_4A = cms_clim_dat %>%
  filter(zone_moisture == "2 A")

ggplot(data = Zone_4A ) + 
  geom_line(aes(x = month/12 + year, y = (exp_count/denom_count)*100000), cex  = 2)+
  xlab("Time") +
  ylab("Incidence per 100k") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  ggtitle("Zone 4A")

print(colnames(Zone_4A))


pairwise_cor = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% # random plants? 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) #



# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")

lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)

# print variables that are highly correlated with literature variables 
print(unique(lit$var1))
print(unique(lit$var2))

# remove variables that are highly correlated with variables found to be significant in 
#prior analyses 
remove = c("avg_wind_speed_100m_mph"  , "FA", "FF",  "avg_temperature_air_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_windchill_2m_f",
           "avg_humidity_specific_2m_gpkg", "tot_precipitation_in", "tot_radiation_solar_total_wpm2",
           "crop_veg_m" , "avg_temperature_wetbulb_2m_f" , "avg_temperature_feelslike_2m_f",
           "avg_temperature_heatindex_2m_f", "avg_pressure_2m_mb" , "avg_radiation_solar_total_wpm2",
            "veg_crop_m")

# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from above 
pairwise_cor_r2 = Zone_4A[, !names(Zone_4A) %in% remove] %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-(contains("deg"))) %>%  # we are not interested in the degree of wind direction but only in the speed so removing this 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 68 pairs of variables still highly correlated

# Old method of variable selection
# randomly select one of each highly correlated pair of variables to keep in the model
# Assign 0 1 status to each pair and if 0, keep var 1, if 1, keep var 2
#var = pairwise_cor_r2  %>%
#  mutate(var_select =  sample(x = 0:1, size  = 65, replace = TRUE)) %>%
#  mutate(retain_variable = ifelse(var_select == 0, var1, var2))
print(unique(pairwise_cor_r2$var1))


# Manually remove variables with high correlation, retaining the fewest number of 
#variables that do not have high correlation with any other variables.
remove_r2 = c( "avg_wind_speed_80m_mph" , "urban", "TR", "HU", 
               "avg_wind_speed_100m_mph", "avg_cloud_cover_tot_pct", "tot_snowdepth_in" )


# Last run, check to make sure no variables are highly correlated

pairwise_cor_r3 = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-c(remove, remove_r2 )) %>%
  dplyr::select(-(contains("deg"))) %>% # we are not interested in the degree of wind direction
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE)
head(pairwise_cor_r3)

# Done! The only remaining correlated pair is vp and evapo (both shown to be significant in the literature). 


## 2.  Make incidence variable and remove variables with no relevance. 
z4a_climate_inc = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-(contains("deg"))) %>% # not interested in direction of wind.
  dplyr::select( -WI, -BW, -EW,  -prm_snw_ic,-crop_irg,
                 -cons_bare, -HF, -DS, -DU) %>% # these variables are "0" the entire dataset so cannot contribute to the model
  drop_na(incidence)
head(z4a_climate_inc)


## 3. Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
z4a_climate_inc$month = as.character(z4a_climate_inc$month)
head(z4a_climate_inc)
print(colnames(z4a_climate_inc))

summary(z4a_climate_inc$DS)

# EW all 
vif_model <- lm(incidence ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
                  avg_wind_speed_10m_mph + FL  + TO +
                  tr_fld_fr + tr_fld_fr + water_bdy + avg_pressure_mean_sea_level_mb +
                  tot_snowfall_in   + HW  + vapor_pressure + month +
                  evapotranspiration_mm, data = z4a_climate_inc)

vif(vif_model)
# High GVIF (> 5) "tr_fld_slt" are potentially problematic.  

# Remove "bare" to test the impact on multicollinearity. 
#vif_model2 <- lm(incidence ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
 #                  avg_wind_speed_10m_mph + FL + HU +
  #                 water_bdy + avg_pressure_mean_sea_level_mb +
   #                tot_snowfall_in + DS  + HF + HW  + vapor_pressure, data = z4a_climate_inc)
#vif(vif_model2)
# Nothing looks problematic here, so dropping "tr_fld_slt" 


# Final variable alterations, based on discussions with team:
# - remove other pressure variables (avg_pressure_tendency_2m_mb, avg_pressure_mean_sea_level_mb )
# - remove "tot_snowfall_in" and "avg_cloud_cover_tot_pct" because of lack of biological plausibility
# - remove "tr_mx_lf" since it is related to leaf species and this is not a plausible biological driver. 
# - remove grassland, this does not tell us much 
head(z4a_climate_inc)


# Define response
y = z4a_climate_inc$incidence
# Define predictors 
x = data.matrix(z4a_climate_inc[, 6:20])

# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 

best_lambda <- lamda_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


## run a model

head(z4a_climate_inc)
weather_var  <- colnames(z4a_climate_inc[6:20])
print(weather_var)

lag_1year = z4a_climate_inc %>% 
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    across(weather_var, ~lag(., 12L))
  ) %>% 
  ungroup() %>%
  drop_na() %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))

glm_model_lag1yr = glm(exp_count ~ avg_humidity_relative_2m_pct + 
                         avg_wind_speed_10m_mph + FL  + TO + water_bdy +
                         tr_fld_fr + tr_fld_fr  + avg_pressure_tendency_2m_mb + avg_pressure_mean_sea_level_mb  +
                         tot_snowfall_in   + HW  + vapor_pressure + month +
                         evapotranspiration_mm + month, family = poisson(link = "log"), 
                       offset = log(denom_count), data = lag_1year )
summary(glm_model_lag1yr)






zone_2A = filter(cms_clim_dat %>% filter(zone_moisture == "2 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(incidence = exp_count/denom_count)
head(zone_2A)
#significant variables: 
model.dlm = dlm(x = zone_2A$tr_fld_fr,  
                y = zone_2A$incidence , q = 12)
summary(model.dlm)



###############################
# 3a

Zone_4A = cms_clim_dat %>%
  filter(zone_moisture == "3 A")

ggplot(data = Zone_4A ) + 
  geom_line(aes(x = month/12 + year, y = (exp_count/denom_count)*100000), cex  = 2)+
  xlab("Time") +
  ylab("Incidence per 100k") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme_bw() +
  ggtitle("Zone 4A")

print(colnames(Zone_4A))


pairwise_cor = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% # random plants? 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) #



# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")

lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)

# print variables that are highly correlated with literature variables 
print(unique(lit$var1))
print(unique(lit$var2))

# remove variables that are highly correlated with variables found to be significant in 
#prior analyses 
remove = c("avg_wind_speed_100m_mph" , "avg_wind_speed_80m_mph"   , "FA", "FF",  "avg_temperature_air_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_windchill_2m_f", "crop_rain",
           "avg_humidity_specific_2m_gpkg", "tot_precipitation_in", "tot_radiation_solar_total_wpm2",
           "crop_veg_m" , "avg_temperature_wetbulb_2m_f" , "avg_temperature_feelslike_2m_f",
           "avg_temperature_heatindex_2m_f", "avg_pressure_2m_mb" , "avg_radiation_solar_total_wpm2",
           "veg_crop_m")

# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from above 
pairwise_cor_r2 = Zone_4A[, !names(Zone_4A) %in% remove] %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-(contains("deg"))) %>%  # we are not interested in the degree of wind direction but only in the speed so removing this 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 68 pairs of variables still highly correlated

# Old method of variable selection
# randomly select one of each highly correlated pair of variables to keep in the model
# Assign 0 1 status to each pair and if 0, keep var 1, if 1, keep var 2
#var = pairwise_cor_r2  %>%
#  mutate(var_select =  sample(x = 0:1, size  = 65, replace = TRUE)) %>%
#  mutate(retain_variable = ifelse(var_select == 0, var1, var2))
print(unique(pairwise_cor_r2$var1))


# Manually remove variables with high correlation, retaining the fewest number of 
#variables that do not have high correlation with any other variables.
remove_r2 = c( "urban", "SV", "TR",
               "avg_wind_speed_100m_mph", "avg_cloud_cover_tot_pct", "tot_snowdepth_in" )


# Last run, check to make sure no variables are highly correlated

pairwise_cor_r3 = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-c(remove, remove_r2 )) %>%
  dplyr::select(-(contains("deg"))) %>% # we are not interested in the degree of wind direction
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE)
head(pairwise_cor_r3)

# Done! The only remaining correlated pair is vp and evapo (both shown to be significant in the literature). 


## 2.  Make incidence variable and remove variables with no relevance. 
z4a_climate_inc = Zone_4A %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf )  %>% 
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-(contains("deg"))) %>% # not interested in direction of wind.
  dplyr::select( -WI, -BW, -EW,  -prm_snw_ic,-crop_irg,
                 -cons_bare, -HF,  -DU) %>% # these variables are "0" the entire dataset so cannot contribute to the model
  drop_na(incidence)
head(z4a_climate_inc)


## 3. Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
z4a_climate_inc$month = as.character(z4a_climate_inc$month)
head(z4a_climate_inc)
print(colnames(z4a_climate_inc))

summary(z4a_climate_inc$HU)

# EW all 
vif_model <- lm(incidence ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
                  avg_wind_speed_10m_mph + FL  + TO + DS + HU + 
                  tr_fld_fr + tr_fld_fr + water_bdy + avg_pressure_mean_sea_level_mb +
                  tot_snowfall_in   + HW  + vapor_pressure +
                  evapotranspiration_mm, data = z4a_climate_inc)

vif(vif_model)
# High GVIF (> 5) "tr_fld_slt" are potentially problematic.  

# Remove "bare" to test the impact on multicollinearity. 
#vif_model2 <- lm(incidence ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
#                  avg_wind_speed_10m_mph + FL + HU +
#                 water_bdy + avg_pressure_mean_sea_level_mb +
#                tot_snowfall_in + DS  + HF + HW  + vapor_pressure, data = z4a_climate_inc)
#vif(vif_model2)
# Nothing looks problematic here, so dropping "tr_fld_slt" 


# Final variable alterations, based on discussions with team:
# - remove other pressure variables (avg_pressure_tendency_2m_mb, avg_pressure_mean_sea_level_mb )
# - remove "tot_snowfall_in" and "avg_cloud_cover_tot_pct" because of lack of biological plausibility
# - remove "tr_mx_lf" since it is related to leaf species and this is not a plausible biological driver. 
# - remove grassland, this does not tell us much 
head(z4a_climate_inc)


# Define response
y = z4a_climate_inc$incidence
# Define predictors 
x = data.matrix(z4a_climate_inc[, 6:20])

# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 

best_lambda <- lamda_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


## run a model

head(z4a_climate_inc)
weather_var  <- colnames(z4a_climate_inc[6:20])
print(weather_var)

lag_1year = z4a_climate_inc %>% 
  as_tibble() %>%
  group_by(zone_moisture) %>% 
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(
    across(weather_var, ~lag(., 12L))
  ) %>% 
  ungroup() %>%
  drop_na() %>%
  mutate(lag = 1) %>%
  mutate(month = as.character(month))

glm_model_lag1yr = glm(exp_count ~ avg_humidity_relative_2m_pct + avg_pressure_tendency_2m_mb +
                         avg_wind_speed_10m_mph + FL  + TO + DS + HU + 
                         tr_fld_fr + tr_fld_fr + water_bdy + avg_pressure_mean_sea_level_mb +
                         tot_snowfall_in   + HW  + vapor_pressure + 
                         evapotranspiration_mm +
                         month, family = poisson(link = "log"), 
                       offset = log(denom_count), data = lag_1year )
summary(glm_model_lag1yr)






zone_2A = filter(cms_clim_dat %>% filter(zone_moisture == "3 A")) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month) %>%
  mutate(incidence = exp_count/denom_count)
head(zone_2A)
#significant variables: 
model.dlm = dlm(x = zone_2A$vapor_pressure,  
                y = zone_2A$incidence , q = 12)
summary(model.dlm)





##################################
# DLGAM 

install.packages("rstan", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(rstan)

devtools::install_github("nicholasjclark/mvgam")
# DL GAM
install.packages("mvgam")
library(mvgam)
library(dplyr)
library(mgcv)
data('portal_data')
install.packages("rjags")
library(rjags)
install.packages("Rcpp", version= "1.0.10")
packageVersion("Rcpp")
install_version("Rcpp", version = "1.0.10", repos = "http://cran.us.r-project.org")
library("Rcpp")
library(Rcpp)
remove.packages("Rcpp")
install.packages("runjags")
library(runjags)
library(rjags)
rm(list=ls())
Sys.setenv(JAGS_HOME= "C:\\Users\\bentssj\\OneDrive - National Institutes of Health\\CCNTM\\JAGS\\JAGS-4.3.1")

version='0.9.1'
if (!require("rjags")) {
  install.packages("rjags")
}

library(rjags)

# my gosh 
data("portal_data")
dplyr::glimpse(portal_data)


lagard <- function(x, n.lag = 6) {
  n <- length(x); X <- matrix(NA, n, n.lag)
  for (i in 1:n.lag) X[i:n, i] <- x[i:n - i + 1]
  X
}


data_all <- list(lag=matrix(0:5,nrow(portal_data),6,byrow=TRUE),
                 y = portal_data$PP,
                 season = portal_data$month,
                 year = portal_data$year,
                 series = rep(as.factor('series1'), NROW(portal_data)),
                 time = 1:NROW(portal_data))
data_all$precip <- lagard(portal_data$precipitation)
data_all$mintemp <- lagard(portal_data$mintemp)

head(data_all$lag, 5)
head(data_all$precip, 5)
head(data_all$mintemp, 5)
head(data_all$y, 5)

plot_mvgam_series(data = data_all)


data_train <- list(lag = data_all$lag[7:184,],
                   y = data_all$y[7:184],
                   series = data_all$series[7:184],
                   season = data_all$season[7:184],
                   year = data_all$year[7:184],
                   time = 7:184,
                   precip = data_all$precip[7:184,],
                   mintemp = data_all$mintemp[7:184,])

data_test <- list(lag = data_all$lag[185:length(data_all$y),],
                  y = data_all$y[185:length(data_all$y)],
                  series = data_all$series[185:length(data_all$y)],
                  season = data_all$season[185:length(data_all$y)],
                  year = data_all$year[185:length(data_all$y)],
                  time = 185:length(data_all$y),
                  precip = data_all$precip[185:length(data_all$y),],
                  mintemp = data_all$mintemp[185:length(data_all$y),])


mod1 <- mvgam(formula =  y ~ te(mintemp, lag, k = c(6, 6)) +
                te(precip, lag, k = c(6, 6)),
              data = data_train,
              newdata = data_test,
              family = poisson(),
              use_stan = FALSE,
              burnin = 4000,
              trend_model = 'None')





##################################################################################
###################################################################################
# June 10 2024 

# june 3 2024 
## redo with % of land cover for climate variables 

library(dplyr)
# Climate variables 
#######################################################################################
#######################################################################################
#######################################################################################


# Read in latest version of Weather Source data. 
setwd("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate")
climate_historical = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/hist_clim_6_4.csv",header = TRUE)


# Read in evapotranspiration data separately and format monthly time variable. 
climate_evapo = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/monthly_evap_data.csv",header = TRUE) %>%
  dplyr::mutate(month.year = month_year)

# Join evapotranspiration and other climate variables into one dataset. 
# Remove min and max variables as we are only looking at averages. 
climate_dat = left_join(climate_historical, climate_evapo, by  = c("month.year", "fips")) %>%
  mutate_at(vars(evapotranspiration_mm), ~replace(., is.na(.), 0)) %>%
  dplyr::select(-(contains("max") | contains("min") | contains("longitude")| contains("latitude"))) %>%
  separate(month.year, c('month', 'year')) %>%
  mutate(month = match(month, month.abb)) %>%
  mutate(year = as.integer(year) + 2000) %>%
  dplyr::select(-X, -NAME, -month_year) 

print(colnames(climate_dat))

# Read in and format FIPS to IECC climate zone crosswalk. 
climate_zone_crosswalk = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/clim_zones_crosswalk_2021_Dec_2023.csv") %>%
  mutate(fips = GEOID) %>%
  mutate(zone_moisture = as.character(zone_moisture))
head(climate_zone_crosswalk)

# Join and summarize climate variables by climate zone 
# We decided to average across climate zone for both meteorological variables and severe weather events.

# meterological variables, we want a mean here 
meteor = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(avg_temperature_air_2m_f:tot_radiation_solar_total_wpm2, ~ mean(.x, na.rm = TRUE)))

evapo = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(evapotranspiration_mm, ~ mean(.x, na.rm = TRUE)))


severe = left_join(climate_dat, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  summarize(across(BW:SV, ~ sum(.x, na.rm = TRUE)))
head(severe)

weather_var = left_join(meteor, evapo, by  = c("zone_moisture", "month", "year"))
w_s_var =  left_join(weather_var, severe, by  = c("zone_moisture", "month", "year"))

county <- read.delim("2023_Gaz_counties_national.txt") %>%
  mutate(fips = GEOID) %>%
  mutate(size = ALAND_SQMI + AWATER_SQMI) %>%
  dplyr::select(fips, size)

lu = left_join(climate_dat, county , by = "fips") 

landuse = left_join(lu , climate_zone_crosswalk, by = "fips") %>%
  drop_na(zone_moisture) %>%  # drop climate zones where we do not have FIPS codes in the disease data (i.e. Puerto Rico)
  group_by(zone_moisture, month, year) %>%
  mutate(cz_size = sum(size), prop_size = size/cz_size) 
head(landuse)

new = landuse[41:72]*landuse$prop_size
landuse_new = data.frame(landuse[1:3], new, landuse[85]) %>%
  group_by(zone_moisture, month, year)  %>%
  summarize(across(bare:water_bdy, ~ sum(.x, na.rm = TRUE)))

##### new variable ensemble 

climate_var = left_join(w_s_var, landuse_new, by = c("zone_moisture", "month", "year"))
head(climate_var)


########################################################################################3
########################################################################################
#########################################################################################

# add in disease data 

cms_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/exposure_zone_month_04232024.csv")
cms_clim_dat = left_join(cms_dat, climate_var, by = c("month", "year", "zone_moisture")) %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg) 
head(cms_clim_dat)



#cf_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/CF_Climate/cf_cz_dat.csv")
#cf_clim_dat = cf_dat %>% dplyr::select(zone_moisture, month, year, cases, population) %>%
#     mutate(month = as.integer(month)) %>% left_join(cf_clim_dat, climate_var, by = c("month", "year", "zone_moisture"))
#head(cf_clim_dat)

####################################
#______________________________________________________________________________________________
# VARIABLE SELECTION
# read in old climate data and join with new disease data

# remove variables we know we will not care about 
sense = cms_clim_dat %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf, -avg_cloud_cover_tot_pct, 
                -cons_bare, -bl_eg_co, -nl_dc_co, -shrb_hrb_f, -veg_crop_m, 
                - crop_veg_m)  %>%
  dplyr::select(-(contains("deg")))

print(colnames(sense))

# 1. Calculate pairwise correlations between all variables and filter 
# out the correlations that are > .5. 
pairwise_cor = sense %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) # 106 variable pairs are highly correlated. 

# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")

lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)

# print variables that are highly correlated with literature variables 
print(unique(lit$var1))
print(unique(lit$var2))

# remove variables that are highly correlated with variables found to be significant in 
#prior analyses 
remove = c("TO", "urban", "avg_temperature_air_2m_f", "avg_temperature_windchill_2m_f",
           "avg_temperature_heatindex_2m_f", 
           "avg_humidity_specific_2m_gpkg", "avg_humidity_relative_2m_pct" ,
           "avg_radiation_solar_total_wpm2", "avg_pressure_2m_mb", 
           "tot_radiation_solar_total_wpm2", "FA", "SV", "avg_temperature_wetbulb_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_feelslike_2m_f")
# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from above 
pairwise_cor_r2 = sense[, !names(sense) %in% remove] %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 68 pairs of variables still highly correlated

# Old method of variable selection
# randomly select one of each highly correlated pair of variables to keep in the model
# Assign 0 1 status to each pair and if 0, keep var 1, if 1, keep var 2
#var = pairwise_cor_r2  %>%
#  mutate(var_select =  sample(x = 0:1, size  = 65, replace = TRUE)) %>%
#  mutate(retain_variable = ifelse(var_select == 0, var1, var2))
print(unique(pairwise_cor_r2$var1))


# Manually remove variables with high correlation, retaining the fewest number of 
#variables that do not have high correlation with any other variables.
remove_r2 = c("avg_wind_speed_100m_mph", "avg_wind_speed_80m_mph", "tot_snowdepth_in", "TR" )


# Last run, check to make sure no variables are highly correlated

pairwise_cor_r3 = sense %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  dplyr::select(-c(remove, remove_r2 )) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE)
head(pairwise_cor_r3)

# Done! The only remaining correlated pair is water body and brackish flooding (both shown to be significant in the literature). 


## 2.  Make incidence variable and remove variables with no relevance. 
ntm_climate_inc = sense %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-BW, -WI, -crop_irg) %>%
  drop_na(incidence)
head(ntm_climate_inc)
sd(ntm_climate_inc$HF)


## 3. Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
ntm_climate_inc$month = as.character(ntm_climate_inc$month)
head(ntm_climate_inc)


vif_model <- lm(incidence ~ avg_pressure_tendency_2m_mb +  avg_pressure_mean_sea_level_mb  + tot_precipitation_in  + DS +
                  DU + EW + FF + FL + HF + HW + HU  + crop_rain + tot_snowfall_in + prm_snw_ic +
                  water_bdy + month + tr_fld_fr  + evapotranspiration_mm + vapor_pressure, data = ntm_climate_inc)
vif(vif_model)
# High GVIF (> 5) indicates that the variables "bare" and "tr_fld_slt" are potentially problematic.  

# Remove "bare" to test the impact on multicollinearity. 
vif_model2 <- lm(incidence ~ avg_pressure_mean_sea_level_mb + tot_precipitation_in  + DS +
                   DU + EW + FF + FL + HF + HW + TO  + crop_rain +
                   grassland + tr_fld_slt + tr_fld_fr + water_bdy + evapotranspiration_mm + month, data = ntm_climate_inc)
vif(vif_model2)
# Nothing looks problematic here, so retaining "tr_fld_slt" but dropping "bare." 

# remove tr_fld_slt


ntm_climate_inc = sense %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-BW, -WI, -crop_irg, -tr_fld_slt) %>%
  drop_na(incidence)
head(ntm_climate_inc)
# Define response
y = ntm_climate_inc$incidence
# Define predictors 
x = data.matrix(ntm_climate_inc[, 6:24])

# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 

best_lambda <- lamda_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# avg_pressure_mean_sea_level_mb  and HF 


### final data 
model_dat  = sense %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-BW, -WI, -crop_irg, -avg_pressure_mean_sea_level_mb, -HF, -prm_snw_ic,
                -avg_wind_speed_10m_mph, -tot_snowfall_in, -EW,  -avg_pressure_tendency_2m_mb, 
                -crop_rain, -HU, -tr_fld_fr, - tr_fld_slt) %>%
  drop_na(incidence)%>% 
  mutate(month = as.character(month))
head(model_dat)
weather_var  = colnames(model_dat[6:14])
print(weather_var)

## june 9 2024 
### create lags for climate variables 

library(purrr)
library(faraway)
#####################
dat = model_dat %>% filter(zone_moisture == "3 A")

# create 12 lags 
datalist = list()
#num = seq(1, 12, 1)
num = c(2, 6, 12)

for(i in num) {
  
  lagged_dat = purrr::map2_dfc(dat[6:14], i, dplyr::lag) 
  colnames(lagged_dat) <- paste(colnames(lagged_dat), i , sep = "_")
  datalist[[i]] <- lagged_dat
  print(lagged_dat)
  
}

# bind together all lagged data
head(datalist)
lag_data = do.call(cbind, c(datalist[[2]], datalist[[6]], datalist[[12]]) )
#lag_data = lag_data[61:120] 
#lag_data = lag_data[41:96] 
head(lag_data)

# join lagged explantory variables with incidence 
inc = model_dat %>%
  filter(zone_moisture == "3 A") %>%
  dplyr::select(exp_count, denom_count)

glm_dat = cbind(inc, lag_data) %>% drop_na() %>%
  mutate(incidence = exp_count/denom_count) %>%
 dplyr::select(-exp_count, -denom_count)


lmod <- lm(incidence ~ ., data = glm_dat)
var_select = step(lmod)
summary(var_select) # 21 SIGNIFCANT predictors

# run variables in model
glm_dat = cbind(inc_4a, big_data) %>% drop_na() %>%
  mutate(incidence = exp_count/denom_count) 

mod_fulldata = glm(exp_count ~ evapotranspiration_mm_2 + FF_2  + FL_2 + tr_fld_fr_2 +
           tot_precipitation_in_3 + FF_3 +  FL_3 + HW_3 + vapor_pressure_3 +
           tot_precipitation_in_4 + evapotranspiration_mm_4  + FF_4 + FL_4 +
           tr_fld_fr_4+ water_bdy_4  + FF_5 + HW_5 + tr_fld_fr_5 + vapor_pressure_5  +
           tot_precipitation_in_6  + FL_6 + HW_6 + tr_fld_fr_6  + water_bdy_6+
           vapor_pressure_6 + tot_precipitation_in_7+ FL_7 + HW_7 + tr_fld_fr_7 +
           water_bdy_7 + tot_precipitation_in_8 + evapotranspiration_mm_8 + FL_8 +
           HW_8 + water_bdy_8  + vapor_pressure_8 + tot_precipitation_in_9  + FF_9 +
           FL_9  + FL_9  + HW_9  + tr_fld_fr_9 + water_bdy_9 + vapor_pressure_9 + 
           tot_precipitation_in_10 + evapotranspiration_mm_10 + FF_10 + FL_10 + HW_10 +
           tr_fld_fr_10 + water_bdy_10  + vapor_pressure_10   + tot_precipitation_in_11 +
           evapotranspiration_mm_11  + FF_11 + HW_11 + tr_fld_fr_11 + water_bdy_11  + 
           tot_precipitation_in_12 + tr_fld_fr_12  + vapor_pressure_12, offset = log(denom_count),
           family = poisson(), data = glm_dat)
summary(mod_fulldata) 
# AICl 976

mod_sigvar = glm(exp_count ~ evapotranspiration_mm_2 + FL_2   + tr_fld_fr_2 +
           HW_3  + tot_precipitation_in_4 + FL_4 + tr_fld_fr_5  +  FL_6 +
           HW_6 + vapor_pressure_6 + FL_7 + water_bdy_7 + FL_8 + FL_9 +
           HW_9, + vapor_pressure_9 + tot_precipitation_in_10 + tr_fld_fr_10  + water_bdy_10 +
           vapor_pressure_10 + tot_precipitation_in_11 + evapotranspiration_mm_11  + tr_fld_fr_11 +
           water_bdy_11  + tot_precipitation_in_12 + evapotranspiration_mm_12 , 
         offset = log(denom_count),
         family = poisson(), data = glm_dat)
summary(mod_sigvar) # 18810366

mod_onevar = glm(exp_count ~  evapotranspiration_mm_2 + FL_9 + tr_fld_fr_10   +tot_precipitation_in_12 +
           HW_6  + vapor_pressure_6, 
         offset = log(denom_count),
         family = poisson(), data = glm_dat)
summary(mod_onevar) 
# AIC: 1267


 
# 2 mo: FL
# 3 mo: FF, HW
# 4 mo: tot precipitation
# 6 mo: FL, HW
# 7 mo: FL, HW, flooded freshwater, water body
# 8 mo: FL
# 9 mo: FL, HW
# 10 mo: FF, flooded fresh water 
# ll mo: evapostranspiration
# 12 mo: tot precipitation



### try for another climate zone 

dat = model_dat %>% filter(zone_moisture == "3 A")

datalist = list()
num = seq(1, 12, 1)

for(i in num) {
  
  lagged_dat = purrr::map2_dfc(dat[6:15], i, dplyr::lag) 
  colnames(lagged_dat) <- paste(colnames(lagged_dat), i , sep = "_")
  datalist[[i]] <- lagged_dat
  
}

big_data = do.call(cbind, datalist) 
big_data = big_data[11:120] 
head(big_data)


inc_3a = model_dat %>%
  filter(zone_moisture == "3 A") %>%
  dplyr::select(exp_count, denom_count)

glm_dat = cbind(inc_3a, big_data) %>% drop_na() %>%
  mutate(incidence = exp_count/denom_count) %>%
   dplyr::select(-exp_count, -denom_count)


install.packages("faraway")
library(faraway)
lmod <- lm(exp_count ~ ., offset = log(denom_count), data = glm_dat)
drop1(lmod)

lmod <- lm(incidence ~ ., data = glm_dat)
daata = step(lmod)
summary(daata)





##############################################################################
  
### make it for cf 

## Rrun models using the CF dataset 
cz_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/CF_Climate/cf_cz_dat.csv")

cfdat = cz_dat %>% dplyr::select(zone_moisture, month, year, cases, population) %>%
  mutate(month = as.character(month)) %>%
  left_join(model_dat, cfdat, by = c("year", "month", "zone_moisture")) %>%
  dplyr::select(-incidence, -denom_count, -exp_count ) %>%
  mutate(incidence = cases/population) 

zones = print(unique(model_dat$zone_moisture))

for(i in zones){
  
  zone_subset = subset(model_dat, model_dat$zone_moisture == i)
  # zone_subset = subset(cfdat, cfdat$zone_moisture == i)
  
  lag_zone_subset = zone_subset %>% 
    mutate(vapor_pressure = vapor_pressure/100) %>%
    as_tibble() %>%
    group_by(zone_moisture) %>% 
    mutate(month = as.numeric(month)) %>%
    arrange(year, month) %>%
    mutate(
      across(weather_var, ~lag(., 6L))
    ) %>% 
    ungroup() %>%
    drop_na() %>%
    mutate(lag = 1) %>%
    mutate(month = as.character(month))
  
  glm_model_lag = glm(exp_count ~ avg_pressure_tendency_2m_mb  + tot_precipitation_in + 
                        DS + DU + EW + FF + FL + HU + HW  +
                        crop_rain  + 
                        tr_fld_fr  + water_bdy +
                        evapotranspiration_mm  +  vapor_pressure + month, family = poisson(link = "log"), 
                      offset = log(denom_count), data = lag_zone_subset)
  
  # glm_model_lag = glm(cases ~ avg_pressure_tendency_2m_mb  + tot_precipitation_in + 
  #                      DS + DU + EW + FF + FL + HU + HW  +
  #                     crop_rain  + 
  #                    tr_fld_fr  + water_bdy +
  #                    evapotranspiration_mm  +  vapor_pressure + month, family = poisson(link = "log"), 
  #                  offset = log(population), data = zone_subset )
  
  
  
  print(i)
  print(summary(glm_model_lag))
  #  CI  = confint(glm_model_lag) 
  # print(CI)
  
}


#################### slow vs fast growers 

cf_sf = read.csv("SG_RG_monthly_data_31may2024.csv")
head(cf_sf)
vis = cf_sf %>%
  group_by(date) %>%
  dplyr::summarize(across(cases:population, ~sum(.x, na.rm = TRUE)))

ggplot(data = vis) +
  geom_line(aes(x = as.Date(date), y = rollmean(SG, k =2, fill = NA), col = "Fast-growing"), lwd =2) +
  geom_line(aes(x = as.Date(date), y = rollmean(RG, k = 2, fill = NA), col = "Slow-growing"), lwd =2) +
  scale_color_manual(values = c("cornflowerblue", "lightgreen")) + 
  theme_bw() +
  ylab("Cases (2 mo rolling average)") +
  xlab("Date")

#### summarize 
climate_zone_crosswalk = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/CCNTM/clim_zones_crosswalk_2021_Dec_2023.csv") %>%
  mutate(fips = GEOID) %>%
  mutate(zone_moisture = as.character(zone_moisture))
head(climate_zone_crosswalk)

# Join and summarize climate variables by climate zone 
# We decided to average across climate zone for both meteorological variables and severe weather events.
climate_zone_cf = left_join(cf_sf, climate_zone_crosswalk , by = "fips") %>%
  drop_na(zone_moisture) %>% 
  group_by(zone_moisture, date) %>%
  dplyr::summarize(across(cases:population, ~sum(.x, na.rm = TRUE))) %>%
  mutate(month = month(date), year = year(date))


# run model  
cf_slow = left_join(climate_zone_cf, climate_var, by = c("month", "year", "zone_moisture")) %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg/100) %>%
  dplyr::select(date, zone_moisture, population, RG, tot_precipitation_in,
                evapotranspiration_mm, DU, FF, FL, HW, tr_fld_fr, tr_fld_slt, water_bdy, 
                vapor_pressure)



dat_cs = cf_slow %>% filter(zone_moisture == "4 A")

# create 12 lags 
datalist = list()
num = seq(1, 12, 1)

for(i in num) {
  
  lagged_dat = purrr::map2_dfc(dat_cs[5:14], i, dplyr::lag) 
  colnames(lagged_dat) <- paste(colnames(lagged_dat), i , sep = "_")
  datalist[[i]] <- lagged_dat
  
}

# bind together all lagged data 
lag_data = do.call(cbind, datalist) 
lag_data = lag_data[61:120] 
head(lag_data)

# join lagged explantory variables with incidence 
inc_4a = cf_slow %>%
  filter(zone_moisture == "4 A") %>%
  ungroup() %>%
  dplyr::select(RG, population)

glm_dat = cbind(inc_4a, lag_data) %>% drop_na() %>%
  mutate(incidence = RG/population) %>%
  dplyr::select(-RG, -population)


lmod <- lm(incidence ~ ., data = glm_dat)
var_select = step(lmod)
summary(var_select) # 21 SIGNIFC

# SG: 15 , fg: 19


################# 

cf_slow = left_join(climate_zone_cf, climate_var, by = c("month", "year", "zone_moisture")) %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg/100) %>%
  dplyr::select(date, zone_moisture, population, SG, tot_precipitation_in,
                evapotranspiration_mm, DU, FF, FL, HW, tr_fld_fr, tr_fld_slt, water_bdy, 
                vapor_pressure)
cf_rapid = left_join(climate_zone_cf, climate_var, by = c("month", "year", "zone_moisture")) %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg/100) %>%
  dplyr::select(date, zone_moisture, population, RG, tot_precipitation_in,
                evapotranspiration_mm, DU, FF, FL, HW, tr_fld_fr, tr_fld_slt, water_bdy, 
                vapor_pressure)

cor(cf_rapid$RG, cf_slow$SG) #.51 


lag_fg = cf_rapid %>%
  filter(date != "2010-01-01") %>%
#  filter(date != "2010-02-01") %>%
 # filter(date != "2010-03-01") %>%
#  filter(date != "2010-04-01") %>%
#  filter(date != "2010-05-01") %>%
 # filter(date != "2010-06-01") %>%
#  filter(date != "2010-07-01") %>%
 # filter(date != "2010-08-01") %>%
#  filter(date != "2010-09-01") %>%
 # filter(date != "2010-10-01") %>%
  arrange(zone_moisture, date) %>% 
  group_by(zone_moisture) %>%
  mutate(RG = rollmean(RG, k = 6, na.pad = TRUE)) %>%
  drop_na()
head(lag_fg)

sg = cf_slow %>%
  filter(date != "2019-12-01" )%>%
#  filter(date != "2019-11-01" ) %>%
 # filter(date != "2019-10-01" ) %>%
#  filter(date != "2019-09-01" ) %>%
 # filter(date != "2019-08-01" ) %>%
 # filter(date != "2019-07-01" ) %>%
#  filter(date != "2019-06-01" ) %>%
#  filter(date != "2019-05-01" ) %>%
 # filter(date != "2019-04-01" ) %>%
#  filter(date != "2019-03-01" ) %>%
  arrange(zone_moisture, date)%>%
  group_by(zone_moisture) %>%
  mutate(SG = rollmean(SG, k = 6, na.pad = TRUE)) %>%
  drop_na()

cor(lag_fg$RG, sg$SG)



### 6 mo smootihn 
# 10 mo: .737
# 9 mo: .734
# 8 mo: . 74
# 7 mo: .744
# 6 mo: .75
# 5 mo: .76
# 4 mo: .76
# 3 mo: .77
# 2 mo: . 78

### 2 month smoothing 
# 10 mo: .601
# 9 mo: .605
# 8 mo: .595
# 7 mo: .59
# 6 mo: .60
# 5 mo: .61
# 4 mo: .62
# 3 mo: .63
# 2 mo: .64





# no smoothing 
# 1 mo .513
# 2 mo .507
# 3 mo .486
# 4 mo .4996 
# 5 mo .482
# 6 mo .471
# 7 mo .46
# 8 mo .46
# 9 mo: .48
# 10 mo: 47



