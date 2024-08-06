
library(tigris)
library(cowplot)

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
                -avg_wind_speed_10m_mph, -tot_snowfall_in, -EW, -avg_pressure_tendency_2m_mb, 
                -crop_rain, -HU, -tr_fld_fr, - tr_fld_slt, -DU) %>%
  drop_na(incidence)%>% 
  mutate(month = as.character(month)) %>% 
  mutate(time = as.numeric(month)/12 + year) %>%
  arrange(zone_moisture, time)
head(model_dat)

zones_cms = unique(model_dat$zone_moisture)
weather_var  = colnames(model_dat[6:13])
print(weather_var)

#### run in double loop

glm_results = list()
zones = c("1 A", "2 A" ,"2 B", "3 A", "3 B", "3 C", "4 A" ,"4 C" ,"5 A", "5 B" ,"6 A")
print(zones)

for(j in zones){
  
  dat = model_dat %>% filter(zone_moisture == j)
  
  print(j)
  datalist = list()
  num = c(2, 6, 12)
  
  for(i in num) {
    
    lagged_dat = purrr::map2_dfc(dat[6:13], i, dplyr::lag) 
    colnames(lagged_dat) <- paste(colnames(lagged_dat), i , sep = "_")
    # lagged_dat = lagged_dat %>% mutate(zone = j)
    datalist[[i]] <- lagged_dat
    
  }
  
  # bind together all lagged data
  lag_data = do.call(cbind, c(datalist[[2]], datalist[[6]], datalist[[12]]) )
  head(lag_data)
  
  # join lagged explantory variables with incidence 
  inc = model_dat %>%
    filter(zone_moisture == j) %>%
    dplyr::select(exp_count, denom_count)
  
  glm_dat = cbind(inc, lag_data) %>% drop_na() 
  
  lmod <- glm(exp_count ~ . -denom_count, offset = log(denom_count), 
             family = poisson(), data = glm_dat)
  var_select = step(lmod)
  print(j)
  print(summary(var_select))
  
  
  glm_results[[j]] <- data.frame(summary.glm(var_select)$coefficients) %>%
    mutate(zone = j) %>%
    filter(Estimate > 0 ) %>%
    mutate(sig = ifelse(Pr...z.. < .05, "yes" , "no"))
  
  
  
} 

glm_data = do.call(rbind, glm_results )

  
glm_data = glm_data %>% filter(sig == "yes")
 


###########################################################################
###  CF data
cf_sf = read.csv("SG_RG_monthly_data_31may2024.csv")
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
  mutate(month = month(date), year = year(date)) %>%
  filter(zone_moisture %in% zones_cms)

head(climate_zone_cf)
print(unique(climate_zone_cf$zone_moisture))


##### run models 
# run in 
glm_results = list()

zones = c("1 A", "2 A" ,"2 B", "3 A", "3 B", "3 C", "4 A" ,"4 C" ,"5 A", "5 B" ,"6 A")
print(zones)

for(j in zones){
  
  dat = model_dat %>% 
    filter(zone_moisture == j)
  print(head(dat))
  
  datalist = list()
  num = c(2, 6, 12)
  
  for(i in num) {
    
    lagged_dat = purrr::map2_dfc(dat[6:13], i, dplyr::lag) 
    colnames(lagged_dat) <- paste(colnames(lagged_dat), i , sep = "_")
    datalist[[i]] <- lagged_dat
    
  }
  
  # bind together all lagged data
  lag_data = do.call(cbind, c(datalist[[2]], datalist[[6]], datalist[[12]]) )
  head(lag_data)
  
  # join lagged explantory variables with incidence 
  inc = climate_zone_cf %>%
    filter(zone_moisture == j) %>%
    dplyr::select(cases, population)
  
  glm_dat = cbind(inc, lag_data) %>% drop_na() %>%
  #  mutate(incidence = cases/population) %>%
    ungroup() %>%
  #  dplyr::select(-cases, -population, -zone_moisture)  
    dplyr::select(-zone_moisture)  
  
 # print(head(glm_dat))
  
  lmod <- glm(cases ~. - population, offset = log(population),
             family = poisson(), data = glm_dat)
  var_select = step(lmod)
  print(j)
  print(summary(var_select))
  
  glm_results[[j]] <- data.frame(summary.glm(var_select)$coefficients) %>%
    mutate(zone = j) %>%
    filter(Estimate > 0 ) %>%
    mutate(sig = ifelse(Pr...z.. < .05, "yes" , "no"))
  
  
  
} 

glm_data = do.call(rbind, glm_results )
glm_data_cf  = glm_data %>%
  filter(sig != "no")


# sapply(lapply(glm_dat, unique), length) number of levels by variable




lmod <- glm(cases ~. - population, offset = log(population),
           family = poisson(), data = glm_dat)
var_select = step(lmod)
print(j)
print(summary(var_select)) 

results_df <- data.frame(summary.glm(var_select)$coefficients)
head(results_df)

################### plot final things outs 
# Fig 1 map 

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
            STATE_NAME != "Guam" & 
            STATE_NAME != "Hawaii")

us_map = ggplot(data = cz_map %>% drop_na(zone_moisture)) + geom_sf(aes(geometry = geometry, fill = as.character(zone_moisture))) +
  theme_bw() + 
  scale_fill_viridis_d(option = "H") +
  theme_void() +
  labs(fill="Climate Zone")  + 
  theme (legend.position = "right")
us_map

hi_map = left_join(us_counties,climate_zone_crosswalk,  by = c("GEOID")) %>%
   filter( STATE_NAME == "Hawaii")
hi_map = ggplot(data = hi_map %>% drop_na(zone_moisture)) + geom_sf(aes(geometry = geometry, fill = as.character(zone_moisture))) +
  theme_bw() + 
  scale_fill_viridis_d(option = "H") +
  theme_void() +
  labs(fill="Climate Zone")  + 
  theme (legend.position = "none") + 
  theme(plot.margin = unit(c("2", "2", "1", "1"), "cm") )   # +
  #annotation_custom(xmin = 2)
hi_map
hi_map2 = draw_plot(hi_map, .6, .6,  .4, .4)
hi_map2


plot_grid(hi_map, us_map)


# Fig 2 
head(climate_zone_cf)
inc_cf = climate_zone_cf %>%
  mutate(incidence = cases/population*100000)

cf_inc = ggplot(data = inc_cf %>% filter(zone_moisture != "4 B")) + 
  geom_line(aes(x = as.numeric(month)/12 + year, y = rollmean(incidence, k =2, fill = NA), col = zone_moisture), lwd = 2) +
  theme_bw() +
  ylab("Incidence per 100k") +
  xlab("Time") +
#  ylim(c(0, 600)) +
#  scale_color_manual(values = c("chartreuse1","darkred",  "royalblue2")) +
  guides(col = guide_legend(title = "Climate zone group")) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme(legend.position="none") +
  scale_color_viridis_d(option = "H")
cf_inc



cms_inc = ggplot(data = model_dat %>% filter(zone_moisture != "4 B")) + 
  geom_line(aes(x = as.numeric(month)/12 + year, y = rollmean(incidence*100000, k =2, fill = NA), col = zone_moisture), lwd = 2) +
  theme_bw() +
  ylab("Incidence per 100k") +
  xlab("Time") +
  #  ylim(c(0, 600)) +
  #  scale_color_manual(values = c("chartreuse1","darkred",  "royalblue2")) +
  guides(col = guide_legend(title = "Climate zone group")) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  theme(legend.position="right") +
  scale_color_viridis_d(option = "H")
cms_inc

plot_grid(cf_inc, cms_inc,  hi_map, us_map,  nrow =2,  labels = c("a", 'b', 'c'), rel_widths = c(.6, .65, .1, .8))


#### results stats 

sum()

head(cf_sf)
cf_sf = read.csv("SG_RG_monthly_data_31may2024.csv") %>%
  mutate(year = year(date)) %>%
  filter(year == 2011)
sum(cf_sf$cases)
sum(cf_sf$population)/10/12

sum(cf_sf$cases)/sum(cf_sf$population)*100000
# 24 to 225
225/24
225/132

cms_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/exposure_zone_month_04232024.csv")  %>%
  filter(year == 2011)
sum(cms_dat$denom_count)/10/12
head(cms_dat)

sum(cms_dat$exp_count)/sum(cms_dat$denom_count)*100000
# 2.04 # 2.88
2.888491/2.036373 # 1.4
2.888491/2.166973

