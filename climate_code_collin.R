library(dplyr)
library(tigris)
library(cowplot)
library(ggplot2)
library(glmnet)
library(car)
library(faraway)
library(tidyverse)

# Read in cleaned climate variables. 
setwd("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate")
climate_var = read.csv("clim_var_final_july10.csv") %>% dplyr::select(-X)
#########################################################################################

# Collin- this is where the disease data (cms_dat) will need to get swapped out.
# This data should use only 1 claim to be incident, alining with your pul from April 23.
cms_dat = read.csv("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/climate/exposure_zone_month_04232024.csv")
# Join disease data with weather variables, create a variable for vapor pressure 
cms_clim_dat = left_join(cms_dat, climate_var, by = c("month", "year", "zone_moisture")) %>%
  mutate(vapor_pressure = avg_pressure_2m_mb*avg_humidity_specific_2m_gpkg)

##################################################################################
#______________________________________________________________________________________________
# VARIABLE SELECTION
# Step 1. Remove variables that have no biological plausibility as risk factors for NTM. 
select = cms_clim_dat %>%
  dplyr::select(-bl_dc_cl, -bl_eg_co, -hb_tr_mx, -lich_moss, -nl_eg_cl,
                -nl_eg_op, -shrb_hrb_f, -shrub_eg, -sprs_herb, 
                -sprs_veg_1, -bl_dc_co, cons_bare, -bare, -grassland, 
                -herbaceous, -nl_dc_co, -nl_eg_co, -shrub_dc, -shrubland,
                -sprs_shrub, -tr_hb_mx, -tree_shrub, -tr_mx_lf, -avg_cloud_cover_tot_pct, 
                -cons_bare, -bl_eg_co, -nl_dc_co, -shrb_hrb_f, -veg_crop_m, 
                - crop_veg_m, -BW, -WI, -crop_irg)  %>%
  dplyr::select(-(contains("deg")))

# Step 2. Calculate pairwise correlations between all variables and filter out the correlations that are > .5. 
pairwise_cor = select %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  distinct(value, .keep_all = TRUE) # 81 variable pairs are highly correlated. 

# Keep variables from the literature (pressure, evapotranspiration, surface water, flood variables)
# so remove every variable that is correlated with these. 
lit_variables = c("avg_pressure_mean_sea_level_mb", "vapor_pressure", "evapotranspiration_mm", "water_bdy", "FL", "FA", "tr_fld_slt", "tr_fld_fr")
lit = pairwise_cor %>%
  mutate(literature = ifelse(var1 %in% lit_variables | var2 %in% lit_variables, 1, 0)) %>%
  filter(literature == 1)
# print variables that are highly correlated with literature variables 
print(unique(lit$var1))
print(unique(lit$var2))

# remove variables that are highly correlated with variables found to be significant in prior analyses
remove = c("TO", "urban", "avg_temperature_air_2m_f", "avg_temperature_windchill_2m_f",
           "avg_temperature_heatindex_2m_f", 
           "avg_humidity_specific_2m_gpkg", "avg_humidity_relative_2m_pct" ,
           "avg_radiation_solar_total_wpm2", "avg_pressure_2m_mb", 
           "tot_radiation_solar_total_wpm2", "FA", "SV", "avg_temperature_wetbulb_2m_f", 
           "avg_temperature_dewpoint_2m_f", "avg_temperature_feelslike_2m_f")
# need to keeo "avg_humidity_specific_2m_gpkg" in for beckeronni

# Recalculate pairwise correlations after removing variables from prior step.
pairwise_cor_r2 = select[, !names(select) %in% remove] %>%
  ungroup() %>%
  dplyr::select(-zone_moisture, -month, -year ) %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(value < 1) %>%
  filter(value > .5)  %>%
  dplyr::distinct(value, .keep_all = TRUE) # 10 pairs of variables still highly correlated
print(unique(pairwise_cor_r2$var1))

# Manually remove variables with high correlation, retaining the fewest number of variables that do not have high correlation with any other variables.
remove_r2 = c("avg_wind_speed_100m_mph", "avg_wind_speed_80m_mph", "tot_snowdepth_in", "TR" )


# Last run, check to make sure no variables remain highly correlated.
pairwise_cor_r3 = select %>%
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
# Done! The only remaining correlated pair is water body and flooding (both shown to be significant in the literature). 


# Step 2.  Make incidence variable and remove variables found to be correlated in prior steps and assess IVF.
ntm_climate_inc = select %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  drop_na(incidence)
##  Use IVF to assess whether there is any existing multicollinearity in the remaining variables. 
ntm_climate_inc$month = as.character(ntm_climate_inc$month)
vif_model <- lm(incidence ~ avg_pressure_tendency_2m_mb +  avg_pressure_mean_sea_level_mb  + tot_precipitation_in  + DS +
                  DU + EW + FF + FL + HF + HW + HU  + crop_rain + tot_snowfall_in + prm_snw_ic +
                  water_bdy + month  + tr_fld_fr  + evapotranspiration_mm + vapor_pressure, data = ntm_climate_inc)
vif(vif_model)
# High GVIF (> 5) indicates that there might be some multicollinearity. Vapor pressure exceeds this threshold minimally but likely not problematic.

# Step 3. Check with lasso regression if any other variables should be omitted.
# Define response
head(ntm_climate_inc)
y = ntm_climate_inc$incidence
# Define predictors 
x = data.matrix(ntm_climate_inc[, 6:24])

# perform k cross validation, which uses a range of test/training datasets to minimize MSE of linear model
# more folds = higher bias, lower variance 
lamda_model <- cv.glmnet(x, y, alpha = 1) # alpha = 0 is a ridge regression
plot(lamda_model) 
best_lambda <- lamda_model$lambda.min
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
# avg_pressure_mean_sea_level_mb and HF can be removed.


# Step 4. Make final dataset. 
model_dat  = select %>%
  mutate(incidence = exp_count/denom_count ) %>%
  dplyr::select(-c(remove, remove_r2)) %>%
  dplyr::select(-avg_pressure_mean_sea_level_mb, -HF, -prm_snw_ic,
                -avg_wind_speed_10m_mph, -tot_snowfall_in, 
                -crop_rain, -HU, -tr_fld_fr, - tr_fld_slt, -DU, -EW) %>%
  drop_na(incidence)%>% 
  mutate(month = as.character(month)) %>% 
  mutate(time = as.numeric(month)/12 + year) %>%
  arrange(zone_moisture, time)
head(model_dat)

# Step 5. Run the model in double loop.
weather_var  = colnames(model_dat[6:14])
print(weather_var)

glm_results = list()
zones = c("1 A", "2 A" ,"2 B", "3 A", "3 B", "3 C", "4 A" ,"4 C" ,"5 A", "5 B" ,"6 A")

for(j in zones){
  
  dat = model_dat %>% filter(zone_moisture == j)
  
  print(j)
  datalist = list()
  num = seq(2, 12, 1)
  
  for(i in num) {
    
    lagged_dat = purrr::map2_dfc(dat[6:14], i, dplyr::lag) 
    colnames(lagged_dat) <- paste(colnames(lagged_dat), i , sep = "_")
    datalist[[i]] <- lagged_dat
    
  }
  
  # bind together all lagged data
  lag_data = do.call(cbind, c(datalist[[2]], datalist[[3]], datalist[[4]], 
                              datalist[[5]], datalist[[6]], datalist[[7]],
                              datalist[[8]], datalist[[9]], datalist[[10]],
                              datalist[[11]], datalist[[12]]) )
  # join lagged explantory variables with incidence 
  inc = model_dat %>%
    filter(zone_moisture == j) %>%
    dplyr::select(exp_count, denom_count)
  glm_dat = cbind(inc, lag_data) %>% drop_na() 
  
  lmod <- glm(exp_count ~ . -denom_count, offset = log(denom_count), 
              family = poisson(), data = glm_dat)
  var_select = step(lmod)
  
  glm_results[[j]] <- data.frame(summary.glm(var_select)$coefficients) %>%
    mutate(zone = j)
  
  
} 

glm_data = do.call(rbind, glm_results )
head(glm_dat)


glm_dat <- tibble::rownames_to_column(glm_data, "name") %>%
  extract(name, into = c("name", "lag"), "(.*)_([^_]+)$") %>%
  drop_na(lag)  %>%
  separate(name, into = c("z", "var"), sep = "\\.", remove = FALSE) %>%
  mutate(var = replace(var, var == "DS", "Dust storms"), 
         var = replace(var, var == "evapotranspiration_mm", "Evapotranspiration"), 
         var = replace(var, var == "FF", "Flash floods"), 
         var = replace(var, var == "avg_pressure_tendency_2m_mb", "Pressure tendency"), 
         var = replace(var, var == "FL", "Floods"), 
         var = replace(var, var == "HW", "High winds"),
         var = replace(var, var == "tot_precipitation_in", "Precipitation"), 
         var = replace(var, var == "vapor_pressure", "Vapor pressure"),
         var = replace(var, var == "water_bdy", "Water body"))
head(glm_dat)
print(unique(glm_dat$var))

oneA = glm_dat %>%
  filter(zone == "2 A")

# Code to plot one 
ggplot(data = glm_dat %>%
         filter(zone == "2 A"), aes(x = as.numeric(lag), y = as.numeric(Estimate)))+
  geom_smooth(se = TRUE) +
  geom_point(col = "black") +
  geom_hline(aes(yintercept = 0), lty = "dashed", lwd = 1) +
  theme_bw() + 
  ylab("Coefficient") + 
  xlab("`Lag (months)") +
  facet_wrap(vars(var), scales = "free", ncol = 2) +
  ggtitle("Zone 2A") 


# Code to save all 
plot_list = list()
zo = print(unique(glm_dat$zone))
for(i in zo){

  plot = ggplot(data = glm_dat %>%  filter(zone == i), aes(x = as.numeric(lag), y = as.numeric(Estimate), col = zone))+
  geom_smooth(span = 1, se = FALSE, lwd = 1.3, col = "darkgreen") +
  geom_hline(aes(yintercept = 0), lty = "dashed", lwd = 1) +
  theme_bw() + 
  ylab("Coefficient") + 
  xlab("`Lag (months)") +
  # geom_line() +
  facet_wrap(vars(var), scales = "free", ncol = 2) +
  ggtitle("Zone", i) 
  
  plot_list[[i]] = plot
  
}

for (i in 1:11) {
  file_name = paste("zone_", i, ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}









# July 19 
# Load in glm results
glm_results = read.csv("glm_dat_export_07172024.csv")
head(glm_results)


glm = glm_results %>% tibble::rownames_to_column(glm_results, "name") %>%
  extract(name, into = c("name", "lag"), "(.*)_([^_]+)$") %>%
  drop_na(lag)  %>%
  separate(name, into = c("z", "var"), sep = "\\.", remove = FALSE) %>%
  mutate(var = replace(var, var == "DS", "Dust storms"), 
         var = replace(var, var == "evapotranspiration_mm", "Evapotranspiration"), 
         var = replace(var, var == "FF", "Flash floods"), 
         var = replace(var, var == "FL", "Floods"), 
         var = replace(var, var == "HW", "High winds"),
         var = replace(var, var == "tot_precipitation_in", "Precipitation"), 
         var = replace(var, var == "vapor_pressure", "Vapor pressure"),
         var = replace(var, var == "water_bdy", "Water body"))









