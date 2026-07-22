# Nepal clean analysis 

# May 10, 2026

# ============================================================
# Nepal dengue analysis — cleaned script
# ============================================================

# load packages 
library(stringr)
library(terra)
library(exactextractr)
library(jsonlite)
library(akima)
library(dplyr)
library(tidyverse)
library(sf)
library(zoo)
library(deSolve)
library(cowplot)
library(ggplot2)
library(ggspatial)
library(ggridges)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(igraph)
library(mgcv)
library(patchwork)
library(purrr)
library(viridisLite)

# set working directory 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/mordecai/data")

# ============================================================
# GEOMETRY AND DISTRICT NAMES
# ============================================================

nepal_districts_sf <- st_read("nepal_district_boundaries.shp")
district_centroids <- st_centroid(nepal_districts_sf)
district_centroids_coords <- st_coordinates(district_centroids)
nepal_districts_sf$longitude <- district_centroids_coords[, 1]
nepal_districts_sf$latitude  <- district_centroids_coords[, 2]

district_names <- read.csv("nepal_dengue_2025.csv") %>%
  separate(District.with.Code, into = c("id", "location"), sep = " ", extra = "merge") %>%
  distinct(id, location) %>%
  arrange(location) %>%
  filter(id != "Not")

location_name <- c(
  "ACHHAM", "ARGHAKHANCHI", "BAGLUNG", "BAITADI", "BAJHANG", "BAJURA",
  "BANKE", "BARA", "BARDIYA", "BHAKTAPUR", "BHOJPUR", "CHITAWAN",
  "DADELDHURA", "DAILEKH", "DANG", "DARCHULA", "DHADING", "DHANKUTA",
  "DHANUSA", "DOLAKHA", "DOLPA", "DOTI", "GORKHA", "GULMI",
  "HUMLA", "ILAM", "JAJARKOT", "JHAPA", "JUMLA", "KAVREPALANCHOK", "KAILALI",
  "KALIKOT", "KANCHANPUR", "KAPILBASTU", "KASKI", "KATHMANDU",
  "KHOTANG", "LALITPUR", "LAMJUNG", "MAHOTTARI", "MAKWANPUR", "MANANG",
  "MORANG", "MUGU", "MUSTANG", "MYAGDI", "NAWALPARASI EAST", "NAWALPARASI WEST",
  "NUWAKOT", "OKHALDHUNGA", "PALPA", "PANCHTHAR", "PARBAT", "PARSA",
  "PYUTHAN", "RAMECHHAP", "RASUWA", "RAUTAHAT", "ROLPA", "RUKUM EAST",
  "RUKUM WEST", "RUPANDEHI", "SALYAN", "SANKHUWASABHA", "SAPTARI", "SARLAHI",
  "SINDHULI", "SINDHUPALCHOK", "SIRAHA", "SOLUKHUMBU", "SUNSARI", "SURKHET",
  "SYANGJA", "TANAHU", "TAPLEJUNG", "TERHATHUM", "UDAYAPUR")

nepal_districts_sf$location <- location_name

# ============================================================
# DISEASE DATA
# ============================================================

month_convert <- data.frame(month = month.name, month_num = 1:12)

dengue_timeseries <- read.csv("nepal_dengue_2025.csv") %>%
  pivot_longer(cols = c('January','February','March','April','May',
                        'June','July','August','September','October',
                        'November','December'),
               names_to = 'month', values_to = 'cases') %>%
  mutate(cases = ifelse(is.na(cases), 0, cases),
         cases = as.numeric(cases)) %>%
  left_join(month_convert, by = "month") %>%
  separate(District.with.Code, into = c("id", "location"), sep = " ", extra = "merge") %>%
  group_by(Year, month) %>%
  mutate(national_cases = sum(cases)) %>%
  ungroup() %>%
  mutate(monthly_time = month_num / 12 + Year) %>%
  group_by(location) %>%
  mutate(district_max_case   = max(cases),
         district_scaled_cases = cases / district_max_case) %>%
  ungroup() %>%
  filter(monthly_time < 2025.25)
head(dengue_timeseries)
print(min(dengue_timeseries$monthly_time))

# ============================================================
# POPULATION DATA
# ============================================================

pop_dat <- read.csv("Nepal_pop.csv")

pop_location <- dengue_timeseries %>%
  left_join(pop_dat, by = "location") %>%
  mutate(census_2021 = as.numeric(gsub(",", "", census_2021))) %>%
  dplyr::select(location, census_2021) %>%
  distinct()

# Area calculations
if (st_is_longlat(nepal_districts_sf)) {
  nepal_districts_sf <- st_transform(nepal_districts_sf, crs = 32644)
}
district_area_df <- nepal_districts_sf %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  dplyr::select(location, area_km2)

# Dengue + geo + pop joined
dengue_dat_geo_pop <- dengue_timeseries %>%
  left_join(pop_dat, by = "location") %>%
  left_join(st_drop_geometry(nepal_districts_sf), by = "location") %>%
  mutate(census_2011 = as.numeric(gsub(",", "", census_2011)),
         census_2021 = as.numeric(gsub(",", "", census_2021))) %>%
  left_join(district_area_df, by = "location")

incidence <- dengue_dat_geo_pop %>%
  group_by(location) %>%
  mutate(all_cases = sum(cases)) %>%
  ungroup() %>%
  mutate(incidence_per_100k = (all_cases / census_2021) * 100000)

# ============================================================
# TEMPERATURE DATA (10 km grid, population-weighted)
# ============================================================

temp_10km <- read.csv("nepal_temperature_monthly_mean_10km_date.csv")

temp_10km$polygon <- lapply(temp_10km$.geo, function(x) {
  geo       <- fromJSON(x)$coordinates
  coords_mat <- matrix(geo[1, , ], ncol = 2)
  st_polygon(list(coords_mat))
})

temp_10km_sf <- st_sf(temp_10km, geometry = st_sfc(temp_10km$polygon, crs = 4326))
temp_10km_sf$centroid <- st_centroid(temp_10km_sf$geometry)
temp_10km_sf <- temp_10km_sf %>%
  mutate(lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2])

long_data <- pivot_longer(temp_10km_sf, cols = starts_with("X"),
                          names_to = "time", values_to = "temperature") %>%
  group_by(centroid) %>%
  mutate(mean = min(temperature),
         mean = mean - 273.15) %>%
  ungroup()

pop_raster <- rast("npl_pop_counts.tif")
pop_raster <- project(pop_raster, "EPSG:4326")

long_sf       <- st_as_sf(long_data)
long_sf$population_sum <- exact_extract(pop_raster, long_sf, 'sum')

# Re-read districts in WGS84 for spatial joins
nepal_districts_sf <- st_read("nepal_district_boundaries.shp")
district_centroids       <- st_centroid(nepal_districts_sf)
district_centroids_coords <- st_coordinates(district_centroids)
nepal_districts_sf$longitude <- district_centroids_coords[, 1]
nepal_districts_sf$latitude  <- district_centroids_coords[, 2]
nepal_districts_sf$location  <- location_name

long_sf_with_districts <- st_join(long_sf, nepal_districts_sf[, "location"], join = st_intersects)

# Population-weighted temperature per bioclimatic zone
bioclimatic <- read.csv("nepal_district_bioclimatic_zones_complete.csv") %>%
  mutate(location = District)

weighted_temp <- long_sf_with_districts %>%
  group_by(location) %>%
  mutate(total_population = sum(population_sum),
         weight           = population_sum / total_population) %>%
  ungroup() %>%
  mutate(temp_weighted_unit = mean * weight) %>%
  group_by(location) %>%
  mutate(dist_temp_weighted = sum(temp_weighted_unit))
head(weighted_temp)

bioclim <- left_join(weighted_temp, bioclimatic, by = "location")

weighted_temp_bx <- bioclim %>%
  mutate(time_clean   = str_replace(time, "X(\\d{4})(\\d{2})_.*", "\\1-\\2"),
         date         = as.Date(paste0(time_clean, "-01")),
         month        = month(date),
         year         = year(date),
         monthly_time = month / 12 + year) %>%
  group_by(Bioclimatic_Zone, monthly_time) %>%
  mutate(total_pop    = sum(population_sum),
         weight_bz    = population_sum / total_pop,
         weighted_temp = weight_bz * temperature,
         bc_temp_weighted = sum(weighted_temp)) %>%
  ungroup() %>%
  filter(Bioclimatic_Zone != "Unknown" | Bioclimatic_Zone != NA) %>%
  as.data.frame() %>%
  dplyr::select(monthly_time, bc_temp_weighted, Bioclimatic_Zone) %>%
  distinct()

# ============================================================
# BIOCLIMATIC ZONE CASE AGGREGATION
# ============================================================

map_district_bz <- left_join(nepal_districts_sf, bioclimatic, by = "location") %>%
  mutate(Bioclimatic_Zone = replace(Bioclimatic_Zone, location == "JUMLA",       "High Mountain"),
         Bioclimatic_Zone = replace(Bioclimatic_Zone, location == "CHITAWAN",    "Siwalik"),
         Bioclimatic_Zone = replace(Bioclimatic_Zone, location == "ARGHAKHANCHI","Hill"),
         Bioclimatic_Zone = factor(Bioclimatic_Zone,
                                   levels = c("High Mountain","Middle Mountain","Hill","Siwalik","Tarai")))

bioclimatic_cases <- left_join(incidence, bioclimatic, by = "location") %>%
  dplyr::select(Province, location, monthly_time, cases, longitude, latitude,
                census_2021, incidence_per_100k, Bioclimatic_Zone, area_km2) %>%
  filter(location != "specified") %>%
  group_by(Bioclimatic_Zone, monthly_time) %>%
  mutate(bz_cases = sum(cases), bz_denom = sum(census_2021)) %>%
  ungroup() %>%
  dplyr::select(monthly_time, Bioclimatic_Zone, bz_cases, bz_denom) %>%
  left_join(weighted_temp_bx, by = c("Bioclimatic_Zone", "monthly_time")) %>%
  mutate(bz_temp = bc_temp_weighted - 273.15) %>%
  drop_na() %>%
  mutate(Bioclimatic_Zone = factor(Bioclimatic_Zone,
                                   levels = c("High Mountain","Middle Mountain","Hill","Siwalik","Tarai")))
bioclimatic_cases
# ============================================================
# FIG 1 COMPONENTS
# ============================================================

# Panel A — national time series
nat_ts <- ggplot(data = dengue_timeseries) +
  geom_line(aes(x = monthly_time, y = national_cases / 1000), lwd = 2) +
  ylab("Cases (per 1000 persons)") + theme_minimal() + xlab("Time (months)") +
  ggtitle("National") +
  theme(plot.title    = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14),
        axis.text.x   = element_text(size = 14),
        axis.text.y   = element_text(size = 14),
        plot.tag      = element_text(face = "bold", size = 20),
        legend.position = "bottom",
        legend.text   = element_text(size = 18),
        legend.title  = element_text(size = 20))

nat_ts <- nat_ts +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
nat_ts

# Panel B — bioclimatic zone time series (faceted)
bioclim_ts <- ggplot(data = bioclimatic_cases) +
  geom_line(aes(x = monthly_time, y = bz_cases / 1000, col = Bioclimatic_Zone), lwd = 2) +
  facet_wrap(vars(Bioclimatic_Zone), scales = "free_y") +
  scale_color_viridis_d(option = "G", name = "Bioclimatic Zone", begin = .4) +
  ylab("Cases (per 1000 persons)") +
  theme_minimal() +
  xlab("Time (months)") +
  theme(plot.title    = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 18),
        strip.text    = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        axis.title.y  = element_text(size = 14),
        axis.text.x   = element_text(size = 10),
        axis.text.y   = element_text(size = 14),
        plot.tag      = element_text(face = "bold", size = 20),
        legend.position = "none",
        legend.text   = element_text(size = 16),
        legend.title  = element_text(size = 18)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                     labels = scales::label_number(accuracy = 0.1))

bioclim_ts <- bioclim_ts +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
bioclim_ts

# Panel C — bioclimatic zone map
main_map <- ggplot(data = map_district_bz) +
  geom_sf(aes(fill = Bioclimatic_Zone), color = "black") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_viridis_d(option = "G", name = "Bioclimatic Zone", begin = .4) +
  theme(plot.title    = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 20),
        axis.title.x  = element_blank(),
        axis.title.y  = element_blank(),
        axis.text.x   = element_blank(),
        axis.text.y   = element_blank(),
        plot.tag      = element_text(face = "bold", size = 20),
        legend.position = "right",
        legend.text   = element_text(size = 10),
        legend.title  = element_text(size = 12),
        guides(fill   = guide_legend(nrow = 2))) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering())
main_map
#fig1c <- main_map +
#  labs(tag = "C") +
#  theme(plot.tag = element_text(size = 20, face = "bold"))
#fig1c

# Assemble fig1_final_2026
#left_fig1 <- plot_grid(nat_ts, bioclim_ts, ncol = 1)
#fig1_final_2026 <- plot_grid(left_fig1, fig1c)
#fig1_final_2026


######################################### rearrange a bit 
head(bioclimatic_cases)
all_region_plot = bioclimatic_cases %>%
  group_by(monthly_time) %>%
  summarize(across(bz_cases:bz_denom, ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(Bioclimatic_Zone = "National")
head(all_region_plot)
head(bioclimatic_cases)

append_n_r = rbind(bioclimatic_cases %>% dplyr::select(monthly_time, bz_cases, bz_denom, Bioclimatic_Zone), all_region_plot) %>%
  mutate(Bioclimatic_Zone = factor(Bioclimatic_Zone, levels = c("Tarai", "Siwalik", "Hill", "Middle Mountain", "High Mountain",  "National")))
head(append_n_r)

# Get zone levels in plotting order
zone_levels <- levels(factor(append_n_r$Bioclimatic_Zone))
zone_levels <- factor(append_n_r$Bioclimatic_Zone, levels = c("Tarai", "Siwalik", "Hill", "Middle Mountain", "High Mountain", 
                                                              "National"))
zone_levels <- c("Tarai", "Siwalik", "Hill", "Middle Mountain", "High Mountain",  "National")

# Generate viridis colors
cols <- viridis(
  n = length(zone_levels),
  option = "G",
  begin = 0.3, 
  end = .9)

# Replace last color with black
#cols[length(cols)] <- "black"
cols[1] <- "black"

time_series_plot <- ggplot(data = append_n_r) +
  geom_line(aes(x = monthly_time,
                y = bz_cases/(bz_denom/1000),
                col = Bioclimatic_Zone),
            linewidth = 2) +
  facet_wrap(vars(Bioclimatic_Zone), scales = "free_y") +
  scale_color_manual(
    values = setNames(rev(cols), zone_levels),
    name = "Bioclimatic Zone" ) +
  ylab("Cases (per 1000 persons)") +
  xlab("Time (months)") +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    strip.text    = element_text(size = 14),
    axis.title.x  = element_text(size = 14),
    axis.title.y  = element_text(size = 14),
    axis.text.x   = element_text(size = 10),
    axis.text.y   = element_text(size = 14),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.position = "none",
    legend.text   = element_text(size = 16),
    legend.title  = element_text(size = 18)) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_number(accuracy = 0.1)) 
time_series_plot

fig1a <- time_series_plot+
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1a

print(zone_levels)
# redo map with correct colors
main_map <- ggplot(data = map_district_bz) +
  geom_sf(aes(fill = Bioclimatic_Zone), color = "black") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_manual(
    values = setNames(rev(cols[2:6]), zone_levels[1:5]),
    name = "Bioclimatic Zone") +
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x   = element_blank(),
    axis.text.y   = element_blank(),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.position = "right",
    legend.text   = element_text(size = 10),
    legend.title  = element_text(size = 12),
    guides(fill = guide_legend(nrow = 2))
  ) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering())

fig1b <- main_map +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1b


fig1_final_2026 = plot_grid(fig1a, fig1b, ncol = 1, rel_heights = c(.5, .5))
fig1_final_2026


###### annual cases 
# plot annual cases 
annual_cases = dengue_timeseries %>%
  group_by(location, Year) %>%
  mutate(annual_count = sum(cases)) %>%
  ungroup()

map_time = left_join(map_district_bz, annual_cases, by = "location") %>%
  distinct()
head(map_time)
class(annual_cases)

annual_cases_map <- ggplot(data = map_time ) +
  geom_sf(aes(fill = log(annual_count))) +
#  geom_sf() +
 # geom_point(aes(col = annual_count)) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(vars(Year)) + 
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_viridis_c(option = "G", name = "Log annual cases", begin = .2) +
  theme(plot.title    = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 20),
        axis.title.x  = element_blank(),
        axis.title.y  = element_blank(),
        axis.text.x   = element_blank(),
        axis.text.y   = element_blank(),
        plot.tag      = element_text(face = "bold", size = 20),
        legend.position = "bottom",
        legend.text   = element_text(size = 12),
        legend.title  = element_text(size = 14),
        guides(fill   = guide_legend(nrow = 2))) 
annual_cases_map

# ============================================================
# THERMAL RESPONSE FUNCTIONS & SEIRS MODEL
# ============================================================

briere <- function(x, c, T0, Tm){
  ifelse((x < T0) | (x > Tm), 0, c * x * (x - T0) * sqrt(Tm - x))
}

quadratic <- function(x, c, T0, Tm){
  ifelse((x < T0) | (x > Tm), 0, c * (x - T0) * (x - Tm))
}

inverted_quadratic <- function(x, c, T0, Tm, timestep) {
  ifelse((x < T0) | (x > Tm),
         1.0 / timestep,
         1.0 / (c * (x - T0) * (x - Tm)))
}

#temp = seq(10, 40, 1)
# make functionf for all ze variables 
# biting rate
a <- function(temp){
  briere(temp, a_c, a_T0, a_Tm)
}

# transmission competence: probability of human infection per bite by an infectious mosquito
b <- function(temp){
  briere(temp, b_c, b_T0, b_Tm)
}

# probability of mosquito infection per bite on an infectious host
c_prob <- function(temp){
  briere(temp, c_c, c_T0, c_Tm)
}

# eggs per female per day 
EFD =  function(temp){
  briere(temp, EFD_c, EFD_T0, EFD_Tm)
}

# probability of egg to adult survival 
PEA =  function(temp){
  quadratic(temp, PEA_c, PEA_T0, PEA_Tm)
}

# mosquito development rate (1/larval development period)
MDR <- function(temp){
  briere(temp, MDR_c, MDR_T0, MDR_Tm)
}

# adult mortality rate (1/larval development period)
mu  <- function(temp){ inverted_quadratic(temp, mu_c, mu_T0, mu_Tm, timestep = 1)}

# parasite development rate
PDR <- function(temp){
  briere(temp, PDR_c, PDR_T0, PDR_Tm)
}


a_c = .000202
a_T0 =  13.35
a_Tm =  40.08

b_c = .000849
b_T0 = 17.05
b_Tm = 35.83

c_c = .000491
c_T0 = 12.22 
c_Tm = 37.46 

EFD_c = .00846
EFD_T0 = 14.58
EFD_Tm = 34.61 

PEA_c = -.00599
PEA_T0 = 13.56
PEA_Tm = 38.29

MDR_c = .000491
MDR_T0 = 11.36 
MDR_Tm = 39.17

PDR_c = .0000665
PDR_T0 = 10.68
PDR_Tm = 45.90

mu_c = -.148
mu_T0 = 9.16 
mu_Tm = 37.73

############################################################
# Define the model 
# Initial state vector
seirs_dengue_model = function(t, state, param){
  with(as.list(state), {
    
    # parameters 
    # FOI = param["FOI"]          # transmission coefficient 
    foi_scalar = param["foi_scalar"] 
    lp = param["lp"]          # latent period
    gamma = param["gamma"]    # infectious period 
    imm = param["imm"]        # immunity from infection  
    N = param["N"]
    hr = param["hr"]
    
    c_base = param["c_base"]
    c_reduction = param["c_reduction"]
    c_start = param["c_start"]
    c_end = param["c_end"]
    
    # Time-dependent reduction in transmission
    c = ifelse(t >= c_start & t <= c_end, c_base * c_reduction, c_base)
    
    
    FOI <- cal_data$FOI_sqrt[t]
    
    # Handle case where FOI might be NA
    if (is.na(FOI) || !is.finite(FOI)) {
      FOI <- 0.001  # Small default value
    }
    
    # Differential equations
    
    dS = -c*foi_scalar*FOI*S*I/N   + imm*R 
    dE =  c*foi_scalar*FOI*S*I/N - lp*E 
    dI =  lp*E - gamma*I
    dR =  I*gamma  -  R*imm 
    
    # Return the rates of change
    list(c(dS, dE, dI, dR)) }) 
}



#############################################################################

# Calibrate each bioclimatic zone independently 

################################################
# 1. HILL
hill = bioclimatic_cases %>% filter(Bioclimatic_Zone == "Hill") %>%
  distinct(monthly_time, bz_cases, bz_temp, bz_denom) 

temp = print(hill$bz_temp)
m = EFD(temp)*PEA(temp)*MDR(temp)
FOI = ( (m*(a(temp)^2)*b(temp)*c(temp)) *exp(-mu(temp)/PDR(temp)) ) / (mu(temp)^2)
print(FOI)

time = seq(1, length(FOI),1 )
monthly_time = print(hill$monthly_time)

# Make temperature dataset
dat_temp = data.frame(FOI, time, temp, monthly_time) %>%
  mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right")) %>%
  mutate(FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2)) %>% 
  mutate(FOI_smooth  = ifelse(FOI_smooth < 0, 0, FOI_smooth)) %>%
  mutate(FOI_sqrt = sqrt(FOI_smooth)) #

######################## Make final calibration data 
cal_data = left_join(hill, dat_temp, by = c("monthly_time"))
observed_data = cal_data$bz_cases

# States
N <- cal_data$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N)

param <- c(
  foi_scalar = 3.6, 
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction = 0.0001,    #.06355,
  c_start = 15,# March 2020
  c_end = 33 )  # September 2021

# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

#Add derived variables
out_df_check_h <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.011) 

hill_cal = ggplot(data = out_df_check_h ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
         lwd = 1.2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.7, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Hill") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
 # scale_color_manual(values = c("Observed" = "gray29", "Predicted" =  "#58A39B" )) + # #2E7C8A"
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" =  cols[4])) + # #2E7C8A"
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
  #  axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.x    = element_blank(),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  theme( panel.grid = element_blank() ) 
hill_cal


#############################################################
# Siwalik
siwalik = bioclimatic_cases %>% filter(Bioclimatic_Zone == "Siwalik")  %>%
  distinct(monthly_time, bz_cases, bz_temp, bz_denom) 

temp = print(siwalik$bz_temp)
m = EFD(temp)*PEA(temp)*MDR(temp)
FOI = ( (m*(a(temp)^2)*b(temp)*c(temp)) *exp(-mu(temp)/PDR(temp)) ) / (mu(temp)^2)

time = seq(1, length(FOI),1 )
monthly_time = print(siwalik$monthly_time)

#
dat_temp = data.frame(FOI, time, temp, monthly_time) %>%
  mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right")) %>%
  mutate(FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2)) %>% 
  mutate(FOI_smooth  = ifelse(FOI_smooth < 0, 0, FOI_smooth)) %>%
  mutate(FOI_sqrt = sqrt(FOI_smooth)) 

siwalik_foi = ggplot(data = dat_temp) +
  geom_line(aes(x = monthly_time, y = FOI_sqrt, col = temp), cex = 3) +
  theme_bw() +
  ggtitle('Siwalik') + xlab("Time") +  ylab("Force of infection") +
  scale_color_viridis_c(option = "G", name = "FOI") +
  theme_minimal() + xlab("Time (months)") 
siwalik_foi

######################## Make final calibration data 
cal_data = left_join(siwalik, dat_temp, by = c("monthly_time"))
head(cal_data)
observed_data = cal_data$bz_cases

# States
N <- cal_data$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N)

param <- c(
  foi_scalar = .45,     #5.29, # 3.6*.831
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction =  0.0001,    #.06355,
  c_start = 15,# March 2020
  c_end = 33 ) #33 #42  # September 2021

time = seq(1, nrow(cal_data),1 )

# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

out_df_check_s <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.0075) # %>% #%>%

siwalik_cal = ggplot(data = out_df_check_s ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
            lwd = 1.2) +
  # geom_line(data = cal_data_hill, aes(x = monthly_time, y = FOI_sqrt*6000, col = "Observed"), lwd = 2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.5, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Siwalik") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" =  cols[5])) + # #2E7C8A"
 # scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#8CCFA5" ))  + #"lightseagreen"
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
  #  axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.title.x    = element_blank(),   # x-axis title
     axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  theme( panel.grid = element_blank() )  

siwalik_cal

###########################################################################
# Middle Mountain 
midmount = bioclimatic_cases %>% filter(Bioclimatic_Zone == "Middle Mountain") %>%
  distinct(monthly_time, bz_cases, bz_temp, bz_denom) 

temp = print(midmount$bz_temp)
m = EFD(temp)*PEA(temp)*MDR(temp)
FOI = ( (m*(a(temp)^2)*b(temp)*c(temp)) *exp(-mu(temp)/PDR(temp)) ) / (mu(temp)^2)
print(FOI)

time = seq(1, length(FOI),1 )
monthly_time = print(midmount$monthly_time)

#
dat_temp = data.frame(FOI, time, temp, monthly_time) %>%
  mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right")) %>%
  mutate(FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2)) %>% 
  mutate(FOI_smooth  = ifelse(FOI_smooth < 0, 0, FOI_smooth)) %>%
  mutate(FOI_sqrt = sqrt(FOI_smooth)) 

midmount_foi = ggplot(data = dat_temp) +
  geom_line(aes(x = monthly_time, y = FOI_sqrt, col = temp), cex = 3) +
  theme_bw() +
  ggtitle('Middle Mountain') +
  xlab("Time") +
  ylab("Force of infection") +
  scale_color_viridis_c(option = "G", name = "FOI") +
  theme_minimal() +
  xlab("Time (months)") 
midmount_foi

######################## Make final calibration data 
cal_data = left_join(midmount, dat_temp, by = c("monthly_time"))
head(cal_data)
observed_data = cal_data$bz_cases

# States
N <- cal_data$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N)

param <- c(
  foi_scalar =  5.12, # 3.10
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction = .000001, # .000001,    #.0001. .0000005, 
  c_start = 15,
  c_end = 33)   #41.75

time = seq(1, nrow(cal_data),1 )
# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

out_df_check_mm <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.0076) # %>% #%>%

middlemountain_cal = ggplot(data = out_df_check_mm ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
            lwd = 1.2) +
  # geom_line(data = cal_data_hill, aes(x = monthly_time, y = FOI_sqrt*6000, col = "Observed"), lwd = 2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.69, 2025)) +
  ggtitle("Middle Mountain") +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#407290" )) + # "#4966A0"
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" =  cols[3])) + # #2E7C8A"
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
  #  axis.title.x    = element_text(size = 20),   # x-axis title
  axis.title.x    = element_blank(),   # x-axis title, 
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  theme( panel.grid = element_blank() ) 
middlemountain_cal

###########################################################################
# Tarai
tarai = bioclimatic_cases %>% filter(Bioclimatic_Zone == "Tarai") %>%
  distinct(monthly_time, bz_cases, bz_temp, bz_denom) 

temp = print(tarai$bz_temp)
m = EFD(temp)*PEA(temp)*MDR(temp)
FOI = ( (m*(a(temp)^2)*b(temp)*c(temp)) *exp(-mu(temp)/PDR(temp)) ) / (mu(temp)^2)
print(FOI)

time = seq(1, length(FOI),1 )
monthly_time = print(tarai$monthly_time)

#
dat_temp = data.frame(FOI, time, temp, monthly_time) %>%
  mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right")) %>%
  mutate(FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2)) %>% 
  mutate(FOI_smooth  = ifelse(FOI_smooth < 0, 0, FOI_smooth)) %>%
  mutate(FOI_sqrt = sqrt(FOI_smooth)) 

tarai_foi = ggplot(data = dat_temp) +
  geom_line(aes(x = monthly_time, y = FOI_sqrt, col = temp), cex = 3) +
  theme_bw() +
  ggtitle('Tarai') +
  ylab("Force of infection") +
  xlab("Time") +
  scale_color_viridis_c(option = "G", name = "FOI") +
  theme_minimal() +
  xlab("Time (months)") 
tarai_foi

######################## Make final calibration data 
cal_data = left_join(tarai, dat_temp, by = c("monthly_time"))
observed_data = cal_data$bz_cases

# States
N <- cal_data$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N)

param <- c(
  foi_scalar = .454,  #3.89, # 2.10
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction = .00001,    # .00001
  c_start = 15,
  c_end = 43 )

time = seq(1, nrow(cal_data),1 )
# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

out_df_check_t <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.0030) 

tarai_cal = ggplot(data = out_df_check_t ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.2) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
             lwd = 1.2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.5, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Tarai") +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" =  cols[6])) + # #2E7C8A"
 # scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#DCF1DF" ))+ #  "#D6EED9"
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
  #  axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.title.x    = element_blank(),   # x-axis title
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme( panel.grid = element_blank() )
tarai_cal 


#################################################
### High mountain 
hm = bioclimatic_cases %>% filter(Bioclimatic_Zone == "High Mountain") %>%
  distinct(monthly_time, bz_cases, bz_temp, bz_denom) 

temp = print(hm$bz_temp)
m = EFD(temp)*PEA(temp)*MDR(temp)
FOI = ( (m*(a(temp)^2)*b(temp)*c(temp)) *exp(-mu(temp)/PDR(temp)) ) / (mu(temp)^2)

time = seq(1, length(FOI),1 )
monthly_time = print(hm$monthly_time)

dat_temp = data.frame(FOI, time, temp, monthly_time) %>%
  mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right")) %>%
  mutate(FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2)) %>% 
  mutate(FOI_smooth  = ifelse(FOI_smooth < 0, 0, FOI_smooth)) %>%
  mutate(FOI_sqrt = sqrt(FOI_smooth) + 1) 

hm_foi = ggplot(data = dat_temp) +
  geom_line(aes(x = monthly_time, y = FOI_sqrt, col = temp), cex = 3) +
  theme_bw() +
  ggtitle('High Mountain') +
  ylab("Force of infection") +
  xlab("Time") +
  scale_color_viridis_c(option = "G", name = "FOI") +
  theme_minimal() +
  xlab("Time (months)") 
hm_foi

ggplot(data = dat_temp) +
  geom_line(aes(x = monthly_time, y = FOI_sqrt, col = temp), cex = 3) +
  theme_bw() +
  ggtitle('High Mountain')

######################## Make final calibration data 
cal_data = left_join(hm, dat_temp, by = c("monthly_time"))

head(cal_data)
observed_data = cal_data$bz_cases

# States
N <- cal_data$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N)

param <- c(
  foi_scalar = 14.38,  #17.71, 
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction = 0.000007,    #.000007, 
  c_start = 15,
  c_end = 33)

time = seq(1, nrow(cal_data),1 )

# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

#Add derived variables
#Add derived variables
out_df_check_hm <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.0065) 

highmountain_cal = ggplot(data = out_df_check_hm ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data, aes(x = monthly_time, y = bz_cases, col = "Observed"),
             lwd = 1.2) +
  # geom_line(data = cal_data_hill, aes(x = monthly_time, y = FOI_sqrt*6000, col = "Observed"), lwd = 2) +
  ylab("Cases") +
  theme_minimal() +
#  xlab("Time") +
  xlim(c(2019.69, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("High Mountain") +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" =  cols[2])) + # #2E7C8A"
 # scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#314782"), name = "") + # "#2C1E4A"
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
  #  axis.title.x    = element_text(size = 14),   # x-axis title
    axis.title.x    = element_blank(),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  theme( panel.grid = element_blank() ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
highmountain_cal 


# Combined zone SEIRS output
fitted_bz_combined <- rbind(
  out_df_check_t  %>% mutate(bz = "Tarai"),
  out_df_check_h  %>% mutate(bz = "Hill"),
  out_df_check_hm %>% mutate(bz = "High Mountain"),
  out_df_check_mm %>% mutate(bz = "Middle Mountain"),
  out_df_check_s  %>% mutate(bz = "Siwalik"))
head(fitted_bz_combined)

# ============================================================
# FIG 2 COMPONENTS
# ============================================================

# Strip axes helper
strip_axes <- function(p, keep_x = FALSE, keep_y = FALSE) {
  p + theme(
    axis.title.x = if (keep_x) element_text(size = 20) else element_blank(),
    axis.title.y = if (keep_y) element_text(size = 20) else element_blank())
}
remove_legend <- function(p) p + theme(legend.position = "none")

shared_legend <- get_legend(highmountain_cal + theme(legend.position = "bottom"))

tarai_cal          <- remove_legend(tarai_cal)
siwalik_cal        <- remove_legend(siwalik_cal)
hill_cal           <- remove_legend(hill_cal)
middlemountain_cal <- remove_legend(middlemountain_cal)
highmountain_cal   <- remove_legend(highmountain_cal)

grid <- plot_grid(tarai_cal, siwalik_cal, hill_cal,
                  middlemountain_cal, highmountain_cal, ncol = 3)
grid

final_plot_fig2a <- plot_grid(grid, shared_legend, ncol = 1, rel_heights = c(1, 0.1))

fig2a <- final_plot_fig2a +
  labs(tag = "\n    A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2a 

# Panel B — temperature by bioclimatic zone
bio_temp <- ggplot(data = weighted_temp_bx %>% filter(monthly_time > 2019)) +
  geom_line(aes(x = monthly_time, y = bc_temp_weighted - 273.15, col = Bioclimatic_Zone), lwd = 1.5) +
  theme_minimal() +
 # scale_color_viridis_d(option = "G", name = "Bioclimatic Zone", begin = .4) +
  scale_color_manual(
    values = setNames(rev(cols[2:6]), zone_levels[1:5]),
    name = "Bioclimatic Zone") +
  ylab("Temperature (Celcius)") + xlab("Time (months)") +
  theme(plot.title=element_text(size=16,face="bold"), plot.subtitle=element_text(size=16),
        axis.title.x=element_text(size=16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        plot.tag=element_text(face="bold",size=20), legend.position="bottom",
        legend.text=element_text(size=10), legend.title=element_text(size=12))

fig2b <- bio_temp +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2b

head(weighted_temp_bx)
ggplot(data = weighted_temp_bx %>% filter(monthly_time > 2010) %>%
         mutate(year = as.integer(monthly_time)) %>%
         group_by(Bioclimatic_Zone, year) %>%
         mutate(mean_pattern = mean(bc_temp_weighted))) +
  geom_line(aes(x = monthly_time, y =  mean_pattern - 273.15, col = Bioclimatic_Zone), lwd = 2) +
  theme_minimal() +
  scale_color_viridis_d(option = "G", name = "Bioclimatic Zone", begin = .4) +
  ylab("Temperature (Celcius)") + xlab("Time in months") +
  theme(plot.title=element_text(size=16,face="bold"), plot.subtitle=element_text(size=16),
        axis.title.x=element_text(size=16), axis.title.y=element_text(size=16),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        plot.tag=element_text(face="bold",size=20), legend.position="bottom",
        legend.text=element_text(size=10), legend.title=element_text(size=12))


################ Figure 2C 

###########################################
## redo at bz level 
bz_immunity_ts <- fitted_bz_combined %>%
  mutate(fraction_immune = R/N, immunity_gap = 1 - fraction_immune,
         month = ((time - 1) %% 12) + 1,
         year      = floor((time - 1) / 12) + 2019) %>%
  filter(year %in% c(2022, 2023, 2024)) %>%
 # filter(month %in% c(6, 7, 8, 9, 10, 11)) %>%
  mutate(monthly_time  = month/12 + year) %>%
  mutate(Bioclimatic_Zone = bz)
head(bz_immunity_ts)

phase_dataset_bz = left_join(bz_immunity_ts, bioclimatic_cases, by = 
                               c("Bioclimatic_Zone", "monthly_time")) %>%
 # filter(monthly_time > 2022.5, month %in% c(6,7,8,9,10,11)) %>%
  filter(monthly_time > 2022.25) %>%
  distinct() %>%
  mutate(incidence = bz_cases/(bz_denom/100000)) %>%
  mutate(log_incidence = log(incidence + .0001)) %>%
  group_by(Bioclimatic_Zone) %>%
  mutate(immunity_gap = lead(immunity_gap, 1)) %>%
  ungroup() %>%
  drop_na()
head(phase_dataset_bz)
summary(phase_dataset_bz$bz_temp)

ggplot(data = phase_dataset_bz, aes(x = monthly_time, y = immunity_gap, color = Bioclimatic_Zone)) +
  geom_line()+
  geom_point(aes(x = monthly_time, y = I/N, col = Bioclimatic_Zone),  col = "black")
  

ggplot(data = phase_dataset_bz) +
  geom_point(aes(x = bz_temp, y = log_incidence, col = Bioclimatic_Zone))
ggplot(data = phase_dataset_bz) +
  geom_point(aes(x = immunity_gap, y = log_incidence, col = Bioclimatic_Zone))
ggplot(data = phase_dataset_bz) +
  geom_point(aes(x = bz_temp, y = immunity_gap, col = log_incidence), cex = 14)


gam_phase <- mgcv::gam(log_incidence  ~ te(bz_temp, immunity_gap),
                       data = phase_dataset_bz, method = "REML")
summary(gam_phase)


#temp_seq   <- seq(min(phase_dataset_bz$bz_temp), max(phase_dataset_bz$bz_temp), length.out=40)
#temp_seq   <- seq(5, max(phase_dataset_bz$bz_temp), length.out=80)
#imm_seq    <- seq(min(phase_dataset_bz$immunity_gap), max(phase_dataset_bz$immunity_gap), length.out=40)
#temp_rng <- quantile(phase_diagram_dat$mean_temp, probs = c(0.05, 0.95), na.rm = TRUE)
#imm_rng  <- quantile(phase_diagram_dat$mean_immunity_gap, probs = c(0.05, 0.95), na.rm = TRUE)

temp_seq <- seq(temp_rng[1], temp_rng[2], length.out = 80)
imm_seq  <- seq(imm_rng[1], imm_rng[2], length.out = 80)

phase_grid <- expand.grid(bz_temp = temp_seq, immunity_gap = imm_seq)
phase_grid$log_inc_hat <- predict(gam_phase, newdata = phase_grid)


### only predict supported areas 
phase_grid$log_inc_hat <- predict(gam_phase, newdata = phase_grid)


#### only where there is data 

# Build the full rectangular candidate grid
temp_seq <- seq(temp_rng[1], temp_rng[2], length.out = 200)
imm_seq  <- seq(imm_rng[1], imm_rng[2], length.out = 200)
phase_grid <- expand.grid(bz_temp = temp_seq, immunity_gap = imm_seq)

# Compute the convex hull of the observed data
hull_idx <- chull(phase_dataset_bz$bz_temp, phase_dataset_bz$immunity_gap)
hull_pts <- phase_dataset_bz[hull_idx, c("bz_temp", "immunity_gap")]

# Keep ONLY grid cells inside the hull -- this makes the grid non-rectangular
in_hull <- point.in.polygon(
  phase_grid$bz_temp, phase_grid$immunity_gap,
  hull_pts$bz_temp, hull_pts$immunity_gap)
phase_grid <- phase_grid[in_hull > 0, ]   # filter rows, don't just NA them

# Now predict only over the supported (non-rectangular) grid
phase_grid$log_inc_hat <- predict(gam_phase, newdata = phase_grid)



# try 
# Predict with standard errors
pred <- predict(gam_phase, newdata = phase_grid, se.fit = TRUE)
phase_grid$log_inc_hat <- pred$fit
phase_grid$se          <- pred$se.fit

# Blank cells where SE exceeds a threshold (e.g. the 90th percentile of SE
# at the observed data points, so anything more uncertain than your data is dropped)
obs_pred <- predict(gam_phase, newdata = phase_dataset_bz, se.fit = TRUE)
se_cutoff <- quantile(obs_pred$se.fit, .95)

phase_grid <- phase_grid[phase_grid$se <= se_cutoff, ]



# mena values 
head(phase_dataset_bz)
mean_vals_phase <- phase_dataset_bz%>%
  group_by(Bioclimatic_Zone) %>%
  mutate(mean_imm = mean(immunity_gap), mean_temperature = mean(bz_temp), mean_inc = mean(incidence)) %>%
  ungroup() %>%
  distinct(mean_imm, mean_temperature, Bioclimatic_Zone, mean_inc) %>%
  mutate(Bioclimatic_Zone = factor(Bioclimatic_Zone, levels = c("High Mountain",
        "Middle Mountain", "Hill", "Siwalik", "Tarai")))

p_phase1 <- ggplot() +
  geom_contour_filled(data = phase_grid,
                      aes(x=bz_temp, y=immunity_gap, z=log_inc_hat/max(log_inc_hat)),
                      alpha=0.65, bins=20) +
  scale_fill_viridis_d(option="B", name="Incidence", begin=0.1, end=0.9, alpha = .7) +
  labs(x="Mean temperature (°C)", y="Susceptible fraction") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(size=16,face="bold"),
        plot.subtitle=element_text(size=16),
        panel.grid = element_blank(),
        axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
        axis.text.x=element_text(size=13), axis.text.y=element_text(size=13),
        plot.tag=element_text(face="bold",size=20), legend.position="bottom",
        legend.text=element_text(size=14), legend.title=element_text(size=20)) 
 # geom_point(data=mean_vals_phase, aes(x=mean_temperature, y=mean_imm),
 #            col="gray30", size=3.5, shape=17) +
 # geom_point(data=mean_vals_phase,
#aes(x=mean_temperature, y=mean_imm, color=Bioclimatic_Zone), 
   #       shape=17, size = 4) + 
  #scale_color_viridis_d(option = "G", name = "Bioclimatic Zone", begin = .4) +
 # geom_text(data=mean_vals_phase, aes(x=mean_temperature, y=mean_imm, label=Bioclimatic_Zone),
    #        color="white", vjust=-1, size=3.0)
p_phase1

p_phase2 <- ggplot() +
  geom_raster(data=phase_grid,
              aes(x=bz_temp, y= immunity_gap, fill=log_inc_hat/max(log_inc_hat)),
              interpolate=FALSE, alpha=0.65) +
  geom_contour(data=phase_grid,
               aes(x=bz_temp, y= immunity_gap, z=log_inc_hat/max(log_inc_hat)),
               colour="white", linewidth=0.3, alpha=0.2, bins=20) +
  scale_fill_viridis_c(option="B", name="Incidence (normalized)", begin=0.1, end=.9,
                       limits=c(0,1), breaks=seq(0,1,.1), alpha = .65,
                       guide=guide_colorbar(barwidth=15, barheight=0.8, ticks=TRUE,
                                            title.position="top", title.hjust=0.5)) +
  labs(x="Mean temperature (°C)", y="Susceptible fraction") +
  theme_bw(base_size=13) +
  theme(plot.title=element_text(size=20,face="bold"), plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        plot.tag=element_text(face="bold",size=20), legend.position="bottom",
        legend.text=element_text(size=10), legend.title=element_text(size=12)) +
  theme(legend.key.size = unit(0.001, "cm"))
p_phase2

legend_p2 <- cowplot::get_plot_component(
  p_phase2 + theme(legend.position = "bottom"),
  "guide-box-bottom")
p1_no_legend <- p_phase1 + theme(legend.position="none")

final_phase <- plot_grid(plot_grid(p1_no_legend, nrow=1), legend_p2,
                         ncol=1, rel_heights=c(.45, 0.1))
final_phase

fig2c <- final_phase +
  labs(tag = "C      ") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2c




# Compile Fig 2 
fig2bc <- plot_grid(fig2b, fig2c, rel_widths = c(.45, .3))

fig2_final2026 <- plot_grid(fig2a, fig2bc, ncol = 1, rel_heights = c(1.5, 1.2))
fig2_final2026



####################################################################################

# Mobility data
#activity_space  <- read.csv("activity_space_distributions_20250602_l_to_o.csv")

#nepal_activity_space_20250602 = activity_space %>%
#  filter(country == "NP")
#write.csv(nepal_activity_space_20250602, "nepal_activity_space_20250602.csv")

#activity_space2 <- read.csv("activity_space_distributions_20250915_l_to_o.csv")
#nepal_activity_space_20250915 = activity_space2 %>%
#  filter(country == "NP")
#write.csv(nepal_activity_space_20250915, "nepal_activity_space_20250915.csv")

#activity_space2 = NA

nepal <- read.csv("nepal_activity_space_20250602.csv") %>%
  filter(country == "NP", day_or_night == "daytime") %>%
  drop_na(visit_longitude, visit_latitude)

#nepal <- read.csv("nepal_activity_space_20250915.csv") %>%
#  filter(country == "NP", day_or_night == "daytime") %>%
#  drop_na(visit_longitude, visit_latitude)

nepal$home_population <- terra::extract(pop_raster,
                                        vect(cbind(nepal$home_longitude, nepal$home_latitude), crs = "EPSG:4326"))[, 2]

nepal_sf <- st_as_sf(nepal, coords = c("home_longitude","home_latitude"),
                     crs = 4326, remove = FALSE)
nepal_with_districts <- st_join(nepal_sf, nepal_districts_sf, join = st_within, left = TRUE)
nepal <- as.data.frame(nepal_with_districts) %>%
  mutate(home_district_location = location) %>%
  dplyr::select(-location, -geometry)

nepal_sf2 <- st_as_sf(nepal, coords = c("visit_longitude","visit_latitude"),
                      crs = 4326, remove = FALSE)
nepal_with_districts2 <- st_join(nepal_sf2, nepal_districts_sf, join = st_within, left = TRUE)
nepal_visitor <- as.data.frame(nepal_with_districts2) %>%
  mutate(visit_district_location = location) %>%
  dplyr::select(-location, -geometry)

mobility_matrix <- nepal_visitor %>%
  ungroup() %>%
  group_by(visit_district_location, home_district_location) %>%
  mutate(pop_flow = sum(home_population, na.rm = TRUE),
         pop_flow = round(pop_flow, digits = 0)) %>%
  distinct(visit_district_location, home_district_location, pop_flow) %>%
  drop_na() %>%
  ungroup() %>%
  dplyr::select(visit_district_location, home_district_location, pop_flow)

mob_df <- as.data.frame(mob_matrix) %>%
  rownames_to_column("home_district") %>%
  pivot_longer(
    -home_district,
    names_to = "visit_district",
    values_to = "pop_flow") %>%
  mutate(log_pop_flow = log10(pop_flow)) %>%
  mutate(log_pop_flow = replace(log_pop_flow, log_pop_flow <1, 0))
head(mob_df)

ggplot(mob_df , aes(x = visit_district,
                   y = home_district,
                   fill = log_pop_flow)) +
  geom_tile() +
  scale_fill_viridis_c() +   # optional if flows are highly skewed
  coord_equal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text = element_text(size = 5),
    panel.grid = element_blank() ) +
  labs( x = "Destination district",
    y = "Origin district",
    fill = "Log population flow" )

# check for asymmetry
#asymmetry <- mob_matrix - t(mob_matrix)
#summary(as.vector(asymmetry[upper.tri(asymmetry)]))
# there is aysmmetry here 

# old code for symmetric 
mob_matrix_sym <- (mob_matrix + t(mob_matrix)) / 2

mobility_network <- graph_from_adjacency_matrix(as.matrix(mob_matrix_sym),
                                                mode = "undirected", weighted = TRUE, diag = FALSE)

mobility_connectivity <- data.frame(
  location              = rownames(mob_matrix_sym),
  total_strength        = strength(mobility_network),
  degree                = degree(mobility_network),
  avg_flow              = strength(mobility_network) / degree(mobility_network),
  betweenness           = betweenness(mobility_network, weights = 1/E(mobility_network)$weight),
  eigenvector_centrality = eigen_centrality(mobility_network, weights = E(mobility_network)$weight)$vector,
  closeness             = closeness(mobility_network, weights = 1/E(mobility_network)$weight))

# asymmetric 
#mobility_network <- graph_from_adjacency_matrix(
#  as.matrix(mob_matrix),
#  mode     = "directed",
#  weighted = TRUE,
#  diag     = FALSE)

mobility_connectivity <- data.frame(
  location               = rownames(mob_matrix),
  in_strength            = strength(mobility_network, mode = "in"),
  out_strength           = strength(mobility_network, mode = "out"),
  net_flow               = strength(mobility_network, mode = "in") -
    strength(mobility_network, mode = "out"),
  in_degree              = degree(mobility_network, mode = "in"),
  out_degree             = degree(mobility_network, mode = "out"),
  avg_in_flow            = strength(mobility_network, mode = "in") /
    degree(mobility_network, mode = "in"),
  avg_out_flow           = strength(mobility_network, mode = "out") /
    degree(mobility_network, mode = "out"),
  betweenness            = betweenness(mobility_network,
                                       weights  = 1/E(mobility_network)$weight,
                                       directed = TRUE),
  eigenvector_centrality = eigen_centrality(mobility_network,
                                            directed = TRUE,
                                            weights  = E(mobility_network)$weight)$vector,
  closeness_in           = closeness(mobility_network, mode = "in",
                                     weights = 1/E(mobility_network)$weight),
  closeness_out          = closeness(mobility_network, mode = "out",
                                     weights = 1/E(mobility_network)$weight))
head(mobility_connectivity)


# Community structure algoritm
incidence_structure <- dengue_dat_geo_pop %>%
  group_by(location) %>%
  mutate(max_cases = max(cases), normalized = cases/max_cases) %>%
  distinct(location, monthly_time, month_num, normalized) %>%
  ungroup() %>%
  filter(location != "specified")

incidence_wide <- incidence_structure %>%
  pivot_wider(names_from = monthly_time, values_from = normalized, id_cols = location) %>%
  column_to_rownames("location")

correlation_matrix <- cor(t(incidence_wide), use = "pairwise.complete.obs")
similarity_matrix  <- 1 / (1 + (1 - correlation_matrix))
diag(similarity_matrix) <- 0
adjacency_matrix   <- similarity_matrix
adjacency_matrix[adjacency_matrix < 0.65] <- 0

network     <- graph_from_adjacency_matrix(adjacency_matrix, mode="undirected",
                                           weighted=TRUE, diag=FALSE)
communities <- cluster_louvain(network)

community_membership <- data.frame(location  = V(network)$name,
                                   community = membership(communities))

calculate_within_module_z <- function(network, communities) {
  membership_vec <- membership(communities)
  n_nodes        <- vcount(network)
  z_scores       <- numeric(n_nodes)
  for (i in 1:n_nodes) {
    node_comm    <- membership_vec[i]
    comm_nodes   <- which(membership_vec == node_comm)
    within_degree <- sum(E(network)[i %--% comm_nodes]$weight, na.rm = TRUE)
    comm_degrees  <- sapply(comm_nodes, function(j)
      sum(E(network)[j %--% comm_nodes]$weight, na.rm = TRUE))
    mean_degree <- mean(comm_degrees); sd_degree <- sd(comm_degrees)
    if (sd_degree > 0) z_scores[i] <- (within_degree - mean_degree) / sd_degree
  }
  z_scores
}
community_membership$within_module_z <- calculate_within_module_z(network, communities)
head(community_membership)

node_modularity_contribution <- function(network, communities) {
  membership_vec <- membership(communities)
  n_nodes        <- vcount(network)
  total_weight   <- sum(E(network)$weight)
  contributions  <- numeric(n_nodes)
  for (i in 1:n_nodes) {
    node_comm    <- membership_vec[i]
    comm_nodes   <- which(membership_vec == node_comm)
    within_weight <- sum(E(network)[i %--% comm_nodes]$weight, na.rm = TRUE)
    node_strength <- strength(network, i)
    expected      <- sum(sapply(comm_nodes, function(j) strength(network, j))) *
      node_strength / (2 * total_weight)
    contributions[i] <- (within_weight - expected) / total_weight
  }
  contributions
}
community_membership$modularity_contrib <- node_modularity_contribution(network, communities)

locations_membership <- left_join(community_membership, nepal_districts_sf, by = "location") %>%
  st_as_sf()
locations_membership$community <- as.factor(locations_membership$community)
head(locations_membership)

hubs        <- locations_membership %>% filter(within_module_z > .80)
hubs_points <- st_centroid(hubs)

hubs        <- locations_membership %>% filter(modularity_contrib > 5.816400e-03)
hubs_points <- st_centroid(hubs)

fig3a_loc <- ggplot(data = locations_membership) +
  geom_sf(aes(fill = community), color = "black", linewidth = 0.2) +
  theme_minimal() +
  labs(fill = "Community") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_viridis_d(option = "B", name = "Community", begin = 0.40, end = .84) +
  theme(plot.title=element_text(size=20), plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        plot.tag=element_text(size=20), legend.position="right",
        legend.text=element_text(size=16), legend.title=element_text(size=18)) +
  geom_sf(data = hubs_points, shape = 8, size = 3, color = "black") +
  geom_sf(data = hubs_points, aes(Shape = "Hub"), size = 3, color = "black")

fig3a <- fig3a_loc +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3a


### check correlation between within_module_z and spatial connectivity 
cor_spa_z = left_join(locations_membership, mobility_connectivity, by = "location",   )
head(cor_spa_z )
cor.test(cor_spa_z$modularity_contrib, cor_spa_z$eigenvector_centrality, method = "spearman")



# plot correlation for supp
eig_nc = ggplot(data = cor_spa_z) +
  geom_point(aes(x =modularity_contrib, y = eigenvector_centrality )) +
  geom_smooth(aes(x =modularity_contrib, y = eigenvector_centrality), method = "lm") +
  theme_bw() + ylab('Eigenvector centrality') + xlab("Modularity Contribution")  +
  theme(plot.title=element_text(size=20), plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        plot.tag=element_text(size=20), legend.position="right",
        legend.text=element_text(size=16), legend.title=element_text(size=18)) 
eig_nc

bw_nc = ggplot(data = cor_spa_z) +
  geom_point(aes(x =modularity_contrib, y = log(betweenness) )) +
  geom_smooth(aes(x =modularity_contrib, y = log(betweenness)), method = "lm") +
  theme_bw() + ylab('Betweeness') + xlab("Modularity Contribution")  +
  theme(plot.title=element_text(size=20), plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        plot.tag=element_text(size=20), legend.position="right",
        legend.text=element_text(size=16), legend.title=element_text(size=18)) 
bw_nc
cor.test(cor_spa_z$modularity_contrib, cor_spa_z$betweenness, method = "spearman")




# Cross-zone connectivity box plot (panel B of fig3)
cross_zone_flows <- mobility_matrix %>%
  left_join(bz_lookup_clean %>% rename(home_bz  = Bioclimatic_Zone),
            by = c("home_district_location"  = "location")) %>%
  left_join(bz_lookup_clean %>% rename(visit_bz = Bioclimatic_Zone),
            by = c("visit_district_location" = "location")) %>%
  drop_na(home_bz, visit_bz) %>%
  mutate(cross_zone = home_bz != visit_bz)

district_cross_zone <- cross_zone_flows %>%
  filter(pop_flow > 0) %>%
  group_by(home_district_location, home_bz) %>%
  summarise(n_connections_total   = n(),
            n_connections_cross   = sum(cross_zone),
            n_connections_within  = sum(!cross_zone),
            has_cross_zone        = any(cross_zone),
            pct_connections_cross = 100 * mean(cross_zone),
            total_flow            = sum(pop_flow),
            cross_zone_flow       = sum(pop_flow[cross_zone]),
            pct_flow_cross        = 100 * cross_zone_flow / total_flow,
            .groups = "drop") %>%
  rename(location = home_district_location) %>%
  group_by(home_bz) %>%
  mutate(median_bz_connect = median(pct_connections_cross)) %>%
  ungroup() %>%
  #mutate(home_bz = factor(home_bz, levels = c("High Mountain","Middle Mountain",
    #                                          "Hill","Siwalik","Tarai")))%>%
  mutate(home_bz = factor(home_bz, levels = c("Tarai", "Siwalik", "Hill", 
                                              "Middle Mountain", "High Mountain" )))


p_cross_bz <- district_cross_zone %>%
  left_join(bz_lookup_clean, by = "location") %>%
  filter(Bioclimatic_Zone != "National") %>%
  ggplot(aes(x = Bioclimatic_Zone, y = pct_connections_cross, fill = Bioclimatic_Zone)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 21, width = 0.5) +
  geom_jitter(aes(colour = Bioclimatic_Zone), width = 0.15, size = 1.8, alpha = 0.7) +
  scale_color_manual(
    values = setNames(rev(cols[2:6]), zone_levels[1:5]),
    name = "Bioclimatic Zone") +
  scale_fill_manual(
    values = setNames(rev(cols[2:6]), zone_levels[1:5]),
    name = "Bioclimatic Zone") +
 scale_x_discrete(limits = zone_levels[1:5]) +
 # scale_fill_viridis_d(option="G", begin=0.4, end=0.9, guide="none") +
 # scale_colour_viridis_d(option="G", begin=0.4, end=0.9, guide="none") +
  labs(x = "Bioclimatic zone", y = "Cross-zone connections (%)") +
  theme_minimal() +
  ylim(c(30, 100)) +
  theme(plot.title=element_text(size=20,face="bold"), plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
        axis.text.x=element_text(size=14,angle=30), axis.text.y=element_text(size=16),
        plot.tag=element_text(face="bold",size=20), legend.position="bottom",
        legend.text=element_text(size=13), legend.title=element_text(size=14))

fig_3b <- p_cross_bz +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig_3b 



#####################################################################################
# Comparison of two models for Figure 3C, model with spatial vs no spatial data 

#####################################################################################
# Figure 3C — spatial vs non-spatial model comparison
# Model 1: each district mirrors the NORMALIZED (0-1) fitted pattern of its
#          assigned bioclimatic zone (from bz_immunity_ts fitted SEIRS output).
# Model 2: each district is a weighted blend of its own zone pattern and a
#          mobility-weighted average of neighbors' zone patterns; weight alpha fit.
# Residuals are computed against each district's observed normalized time series.
#####################################################################################

# ── Bioclimatic zone lookup (district -> zone) ───────────────────────────────
bz_lookup_clean <- map_district_bz %>%
  st_drop_geometry() %>%
  distinct(location, Bioclimatic_Zone)

# ── Zone-level fitted pattern, normalized 0-1 within each zone ────────────────
# Uses the already-fitted SEIRS predicted_cases from fitted_bz_combined (via
# bz_immunity_ts), NOT a re-run of the mechanistic model.
zone_pattern <- fitted_bz_combined %>%
  mutate(month = ((time - 1) %% 12) + 1,
         year  = floor((time - 1) / 12) + 2019,
         monthly_time = round(month/12 + year, 3),
         Bioclimatic_Zone = bz) %>%
  group_by(Bioclimatic_Zone) %>%
  mutate(zone_pattern_norm = predicted_cases / max(predicted_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(Bioclimatic_Zone, monthly_time, zone_pattern_norm)

# ── Observed district time series, normalized 0-1 within each district ────────
obs_district_norm <- dengue_timeseries %>%
  filter(location != "specified") %>%
  mutate(monthly_time = round(as.numeric(monthly_time), 3)) %>%
  group_by(location) %>%
  mutate(obs_norm = cases / max(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(location, monthly_time, cases, obs_norm) %>%
  left_join(bz_lookup_clean, by = "location")

# ── MODEL 1: district pattern = its zone's normalized pattern ─────────────────
model1_long <- obs_district_norm %>%
  left_join(zone_pattern, by = c("Bioclimatic_Zone", "monthly_time")) %>%
  drop_na(obs_norm, zone_pattern_norm) %>%
  rename(m1_pred_norm = zone_pattern_norm)

# ── Build wide matrices for the spatial (Model 2) step ────────────────────────
# Districts/times common to the model
districts_in_model <- sort(unique(model1_long$location))
shared_times       <- sort(unique(model1_long$monthly_time))

# Model 1 predicted (zone pattern) wide: districts x time
m1_wide <- model1_long %>%
  dplyr::select(location, monthly_time, m1_pred_norm) %>%
  pivot_wider(names_from = monthly_time, values_from = m1_pred_norm,
              id_cols = location) %>%
  arrange(location) %>%
  column_to_rownames("location")

# Observed normalized wide: districts x time
obs_wide_norm <- model1_long %>%
  dplyr::select(location, monthly_time, obs_norm) %>%
  pivot_wider(names_from = monthly_time, values_from = obs_norm,
              id_cols = location) %>%
  arrange(location) %>%
  column_to_rownames("location")

# ── Mobility weight matrix W, restricted to districts in the model ────────────
districts_in_model <- intersect(districts_in_model, rownames(mob_matrix_sym))
mob_sub  <- mob_matrix_sym[districts_in_model, districts_in_model]
row_sums <- rowSums(mob_sub); row_sums[row_sums == 0] <- 1
W        <- mob_sub / row_sums   # row-normalized neighbor weights

# Align all matrices to W's district ordering and shared time columns
m1_wide       <- m1_wide[rownames(W), , drop = FALSE]
obs_wide_norm <- obs_wide_norm[rownames(W), , drop = FALSE]
shared_cols   <- intersect(colnames(m1_wide), colnames(obs_wide_norm))
m1_wide       <- m1_wide[, shared_cols, drop = FALSE]
obs_wide_norm <- obs_wide_norm[, shared_cols, drop = FALSE]

# ── MODEL 2: blend own zone pattern with mobility-weighted neighbor pattern ───
# spatial_pred = (1 - alpha) * own_zone_pattern + alpha * neighbor_zone_pattern
# alpha fit by minimizing RMSE against observed normalized cases.
alpha_grid <- seq(0, 1, by = 0.02)

rmse_fn <- function(alpha, W, m1_wide, obs_wide_norm) {
  neighbor_pred <- W %*% as.matrix(m1_wide)
  spatial_pred  <- (1 - alpha) * as.matrix(m1_wide) + alpha * neighbor_pred
  sqrt(mean((as.matrix(obs_wide_norm) - spatial_pred)^2, na.rm = TRUE))
}

rmse_grid <- sapply(alpha_grid, rmse_fn, W = W,
                    m1_wide = m1_wide, obs_wide_norm = obs_wide_norm)
alpha_opt <- alpha_grid[which.min(rmse_grid)]
print(alpha_opt)

neighbor_pred_opt <- W %*% as.matrix(m1_wide)
spatial_pred_opt  <- (1 - alpha_opt) * as.matrix(m1_wide) + alpha_opt * neighbor_pred_opt

# ── Model 2 predictions back to long form ─────────────────────────────────────
model2_long <- as.data.frame(spatial_pred_opt) %>%
  mutate(location = rownames(spatial_pred_opt)) %>%
  pivot_longer(-location, names_to = "monthly_time", values_to = "m2_pred_norm") %>%
  mutate(monthly_time = round(as.numeric(monthly_time), 3))

# ── Combine both models with observed, compute per-district residual sums ─────
baseline_observed <- model1_long %>%
  dplyr::select(location, monthly_time, Bioclimatic_Zone, obs_norm, m1_pred_norm) %>%
  left_join(model2_long, by = c("location", "monthly_time")) %>%
  drop_na(obs_norm, m1_pred_norm, m2_pred_norm) %>%
  mutate(residual_1 = m1_pred_norm - obs_norm,   # non-spatial (zone-only) model
         residual_2 = m2_pred_norm - obs_norm) %>% # spatial model
  group_by(location, Bioclimatic_Zone) %>%
  summarise(sum_reds1 = sum(abs(residual_1)),
            sum_reds2 = sum(abs(residual_2)),
            .groups = "drop")
head(baseline_observed)

# summary stats 
# ── Summary statistics: improvement from Model 1 to Model 2 ───────────────────

# Percent of districts that improved (lower summed residuals under the spatial model)
districts_improved <- baseline_observed %>%
  mutate(improved = sum_reds2 < sum_reds1)

pct_improved <- 100 * mean(districts_improved$improved)
cat("Percent of districts improved by spatial model:", round(pct_improved, 1), "%\n")
cat("(", sum(districts_improved$improved), "of", nrow(districts_improved), "districts )\n")

# ── Change in RMSE (Model 1 -> Model 2), on the normalized scale ──────────────
# Computed across all district-month observations, consistent with how alpha was fit
rmse_m1 <- rmse_fn(alpha = 0, W = W, m1_wide = m1_wide, obs_wide_norm = obs_wide_norm)
rmse_m2 <- rmse_fn(alpha = alpha_opt, W = W, m1_wide = m1_wide, obs_wide_norm = obs_wide_norm)

cat("RMSE Model 1 (non-spatial, alpha = 0):", round(rmse_m1, 4), "\n")
cat("RMSE Model 2 (spatial, alpha =", alpha_opt, "):", round(rmse_m2, 4), "\n")
cat("Absolute RMSE reduction:", round(rmse_m1 - rmse_m2, 4), "\n")
cat("Percent RMSE reduction:", round(100 * (rmse_m1 - rmse_m2) / rmse_m1, 2), "%\n")


# Test whether a saturating (log) transform of flows improves fit
mob_sub_log <- log1p(mob_matrix_sym[districts_in_model, districts_in_model])
row_sums_log <- rowSums(mob_sub_log); row_sums_log[row_sums_log == 0] <- 1
W_log <- mob_sub_log / row_sums_log

rmse_grid_log <- sapply(alpha_grid, rmse_fn, W = W_log,
                        m1_wide = m1_wide, obs_wide_norm = obs_wide_norm)
alpha_opt_log <- alpha_grid[which.min(rmse_grid_log)]
min(rmse_grid_log) 
(0.152558 - 0.1577 )/0.1577


# ── Plot: non-spatial error (x) vs spatial improvement (y) ────────────────────
model_compare_collab <- ggplot(
  data = baseline_observed %>%
    mutate(Bioclimatic_Zone = factor(Bioclimatic_Zone,
                                     levels = c("High Mountain","Middle Mountain",
                                                "Hill","Siwalik","Tarai")))) +
  theme_minimal() +
  geom_point(aes(x = sum_reds1, y = sum_reds1 - sum_reds2, col = Bioclimatic_Zone),
             size = 3, alpha = .9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray27") +
  ylab("Spatial model improvement") + xlab("Non-spatial model errors") +
  #scale_color_viridis_d(option = "G", name = "Bioclimatic Zone", begin = .2, end = .95) +
  scale_color_manual(values = setNames(rev(cols[2:6]), zone_levels[1:5]),
    name = "Bioclimatic Zone") +
  theme(strip.text=element_text(size=20), plot.title=element_text(size=20),
        plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
        axis.text.x=element_text(size=14), axis.text.y=element_text(size=14),
        plot.tag=element_text(face="bold",size=20), legend.position="none",
        legend.text=element_text(size=16), legend.title=element_text(size=18)) +
  ggtitle(" ")
model_compare_collab

fig3c_share <- model_compare_collab +
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3c_share


#### calculate percent of visits from a specific region 
# ── Decompose neighbor weighting: own-zone vs cross-zone contribution ─────────

# Make sure we have the zone for each district in W's row order
zone_vec <- bz_lookup_clean$Bioclimatic_Zone[match(rownames(W), bz_lookup_clean$location)]
names(zone_vec) <- rownames(W)

# For each district (row of W), split its neighbor weights into
# same-zone vs different-zone, based on the column district's zone
same_zone_frac  <- numeric(nrow(W))
cross_zone_frac <- numeric(nrow(W))

for (i in seq_len(nrow(W))) {
  row_w        <- W[i, ]
  col_zones    <- zone_vec[colnames(W)]
  own_zone     <- zone_vec[i]
  
  total_w      <- sum(row_w)                          # = 1 since W is row-normalized
  same_w       <- sum(row_w[col_zones == own_zone], na.rm = TRUE)
  cross_w      <- sum(row_w[col_zones != own_zone], na.rm = TRUE)
  
  same_zone_frac[i]  <- same_w  / total_w
  cross_zone_frac[i] <- cross_w / total_w
}

zone_decomp <- tibble(
  location        = rownames(W),
  Bioclimatic_Zone = zone_vec,
  same_zone_frac  = same_zone_frac,
  cross_zone_frac = cross_zone_frac)

# ── Overall: what fraction of the spatial (neighbor) weighting is cross-zone? ──
# Averaged across districts
mean_cross_zone <- mean(zone_decomp$cross_zone_frac, na.rm = TRUE)
cat("Mean fraction of neighbor weighting from OTHER zones:", round(100 * mean_cross_zone, 1), "%\n")
cat("Mean fraction from OWN zone:", round(100 * (1 - mean_cross_zone), 1), "%\n")

################################################################
# Compile fig 3 

fig3_bottom <- plot_grid(fig_3b, fig3c_share, rel_widths = c(.72, .45))

fig3_2026 <- plot_grid(fig3a, fig3_bottom, ncol = 1, rel_heights = c(.8, .55))
fig3_2026
# 1300 x 1100

################################################################

# ============================================================
# FIG 4 — intervention
# ============================================================

# District connectivity ranking (eigenvector centrality)
district_connectivity <- mobility_connectivity %>%
  dplyr::select(location, eigenvector_centrality) %>%
  arrange(desc(eigenvector_centrality)) %>%
  mutate(connectivity_rank = row_number(), connectivity_pct = connectivity_rank / n())

# NOTE: baseline_observed (from Fig 3C) is district-level SUMMARIES, not a
# monthly time series, so it has no monthly_time/cases columns. Build the
# ranked monthly dataset directly from obs_district_norm instead.
spatial_ranked <- obs_district_norm %>%
  filter(location %in% districts_in_model) %>%
  left_join(district_connectivity, by = "location") %>%
  filter(monthly_time > 2022) %>%
  mutate(year  = as.integer(monthly_time),
         month = round((monthly_time - year) * 12, digits = 1))

# Total observed cases per district (weighting factor)
case_total <- spatial_ranked %>%
  group_by(location) %>%
  summarise(total_cases_location = sum(cases, na.rm = TRUE), .groups = "drop")

# ── Strategies ────────────────────────────────────────────────────────────────
top_connected <- district_connectivity %>% arrange(connectivity_rank) %>%
  slice_head(n = 10) %>% pull(location)

top_largest <- pop_location %>%
  filter(location %in% districts_in_model) %>%
  distinct(location, census_2021) %>% arrange(desc(census_2021)) %>%
  slice_head(n = 10) %>% pull(location)

set.seed(123)   # reproducibility for the random strategies
n_random_sims <- 100
random_strategies <- setNames(
  lapply(seq_len(n_random_sims), function(i) sample(districts_in_model, 10)),
  paste0("Random_", seq_len(n_random_sims)))

district_connectivity2 <- mobility_connectivity %>%
  dplyr::select(location, eigenvector_centrality, betweenness) %>%
  arrange(desc(eigenvector_centrality)) %>%
  mutate(connectivity_rank = row_number(), connectivity_pct = connectivity_rank/n()) %>%
  arrange(desc(betweenness)) %>%
  mutate(betweenness_rank = row_number(), betweenness_pct = betweenness_rank/n()) %>%
  arrange(connectivity_rank)

top_betweenness <- district_connectivity2 %>% arrange(betweenness_rank) %>%
  slice_head(n = 10) %>% pull(location)

strategies <- c(
  list("Spatially connected\n(eigenvector)" = top_connected,
       "Spatially connected\n(betweenness)" = top_betweenness,
       "Most populous"                            = top_largest),
  random_strategies)

timing_windows  <- list(c(6,7,8), c(7,8,9), c(8,9,10), c(9,10,11))
alpha_opt       <- 0.8
reduction_pct   <- 0.30

# ── Baseline (no intervention) spatial prediction ────────────────────────────
baseline_wide <- spatial_ranked %>%
  distinct(location, monthly_time, obs_norm) %>%
  pivot_wider(names_from = monthly_time, values_from = obs_norm, id_cols = location) %>%
  arrange(location) %>% filter(location %in% districts_in_model) %>%
  column_to_rownames("location")
baseline_wide <- baseline_wide[rownames(W), , drop = FALSE]

neighborhood_baseline <- W %*% as.matrix(baseline_wide)
spatial_pred_baseline <- (1 - alpha_opt) * as.matrix(baseline_wide) + alpha_opt * neighborhood_baseline

baseline_scenario <- as.data.frame(spatial_pred_baseline) %>%
  mutate(location = rownames(spatial_pred_baseline)) %>%
  pivot_longer(-location, names_to = "monthly_time", values_to = "pred") %>%
  mutate(monthly_time = round(as.numeric(monthly_time), 3)) %>%
  left_join(case_total, by = "location") %>%
  mutate(pred_weighted = pred * total_cases_location)
max_sum_no_intervention <- sum(baseline_scenario$pred_weighted, na.rm = TRUE)

# ── Loop over strategies x timing windows ─────────────────────────────────────
all_results <- data.frame()
for (strat_name in names(strategies)) {
  target_locs <- strategies[[strat_name]]
  for (tw in timing_windows) {
    midpoint     <- tw[2]
    window_label <- paste0("months ", tw[1], "–", tw[3])
    
    scenario_data <- spatial_ranked %>%
      mutate(normalized_reduction = ifelse(month %in% tw & location %in% target_locs,
                                           obs_norm * reduction_pct, obs_norm))
    
    scenario_wide <- scenario_data %>%
      distinct(location, monthly_time, normalized_reduction) %>%
      pivot_wider(names_from = monthly_time, values_from = normalized_reduction, id_cols = location) %>%
      arrange(location) %>% filter(location %in% districts_in_model) %>%
      column_to_rownames("location")
    scenario_wide <- scenario_wide[rownames(W), , drop = FALSE]
    
    neighborhood_pred <- W %*% as.matrix(scenario_wide)
    spatial_pred      <- (1 - alpha_opt) * as.matrix(scenario_wide) + alpha_opt * neighborhood_pred
    
    intervention_scenario <- as.data.frame(spatial_pred) %>%
      mutate(location = rownames(spatial_pred)) %>%
      pivot_longer(-location, names_to = "monthly_time", values_to = "intervention_effect") %>%
      mutate(monthly_time = round(as.numeric(monthly_time), 3)) %>%
      left_join(case_total, by = "location") %>%
      mutate(intervention_effect_weighted = intervention_effect * total_cases_location)
    
    intervention_burden <- sum(intervention_scenario$intervention_effect_weighted, na.rm = TRUE)
    pct_reduction <- 100 * (max_sum_no_intervention - intervention_burden) / max_sum_no_intervention
    
    all_results <- rbind(all_results, data.frame(strategy = strat_name,
                                                 timing_window = window_label,
                                                 midpoint_month = midpoint,
                                                 intervention_burden = intervention_burden,
                                                 pct_reduction = pct_reduction))
  }
}

# ── Summarize random strategies ───────────────────────────────────────────────
random_summary <- all_results %>%
  filter(str_starts(strategy, "Random_")) %>%
  group_by(midpoint_month, timing_window) %>%
  summarise(random_median = median(pct_reduction),
            random_lo = quantile(pct_reduction, 0.25),
            random_hi = quantile(pct_reduction, 0.75),
            random_hi_ex = quantile(pct_reduction, 0.975),
            random_lo_ex = quantile(pct_reduction, 0.025), .groups = "drop")

fixed_results <- all_results %>% filter(!str_starts(strategy, "Random_"))

# ── Plot ──────────────────────────────────────────────────────────────────────

intervent_share <- ggplot() +
  geom_line(data = random_summary,
            aes(x = midpoint_month, y = random_median, colour = "Random"),
            linewidth = 2.1, linetype = "dashed") +
  geom_point(data = random_summary,
             aes(x = midpoint_month, y = random_median, colour = "Random"), size = 3.5) +
  geom_line(data = fixed_results %>% filter(strategy != "Spatially connected\n(betweenness)"),
            aes(x = midpoint_month, y = pct_reduction, colour = strategy, group = strategy),
            linewidth = 2.1) +
  geom_point(data = fixed_results %>% filter(strategy != "Spatially connected\n(betweenness)"),
             aes(x = midpoint_month, y = pct_reduction, colour = strategy), size = 3.5) +
  scale_x_continuous(breaks = c(7,8,9,10),
                     labels = c("Jun–Aug","Jul–Sep","Aug–Oct","Sep–Nov")) +
  scale_color_manual(name = "Strategy",
                     values = c("Most populous" = "gray35",
                                "Spatially connected\n(eigenvector)" = "tan1",
                                "Random" = "grey50")) +
  labs(x = "Intervention timing", y = "Reduction in dengue (%)", colour = "Strategy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 20, face = "bold"), plot.subtitle = element_text(size = 20),
        axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        plot.tag = element_text(face = "bold", size = 20), legend.position = "right",
        legend.text = element_text(size = 14), legend.title = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(floor(min(all_results$pct_reduction)),
                                  ceiling(max(all_results$pct_reduction)), by = 1))

intervent_share <- ggplot() +
  # Random strategy uncertainty ribbon (2.5th–97.5th percentile)
  geom_ribbon(data = random_summary,
              aes(x = midpoint_month, ymin = random_lo, ymax = random_hi),
              fill = "grey50", alpha = 0.40) +
  geom_ribbon(data = random_summary,
              aes(x = midpoint_month, ymin = random_lo_ex, ymax = random_hi_ex),
              fill = "grey90", alpha = 0.40) +
  geom_line(data = random_summary,
            aes(x = midpoint_month, y = random_median, colour = "Random"),
            linewidth = 2.1, linetype = "dashed") +
  geom_point(data = random_summary,
             aes(x = midpoint_month, y = random_median, colour = "Random"), size = 3.5) +
  geom_line(data = fixed_results %>% filter(strategy != "Spatially connected\n(betweenness)"),
            aes(x = midpoint_month, y = pct_reduction, colour = strategy, group = strategy),
            linewidth = 2.1) +
  geom_point(data = fixed_results %>% filter(strategy != "Spatially connected\n(betweenness)"),
             aes(x = midpoint_month, y = pct_reduction, colour = strategy), size = 3.5) +
  scale_x_continuous(breaks = c(7,8,9,10),
                     labels = c("Jun–Aug","Jul–Sep","Aug–Oct","Sep–Nov")) +
  scale_color_manual(name = "Strategy",
                     values = c("Most populous" = "gray35",
                                "Spatially connected\n(eigenvector)" = "tan1",
                                "Random" = "grey50")) +
  labs(x = "Intervention timing", y = "Reduction in dengue (%)", colour = "Strategy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 20, face = "bold"), plot.subtitle = element_text(size = 20),
        axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        plot.tag = element_text(face = "bold", size = 20), legend.position = "right",
        legend.text = element_text(size = 14), legend.title = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(floor(min(all_results$pct_reduction)),
                                  ceiling(max(all_results$pct_reduction)), by = 1))

finalshare <- intervent_share +
  labs(tag = "D") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
finalshare

finalshare <- intervent_share +
  labs(tag = "D") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
finalshare





# ============================================================
# FIG 4 — intervention strategies 
# ============================================================

# Panel A&B — strategy map + line plot

# Re-run strategies with betweenness excluded from fig_4a but included in map
strategies_fig4 <- c(
  list("Spatially connected\n(eigenvector)" = top_connected,
       "Spatially connected\n(betweenness)" = top_betweenness,
       "Most populous"                            = top_largest),
  random_strategies)

all_results_fig4 <- data.frame()
for (strat_name in names(strategies_fig4)) {
  target_locs <- strategies_fig4[[strat_name]]
  for (tw in timing_windows) {
    midpoint     <- tw[2]
    window_label <- paste0("months ", tw[1], "–", tw[3])
    scenario_data <- spatial_ranked %>%
      mutate(normalized_reduction = ifelse(month %in% tw & location %in% target_locs,
                                           obs_norm * reduction_pct, obs_norm))
    scenario_wide <- scenario_data %>%
      distinct(location, monthly_time, normalized_reduction) %>%
      pivot_wider(names_from=monthly_time, values_from=normalized_reduction, id_cols=location) %>%
      arrange(location) %>% filter(location %in% districts_in_model) %>%
      column_to_rownames("location")
    scenario_wide <- scenario_wide[rownames(W), , drop=FALSE]
    neighborhood_pred <- W %*% as.matrix(scenario_wide)
    spatial_pred      <- (1-alpha_opt)*as.matrix(scenario_wide) + alpha_opt*neighborhood_pred
    intervention_scenario <- as.data.frame(spatial_pred) %>%
      mutate(location = rownames(spatial_pred)) %>%
      pivot_longer(-location, names_to="monthly_time", values_to="intervention_effect") %>%
      mutate(monthly_time = round(as.numeric(monthly_time), 3)) %>%
      left_join(case_total, by="location") %>%
      mutate(intervention_effect_weighted = intervention_effect * total_cases_location)
    intervention_burden <- sum(intervention_scenario$intervention_effect_weighted)
    pct_reduction <- 100*(max_sum_no_intervention - intervention_burden)/max_sum_no_intervention
    all_results_fig4 <- rbind(all_results_fig4,
                              data.frame(strategy=strat_name, timing_window=window_label,
                                         midpoint_month=midpoint,
                                         intervention_burden=intervention_burden,
                                         pct_reduction=pct_reduction))
  }
}

random_summary_fig4 <- all_results_fig4 %>%
  filter(str_starts(strategy, "Random_")) %>%
  group_by(midpoint_month, timing_window) %>%
  summarise(random_median=median(pct_reduction),
            random_lo=quantile(pct_reduction,0.025),
            random_hi=quantile(pct_reduction,0.975),
            random_hi_ex = quantile(pct_reduction, 0.975),
            random_lo_ex = quantile(pct_reduction, 0.025), groups="drop")
fixed_results_fig4 <- all_results_fig4 %>% filter(!str_starts(strategy, "Random_"))


head(all_results_fig4)
random_vals <- all_results_fig4 %>%
  #filter(timing_window == "months 8-10") %>% 
  filter(str_starts(strategy, "Random_")) %>%
  filter(midpoint_month == 9) %>%
  pull(pct_reduction) 
head(random_vals)
ecdf_fun <- ecdf(random_vals)

ecdf_fun(8.6) # populous
ecdf_fun(10.2) # connected



intervent_fig4a <- ggplot() +
 # geom_ribbon(data=random_summary_fig4 ,
 #             aes(x=midpoint_month, ymin=random_lo, ymax=random_hi),
 #             fill="grey70", alpha=0.4) +
  geom_line(data=random_summary_fig4,
            aes(x=midpoint_month, y=random_median, colour="Random"),
            linewidth=2.1, linetype="dashed") +
  geom_ribbon(data = random_summary,
              aes(x = midpoint_month, ymin = random_lo, ymax = random_hi),
              fill = "grey50", alpha = 0.40) +
  geom_ribbon(data = random_summary,
              aes(x = midpoint_month, ymin = random_lo_ex, ymax = random_hi_ex),
              fill = "grey90", alpha = 0.40) +
  ggtitle("A") + 
  geom_point(data=random_summary_fig4,
             aes(x=midpoint_month, y=random_median, colour="Random"), size=3.5) +
  geom_line(data=fixed_results_fig4 %>% filter(strategy != "Spatially connected\n(betweenness)"),
            aes(x=midpoint_month, y=pct_reduction, colour=strategy, group=strategy),
            linewidth=2.1) +
  geom_point(data=fixed_results_fig4 %>% filter(strategy != "Spatially connected\n(betweenness)"),
             aes(x=midpoint_month, y=pct_reduction, colour=strategy), size=3.5) +
  scale_x_continuous(breaks=c(7,8,9,10),
                     labels=c("Jun–Aug","Jul–Sep","Aug–Oct","Sep–Nov")) +
  scale_color_manual(name="Strategy",
                     values=c("Spatially connected\n(eigenvector)"= "darkorange", #"cyan4",
                              "Spatially connected\n(betweenness)"="blue4",
                              "Most populous"="black","Random"="grey50")) +
  labs(x="Intervention timing", y="Reduction in dengue burden (%)", colour="Strategy") +
  theme_minimal(base_size=14) +
  theme(plot.title=element_text(size=20,face="bold"), plot.subtitle=element_text(size=20),
        axis.title.x=element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        plot.tag=element_text(face="bold",size=20), legend.position="bottom",
        # Remove grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Add axis lines
        axis.line = element_line(color = "black", linewidth = 0.6),
        # Optional: add tick marks
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text=element_text(size=14), legend.title=element_text(size=20)) +
  scale_y_continuous(breaks=seq(floor(min(all_results_fig4$pct_reduction)),
                                ceiling(max(all_results_fig4$pct_reduction)), by=1)) 
intervent_fig4a


#fig_4a <- intervent_fig4a +
#  labs(tag = "A") +
#  theme(plot.tag = element_text(size = 20, face = "bold"))

# Strategy map
district_centroids_map <- map_district_bz %>%
  mutate(geometry_centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  mutate(lon = st_coordinates(geometry_centroid)[,1],
         lat = st_coordinates(geometry_centroid)[,2]) %>%
  dplyr::select(location, lon, lat)

jitter_offset  <- 0.10
strategy_points <- bind_rows(
  tibble(location=top_largest,     strategy="Most populous"),
  tibble(location=top_connected,   strategy="Spatially connected\n(eigenvector)"),
  tibble(location=top_betweenness, strategy="Spatially connected\n(betweenness)")) %>%
  left_join(district_centroids_map, by="location") %>%
  group_by(location) %>%
  mutate(n_groups=n(), row_idx=row_number()) %>%
  ungroup() %>%
  mutate(angle = case_when(n_groups==1~0,
                           n_groups==2&row_idx==1 ~ -pi/2, n_groups==2&row_idx==2 ~  pi/2,
                           n_groups==3&row_idx==1 ~  pi/2, n_groups==3&row_idx==2 ~ -pi/6,
                           n_groups==3&row_idx==3 ~ -5*pi/6, TRUE~0),
         offset   = ifelse(n_groups==1, 0, jitter_offset),
         lon_plot = lon + offset*cos(angle),
         lat_plot = lat + offset*sin(angle))

strat_map <- ggplot(data = map_district_bz) +
  geom_sf(fill="white", color="black", linewidth=0.3) +
  geom_point(data=strategy_points %>% filter(strategy != "Spatially connected\n(betweenness)"),
             aes(x=lon_plot, y=lat_plot, colour=strategy, shape=strategy),
             size=3.5, alpha=0.9) +
 # ggtitle("A") +
  scale_colour_manual(name="Strategy",
                      values=c("Spatially connected\n(eigenvector)"= "darkorange",  #"cyan4",
                               "Spatially connected\n(betweenness)"="blue4",
                               "Most populous"="black")) +
  scale_shape_manual(name="Strategy",
                     values=c("Most populous"=16,"Spatially connected\n(eigenvector)"=17,
                              "Spatially connected\n(betweenness)"=15)) +
  theme_minimal() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        plot.title=element_text(size=20,face="bold"),
        axis.title.x= element_blank(), axis.title.y= element_blank(),
        axis.text.x= element_blank(), axis.text.y= element_blank(),
        plot.tag=element_text(face="bold",size=20), legend.position="none",
        legend.text=element_text(size=14), legend.title=element_text(size=20)) # +
 # xlab("Longitude") + ylab("Latitude")
strat_map

fig4a_inset <- ggdraw() +
  draw_plot(intervent_fig4a, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(
    strat_map,
    x = 0.14,   # horizontal position
    y = 0.66,   # vertical position
    width = 0.29,
    height = 0.29 )
fig4a_inset


fig_4a <- fig4a_inset +
 # labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig_4a



#############################################################
# May 11 
# FIG 4 intervention effectiveness across
#             network modularity x climate forcing heterogeneity
# ============================================================

set.seed(42)
n_nodes    <- 60; n_clusters <- 6; n_hubs <- 12; n_per_clust <- 8
hub_nodes  <- 1:n_hubs; peri_nodes <- (n_hubs+1):n_nodes
hub_cluster_membership <- rep(1:n_clusters, each=2)
node_cluster <- c(hub_cluster_membership,
                  rep(1,n_per_clust), rep(2,n_per_clust), rep(3,n_per_clust),
                  rep(4,n_per_clust), rep(5,n_per_clust), rep(6,n_per_clust))

make_network <- function(between_w, net_label) {
  el <- data.frame(from=integer(), to=integer(), weight=numeric())
  for (cl in 1:n_clusters) {
    h  <- which(hub_cluster_membership == cl)
    el <- rbind(el, data.frame(from=h[1], to=h[2], weight=0.70))
  }
  pairs <- combn(1:n_hubs, 2, simplify=FALSE)
  for (hp in pairs)
    if (hub_cluster_membership[hp[1]] != hub_cluster_membership[hp[2]])
      el <- rbind(el, data.frame(from=hp[1], to=hp[2], weight=between_w))
  for (cl in 1:n_clusters) {
    h_cl <- which(hub_cluster_membership == cl)
    p_cl <- which(node_cluster == cl & !(1:n_nodes %in% hub_nodes))
    for (h in h_cl) for (p in p_cl) el <- rbind(el, data.frame(from=h, to=p, weight=0.60))
  }
  for (cl in 1:n_clusters) {
    p_cl <- which(node_cluster == cl & !(1:n_nodes %in% hub_nodes))
    if (length(p_cl) >= 2) {
      pp <- combn(p_cl, 2, simplify=FALSE)
      for (p in pp) el <- rbind(el, data.frame(from=p[1], to=p[2], weight=0.25))
    }
  }
  g        <- graph_from_data_frame(el, directed=FALSE, vertices=data.frame(name=1:n_nodes))
  A        <- as.matrix(as_adjacency_matrix(g, attr="weight"))
  rs       <- rowSums(A); rs[rs==0] <- 1; W_net <- A/rs
  eig      <- eigen_centrality(g, weights=E(g)$weight)$vector
  eig_norm <- unname(eig/max(eig))
  top_hubs <- order(eig, decreasing=TRUE)[1:n_hubs]
  comm     <- cluster_louvain(g, weights=E(g)$weight); Q <- modularity(comm)
  list(g=g, el=el, W=W_net, eig_norm=eig_norm, top_hubs=top_hubs,
       modularity=Q, between_w=between_w, label=net_label)
}

network_configs <- list(make_network(0.05, "High"),
                        make_network(0.30, "Moderate"),
                        make_network(0.60, "Low"))

# ------------------------------------------------------------------
# Climate forcing: symmetric spread around Hill FOI mean
# ------------------------------------------------------------------
head(cal_data_hill)
hill_foi_mean <- mean(cal_data_hill$FOI_sqrt, na.rm = TRUE)
hill_foi_sd   <- sd(cal_data_hill$FOI_sqrt,   na.rm = TRUE)

make_climate_config <- function(spread_fraction, label) {
  hub_foi  <- hill_foi_mean * (1 + spread_fraction)
  peri_foi <- hill_foi_mean * (1 - spread_fraction)
  foi_vec  <- ifelse(1:n_nodes %in% hub_nodes,
                     pmax(hub_foi,  0),
                     pmax(peri_foi, 0))
  foi_vec  <- foi_vec * (hill_foi_mean / mean(foi_vec))
  data.frame(node     = 1:n_nodes,
             foi      = foi_vec,
             config   = label,
             hub_foi  = round(mean(foi_vec[hub_nodes]),  4),
             peri_foi = round(mean(foi_vec[peri_nodes]), 4),
             sd_foi   = round(sd(foi_vec), 4))
}

climate_configs <- bind_rows(
  make_climate_config(0,    "Homogeneous"),
  make_climate_config(0.25, "Moderate"),
  make_climate_config(0.50, "Heterogeneous"))

climate_levels <- c("Homogeneous", "Moderate", "Heterogeneous")

# Print spread summary to check values look reasonable
print(climate_configs %>% distinct(config, hub_foi, peri_foi, sd_foi))

# ------------------------------------------------------------------
# Fixed initial immunity (homogeneous, evolves dynamically)
# ------------------------------------------------------------------
national_mean  <- 0.20
fixed_immunity <- rep(national_mean, n_nodes)

# ------------------------------------------------------------------
# SEIR simulation with node-specific climate forcing
# ------------------------------------------------------------------
N_per_node <- 100000; sim_steps <- 24
beta_base  <- 0.25;   season_low <- 0.10
gamma_s    <- 1/(6/30); lp_s <- 1/(4/30)
imm_wane   <- 1/15;   alpha_mix  <- 0.70

run_seir_climate <- function(init_immunity, foi_vec, W, top_hubs,
                             n_steps = sim_steps,
                             gamma = gamma_s, lp = lp_s,
                             imm_w = imm_wane, alpha = alpha_mix,
                             intervene_nodes = NULL,
                             intervention_start = 1,
                             reduction = 0.80) {
  n  <- nrow(W)
  N  <- rep(N_per_node, n)
  R0 <- init_immunity * N
  I0 <- 0.005 * N; E0 <- 0.005 * N
  S0 <- N - R0 - I0 - E0
  S  <- S0; E <- E0; I <- I0; R <- R0
  
  # Node-specific beta scaled by FOI relative to hill mean
  beta_vec_base <- beta_base * (foi_vec / hill_foi_mean)
  
  results <- vector("list", n_steps)
  for (t in seq_len(n_steps)) {
    month_of_year <- ((t - 1) %% 12) + 1
    season_scalar <- ifelse(month_of_year %in% 6:11, 1.0, season_low)
    beta_vec <- beta_vec_base
    if (!is.null(intervene_nodes) && t >= intervention_start)
      beta_vec[intervene_nodes] <- beta_vec[intervene_nodes] * (1 - reduction)
    I_frac    <- I / N
    I_frac_nb <- as.vector(W %*% I_frac)
    I_eff     <- (1 - alpha) * I_frac + alpha * I_frac_nb
    lambda    <- season_scalar * beta_vec * I_eff
    new_E <- lambda * S; new_I <- lp * E; new_R <- gamma * I
    dS <- -new_E + imm_w * R
    dE <-  new_E - new_I
    dI <-  new_I - new_R
    dR <-  new_R - imm_w * R
    S  <- pmax(S + dS, 0); E <- pmax(E + dE, 0)
    I  <- pmax(I + dI, 0); R <- pmax(R + dR, 0)
    sc <- N / (S + E + I + R)
    S  <- S*sc; E <- E*sc; I <- I*sc; R <- R*sc
    results[[t]] <- data.frame(time=t, node=1:n,
                               S=S, E=E, I=I, R=R,
                               new_inf=new_E, imm_frac=R/N)
  }
  bind_rows(results)
}

# ------------------------------------------------------------------
# Factorial simulation: 3 networks x 3 climate configs
# ------------------------------------------------------------------
n_random_reps      <- 24
intervention_start <- 1

factorial_results_climate <- map_dfr(network_configs, function(nc) {
  map_dfr(climate_levels, function(clim_cfg) {
    
    foi_vec <- climate_configs %>%
      filter(config == clim_cfg) %>%
      arrange(node) %>%
      pull(foi)
    
    baseline <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                 intervene_nodes = NULL) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_inf <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                intervene_nodes = nc$top_hubs,
                                intervention_start = intervention_start) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_reduction <- 100 * (baseline - hub_inf) / baseline
    
    rand_reductions <- map_dbl(seq_len(n_random_reps), function(r) {
      rand_nodes <- sample(setdiff(1:n_nodes, nc$top_hubs), n_hubs)
      rand_inf   <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                     intervene_nodes = rand_nodes,
                                     intervention_start = intervention_start) %>%
        summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
      100 * (baseline - rand_inf) / baseline
    })
    
    tibble(network_label  = nc$label,
           modularity     = nc$modularity,
           climate_config = clim_cfg,
           baseline       = baseline,
           hub_reduction  = hub_reduction,
           rand_median    = median(rand_reductions),
           rand_lo        = quantile(rand_reductions, 0.025),
           rand_hi        = quantile(rand_reductions, 0.975),
           hub_advantage  = hub_reduction - median(rand_reductions))
  })
})

factorial_results_climate <- factorial_results_climate %>%
  mutate(network_label  = factor(network_label,  levels = c("High","Moderate","Low")),
         climate_config = factor(climate_config, levels = climate_levels))

# ------------------------------------------------------------------
# Heatmap — 3 (modularity) x 3 (climate heterogeneity)
# ------------------------------------------------------------------
p_heatmap_climate <- factorial_results_climate %>%
  ggplot(aes(x = climate_config,
             y = fct_rev(network_label),
             fill = hub_advantage / 3)) +
  geom_tile(colour = "white", linewidth = 0.0) +
  scale_fill_viridis_c(option = "B",
                       name   = "Intervention\nEffectiveness",
                       begin  = 0.1, end = 0.9) +
  labs(x = "Climate forcing heterogeneity",
       y = "Network modularity") +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(size = 20, face = "bold"),
        plot.subtitle   = element_text(size = 20),
        axis.title.x    = element_text(size = 20),
        axis.title.y    = element_text(size = 20),
        axis.text.x     = element_text(size = 16, angle = 20, hjust = 1),
        axis.text.y     = element_text(size = 16),
        plot.tag        = element_text(face = "bold", size = 20),
        legend.position = "right",
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.key.size = unit(1.0, "cm"))
p_heatmap_climate 

fig4b_new <- p_heatmap_climate +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig4b_new

#fig4_final2026 <- plot_grid(fig4ab, fig4d_new, ncol = 1)
fig4_final2026 <- plot_grid(fig4a_inset, fig4b_new, ncol = 2)
fig4_final2026




############### May 13 
# play with metapop dynamics 

set.seed(42)
n_nodes    <- 60; n_clusters <- 6; n_hubs <- 12; n_per_clust <- 8
hub_nodes  <- 1:n_hubs; peri_nodes <- (n_hubs+1):n_nodes
hub_cluster_membership <- rep(1:n_clusters, each=2)
node_cluster <- c(hub_cluster_membership,
                  rep(1,n_per_clust), rep(2,n_per_clust), rep(3,n_per_clust),
                  rep(4,n_per_clust), rep(5,n_per_clust), rep(6,n_per_clust))

make_network <- function(between_w, net_label) {
  el <- data.frame(from=integer(), to=integer(), weight=numeric())
  # Within-cluster hub-hub connections
  for (cl in 1:n_clusters) {
    h  <- which(hub_cluster_membership == cl)
    el <- rbind(el, data.frame(from=h[1], to=h[2], weight=0.70))
  }
  # Between-cluster hub-hub connections (varies by modularity)
  pairs <- combn(1:n_hubs, 2, simplify=FALSE)
  for (hp in pairs)
    if (hub_cluster_membership[hp[1]] != hub_cluster_membership[hp[2]])
      el <- rbind(el, data.frame(from=hp[1], to=hp[2], weight=between_w))
  # Hub-periphery connections within cluster
  for (cl in 1:n_clusters) {
    h_cl <- which(hub_cluster_membership == cl)
    p_cl <- which(node_cluster == cl & !(1:n_nodes %in% hub_nodes))
    for (h in h_cl) for (p in p_cl)
      el <- rbind(el, data.frame(from=h, to=p, weight=0.60))
  }
  # Periphery-periphery connections within cluster
  for (cl in 1:n_clusters) {
    p_cl <- which(node_cluster == cl & !(1:n_nodes %in% hub_nodes))
    if (length(p_cl) >= 2) {
      pp <- combn(p_cl, 2, simplify=FALSE)
      for (p in pp) el <- rbind(el, data.frame(from=p[1], to=p[2], weight=0.25))
    }
  }
  g   <- graph_from_data_frame(el, directed=FALSE, vertices=data.frame(name=1:n_nodes))
  A   <- as.matrix(as_adjacency_matrix(g, attr="weight"))
  
  # Build W as a proper transmission matrix:
  # diagonal = local within-patch weight, off-diagonal = connectivity weights
  # then row-normalize so rows sum to 1
  diag(A) <- 1.0          # within-patch self-weight
  rs      <- rowSums(A)
  W_net   <- A / rs       # row-normalized: each row sums to 1
  # diagonal entry = local share, off-diag = neighbor share
  
  eig      <- eigen_centrality(g, weights=E(g)$weight)$vector
  eig_norm <- unname(eig/max(eig))
  top_hubs <- order(eig, decreasing=TRUE)[1:n_hubs]
  comm     <- cluster_louvain(g, weights=E(g)$weight); Q <- modularity(comm)
  list(g=g, el=el, W=W_net, eig_norm=eig_norm, top_hubs=top_hubs,
       modularity=Q, between_w=between_w, label=net_label)
}

network_configs <- list(make_network(0.05, "High"),
                        make_network(0.30, "Moderate"),
                        make_network(0.60, "Low"))

# ------------------------------------------------------------------
# Climate forcing: symmetric spread around Hill FOI mean
# ------------------------------------------------------------------
hill = bioclimatic_cases %>% filter(Bioclimatic_Zone == "Hill") %>%
  distinct(monthly_time, bz_cases, bz_temp, bz_denom) 

temp = print(hill$bz_temp)
m = EFD(temp)*PEA(temp)*MDR(temp)
FOI = ( (m*(a(temp)^2)*b(temp)*c(temp)) *exp(-mu(temp)/PDR(temp)) ) / (mu(temp)^2)
print(FOI)

time = seq(1, length(FOI),1 )
monthly_time = print(hill$monthly_time)

# Make temperature dataset
dat_temp = data.frame(FOI, time, temp, monthly_time) %>%
  mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right")) %>%
  mutate(FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2)) %>% 
  mutate(FOI_smooth  = ifelse(FOI_smooth < 0, 0, FOI_smooth)) %>%
  mutate(FOI_sqrt = sqrt(FOI_smooth)) #

######################## Make final calibration data 
cal_data = left_join(hill, dat_temp, by = c("monthly_time"))

hill_foi_mean <- mean(cal_data$FOI_sqrt, na.rm = TRUE)

make_climate_config <- function(spread_fraction, label) {
  hub_foi  <- hill_foi_mean * (1 + spread_fraction)
  peri_foi <- hill_foi_mean * (1 - spread_fraction)
  foi_vec  <- ifelse(1:n_nodes %in% hub_nodes,
                     pmax(hub_foi,  0),
                     pmax(peri_foi, 0))
  foi_vec  <- foi_vec * (hill_foi_mean / mean(foi_vec))
  data.frame(node     = 1:n_nodes,
             foi      = foi_vec,
             config   = label,
             hub_foi  = round(mean(foi_vec[hub_nodes]),  4),
             peri_foi = round(mean(foi_vec[peri_nodes]), 4),
             sd_foi   = round(sd(foi_vec), 4))
}

climate_configs <- bind_rows(
  make_climate_config(0,    "Homogeneous"),
  make_climate_config(0.25, "Moderate"),
  make_climate_config(0.50, "Heterogeneous"))

climate_levels <- c("Homogeneous", "Moderate", "Heterogeneous")
print(climate_configs %>% distinct(config, hub_foi, peri_foi, sd_foi))

# ------------------------------------------------------------------
# Fixed initial immunity (homogeneous, evolves dynamically)
# ------------------------------------------------------------------
national_mean  <- 0.20
fixed_immunity <- rep(national_mean, n_nodes)

# ------------------------------------------------------------------
# SEIR simulation — transmission via full connectivity matrix W
# No alpha blending; W diagonal encodes local transmission,
# off-diagonal encodes between-patch transmission
# ------------------------------------------------------------------
N_per_node <- 100000; sim_steps <- 24
beta_base  <- 0.25;   season_low <- 0.10
gamma_s    <- 1/(6/30); lp_s <- 1/(4/30)
imm_wane   <- 1/15

run_seir_climate <- function(init_immunity, foi_vec, W, top_hubs,
                             n_steps = sim_steps,
                             gamma = gamma_s, lp = lp_s,
                             imm_w = imm_wane,
                             intervene_nodes = NULL,
                             intervention_start = 1,
                             reduction = 0.40) {
  n  <- nrow(W)
  N  <- rep(N_per_node, n)
  R0 <- init_immunity * N
  I0 <- 0.005 * N; E0 <- 0.005 * N
  S0 <- N - R0 - I0 - E0
  S  <- S0; E <- E0; I <- I0; R <- R0
  
  # Node-specific beta scaled by FOI relative to hill mean
  beta_vec_base <- beta_base * (foi_vec / hill_foi_mean)
  
  results <- vector("list", n_steps)
  for (t in seq_len(n_steps)) {
    month_of_year <- ((t - 1) %% 12) + 1
    season_scalar <- ifelse(month_of_year %in% 6:11, 1.0, season_low)
    
    beta_vec <- beta_vec_base
    if (!is.null(intervene_nodes) && t >= intervention_start)
      beta_vec[intervene_nodes] <- beta_vec[intervene_nodes] * (1 - reduction)
    
    # Standard metapopulation transmission:
    # W %*% (I/N) gives each node a weighted mix of local + neighbor
    # infectious pressure, with weights determined entirely by W
    I_frac <- I / N
    I_eff  <- as.vector(W %*% I_frac)   # no alpha — W encodes the full mixing
    
    lambda <- season_scalar * beta_vec * I_eff
    new_E  <- lambda * S; new_I <- lp * E; new_R <- gamma * I
    dS <- -new_E + imm_w * R
    dE <-  new_E - new_I
    dI <-  new_I - new_R
    dR <-  new_R - imm_w * R
    S  <- pmax(S + dS, 0); E <- pmax(E + dE, 0)
    I  <- pmax(I + dI, 0); R <- pmax(R + dR, 0)
    sc <- N / (S + E + I + R)
    S  <- S*sc; E <- E*sc; I <- I*sc; R <- R*sc
    results[[t]] <- data.frame(time=t, node=1:n,
                               S=S, E=E, I=I, R=R,
                               new_inf=new_E, imm_frac=R/N)
  }
  bind_rows(results)
}

# ------------------------------------------------------------------
# Factorial simulation: 3 networks x 3 climate configs
# ------------------------------------------------------------------
n_random_reps      <- 24
intervention_start <- 1

factorial_results_climate <- map_dfr(network_configs, function(nc) {
  map_dfr(climate_levels, function(clim_cfg) {
    
    foi_vec <- climate_configs %>%
      filter(config == clim_cfg) %>%
      arrange(node) %>%
      pull(foi)
    
    baseline <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                 intervene_nodes = NULL) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_inf <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                intervene_nodes = nc$top_hubs,
                                intervention_start = intervention_start) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_reduction <- 100 * (baseline - hub_inf) / baseline
    
    rand_reductions <- map_dbl(seq_len(n_random_reps), function(r) {
      rand_nodes <- sample(setdiff(1:n_nodes, nc$top_hubs), n_hubs)
      rand_inf   <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                     intervene_nodes = rand_nodes,
                                     intervention_start = intervention_start) %>%
        summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
      100 * (baseline - rand_inf) / baseline
    })
    
    tibble(network_label  = nc$label,
           modularity     = nc$modularity,
           climate_config = clim_cfg,
           baseline       = baseline,
           hub_reduction  = hub_reduction,
           rand_median    = median(rand_reductions),
           rand_lo        = quantile(rand_reductions, 0.025),
           rand_hi        = quantile(rand_reductions, 0.975),
           hub_advantage  = hub_reduction - median(rand_reductions))
  })
})

factorial_results_climate <- factorial_results_climate %>%
  mutate(network_label  = factor(network_label,  levels = c("High","Moderate","Low")),
         climate_config = factor(climate_config, levels = climate_levels))

# ------------------------------------------------------------------
# Heatmap — 3 (modularity) x 3 (climate heterogeneity)
# ------------------------------------------------------------------
p_heatmap_climate1 <- factorial_results_climate %>%
  ggplot(aes(x = climate_config,
             y = fct_rev(network_label),
             fill = hub_advantage / 3)) +
  geom_tile(colour = "white", linewidth = 0.0) +
  scale_fill_viridis_c(option = "B",
                       name   = "Intervention\nEffectiveness (%)",
                       begin  = 0.1, end = 0.9) +
  labs(x = "Climate forcing heterogeneity",
       y = "Network modularity") +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(size = 20, face = "bold"),
        plot.subtitle   = element_text(size = 20),
        axis.title.x    = element_text(size = 20),
        axis.title.y    = element_text(size = 20),
        axis.text.x     = element_text(size = 16, angle = 20, hjust = 1),
        axis.text.y     = element_text(size = 16),
        plot.tag        = element_text(face = "bold", size = 20),
        legend.position = "right",
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.key.size = unit(1.0, "cm"))
p_heatmap_climate1

fig4b_new <- p_heatmap_climate1 +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig4b_new

fig4_final2026 <- plot_grid(fig4a_inset, fig4b_new, ncol = 2)
fig4_final2026



## check this 
improvement_vs_crosszone <- baseline_observed %>%
  mutate(improvement = sum_reds1 - sum_reds2) %>%
  left_join(zone_decomp %>% dplyr::select(location, cross_zone_frac), by = "location")

cor.test(improvement_vs_crosszone$cross_zone_frac,
         improvement_vs_crosszone$improvement, method = "spearman")



#####################################################################################

# supp figure with betweenness 
intervent_supp <- ggplot() +
  geom_line(data = random_summary,
            aes(x = midpoint_month, y = random_median, colour = "Random"),
            linewidth = 2.1, linetype = "dashed") +
  geom_ribbon(data = random_summary,
              aes(x = midpoint_month, ymin = random_lo, ymax = random_hi),
              fill = "grey50", alpha = 0.40) +
  geom_ribbon(data = random_summary,
              aes(x = midpoint_month, ymin = random_lo_ex, ymax = random_hi_ex),
              fill = "grey90", alpha = 0.40) +
  geom_point(data = random_summary,
             aes(x = midpoint_month, y = random_median, colour = "Random"), size = 3.5) +
  geom_line(data = fixed_results ,
            aes(x = midpoint_month, y = pct_reduction, colour = strategy, group = strategy),
            linewidth = 2.1) +
  geom_point(data = fixed_results, 
             aes(x = midpoint_month, y = pct_reduction, colour = strategy), size = 3.5) +
  scale_x_continuous(breaks = c(7,8,9,10),
                     labels = c("Jun–Aug","Jul–Sep","Aug–Oct","Sep–Nov")) +
  scale_color_manual(name = "Strategy",
                     values = c("Most populous" = "gray35",
                                "Spatially connected\n(eigenvector)" = "tan1",
                                "Spatially connected\n(betweenness)" = "orangered",
                                "Random" = "grey50")) +
  labs(x = "Intervention timing", y = "Reduction in dengue (%)", colour = "Strategy") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 20, face = "bold"), plot.subtitle = element_text(size = 20),
        axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
        plot.tag = element_text(face = "bold", size = 20), legend.position = "right",
        # Remove grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Add axis lines
        axis.line = element_line(color = "black", linewidth = 0.6),
        # Optional: add tick marks
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size = 14), legend.title = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(floor(min(all_results$pct_reduction)),
                                  ceiling(max(all_results$pct_reduction)), by = 1))
intervent_supp



###################################################
### supp intervention effectiveness 

run_seir_climate <- function(init_immunity, foi_vec, W, top_hubs,
                             n_steps = sim_steps,
                             gamma = gamma_s, lp = lp_s,
                             imm_w = imm_wane,
                             intervene_nodes = NULL,
                             intervention_start = 1,
                             reduction = 0.10) {
  n  <- nrow(W)
  N  <- rep(N_per_node, n)
  R0 <- init_immunity * N
  I0 <- 0.005 * N; E0 <- 0.005 * N
  S0 <- N - R0 - I0 - E0
  S  <- S0; E <- E0; I <- I0; R <- R0
  
  # Node-specific beta scaled by FOI relative to hill mean
  beta_vec_base <- beta_base * (foi_vec / hill_foi_mean)
  
  results <- vector("list", n_steps)
  for (t in seq_len(n_steps)) {
    month_of_year <- ((t - 1) %% 12) + 1
    season_scalar <- ifelse(month_of_year %in% 6:11, 1.0, season_low)
    
    beta_vec <- beta_vec_base
    if (!is.null(intervene_nodes) && t >= intervention_start)
      beta_vec[intervene_nodes] <- beta_vec[intervene_nodes] * (1 - reduction)
    
    # Standard metapopulation transmission:
    # W %*% (I/N) gives each node a weighted mix of local + neighbor
    # infectious pressure, with weights determined entirely by W
    I_frac <- I / N
    I_eff  <- as.vector(W %*% I_frac)   # no alpha — W encodes the full mixing
    
    lambda <- season_scalar * beta_vec * I_eff
    new_E  <- lambda * S; new_I <- lp * E; new_R <- gamma * I
    dS <- -new_E + imm_w * R
    dE <-  new_E - new_I
    dI <-  new_I - new_R
    dR <-  new_R - imm_w * R
    S  <- pmax(S + dS, 0); E <- pmax(E + dE, 0)
    I  <- pmax(I + dI, 0); R <- pmax(R + dR, 0)
    sc <- N / (S + E + I + R)
    S  <- S*sc; E <- E*sc; I <- I*sc; R <- R*sc
    results[[t]] <- data.frame(time=t, node=1:n,
                               S=S, E=E, I=I, R=R,
                               new_inf=new_E, imm_frac=R/N)
  }
  bind_rows(results)
}

# ------------------------------------------------------------------
# Factorial simulation: 3 networks x 3 climate configs
# ------------------------------------------------------------------
n_random_reps      <- 24
intervention_start <- 1

factorial_results_climate <- map_dfr(network_configs, function(nc) {
  map_dfr(climate_levels, function(clim_cfg) {
    
    foi_vec <- climate_configs %>%
      filter(config == clim_cfg) %>%
      arrange(node) %>%
      pull(foi)
    
    baseline <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                 intervene_nodes = NULL) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_inf <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                intervene_nodes = nc$top_hubs,
                                intervention_start = intervention_start) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_reduction <- 100 * (baseline - hub_inf) / baseline
    
    rand_reductions <- map_dbl(seq_len(n_random_reps), function(r) {
      rand_nodes <- sample(setdiff(1:n_nodes, nc$top_hubs), n_hubs)
      rand_inf   <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                     intervene_nodes = rand_nodes,
                                     intervention_start = intervention_start) %>%
        summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
      100 * (baseline - rand_inf) / baseline
    })
    
    tibble(network_label  = nc$label,
           modularity     = nc$modularity,
           climate_config = clim_cfg,
           baseline       = baseline,
           hub_reduction  = hub_reduction,
           rand_median    = median(rand_reductions),
           rand_lo        = quantile(rand_reductions, 0.025),
           rand_hi        = quantile(rand_reductions, 0.975),
           hub_advantage  = hub_reduction - median(rand_reductions))
  })
})

factorial_results_climate <- factorial_results_climate %>%
  mutate(network_label  = factor(network_label,  levels = c("High","Moderate","Low")),
         climate_config = factor(climate_config, levels = climate_levels))

# ------------------------------------------------------------------
# Heatmap — 3 (modularity) x 3 (climate heterogeneity)
# ------------------------------------------------------------------
p_heatmap_climate10 <- factorial_results_climate %>%
  ggplot(aes(x = climate_config,
             y = fct_rev(network_label),
             fill = hub_advantage / 3)) +
  geom_tile(colour = "white", linewidth = 0.0) +
  scale_fill_viridis_c(option = "B",
                       name   = "Intervention\nEffectiveness (%)",
                       begin  = 0.1, end = 0.4) +
  labs(x = "Climate forcing heterogeneity",
       y = "Network modularity") +
  ggtitle("10% intervention effectivness") +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(size = 20),
        plot.subtitle   = element_text(size = 20),
        axis.title.x    = element_text(size = 20),
        axis.title.y    = element_text(size = 20),
        axis.text.x     = element_text(size = 16, angle = 20, hjust = 1),
        axis.text.y     = element_text(size = 16),
        plot.tag        = element_text(face = "bold", size = 20),
        legend.position = "right",
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.key.size = unit(1.0, "cm"))
p_heatmap_climate10


### now 50 

run_seir_climate <- function(init_immunity, foi_vec, W, top_hubs,
                             n_steps = sim_steps,
                             gamma = gamma_s, lp = lp_s,
                             imm_w = imm_wane,
                             intervene_nodes = NULL,
                             intervention_start = 1,
                             reduction = 0.70) {
  n  <- nrow(W)
  N  <- rep(N_per_node, n)
  R0 <- init_immunity * N
  I0 <- 0.005 * N; E0 <- 0.005 * N
  S0 <- N - R0 - I0 - E0
  S  <- S0; E <- E0; I <- I0; R <- R0
  
  # Node-specific beta scaled by FOI relative to hill mean
  beta_vec_base <- beta_base * (foi_vec / hill_foi_mean)
  
  results <- vector("list", n_steps)
  for (t in seq_len(n_steps)) {
    month_of_year <- ((t - 1) %% 12) + 1
    season_scalar <- ifelse(month_of_year %in% 6:11, 1.0, season_low)
    
    beta_vec <- beta_vec_base
    if (!is.null(intervene_nodes) && t >= intervention_start)
      beta_vec[intervene_nodes] <- beta_vec[intervene_nodes] * (1 - reduction)
    
    # Standard metapopulation transmission:
    # W %*% (I/N) gives each node a weighted mix of local + neighbor
    # infectious pressure, with weights determined entirely by W
    I_frac <- I / N
    I_eff  <- as.vector(W %*% I_frac)   # no alpha — W encodes the full mixing
    
    lambda <- season_scalar * beta_vec * I_eff
    new_E  <- lambda * S; new_I <- lp * E; new_R <- gamma * I
    dS <- -new_E + imm_w * R
    dE <-  new_E - new_I
    dI <-  new_I - new_R
    dR <-  new_R - imm_w * R
    S  <- pmax(S + dS, 0); E <- pmax(E + dE, 0)
    I  <- pmax(I + dI, 0); R <- pmax(R + dR, 0)
    sc <- N / (S + E + I + R)
    S  <- S*sc; E <- E*sc; I <- I*sc; R <- R*sc
    results[[t]] <- data.frame(time=t, node=1:n,
                               S=S, E=E, I=I, R=R,
                               new_inf=new_E, imm_frac=R/N)
  }
  bind_rows(results)
}

# ------------------------------------------------------------------
# Factorial simulation: 3 networks x 3 climate configs
# ------------------------------------------------------------------
n_random_reps      <- 24
intervention_start <- 1

factorial_results_climate <- map_dfr(network_configs, function(nc) {
  map_dfr(climate_levels, function(clim_cfg) {
    
    foi_vec <- climate_configs %>%
      filter(config == clim_cfg) %>%
      arrange(node) %>%
      pull(foi)
    
    baseline <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                 intervene_nodes = NULL) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_inf <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                intervene_nodes = nc$top_hubs,
                                intervention_start = intervention_start) %>%
      summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
    
    hub_reduction <- 100 * (baseline - hub_inf) / baseline
    
    rand_reductions <- map_dbl(seq_len(n_random_reps), function(r) {
      rand_nodes <- sample(setdiff(1:n_nodes, nc$top_hubs), n_hubs)
      rand_inf   <- run_seir_climate(fixed_immunity, foi_vec, nc$W, nc$top_hubs,
                                     intervene_nodes = rand_nodes,
                                     intervention_start = intervention_start) %>%
        summarise(total_inf = sum(new_inf)) %>% pull(total_inf)
      100 * (baseline - rand_inf) / baseline
    })
    
    tibble(network_label  = nc$label,
           modularity     = nc$modularity,
           climate_config = clim_cfg,
           baseline       = baseline,
           hub_reduction  = hub_reduction,
           rand_median    = median(rand_reductions),
           rand_lo        = quantile(rand_reductions, 0.025),
           rand_hi        = quantile(rand_reductions, 0.975),
           hub_advantage  = hub_reduction - median(rand_reductions))
  })
})

factorial_results_climate <- factorial_results_climate %>%
  mutate(network_label  = factor(network_label,  levels = c("High","Moderate","Low")),
         climate_config = factor(climate_config, levels = climate_levels))

# ------------------------------------------------------------------
# Heatmap — 3 (modularity) x 3 (climate heterogeneity)
# ------------------------------------------------------------------
p_heatmap_climate50 <- factorial_results_climate %>%
  ggplot(aes(x = climate_config,
             y = fct_rev(network_label),
             fill = hub_advantage / 3)) +
  geom_tile(colour = "white", linewidth = 0.0) +
  scale_fill_viridis_c(option = "B",
                       name   = "Intervention\nEffectiveness (%)",
                       begin  = 0.3, end = 0.9) +
  ggtitle("70% intervention effectivness") +
  labs(x = "Climate forcing heterogeneity",
       y = "Network modularity") +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(size = 20),
        plot.subtitle   = element_text(size = 20),
        axis.title.x    = element_text(size = 20),
        axis.title.y    = element_text(size = 20),
        axis.text.x     = element_text(size = 16, angle = 20, hjust = 1),
        axis.text.y     = element_text(size = 16),
        plot.tag        = element_text(face = "bold", size = 20),
        legend.position = "right",
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.key.size = unit(1.0, "cm"))
p_heatmap_climate50

plot_grid(p_heatmap_climate10, p_heatmap_climate50)
