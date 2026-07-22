# https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pntd.0005568/2/pntd.0005568.s002.pdf?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20250513%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20250513T214856Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=5b2789241af63159f536780eb62002080892cbe7a32682042ab7e2f3c6f595927ee4695c20a5b86e21e41ed895a7cce854fbdaaaef6409de7ac1e369c36fd171770c7c5225036436e69c8484ff9ee8e57f4cea5ff338625a8ab7c5779e20bb11b3d97d6d9192a8ebee546363860b12c5d7ae23910b3f48b0fa29cd0d7d283c15fec65ee018e4ad626773adf730a528df89c8d643a826016abfae85dd997f367564a185c600df7ee8d3032fb817e59bbdd3d1662d7c46d9dd63cfe2686f2484afe188b9039185f3f6f084a799f3aec8ef98df261e6a8e3c2c3bd34c9a38de12629068555239eca91feae90edc16547b3520832ecc55e82f82ec07f4ddd700c025
briere <- function(x, c, T0, Tm){
  ifelse((x < T0) | (x > Tm), 0, c * x * (x - T0) * sqrt(Tm - x))}

quadratic <- function(x, c, T0, Tm){
  ifelse((x < T0) | (x > Tm), 0, c * (x - T0) * (x - Tm))}

inverted_quadratic <- function(x, c, T0, Tm, timestep) {
  ifelse((x < T0) | (x > Tm),
         1.0 / timestep,
         1.0 / (c * (x - T0) * (x - Tm)))}

# biting rate
a <- function(temp){
  briere(temp, a_c, a_T0, a_Tm)}

# transmission competence: probability of human infection per bite by an infectious mosquito
b <- function(temp){
  briere(temp, b_c, b_T0, b_Tm)}

# probability of mosquito infection per bite on an infectious host
c_prob <- function(temp){
  briere(temp, c_c, c_T0, c_Tm)}

# eggs per female per day 
EFD =  function(temp){
  briere(temp, EFD_c, EFD_T0, EFD_Tm)}

# probability of egg to adult survival 
PEA =  function(temp){
  quadratic(temp, PEA_c, PEA_T0, PEA_Tm)}

# mosquito development rate (1/larval development period)
MDR <- function(temp){
  briere(temp, MDR_c, MDR_T0, MDR_Tm)}

# adult mortality rate (1/larval development period)
mu  <- function(temp){ inverted_quadratic(temp, mu_c, mu_T0, mu_Tm, timestep = 1)}

# parasite development rate
PDR <- function(temp){
  briere(temp, PDR_c, PDR_T0, PDR_Tm)}

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
      FOI <- 0.001 }
    
    # Differential equations
    dS = -c*foi_scalar*FOI*S*I/N   + imm*R 
    dE =  c*foi_scalar*FOI*S*I/N - lp*E 
    dI =  lp*E - gamma*I
    dR =  I*gamma  -  R*imm 
    
    # Return the rates of change
    list(c(dS, dE, dI, dR)) }) }

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
head(cal_data_hill)
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
  geom_line(data = cal_data_hill, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
            lty= "dotted" , lwd = 2.2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.7, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Hill") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#2E7C8A")) +
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 20),   # x-axis title
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
  geom_line(data = cal_data_siwalik, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
            lty = "dotted", lwd = 2.2) +
  # geom_line(data = cal_data_hill, aes(x = monthly_time, y = FOI_sqrt*6000, col = "Observed"), lwd = 2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.5, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Siwalik") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "lightseagreen"))  +
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
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
  geom_line(aes(x = monthly_time, y = FOI_sqrt, col = temp), cex = 3) + theme_bw() +
  ggtitle('Middle Mountain') +
  xlab("Time") +
  ylab("Force of infection") +
  scale_color_viridis_c(option = "G", name = "FOI") +
  theme_minimal() +  xlab("Time (months)") 
midmount_foi

######################## Make final calibration data 
cal_data = left_join(midmount, dat_temp, by = c("monthly_time"))
head(cal_data_midmount)
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
  geom_line(data = cal_data_midmount, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
            lty = "dotted", lwd = 2.2) +
  # geom_line(data = cal_data_hill, aes(x = monthly_time, y = FOI_sqrt*6000, col = "Observed"), lwd = 2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.69, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Middle Mountain") +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#2F4F8F")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  theme( panel.grid = element_blank() ) 
middlemountain_cal



tune_foi_scalar <- function(cal_data, report_scalar, c_reduction, c_end,
                            scalar_seq = seq(14, 17, by = 0.02)) {
  
  post2022_idx <- which(cal_data$monthly_time > 2019.5)
  
  N <- cal_data$bz_denom[1]
  state <- c(S = 0.99 * N, E = 0.005 * N, I = 0.005 * N, R = 0.00 * N)
  time  <- seq(1, nrow(cal_data), 1)
  
  results <- data.frame(foi_scalar = scalar_seq, rmse = NA_real_)
  
  for (i in seq_along(scalar_seq)) {
    param <- c(
      foi_scalar  = scalar_seq[i],
      lp          = 1/(4/30),
      gamma       = 1/(6/30),
      N           = cal_data$bz_denom[1],
      imm         = 1/14,
      c_base      = 1,
      c_reduction = c_reduction,
      c_start     = 15,
      c_end       = c_end)
    out <- tryCatch(
      as.data.frame(ode(y = state, times = time,
                        func = seirs_dengue_model, parms = param)) %>%
        mutate(N = S + E + I + R,
               predicted_cases = I * report_scalar),
      error = function(e) NULL)
    if (is.null(out)) next
    predicted <- out$predicted_cases[post2022_idx]
    observed  <- cal_data$bz_cases[post2022_idx]
    results$rmse[i] <- sqrt(mean((predicted - observed)^2, na.rm = TRUE))
  }
  
  best <- results[which.min(results$rmse), ]
  cat("Best foi_scalar:", best$foi_scalar, "| RMSE:", round(best$rmse, 4), "\n")
  return(results)
}

# Hill
dat_temp  <- make_dat_temp(hill$bz_temp, hill$monthly_time)
hill_tune <- tune_foi_scalar(cal_data_hill, report_scalar = 0.011,
                             c_reduction = 0.0001, c_end = 33)

# Siwalik
dat_temp     <- make_dat_temp(siwalik$bz_temp, siwalik$monthly_time)
siwalik_tune <- tune_foi_scalar(cal_data_siwalik, report_scalar = 0.004,
                                c_reduction = 0.0001, c_end = 33)

# Middle Mountain
dat_temp     <- make_dat_temp(midmount$bz_temp, midmount$monthly_time)
mm_tune      <- tune_foi_scalar(cal_data_midmount, report_scalar = 0.007,
                                c_reduction = 0.000001, c_end = 33)

# Tarai
dat_temp   <- make_dat_temp(tarai$bz_temp, tarai$monthly_time)
tarai_tune <- tune_foi_scalar(cal_data_tarai, report_scalar = 0.0022,
                              c_reduction = 0.00001, c_end = 43)

# High Mountain

make_dat_temp <- function(bz_temp_vec, bz_monthly_time) {
  temp <- bz_temp_vec
  m    <- EFD(temp) * PEA(temp) * MDR(temp)
  FOI  <- ((m * (a(temp)^2) * b(temp) * c_prob(temp)) * exp(-mu(temp) / PDR(temp))) / (mu(temp)^2)
  time <- seq_along(FOI)
  data.frame(FOI = FOI, time = time, temp = temp, monthly_time = bz_monthly_time) %>%
    mutate(FOI_smooth = rollmean(FOI, k = 3, fill = NA, align = "right"),
           FOI_smooth = na.approx(FOI_smooth, x = time, na.rm = FALSE, rule = 2),
           FOI_smooth = ifelse(FOI_smooth < 0, 0, FOI_smooth),
           FOI_sqrt   = sqrt(FOI_smooth) + 1)
}

dat_temp <- make_dat_temp(hm$bz_temp, hm$monthly_time)
hm_tune  <- tune_foi_scalar(cal_data_hm, report_scalar = 0.0045,
                            c_reduction = 0.000007, c_end = 33)


############################################################################
# TARAI
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
  xlab("Time (months)") +
  theme(
    plot.title      = element_text(size = 20, face = "bold"),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 14),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) 
tarai_foi

######################## Make final calibration data 
cal_data_tarai = left_join(tarai, dat_temp, by = c("monthly_time"))
observed_data = cal_data_tarai$bz_cases

# States
N <- cal_data_tarai$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N
)

param <- c(
  foi_scalar = 2.10, # 2.10
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data_tarai$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction = .00001,    # .00001
  c_start = 15,
  c_end = 43 # 44
  
)

time = seq(1, nrow(cal_data_tarai),1 )
# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

out_df_check_t <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.0022) 

tarai_cal = ggplot(data = out_df_check_t ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.2) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data_tarai, aes(x = monthly_time, y = bz_cases, col = "Observed"), 
            lty = "dotted", lwd = 2.2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.5, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("Tarai") +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#D6EED9"))+
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme( panel.grid = element_blank() )
tarai_cal 


# dynamics 
dynamics_tarai = left_join(out_df_check, cal_data_tarai, by = "time" ) %>%
  filter(monthly_time > 2022.5)

ggplot(data = dynamics_tarai) +
  geom_point(aes(x = R/N, y= bz_temp, col = log(bz_cases + 1)), cex = 23) +
  theme_bw() +
  scale_fill_viridis_d()

interp_data <- with(dynamics_tarai , {
  interp(x = R/N, y = bz_temp, z = log(bz_cases + 1), duplicate = "mean", linear = FALSE)
})
interp_df <- as.data.frame(expand.grid(x = interp_data$x, y = interp_data$y))
interp_df$z <- as.vector(interp_data$z)

tarai_dino = ggplot(data = interp_df %>% drop_na(), aes(x = x, y = y, fill = z)) +
  geom_raster() +
  scale_fill_viridis_c(name = "log cases") +
  theme_bw() + 
  ylab("Temperature") + xlab("Fraction immune") +
  ggtitle("Tarai")
tarai_dino



#########################################
# HIGH MOUNTAIN
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
  xlab("Time (months)") +
  theme(
    plot.title      = element_text(size = 20, face = "bold"),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 20),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 14),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) 
hm_foi



ggplot(data = dat_temp) +
  geom_line(aes(x = monthly_time, y = FOI_sqrt*196, col = temp), cex = 3) +
  theme_bw() +
  ggtitle('High Mountain')

######################## Make final calibration data 
cal_data_hm = left_join(hm, dat_temp, by = c("monthly_time"))

head(cal_data_hm)
observed_data = cal_data_hm$bz_cases

# States
N <- cal_data_hm$bz_denom[1] 

state <- c(
  S = 0.99 * N,
  E = 0.005* N,       
  I = 0.005 * N,
  R = 0.00 * N)

param <- c(
  foi_scalar = 19, # 3.18
  lp = 1/(4/30),                  
  gamma = 1/(6/30),                  
  N = cal_data_hm$bz_denom[1] , 
  imm = 1/14, 
  c_base = 1,
  c_reduction = .000007,    #.000007, 
  c_start = 15,
  c_end = 33 #41.75
  
)

time = seq(1, nrow(cal_data_hm),1 )

# Solve the ODE system
out_check <- ode(y = state, times = time, func = seirs_dengue_model, parms = param)

#Add derived variables
#Add derived variables
out_df_check_hm <- as.data.frame(out_check) %>%
  mutate(N = S + E + I + R) %>%
  mutate(predicted_cases = I*.0025) 

highmountain_cal = ggplot(data = out_df_check_hm ) + 
  annotate("rect", xmin = 2019 + 15/12, xmax = 2019 + 33/12, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  geom_line(aes(x = time/12 + 2019, y = predicted_cases, col = "Predicted"), lwd = 2.2)  +
  geom_line(data = cal_data_hm, aes(x = monthly_time, y = bz_cases, col = "Observed"),
            lty = "dotted", lwd = 2.2) +
  # geom_line(data = cal_data_hill, aes(x = monthly_time, y = FOI_sqrt*6000, col = "Observed"), lwd = 2) +
  ylab("Cases") +
  theme_minimal() +
  xlab("Time") +
  xlim(c(2019.69, 2025)) +
  # ylim(c(0, 60)) +
  ggtitle("High Mountain") +
  scale_color_manual(values = c("Observed" = "gray29", "Predicted" = "#2C1E4A"), name = "") +
  theme(
    plot.title      = element_text(size = 20),   # title
    plot.subtitle =   element_text(size = 20), 
    axis.title.x    = element_text(size = 14),   # x-axis title
    axis.title.y    = element_text(size = 20),   # y-axis title
    axis.text.x     = element_text(size = 10),   # x-axis tick labels
    axis.text.y     = element_text(size = 14) ,   # y-axis tick labels
    plot.tag         = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
  theme( panel.grid = element_blank() ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
highmountain_cal 

# dynamics 
dynamics_hm = left_join(out_df_check, cal_data_hm, by = "time" ) %>%
  filter(monthly_time > 2022.5)

head(dynamics_hm)
ggplot(data = dynamics_hm) +
  geom_point(aes(x = R/N, y= bz_temp, col = log(bz_cases + 1)), cex = 23) +
  theme_bw() +
  scale_fill_viridis_d()

interp_data <- with(dynamics_hm , {
  interp(x = R/N, y = bz_temp, z = log(bz_cases + 1), duplicate = "mean", linear = FALSE)
})
interp_df <- as.data.frame(expand.grid(x = interp_data$x, y = interp_data$y))
interp_df$z <- as.vector(interp_data$z)

hm_dino = ggplot(data = interp_df %>% drop_na(), aes(x = x, y = y, fill = z)) +
  geom_raster() +
  scale_fill_viridis_c(name = "log cases") +
  theme_bw() + 
  ylab("Temperature") + xlab("Fraction immune") +
  ggtitle("High Mountain")
hm_dino

#### dataset 

fitted_bz_combined = rbind(out_df_check_t %>% mutate(bz = "Tarai"), 
                           out_df_check_h %>% mutate(bz = "Hill"),
                           out_df_check_hm %>% mutate(bz = "High Mountain"),
                           out_df_check_mm %>% mutate(bz = "Middle Mountain"),
                           out_df_check_s %>% mutate(bz = "Siwalik"))
head(fitted_bz_combined)


#################################
plot_grid(tarai_cal , siwalik_cal,hill_cal, middlemountain_cal, highmountain_cal, ncol = 3)
plot_grid(hm_dino, tarai_dino, midmount_dino, hill_dino, siwalik_dino)


plot_grid(tarai_foi, siwalik_foi, hill_foi, midmount_foi, hm_foi )

############################################################
# Make figure 1 
library(cowplot)
strip_axes <- function(p, keep_x = FALSE, keep_y = FALSE) {
  p + theme(
    axis.title.x = if (keep_x) element_text(size = 20) else element_blank(),
    axis.title.y = if (keep_y) element_text(size = 20) else element_blank() )}

tarai_cal         <- strip_axes(tarai_cal, keep_x = FALSE, keep_y = TRUE)
siwalik_cal       <- strip_axes(siwalik_cal, keep_x = FALSE, keep_y = FALSE)
hill_cal          <- strip_axes(hill_cal, keep_x = FALSE, keep_y = FALSE)

middlemountain_cal <- strip_axes(middlemountain_cal, keep_x = FALSE, keep_y = TRUE)
highmountain_cal   <- strip_axes(highmountain_cal, keep_x = FALSE, keep_y = FALSE)

shared_legend <- get_legend(
  highmountain_cal + theme(legend.position = "bottom"))

remove_legend <- function(p) p + theme(legend.position = "none")

tarai_cal          <- remove_legend(tarai_cal)
siwalik_cal        <- remove_legend(siwalik_cal)
hill_cal           <- remove_legend(hill_cal)
middlemountain_cal <- remove_legend(middlemountain_cal)
highmountain_cal   <- remove_legend(highmountain_cal)

grid <- plot_grid(
  tarai_cal, siwalik_cal, hill_cal,
  middlemountain_cal, highmountain_cal,
  ncol = 3)

final_plot_fig2a <- plot_grid(
  grid,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1))

fig2a = final_plot_fig2a + 
  labs(tag = "
    A") +
  # labs(tag = "A: SEIRS models predict bioclimatic zone dengue dynamics.") + 
  theme( plot.tag = element_text(size = 20, face = "bold"))
fig2a 
