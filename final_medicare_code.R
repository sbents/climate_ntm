# Final CMS Code 
setwd("C:/Users/bentssj/OneDrive - National Institutes of Health/Year_2024/cms")
############################################################################
############### June 29, 2024 ############################################
library(cowplot)
library(dplyr)
library(ggplot2)
#########################################################################

# Load numerator and denominator data. 
num_dat =  read.csv("region_inc_june28.csv") # 2 claims >1 day and <12 months case definition
denom_dat =  read.csv("region_year_counts_04242024.csv") %>%
  dplyr::select(year, region, denom_count) 
# Join data.  
inc_dat = left_join(num_dat, denom_dat, by  = c("year", "region"))

# Calculate total incident cases over study period.  
print(sum(inc_dat$inc_count)) #59724

#Summarize regional data into national. 
ma = c(75, 74, 74, 74, 74, 73, 73, 73, 73, 73) # national median age from 2010-2019, from Collin.
nat_inc = inc_dat %>% 
  group_by(year) %>%
  summarize(across(inc_count:denom_count, ~ sum(.x))) %>%
  mutate(region = "National") %>%
  mutate(median_age = ma)

# Calculate average annual incidence. 
sum(nat_inc$inc_count)/sum(nat_inc$denom_count)*100000

# Calculate annual percent change. 
summary = glm(inc_count ~ year + median_age, offset = log(denom_count), data
              = nat_inc, family = poisson(link = "log"))
print(summary) # .0178
print(confint(summary)) # (.0117, .0239)

# Plot regional and national time series. 
plot_dat = rbind(inc_dat, nat_inc %>% dplyr::select(-median_age)) %>%
  mutate(incidence = (inc_count/denom_count)*100000, Region = region)
plot_geo = ggplot(data = plot_dat, aes(x = year, y = incidence, color = Region)) +
  geom_line(lwd = 2) +
  theme_bw() + 
  ylab("Incidence per 100k") +
  xlab("Year") +
  scale_color_manual(values = c("tomato4","black" , "tomato", "lightsteelblue2",  "royalblue1"))+
  scale_x_continuous(breaks = 2010:2019 ,labels = c("2010", "2011", "2012", "2013", "2014", "2015",
                                                    "2016", "2017", "2018", "2019"))  +
  theme(legend.position="bottom")
plot_geo

# Read in demographic data. 
demographics = read.csv("his_07172024.csv")

age_plot = ggplot(data = demographics) +
  geom_line(aes(x = year, y = (age_65_69/denom_age_65_69)*100000,  col = "65-69"), lwd = 1.7) +
  geom_line(aes(x = year, y = (age_70_79/denom_age_70_79)*100000, col = "70-79"), lwd = 1.7) +
  geom_line(aes(x = year, y = (age_80_plus/denom_age_80_plus)*100000, col = "80+"), lwd = 1.7) +
  theme_bw() +
  scale_colour_manual(name = "Age", values = c("65-69" = "tomato4", "70-79" = "tomato", "80+" ="peachpuff" )) +
  theme(legend.position="bottom")+
  ylim(c(0, 56)) +
  scale_x_continuous(breaks = 2010:2019 ,labels = c("2010", "2011", "2012", "2013", "2014", "2015",
                                                    "2016", "2017", "2018", "2019")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())
age_plot 

sex_plot = ggplot(data = demographics, aes(x = year, y = (sex_fml/denom_sex_fml)*100000, col = "Female"), lwd = 1.7)+
  geom_line(lwd = 1.7) +
  geom_line(data = demographics, aes(x = year, y = (sex_ml/denom_sex_ml)*100000, col = "Male"), lwd = 1.7) +
  theme_bw() +
  ylab("Incidence per 100k") +
  ylim(c(0, 56)) +
  scale_color_manual(name = "Sex", values = c("Female" = "salmon2", "Male" = "salmon4"))+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks = 2010:2019 ,labels = c("2010", "2011", "2012", "2013", "2014", "2015",
                                                    "2016", "2017", "2018", "2019")) +
  theme(axis.title.x = element_blank())
sex_plot

re_plot = ggplot(data = demographics, aes(x = year, y = (race_wht/denom_race_wht)*100000, col = "White"), lwd = 1.7)+
  geom_line(lwd = 1.7) +
  geom_line(data = demographics, aes(x = year, y = (race_his/denom_race_his)*100000, col = "Hispanic"), lwd = 1.7) +
  geom_line(data = demographics, aes(x = year, y = (race_blk/denom_race_blk)*100000, col = "Black"), lwd = 1.7) +
  geom_line(data = demographics, aes(x = year, y = (race_asi/denom_race_asi)*100000, col = "Asian"), lwd = 1.7) +
  theme_bw() +
  ylim(c(0, 56)) +
  scale_color_manual(name = "Race", values = c("Asian" = "lightsteelblue1", "Black" = "skyblue1", "Hispanic" = "royalblue1", "White" = "black"))+ 
  theme(legend.position="bottom")+
  scale_x_continuous(breaks = 2010:2019 ,labels = c("2010", "2011", "2012", "2013", "2014", "2015",
                                                    "2016", "2017", "2018", "2019")) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank())
re_plot

fig1_dem = plot_grid(plot_geo, age_plot, sex_plot, re_plot, labels = c('a', 'b', 'c', 'd'), nrow =2) # Figure 2 for manuscript
fig1_dem

#### Calculate differences between demographic groups.
# Age
baseline_age = print(demographics$age_65_69[10]/demographics$denom_age_65_69[10])
a1 = print(demographics$age_70_79[10]/demographics$denom_age_70_79[10])
a2 = print(demographics$age_80_plus[10]/demographics$denom_age_80_plus[10])

a2/baseline_age # 3.3
a2/a1 #1.3 

# Sex 
baseline_sex = print(demographics$sex_ml[10]/demographics$denom_sex_ml[10])
s1 = print(demographics$sex_fml[10]/demographics$denom_sex_fml[10])
s1/baseline_sex

# Race/ethnicity
baseline_race = print(demographics$race_asi[10]/demographics$denom_race_asi[10])
r1 = print(demographics$race_wht[10]/demographics$denom_race_wht[10])
r2 = print(demographics$race_blk[10]/demographics$denom_race_blk[10])
r3 = print(demographics$race_his[10]/demographics$denom_race_his[10])

baseline_race/r1 # 1.4
baseline_race/r2 # 4.3
baseline_race/r3 # 3.1 


###################################################################
# Comorbidities analysis. 
co = read.csv("comorbid19_july10.csv") %>%
  drop_na(percent) %>%
  filter(NTM != "<11") %>%
  mutate(disease = replace(disease, disease == "inter_lung_dis", "Interstitial Lung Disease")) %>%
  mutate(disease = replace(disease, disease == "Alpha 1 Anti-Trypsin", "Alpha 1 Anti-Trypsin Deficiency")) %>%
  mutate(disease = replace(disease, disease == "Cocci", "Coccidioidomycosis")) %>%
  filter(disease != "Disseminated Nontuberculous Mycobacterial Disease") %>%
  filter(disease != "Emphysema") 

# Separate out by population size for plotting.
co_pop = co %>% dplyr::select(disease, percent) %>%
  mutate(Population = "Medicare population") %>%
  mutate(percent = as.numeric(percent))
co_ntm = co %>% dplyr::select(disease, percent.1) %>%
  mutate(percent = as.numeric(percent.1)) %>%
  dplyr::select(-percent.1) %>%
  mutate(Population = "NTM population") 
comor = rbind(co_pop, co_ntm) %>%
  arrange(Population, -percent)

# Order conditions.
list = c("Bronchiectasis", "Chronic Obstructive Pulmonary Disease (COPD)",
         "Gastroesophageal Reflux Disease (GERD)",  "Asthma",  "Interstitial Lung Disease",
         "Diabetes", "Lung Malignancy",  "Rheumatoid Arthritis", "Pulmonary Tuberculosis",
         "Acquired Immunodeficiency", "Sjögren's Syndrome", "Idiopathic Pulmonary Fibrosis", "Primary Immunodeficiency", 
         "Inflammatory Bowel Disease", "Allergic Bronchopulmonary Aspergillosis", "Sarcoidosis", "Coccidioidomycosis", "AIDS", "Alpha 1 Anti-Trypsin Deficiency", "Cystic Fibrosis")
list1 = rev(list)
print(list1)

# Comorbidity plot. 
fig_2a =  ggplot(data = comor) +
  geom_col(aes(x = factor (disease, levels =  c (print(list1)
  )), y = percent, fill  = Population), position = "dodge") + 
  theme_bw() + 
  ylab("Percent of population (%)") +
  xlab("Disease") +
  scale_fill_manual(values = c("lightsteelblue2", "tomato")) +
  coord_flip() +
  theme(
    axis.title.y = element_blank()
  ) +
  theme(legend.position="bottom")  
fig_2a

# Risk ratio plot. 
fig_2b = ggplot(data = co %>% mutate(RR = as.numeric(RR)) %>% filter(RR < 125)  %>% mutate(RR = round(RR, digits =1))) +
  geom_col(aes(x = reorder(disease, RR), y = as.numeric(RR)), fill = "blue4") + 
  theme_bw() + 
  ylab("Relative prevalence") +
  xlab("Disease") +
  coord_flip() +
  theme(
    axis.title.y = element_blank()) +
  geom_text(aes(x = disease, y = RR, label = RR), size = 3, hjust = -.25, vjust = .5, position = "stack")
fig_2b

fig_2 = plot_grid(fig_2a, fig_2b, nrow = 2, labels = c("a", "b"), rel_heights = c(1.25, 1))
fig_2

# Calculate annual percent change for region, sex, and race. 
# Load in median age data. 
median_age = read.csv("medianage_06202024.csv")
apc_dat = left_join(plot_dat, median_age, by = c("year", "region") ) %>%
  filter(region != "National")

geo = print(unique(apc_dat$region))

for(i in geo){
  print(i)
  summary = glm(inc_count ~ year + median_age, offset = log(denom_count), data
                = apc_dat %>% filter(region == i), family = poisson(link = "log"))
  print(summary)
  print(confint(summary))
}

# Read in median age for race/ethnicity.
# Race, here 1 = White, 2 = Black, 4 = Asian, 5 = Hispanic
ma_re = read.csv("mage_race_07182024.csv") %>%
  mutate(year =ï..year )

# White 
white = demographics %>%
  dplyr::select(race_wht,denom_race_wht, year ) %>%
  left_join(ma_re %>% filter(bene_race_cd == 1), by = "year")
summary = glm(race_wht ~ year + median_age, offset = log(denom_race_wht), data = white, family = poisson(link = "log"))
print(summary)
print(confint(summary))

# Hispanic 
hispanic = demographics %>%
  dplyr::select(race_his,denom_race_his, year ) %>%
  left_join(ma_re %>% filter(bene_race_cd == 5), by = "year")
summary = glm(race_his ~ year + median_age, offset = log(denom_race_his), data = hispanic, family = poisson(link = "log"))
print(summary)
print(confint(summary))

# Black
black = demographics %>%
  dplyr::select(race_blk,denom_race_blk, year ) %>%
  left_join(ma_re %>% filter(bene_race_cd == 2), by = "year")
summary = glm(race_blk ~ year + median_age, offset = log(denom_race_blk), data = black, family = poisson(link = "log"))
print(summary)
print(confint(summary))

# Asian 
asian = demographics %>%
  dplyr::select(race_asi,denom_race_asi, year ) %>%
  left_join(ma_re %>% filter(bene_race_cd == 4), by = "year")
summary = glm(race_asi ~ year + median_age, offset = log(denom_race_asi), data = asian, family = poisson(link = "log"))
print(summary)
print(confint(summary))

############################ sex 
# Load median age for sex. Here 2 = female, 1 = male. 
ma_s = read.csv("mage_sex_07182024.csv") %>%
  mutate(year =ï..year )

# Female
fem = demographics %>%
  dplyr::select(sex_fml,denom_sex_fml, year ) %>%
  left_join(ma_s %>% filter(sex == 2), by = "year")
summary = glm(sex_fml ~ year + median_age, offset = log(denom_sex_fml), data = fem, family = poisson(link = "log"))
print(summary)
print(confint(summary))

# Male
mal = demographics %>%
  dplyr::select(sex_ml,denom_sex_ml, year ) %>%
  left_join(ma_s %>% filter(sex == 1), by = "year")
summary = glm(sex_ml ~ year + median_age, offset = log(denom_sex_ml), data = mal, family = poisson(link = "log"))
print(summary)
print(confint(summary))


########################################################################
# Plots for sensitivity analyses. 
oneclaim = read.csv("region_year_counts_04242024.csv")
region_overall = oneclaim %>%
  group_by(year) %>%
  dplyr::summarize(across(denom_count:race_asi, ~sum(.x))) %>%
  mutate(region = "National")
sum(region_overall$cases_count)

# bind all data together 
total_data = rbind(oneclaim, region_overall) %>%
  mutate(rate_per100k = cases_count/denom_count*100000)
total_data$region = factor(total_data$region, levels = c("National", "Northeast", "West", "South", "Midwest"))

t1 = ggplot(data = total_data, aes(x = year, y = (cases_count/denom_count)*100000, col = region))+
  geom_line(lwd = 2) +
  theme_bw() +
  ylab("Incidence per 100k") + 
  xlab("Year") +
  scale_color_manual(values = c("tomato4","black" , "tomato", "lightsteelblue2",  "royalblue1" )) +
  guides(col=guide_legend(title="Region")) +
  scale_x_continuous(breaks = 2010:2019 ,labels = c("2010", "2011", "2012", "2013", "2014", "2015",
                                                    "2016", "2017", "2018", "2019"))  +
  theme(axis.title.x = element_blank()) +
  theme(legend.position="bottom")
t1 # Figure 1 for manuscript sensitivity analysis

# Claims data from Collin. 

# Claims per unique individual. 
claims = c(seq(1, 11,1))
count = c(26376, 12481, 8656, 6640, 5210, 4352, 
          3597, 3154, 2826, 2527, 24692)
all_claims = data.frame(claims, count)

ggplot(data = all_claims) +
  geom_col(aes(x = claims, y = count/sum(count)), fill = "royalblue1") +
  theme_bw() +
  xlab("Claims per unique individual") +
  ylab("Percent") + ggtitle("2019 incident cases based one 1 claim defintion")

# Days between claims for 1 vs 2 claims. 
# 1 claim
claims = seq(950, 2700, 250)
count = c(1089, 1289, 1191, 927, 652, 502, 337, 198)
all_claims = data.frame(claims, count)
ct1 = ggplot(data = all_claims) +
  geom_col(aes(x = claims, y = count), fill = "tomato2") +
  theme_bw() +
  xlab("Days between claims") +
  ylab("Frequency") + 
  ggtitle("1 claim")

# 2 claims
claims = seq(950, 2700, 250)
count = c(251, 485, 485, 400, 303, 256, 174, 115)
all_claims = data.frame(claims, count)
ct2 = ggplot(data = all_claims) +
  geom_col(aes(x = claims, y = count), fill = "tomato2") +
  theme_bw() +
  xlab("Days between claims") +
  ylab("Frequency") +ggtitle("2 claims")

plot_grid(ct1, ct2)

