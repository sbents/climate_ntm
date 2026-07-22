library(here)
source(here("0-config.R"))
source(here("0-functions.R"))

########################################################
#_______________________________________________________
# @Organization --  UCSF 
# @Project -- Multipathogen Analysis 
# @Author -- Samantha Bents, sjbents@stanford.edu, May 9, 2026 
# @Description -- Let's see where this goes! 
########################################################


########################################################
# Pull in data 
########################################################
# 1. Bangladesh multiplex serology 

# Read in public ids 
public_ids = read.csv(file = here("data/bangl/public_ids", "public-ids.csv")) %>%
  distinct(dataid, clusterid, block, clusterid_r, block_r)
head(public_ids)

# Read in GPS data to join to ids to orient blocks, public blocks, and their locations
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 
head(gps_dat)

# Joing this to cluster information
public_id_cluster = public_ids %>%
  distinct(clusterid, block, block_r, clusterid_r) %>%
  left_join(gps_dat, by = c("block", "block_r"))
head(public_id_cluster)

# Define what is a vaccine vs pathogen antigen 
antigen_vax = c("Rubella", "Measles", "Tetanus", "Diptheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )

# Load in multiplex serology 
  # remove pathogens with no signal- schisto control and covid  
  # define seropositivity based on cutoff 
  # only visit 3 data 
luminex_bangl <- read.csv(file = here("data/bangl/luminex/final", 
                                      "washb_bangl_luminex_igg_seropos_2025-09-21.csv")) %>%
  left_join(public_id_cluster, by = c("clusterid")) %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST")) %>%
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(mfi, agemonth, clusterid, childid, antigen, pathogen, seropos, block, block_r, med_qgpslong, med_qgpslat) 

# Check prevalences of invdividual antigens 
prevalence_check_bangl = luminex_bangl %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_bangl)
print(unique(prevalence_check_bangl$antigen))
print(unique(prevalence_check_bangl$pathogen))

prev_antigen_bangl = prevalence_check_bangl  %>%
  distinct(antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
bangl_targeting_individual = prevalence_check_bangl %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
      any(antigen == "norogi5"   & seropos == 1) &
        any(antigen == "norogii4"  & seropos == 1) &
        any(antigen == "norogii3"  & seropos == 1) &
        any(antigen == "norogii17" & seropos == 1) &
        any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Tetanus" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus", "Campylobacter","Salmonella") & seropos ==1, 1 , 0 )) %>%
  summarise(vpd = max(vpd), wash_water = max(wash_water), wash_fh = max(wash_fh), across(c(block, block_r, med_qgpslong, med_qgpslat), first)) %>%
  ungroup() %>%
  distinct(childid, block, block_r, med_qgpslong, med_qgpslat, vpd, wash_water, wash_fh) %>%
  pivot_longer(cols = c(vpd, wash_water, wash_fh), names_to = "intervention", values_to = "intervention_needed")
head(bangl_targeting_individual)
length(unique(bangl_targeting_individual$childid))

  
# targeting need by cluster 
bangl_targeting_cluster = bangl_targeting_individual %>% 
  group_by(block, intervention) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(intervention_needed, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, intervention, non_na_denom, num, fraction) %>%
  mutate(location = "Bangladesh") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block)
head(bangl_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = bangl_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")


# add Bangl STH 
sth = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
  mutate(need_int = ifelse(al == 1 | tt == 1 | hw == 1, 1, 0)) %>%
  group_by(block) %>%
  summarise(non_na_denom = n_distinct(dataid, personid), num   = sum(need_int, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, non_na_denom, num, fraction)  %>%
  mutate(intervention = "mda", location = "Bangladesh")  %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block)
head(sth)

bangl_targeting_cluster = rbind(bangl_targeting_cluster , sth)

# plot histogram of distribution of intervention need again 
ggplot(data = bangl_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")

  

########################################################
# 2. Kenya multiplex serology 

treatment_assignment = read.csv(file = here("data/kenya/public_ids", 
                                            "cluster_tx_masked.csv")) 

treatment_assignment = read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)
head(treatment_assignment)

antigen_vax = c("Rubella", "Measles", "Tetanus", "Diptheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )

luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                     "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST")) %>%
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(clusterid, childid, antigen, pathogen, seropos, block) 

prevalence_check_kenya = luminex_kenya %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_kenya)
print(unique(prevalence_check_kenya$antigen))
print(unique(prevalence_check_kenya$pathogen))

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
kenya_targeting_individual = prevalence_check_kenya %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus","Campylobacter","Salmonella", "E. histolytica", "shigella") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("P. malariae" , "P. falciparum", "P. ovale") & seropos ==1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("T. solium" , "Schistosomiasis", "Trachoma") & seropos == 1, 1, 0)) %>%
  summarise(vpd = max(vpd), wash_water = max(wash_water), wash_fh    = max(wash_fh), malaria = max(malaria),
            mda = max(mda),
            across(c(block), first)) %>%
  ungroup() %>%
  distinct(childid, block, vpd, wash_water, wash_fh, malaria, mda) %>%
  pivot_longer(cols = c(vpd, wash_water, wash_fh, malaria, mda), names_to = "intervention", values_to = "intervention_needed")
head(kenya_targeting_individual)

# targeting need by cluster 
kenya_targeting_cluster = kenya_targeting_individual %>% 
  group_by(block, intervention) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(intervention_needed, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num / non_na_denom, location = "Kenya", spatial_cluster = block ) %>%
  dplyr::select(-block) 
head(kenya_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = kenya_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Kenya")


########################################################
# 3. Cambodia multiplex serology 

# Load disease data. 
cambodia_serology_public = readr::read_csv(file = here("data/cambodia", "cambodia_serology_public.csv")) %>%
  mutate(womanid = ...1)
head(cambodia_serology_public)
#ttmb     : Tetanous toxoid, bm14     : Lymphatic filariasis bm14, bm33     : Lymphatic filariasis bm33, wb123    : Lymphatic filariasis wb123
#nie      : Strongyloides stercoralis NIE, sag2a    : Toxoplasma gondii SAG2A , t24      : Taenia solium T24, pfmsp19  : Plasmodium falciparum MSP-1(19)
#pvmsp19  : Plasmodium vivax MSP-1(19)

# Load location data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
gps_dat = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon)
head(gps_dat)

# Transform into seropositive vs seronegative using: https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004699#pntd-0004699-t001
# https://journals.asm.org/doi/10.1128/cvi.00052-16#T1 for tetanus
antigen = c("ttmb", "bm14", "wb123", "bm33", "nie", "sag2a", "t24", "pfmsp19", "pvmsp19")
cutoff = c(100, 65, 115, 966,792, 159, 486, 343, 196 )
pathogen = c("Tetanus", "Lymphatic filariasis", "Lymphatic filariasis", "Lymphatic filariasis", 
             "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
             "Plasmodium vivax")
antigen_path_dat = data.frame(antigen, cutoff, pathogen)

antigen_vax = c( "Tetanus")
antigen_path = c("Lymphatic filariasis", "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
                 "Plasmodium vivax")

prevalence_check_cam = cambodia_serology_public %>%
  pivot_longer(cols = ttmb:gst, names_to = "antigen", values_to = "value") %>%
  filter(antigen != "gst") %>%
  filter(antigen != "bm33") %>% # we dont want this in our defintion
  mutate(psuid = psuid + 1) %>% 
  left_join(antigen_path_dat, by = "antigen") %>%
  mutate(seropos = ifelse(value > cutoff & pathogen %in% antigen_path , 1, 
                          ifelse(value < cutoff & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(womanid, psuid, age, parity, antigen, pathogen, seropos) %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90) 
head(prevalence_check_cam) 
print(unique(prevalence_check_cam$antigen))
print(unique(prevalence_check_cam$pathogen))

# Targeting groups by individual
# make sure to use two antigen positvity defintion for LF
cam_targeting_individual = prevalence_check_cam %>%
  group_by(womanid) %>%
  mutate(lf_seropos = ifelse(any(antigen == "bm14" & seropos == 1) & any(antigen == "wb123" & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Lymphatic filariasis", lf_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Tetanus") & seropos == 1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("Strongyloides stercoralis" , "Lymphatic filariasis" ) & seropos == 1, 1, 0)) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Toxoplasma gondii") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("Plasmodium vivax", "P. falciparum") & seropos ==1, 1, 0)) %>%
  summarise(vpd = max(vpd), wash_fh = max(wash_fh), malaria = max(malaria), mda = max(mda),
            across(c(psuid), first)) %>%
  ungroup() %>%
  distinct(womanid, psuid, vpd, wash_fh, malaria, mda) %>%
  pivot_longer(cols = c(vpd, wash_fh, malaria, mda), names_to = "intervention", values_to = "intervention_needed")
head(cam_targeting_individual)

# targeting need by cluster 
cam_targeting_cluster = cam_targeting_individual %>% 
  group_by(psuid, intervention) %>%
  summarise(non_na_denom = n_distinct(womanid), num   = sum(intervention_needed, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num / non_na_denom, location = "Cambodia", spatial_cluster = psuid ) %>%
  dplyr::select(-psuid)
head(cam_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = cam_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Cambodia")

########################################################
# Join data together 

multipathogen_indices = rbind(cam_targeting_cluster, 
                              kenya_targeting_cluster,
                              bangl_targeting_cluster) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup()
head(multipathogen_indices )


########################################################
# Compare targeting by multipathogen index 

# diversity functions 
shannon_entropy <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  -sum(p * log(p))}

gini_simpson <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  1 - sum(p^2)}

alpha_diversity <- function(p) sum(p > 0)

rao_quadratic <- function(p) {
  if (length(p) < 2) return(0)
  sum(sapply(combn(p, 2, simplify = FALSE), prod))}

# make data into wide, one row per spatial_cluster per location, columns = interventions
cluster_wide <- multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, intervention, fraction) %>%
  pivot_wider(names_from = intervention, values_from = fraction, values_fill = 0)

# function to calculate the minimun number of clusters needed to target at least 80% of each disease
#min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
#                                      rank_col, interventions_in_combo,
#                                      threshold = 0.80) {
#  
#  # Use fraction_rao for rao_quadratic, fraction for everything else
#  frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
#  
#  total_burden <- cluster_prev_sub %>%
#    filter(intervention %in% interventions_in_combo) %>%
#    group_by(intervention) %>%
#    # summarise(total = sum(num, na.rm = TRUE), .groups = "drop")
#   # summarise(total = sum(fraction, na.rm = TRUE), .groups = "drop")
#    summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
#  
#  ranked_clusters <- cluster_scores_sub %>%
#    arrange(desc(.data[[rank_col]])) %>%
#    pull(spatial_cluster)
#  
#  cumulative <- tibble(intervention = interventions_in_combo, cum_burden = 0)
#  
#  for (i in seq_along(ranked_clusters)) {
#    clust <- ranked_clusters[i]
#    this_cluster <- cluster_prev_sub %>%
#      filter(spatial_cluster == clust, intervention %in% interventions_in_combo) %>%
#      dplyr::select(intervention, fraction)
#    #  dplyr::select(intervention, fraction, num)
#    
#    cumulative <- cumulative %>%
#      left_join(this_cluster, by = "intervention") %>%
#      mutate(cum_burden = cum_burden + replace_na(fraction, 0)) %>%
#     # mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
#      dplyr::select(intervention, cum_burden)
#    
#    pct_reached <- cumulative %>%
#      left_join(total_burden, by = "intervention") %>%
#      mutate(pct = cum_burden / total)
#    
#    if (all(pct_reached$pct >= threshold)) {
#      return(tibble(n_clusters = i, rank_method = rank_col))
#    } }
# return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
#}

min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
                                      rank_col, interventions_in_combo,
                                      threshold = 0.80) {
  
  # Use fraction_rao for rao_quadratic, fraction for everything else
 # frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
  frac_col <- if (rank_col == "rao_quadratic") "fraction" else "fraction"
  
  total_burden <- cluster_prev_sub %>%
    filter(intervention %in% interventions_in_combo) %>%
    group_by(intervention) %>%
  #  summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
    summarise(total = sum(.data[["num"]], na.rm = TRUE), .groups = "drop")
  
  
  ranked_clusters <- cluster_scores_sub %>%
    arrange(desc(.data[[rank_col]])) %>%
    pull(spatial_cluster)
  
  cumulative <- tibble(intervention = interventions_in_combo, cum_burden = 0)
  
  for (i in seq_along(ranked_clusters)) {
    clust <- ranked_clusters[i]
    this_cluster <- cluster_prev_sub %>%
      filter(spatial_cluster == clust, intervention %in% interventions_in_combo) %>%
      dplyr::select(intervention, all_of(frac_col), num) %>%
      rename(frac_val = all_of(frac_col))
    
    cumulative <- cumulative %>%
      left_join(this_cluster, by = "intervention") %>%
     # mutate(cum_burden = cum_burden + replace_na(frac_val, 0)) %>%
      mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
      dplyr::select(intervention, cum_burden)
    
    pct_reached <- cumulative %>%
      left_join(total_burden, by = "intervention") %>%
      mutate(pct = cum_burden / total)
    
    if (all(pct_reached$pct >= threshold)) {
      return(tibble(n_clusters = i, rank_method = rank_col))
    }
  }
  return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
}

# run across all locations for combinations of pathogens with at least 3 intervetions 
rank_methods <- c("shannon", "alpha_div", "gini_simpson", "rao_quadratic")

results_all <- map_dfr(unique(multipathogen_indices$location), function(loc) {
  
  loc_data <- multipathogen_indices %>% filter(location == loc)
  all_interventions <- unique(loc_data$intervention)
  n_interventions <- length(all_interventions)
  
  # All combinations of size 3 up to n_interventions
  combos <- map(2:n_interventions, ~ combn(all_interventions, .x, simplify = FALSE)) %>%
    flatten()
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Total clusters in location — calculated from full loc_data, not the combo subset
    n_total_clusters <- n_distinct(loc_data$spatial_cluster)
    
    cluster_prev_sub <- loc_data %>% filter(intervention %in% combo)
    
    # Recompute diversity scores for this combo only
    cluster_scores_sub <- cluster_wide %>%
      filter(location == loc) %>%
      dplyr::select(spatial_cluster, any_of(combo)) %>%
      rowwise() %>%
      mutate(shannon      = shannon_entropy(c_across(all_of(combo))),
        alpha_div    = alpha_diversity(c_across(all_of(combo))),
        gini_simpson = gini_simpson(c_across(all_of(combo))),
        rao_quadratic = rao_quadratic(c_across(all_of(combo)))  ) %>%
      ungroup()
    
    n_total_clusters <- n_distinct(cluster_prev_sub$spatial_cluster)
    
    map_dfr(rank_methods, function(rm) {
      res <- min_clusters_to_threshold(cluster_prev_sub      = cluster_prev_sub,
        cluster_scores_sub    = cluster_scores_sub,
        rank_col              = rm,
        interventions_in_combo = combo )
      res %>% mutate(location = loc,
        combo = combo_label,
        n_total_clusters = n_total_clusters,
        prop_clusters   = n_clusters / n_total_clusters )
      })
  })
})

# ---------------------------------------------------------------
# PLOT: distribution of proportion of clusters needed, facet by location
# ---------------------------------------------------------------

method_labels <- c(shannon      = "Shannon entropy",
  alpha_div    = "Alpha diversity",
  gini_simpson = "Gini-Simpson",
  rao_quadratic = "Rao's quadratic entropy")

results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = fct_reorder(rank_method, prop_clusters, median, na.rm = TRUE)) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 21, width = 0.5) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    title    = "Proportion of clusters needed to reach 80% coverage",
    # subtitle = "Across all intervention combinations of ≥3 groups",
    x        = NULL,
    y        = "Proportion of spatial clusters required" ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = factor(rank_method, levels = c(
    "Rao's quadratic entropy",
    "Shannon entropy", 
    "Gini-Simpson",
    "Alpha diversity"))) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
 # stat_summary(fun = mean, geom = "bar", alpha = 0.7, width = 0.6) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.6, shape = 16) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    #title = "Proportion of clusters needed to reach 80% coverage",
    x     = NULL,
    y     = "Proportion of clusters to reach 80% coverage"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0.60, .86)) +
  scale_fill_viridis_d(option = "D" , end = .7 , name = "Strategy", alpha = .66) +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    legend.position = "bottom") 



#################################### Map out all Rao surfaces 
# Bangl 
gps_dat_bangl = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat)  %>%
  mutate(spatial_cluster = block_r)
head(gps_dat_bangl)

bangl_rao_clusters = left_join(bangl_targeting_cluster, gps_dat_bangl, by = "spatial_cluster") %>%
  mutate(lat = med_qgpslat, long = med_qgpslong) %>%
  dplyr::select(-block, -block_r, -med_qgpslat, -med_qgpslong ) 
head(bangl_rao_clusters)

# Cam
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 

gps_dat_cam = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon) %>%
  mutate(spatial_cluster = psuid)
head(gps_dat_cam)

cam_rao_clusters = left_join(cam_targeting_cluster, gps_dat_cam, by = "spatial_cluster") %>%
  mutate(lat = dhslat, long = dhslon) %>%
  dplyr::select(-psuid, -dhslat, -dhslon ) 
head(cam_rao_clusters)

# Kenya 
gps_dat_kenya = readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat))  %>%
  distinct(block, long, lat) %>%
  mutate(spatial_cluster = block) %>%
  ungroup() %>%
  dplyr::select(-block)
head(gps_dat_kenya)

kenya_rao_clusters = left_join(kenya_targeting_cluster, gps_dat_kenya, by = "spatial_cluster") 
head(kenya_rao_clusters)

###################################
# Put together 

dat_locations = rbind(bangl_rao_clusters, cam_rao_clusters, kenya_rao_clusters) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup() %>% 
  drop_na()
head(dat_locations)

####################################
# Map 

# Calculate Rao per spatial cluster using all interventions present
rao_by_cluster = dat_locations %>%
  group_by(location, spatial_cluster, lat, long) %>%
  summarise(
    rao = rao_quadratic(fraction),
   # rao = rao_quadratic(fraction_rao),
    .groups = "drop") %>%
  group_by(location) %>%
  mutate(max_rao = max(rao)) %>%
  mutate(rao = rao/max_rao) %>%
  ungroup() %>%
  drop_na()

# Calculate Moran's I per location
library(spdep)
head(rao_by_cluster)
morans_by_location = rao_by_cluster %>%
  group_by(location) %>%
  group_modify(~ {
    coords <- as.matrix(.x[, c("long", "lat")])
    nb     <- knn2nb(knearneigh(coords, k = 6))
    lw     <- nb2listw(nb, style = "W")
    mi     <- moran.test(.x$rao, lw)
    tibble(moran_i = mi$estimate[["Moran I statistic"]])
  }) %>%
  ungroup() %>%
  mutate(label = paste0("Moran's I = ", formatC(moran_i, digits = 2, format = "f")))

# Join labels back for plotting
rao_plot_dat = left_join(rao_by_cluster, morans_by_location, by = "location")

# Plot
ggplot(rao_by_cluster, aes(x = long, y = lat, color = rao)) +
  geom_point(size = 8, alpha = 0.9) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nentropy") +
  facet_wrap(vars(location), scales = "free") +
  geom_text(data = morans_by_location,
            aes(label = label),
            x = Inf, y = Inf,
            hjust = 1.05, vjust = 1.5,
            inherit.aes = FALSE,
            size = 5.5, color = "black" ) +
  labs(
  #  title = "Spatial distribution of multipathogen burden diversity",
   # subtitle = "Rao's quadratic entropy across spatial clusters",
    x = "Longitude",
    y = "Latitude") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x =  element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 14), 
    legend.position = "bottom") 


#### Part 2 ###################################
################################################
# plot effiency vs autocorrelation within treated units 
dat_locations = dat_locations %>% drop_na()

# Step 1: Efficiency + Rao surface + Moran's I, all per combo
results_combo = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data          <- dat_locations %>% filter(location == loc)
  all_interventions <- unique(loc_data$intervention)
  n_total           <- n_distinct(loc_data$spatial_cluster)
  combos            <- map(2:length(all_interventions), ~ combn(all_interventions, .x, simplify = FALSE)) %>% flatten()
  
  # GPS coords per cluster (fixed for this location)
  coords <- loc_data %>%
    distinct(spatial_cluster, lat, long) %>%
    arrange(spatial_cluster)
  nb <- knn2nb(knearneigh(as.matrix(coords[, c("long", "lat")]), k = 4))
  lw <- nb2listw(nb, style = "W")
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Rao surface: one value per cluster for this combo
    rao_surface <- loc_data %>%
      filter(intervention %in% combo) %>%
      group_by(spatial_cluster) %>%
      summarise(rao_val = rao_quadratic(fraction), .groups = "drop") %>%
      arrange(spatial_cluster)
    
    # Moran's I on this combo's Rao surface
    moran_i <- moran.test(rao_surface$rao_val, lw)$estimate[["Moran I statistic"]]
    
    # Efficiency: rank clusters by Rao, walk to 80%
    ranked <- rao_surface %>% arrange(desc(rao_val)) %>% pull(spatial_cluster)
    
    totals <- loc_data %>%
      filter(intervention %in% combo) %>%
      group_by(intervention) %>%
      summarise(total = sum(num, na.rm = TRUE), .groups = "drop")
    
    cumulative <- tibble(intervention = combo, cum = 0)
    
    n_clusters <- NA_integer_
    for (i in seq_along(ranked)) {
      this <- loc_data %>%
        filter(spatial_cluster == ranked[i], intervention %in% combo) %>%
        dplyr::select(intervention, num)
      
      cumulative <- cumulative %>%
        left_join(this, by = "intervention") %>%
        mutate(cum = cum + replace_na(num, 0)) %>%
        dplyr::select(intervention, cum)
      
      pct <- cumulative %>%
        left_join(totals, by = "intervention") %>%
        mutate(pct = cum / total)
      
      if (all(pct$pct >= 0.80)) {
        n_clusters <- i
        break
      }
    }
    
    tibble(location = loc, combo = combo_label,
           n_clusters = n_clusters,
           prop_clusters = n_clusters / n_total,
           moran_i = moran_i)
  })
})

# Step 2: Plot
ggplot(results_combo, aes(x = moran_i, y = prop_clusters, color = location)) +
  geom_point(size = 2.5, alpha = 0.6) +
  facet_wrap(vars(location), scales = "free_x") +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  labs(
    x     = "Spatial autocorrelation (Moran's I)",
    y     = "Proportion of clusters to reach 80% coverage",
    color = "Location") +
  scale_color_viridis_d(option = "D", end = 0.7) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme( strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x =  element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 14), 
    legend.position = "bottom") 

library(ggrepel)

# Identify points to label per location
to_label = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  group_by(location) %>%
  mutate(
    label = case_when(
      prop_clusters == min(prop_clusters) ~ combo,
      moran_i == max(moran_i)             ~ combo,
      TRUE                                ~ NA_character_ ) ) %>%
  ungroup()

ggplot(to_label, aes(x = moran_i, y = prop_clusters, color = location)) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  facet_wrap(vars(location), scales = "free_x") +
  geom_label_repel(data = filter(to_label, !is.na(label)),
    aes(label = label),
    size = 6, show.legend = FALSE,
    max.overlaps = 20 , col = "black") +
  labs( x  = "Spatial autocorrelation (Moran's I)",
    y     = "Proportion of clusters to reach 80% coverage",
    color = "Location" ) +
  scale_color_viridis_d(option = "D", end = 0.7) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme( strip.text = element_text(size = 15),
         plot.title = element_text(size = 14),
         axis.text.x =  element_text(size = 14),
         axis.text.y = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         axis.title.x = element_text(size = 14), 
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 14), 
         legend.position = "bottom") 
head(results_combo)

################################################
################################################
# Compare to a VPD focused intervention 

# Step 1: Rao ranking per location (same as before)
rao_ranking = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data <- dat_locations %>% filter(location == loc)
  
  loc_data %>%
    group_by(spatial_cluster) %>%
    summarise(rao_val = rao_quadratic(fraction), .groups = "drop") %>%
    arrange(desc(rao_val)) %>%
    mutate(rank_rao = row_number(), location = loc)})

# Step 2: VPD ranking per location (by cluster-level VPD fraction)
vpd_ranking = dat_locations %>%
  filter(intervention == "vpd") %>%
  group_by(location) %>%
  arrange(desc(fraction)) %>%
  mutate(rank_vpd = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, spatial_cluster, rank_vpd)

# Step 3: Build cumulative curves
cumulative_curves = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data    <- dat_locations %>% filter(location == loc)
  n_clusters  <- n_distinct(loc_data$spatial_cluster)
  
  # Add total as an extra intervention
  total_data <- loc_data %>%
    group_by(spatial_cluster) %>%
    summarise(num = sum(num, na.rm = TRUE), .groups = "drop") %>%
    mutate(intervention = "total")
  
  loc_data_with_total <- bind_rows(loc_data, total_data)
  
  rao_order <- rao_ranking %>%
    filter(location == loc) %>%
    arrange(rank_rao) %>%
    pull(spatial_cluster)
  
  vpd_order <- vpd_ranking %>%
    filter(location == loc) %>%
    arrange(rank_vpd) %>%
    pull(spatial_cluster)
  
  map_dfr(unique(loc_data_with_total$intervention), function(intv) {
    intv_data <- loc_data_with_total %>%
      filter(intervention == intv) %>%
      dplyr::select(spatial_cluster, num)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(intv_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    vpd_cum <- map_dbl(seq_along(vpd_order), function(i) {
      sum(intv_data %>% filter(spatial_cluster %in% vpd_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, intervention = intv,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated   = rao_cum,
             strategy      = "Rao"),
      tibble(location = loc, intervention = intv,
             frac_clusters = seq_along(vpd_order) / n_clusters,
             cum_treated   = vpd_cum,
             strategy      = "VPD")
    )
  })
})

# Step 4: Plot — make total visually distinct with thicker line
ggplot(cumulative_curves %>% filter(intervention != "total"), aes(x = frac_clusters, y = cum_treated,
                              color = intervention, linetype = strategy,
                              linewidth = intervention == "total")) +
  geom_line() +
  scale_linetype_manual(values = c("Rao" = "solid", "VPD" = "dashed")) +
  scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = 0.8), guide = "none") +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    x        = "Fraction of spatial clusters visited",
    y        = "Cumulative number of people treated",
    color    = "Intervention",
    linetype = "Strategy"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme( strip.text = element_text(size = 15),
         plot.title = element_text(size = 14),
         axis.text.x =  element_text(size = 14),
         axis.text.y = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         axis.title.x = element_text(size = 14), 
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 14), 
         legend.position = "bottom") 

######################################################
# Compare to just a single VPD 

# Step 1: Rao ranking per location (same as before)
rao_ranking = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data <- dat_locations %>% filter(location == loc)
  
  loc_data %>%
    group_by(spatial_cluster) %>%
    summarise(rao_val = rao_quadratic(fraction), .groups = "drop") %>%
    arrange(desc(rao_val)) %>%
    mutate(rank_rao = row_number(), location = loc)})

# Step 2: Single pathogen ranking per location
# Step 2: Single pathogen ranking per location
# Measles for Kenya and Bangladesh, Tetanus for Cambodia
# Pull from cluster-level fraction of that single pathogen

measles_bangl <- prevalence_check_bangl %>%
  filter(pathogen == "Measles") %>%
  group_by(block) %>%
  summarise(fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(location = "Bangladesh", spatial_cluster = block) %>%
  dplyr::select(-block)

measles_kenya <- prevalence_check_kenya %>%
  filter(pathogen == "Measles") %>%
  group_by(block) %>%
  summarise(fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(location = "Kenya", spatial_cluster = block) %>%
  dplyr::select(-block)

tetanus_cam <- prevalence_check_cam %>%
  filter(pathogen == "Tetanus") %>%
  group_by(psuid) %>%
  summarise(fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(location = "Cambodia", spatial_cluster = psuid) %>%
  dplyr::select(-psuid)

single_pathogen_ranking <- bind_rows(measles_bangl, measles_kenya, tetanus_cam) %>%
  group_by(location) %>%
  arrange(desc(fraction)) %>%
  mutate(rank_single = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, spatial_cluster, rank_single)


# Step 3: Build cumulative curves
cumulative_curves = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data    <- dat_locations %>% filter(location == loc)
  n_clusters  <- n_distinct(loc_data$spatial_cluster)
  
  # Add total as an extra intervention
  total_data <- loc_data %>%
    group_by(spatial_cluster) %>%
    summarise(num = sum(num, na.rm = TRUE), .groups = "drop") %>%
    mutate(intervention = "total")
  
  loc_data_with_total <- bind_rows(loc_data, total_data)
  
  rao_order <- rao_ranking %>%
    filter(location == loc) %>%
    arrange(rank_rao) %>%
    pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>%
    arrange(rank_single) %>%
    pull(spatial_cluster)
  
  map_dfr(unique(loc_data_with_total$intervention), function(intv) {
    intv_data <- loc_data_with_total %>%
      filter(intervention == intv) %>%
      dplyr::select(spatial_cluster, num)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(intv_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE) })
    
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(intv_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE) })
    
    bind_rows(
      tibble(location = loc, intervention = intv,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated   = rao_cum,
             strategy      = "Rao"),
      tibble(location = loc, intervention = intv,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated   = single_cum,
             strategy      = "Single pathogen"))
  })
})

# Step 4: Plot — make total visually distinct with thicker line
ggplot(cumulative_curves %>% filter(intervention != "total"), aes(x = frac_clusters, y = cum_treated,
                                                                  color = intervention, linetype = strategy,
                                                                  linewidth = intervention == "total")) +
  geom_line() +
  scale_linetype_manual(values = c("Rao" = "solid", "Single pathogen" = "dashed")) +
  scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = 0.8), guide = "none") +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    x        = "Fraction of spatial clusters visited",
    y        = "Cumulative number of people treated",
    color    = "Intervention",
    linetype = "Strategy" ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme( strip.text = element_text(size = 15),
         plot.title = element_text(size = 14),
         axis.text.x =  element_text(size = 14),
         axis.text.y = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         axis.title.x = element_text(size = 14), 
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 14), 
         legend.position = "bottom") 

###### cumulative fraction 
cumulative_curves = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data    <- dat_locations %>% filter(location == loc)
  n_clusters  <- n_distinct(loc_data$spatial_cluster)
  
  total_data <- loc_data %>%
    group_by(spatial_cluster) %>%
    summarise(num = sum(num, na.rm = TRUE), .groups = "drop") %>%
    mutate(intervention = "total")
  
  loc_data_with_total <- bind_rows(loc_data, total_data)
  
  rao_order <- rao_ranking %>%
    filter(location == loc) %>%
    arrange(rank_rao) %>%
    pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>%
    arrange(rank_single) %>%
    pull(spatial_cluster)
  
  map_dfr(unique(loc_data_with_total$intervention), function(intv) {
    intv_data   <- loc_data_with_total %>%
      filter(intervention == intv) %>%
      dplyr::select(spatial_cluster, num)
    
    total_need  <- sum(intv_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(intv_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE) / total_need
    })
    
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(intv_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE) / total_need
    })
    
    bind_rows(
      tibble(location = loc, intervention = intv,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_fraction  = rao_cum,
             strategy      = "Rao"),
      tibble(location = loc, intervention = intv,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_fraction  = single_cum,
             strategy      = "Single pathogen (Measles/Tetanus)") )
  })
})

ggplot(cumulative_curves %>% filter(intervention != "total"), aes(x = frac_clusters, y = cum_fraction,
                              color = intervention, linetype = strategy,
                              linewidth = intervention == "total")) +
  geom_line() +
  scale_linetype_manual(values = c("Rao" = "solid", "Single pathogen (Measles/Tetanus)" = "dashed")) +
  scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = 0.8), guide = "none") +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location, intervention), scales = "free_y") +
  labs(
    x        = "Fraction of spatial clusters visited",
    y        = "Cumulative fraction of people in need treated",
    color    = "Intervention",
    linetype = "Strategy" ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme( strip.text = element_text(size = 15),
         plot.title = element_text(size = 14),
         axis.text.x =  element_text(size = 14),
         axis.text.y = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         axis.title.x = element_text(size = 14), 
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 14), 
         legend.position = "bottom") 


###################################################
# May 10, try to look at only one promising combination including a VPD

# Step 1: Identify best combo per location (Moran's I >= 0.30, includes VPD, most efficient)
best_combos = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  filter(moran_i >= 0.20) %>%
  filter(str_detect(combo, "vpd")) %>%
  group_by(location) %>%
  slice_min(prop_clusters, n = 1) %>%
  ungroup() %>%
  dplyr::select(location, combo, prop_clusters, moran_i)

# Step 2: Map pathogens to interventions per location
pathogen_to_intervention = list(
  Bangladesh = list(
    vpd        = c("Measles", "Tetanus", "Rubella"),
    wash_water = c("Cryptosporidium", "Giardia"),
    wash_fh    = c("Norovirus", "Campylobacter", "Salmonella"),
    mda        = c("STH") ),
  Kenya = list(
    vpd        = c("Measles", "Rubella"),
    wash_water = c("Cryptosporidium", "Giardia"),
    wash_fh    = c("Norovirus", "Campylobacter", "Salmonella", "E. histolytica", "Shigella"),
    malaria    = c("P. malariae", "P. falciparum", "P. ovale"),
    mda        = c("T. solium", "Schistosomiasis", "Trachoma") ),
  Cambodia = list(
    vpd     = c("Tetanus"),
    mda     = c("Strongyloides stercoralis", "Lymphatic filariasis"),
    wash_fh = c("Toxoplasma gondii"),
    malaria = c("Plasmodium falciparum", "Plasmodium vivax") ))

# Step 3: Aggregate to cluster level per pathogen from individual data
# Step 3: Aggregate to cluster level per pathogen from individual data
cluster_by_pathogen = bind_rows(
  prevalence_check_bangl %>%
    group_by(pathogen, block) %>%
    summarise(num = sum(seropos, na.rm = TRUE),
              fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
    rename(spatial_cluster = block) %>%
    mutate(location = "Bangladesh"),
  
  prevalence_check_kenya %>%
    group_by(pathogen, block) %>%
    summarise(num = sum(seropos, na.rm = TRUE),
              fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
    rename(spatial_cluster = block) %>%
    mutate(location = "Kenya"),
  
  prevalence_check_cam %>%
    group_by(pathogen, psuid) %>%
    summarise(num = sum(seropos, na.rm = TRUE),
              fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
    rename(spatial_cluster = psuid) %>%
    mutate(location = "Cambodia")
) %>%
  group_by(location, spatial_cluster, pathogen) %>%
  summarise(num = max(num), fraction = max(fraction), .groups = "drop")

# Add STH AFTER cluster_by_pathogen is built
sth_by_pathogen = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, al, tt, hw) %>%
  pivot_longer(cols = c(al, tt, hw), names_to = "pathogen", values_to = "seropos") %>%
  mutate(pathogen = recode(pathogen, "al" = "Ascaris", "tt" = "Trichuris", "hw" = "Hookworm")) %>%
  group_by(block, pathogen) %>%
  summarise(num = sum(seropos, na.rm = TRUE),
            fraction = mean(seropos, na.rm = TRUE), .groups = "drop") %>%
  rename(spatial_cluster = block) %>%
  mutate(location = "Bangladesh")

cluster_by_pathogen = bind_rows(cluster_by_pathogen, sth_by_pathogen)

# Update pathogen_to_intervention for Bangladesh mda
pathogen_to_intervention[["Bangladesh"]][["mda"]] = c("Ascaris", "Trichuris", "Hookworm")




# Step 4: Build cumulative curves per pathogen for best combo per location
cumulative_pathogen_curves = map_dfr(unique(best_combos$location), function(loc) {
  
  best        <- best_combos %>% filter(location == loc)
  combo_interventions <- str_split(best$combo, " \\+ ")[[1]]
  
  # Get pathogens in this combo
  pathogens_in_combo <- unlist(pathogen_to_intervention[[loc]][combo_interventions])
  
  loc_data    <- dat_locations %>% filter(location == loc)
  n_clusters  <- n_distinct(loc_data$spatial_cluster)
  
  rao_order <- rao_ranking %>%
    filter(location == loc) %>%
    arrange(rank_rao) %>%
    pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>%
    arrange(rank_single) %>%
    pull(spatial_cluster)
  
  # Pathogen-level cluster data for this location
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, pathogen = path,
             combo = best$combo,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated   = rao_cum,
             cum_fraction  = rao_cum / total_need,
             strategy      = "Rao"),
      tibble(location = loc, pathogen = path,
             combo = best$combo,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated   = single_cum,
             cum_fraction  = single_cum / total_need,
             strategy      = "Single pathogen")  )
  })})

# Step 5: Plot cumulative number treated
p_count = ggplot(cumulative_pathogen_curves,
                 aes(x = frac_clusters, y = cum_treated,
                     color = pathogen, linetype = strategy)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c("Rao" = "solid", "Single pathogen" = "dashed")) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location, pathogen), scales = "free_y") +
  labs(x = "Fraction of spatial clusters visited",
       y = "Cumulative number of people treated",
       color = "Pathogen", linetype = "Strategy") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 15),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "none")

# Step 6: Plot cumulative fraction treated
p_fraction = ggplot(cumulative_pathogen_curves,
                    aes(x = frac_clusters, y = cum_fraction,
                        color = pathogen, linetype = strategy)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c("Rao" = "solid", "Single pathogen" = "dashed")) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location, pathogen), scales = "free_y") +
  labs(x = "Fraction of spatial clusters visited",
       y = "Cumulative fraction of people in need treated",
       color = "Pathogen", linetype = "Strategy") +
  theme_bw(base_size = 11) +
  geom_vline(xintercept = .80, lty = "dashed", col = "gray") +
  theme(strip.text = element_text(size = 15),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "bottom")

p_count
p_fraction

##########################################################################
# try to use the pathogen prevalences themselves, as opposed to guided by the overall strats 

# Step 1: remains the same - use existing best_combos

# Step 2: Build pathogen-level Rao ranking per location using best combo's pathogens
rao_ranking_pathogen = map_dfr(unique(best_combos$location), function(loc) {
  
  best               <- best_combos %>% filter(location == loc)
  combo_interventions <- str_split(best$combo, " \\+ ")[[1]]
  pathogens_in_combo <- unlist(pathogen_to_intervention[[loc]][combo_interventions])
  
  # Wide format: one row per cluster, one col per pathogen
  path_wide <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo) %>%
    dplyr::select(spatial_cluster, pathogen, fraction) %>%
    pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
  
  path_cols <- setdiff(names(path_wide), "spatial_cluster")
  
  path_wide %>%
    rowwise() %>%
    mutate(rao_val = rao_quadratic(c_across(all_of(path_cols)))) %>%
    ungroup() %>%
    arrange(desc(rao_val)) %>%
    mutate(rank_rao = row_number(), location = loc) %>%
    dplyr::select(location, spatial_cluster, rao_val, rank_rao)
})

# Step 3: Build cumulative curves using pathogen-level Rao ranking
cumulative_pathogen_curves_v2 = map_dfr(unique(best_combos$location), function(loc) {
  
  best               <- best_combos %>% filter(location == loc)
  combo_interventions <- str_split(best$combo, " \\+ ")[[1]]
  pathogens_in_combo <- unlist(pathogen_to_intervention[[loc]][combo_interventions])
  
  loc_data   <- dat_locations %>% filter(location == loc)
  n_clusters <- n_distinct(loc_data$spatial_cluster)
  
  rao_order <- rao_ranking_pathogen %>%
    filter(location == loc) %>%
    arrange(rank_rao) %>%
    pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>%
    arrange(rank_single) %>%
    pull(spatial_cluster)
  
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, pathogen = path,
             combo = best$combo,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated   = rao_cum,
             cum_fraction  = rao_cum / total_need,
             strategy      = "Rao"),
      tibble(location = loc, pathogen = path,
             combo = best$combo,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated   = single_cum,
             cum_fraction  = single_cum / total_need,
             strategy      = "Single pathogen")
    )
  })
})

# Plot cumulative number treated
ggplot(cumulative_pathogen_curves_v2,
       aes(x = frac_clusters, y = cum_treated,
           color = pathogen, linetype = strategy)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c("Rao" = "solid", "Single pathogen" = "dashed")) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location, pathogen), scales = "free_y") +
  labs(x = "Fraction of spatial clusters visited",
       y = "Cumulative number of people treated",
       color = "Pathogen", linetype = "Strategy") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 15),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "none")

# Plot cumulative fraction treated
ggplot(cumulative_pathogen_curves_v2,
       aes(x = frac_clusters, y = cum_fraction,
           color = pathogen, linetype = strategy)) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c("Rao" = "solid", "Single pathogen" = "dashed")) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  facet_wrap(vars(location, pathogen), scales = "free_y") +
  geom_vline(xintercept = 0.80, lty = "dashed", col = "gray") +
  labs(x = "Fraction of spatial clusters visited",
       y = "Cumulative fraction of people in need treated",
       color = "Pathogen", linetype = "Strategy") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 15),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "bottom")

#### spatial locations to target 80% of disease

clusters_80 = cumulative_pathogen_curves_v2 %>%
  group_by(location, pathogen, strategy, combo) %>%
  summarise(
    prop_clusters_80 = frac_clusters[which(cum_fraction >= 0.80)[1]],
    .groups = "drop" )

# Add combo as plot title per location
combo_labels = best_combos %>% dplyr::select(location, combo)

clusters_80 = left_join(clusters_80, combo_labels, by = "location")

# Plot
# Get Rao ordering per location
pathogen_order = clusters_80 %>%
  filter(strategy == "Rao (pathogen-level)") %>%
  arrange(location, prop_clusters_80) %>%
  group_by(location) %>%
  mutate(rao_rank = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, pathogen, rao_rank)

clusters_80 = left_join(clusters_80, pathogen_order, by = c("location", "pathogen"))

ggplot(clusters_80, aes(x = reorder(pathogen, rao_rank), y = prop_clusters_80,
                      fill = strategy)) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.7),
  #         width = 0.6, alpha = 0.8) +
  geom_point(aes(fill = strategy), cex = 4, shape = 24) + 
  facet_wrap(vars(location, combo.y), scales = "free_x", labeller = label_wrap_gen(width = 40)) +
  scale_fill_viridis_d(option = "D", end = 0.8, begin = 0, name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  labs(x = "Pathogen",
       y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(
    strip.text      = element_text(size = 11),
    axis.text.x     = element_text(angle = 35, hjust = 1, size = 12),
    axis.text.y     = element_text(size = 12),
    axis.title      = element_text(size = 13),
    legend.text     = element_text(size = 12),
    legend.title    = element_text(size = 13),
    legend.position = "bottom" )









##############################################################################################################################
# May 12 ######################################################
# Individual pathogens only! 

########################################################
# Pull in data 
########################################################
# 1. Bangladesh multiplex serology 

# Read in public ids 
public_ids = read.csv(file = here("data/bangl/public_ids", "public-ids.csv")) %>%
  distinct(dataid, clusterid, block, clusterid_r, block_r)
head(public_ids)

# Read in GPS data to join to ids to orient blocks, public blocks, and their locations
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 
head(gps_dat)

# Joing this to cluster information
public_id_cluster = public_ids %>%
  distinct(clusterid, block, block_r, clusterid_r) %>%
  left_join(gps_dat, by = c("block", "block_r"))
head(public_id_cluster)

# Define what is a vaccine vs pathogen antigen 
antigen_vax = c("Rubella", "Measles", "Tetanus", "Diptheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )

# Load in multiplex serology 
# remove pathogens with no signal- schisto control and covid  
# define seropositivity based on cutoff 
# only visit 3 data 
luminex_bangl <- read.csv(file = here("data/bangl/luminex/final", 
                                      "washb_bangl_luminex_igg_seropos_2025-09-21.csv")) %>%
  left_join(public_id_cluster, by = c("clusterid")) %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST")) %>%
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(mfi, agemonth, clusterid, childid, antigen, pathogen, seropos, block, block_r, med_qgpslong, med_qgpslat) 

# Check prevalences of invdividual antigens 
prevalence_check_bangl = luminex_bangl %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_bangl)
print(unique(prevalence_check_bangl$antigen))
print(unique(prevalence_check_bangl$pathogen))

prev_antigen_bangl = prevalence_check_bangl  %>%
  distinct(antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
bangl_targeting_individual = prevalence_check_bangl %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate( crypto_seropos = ifelse(
    any(antigen == "c17"   & seropos == 1) |
      any(antigen == "c23"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Cryptosporidium", crypto_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Tetanus" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus", "Campylobacter","Salmonella") & seropos ==1, 1 , 0 )) %>%
  filter(!pathogen %in% c("Norovirus", "Campylobacter", "Salmonella", "Cryptosporidium", "Giardia", "Shigella"))
print(unique(bangl_targeting_individual$pathogen))
head(bangl_targeting_individual)


# summarise(vpd = max(vpd), wash_water = max(wash_water), wash_fh = max(wash_fh), across(c(block, block_r, med_qgpslong, med_qgpslat), first)) %>%
 # ungroup() %>%
 # distinct(childid, block, block_r, med_qgpslong, med_qgpslat, vpd, wash_water, wash_fh) %>%
 # pivot_longer(cols = c(vpd, wash_water, wash_fh), names_to = "intervention", values_to = "intervention_needed")
#head(bangl_targeting_individual)
#length(unique(bangl_targeting_individual$childid))


# targeting need by cluster 
bangl_targeting_cluster = bangl_targeting_individual %>% 
  group_by(block, pathogen) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, pathogen, non_na_denom, num, fraction) %>%
  mutate(location = "Bangladesh") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(intervention = "vpd")
head(bangl_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = bangl_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")


# add Bangl STH 
sth = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
  pivot_longer(cols = c(al, tt, giar, hw, sth),
    names_to = "pathogen",
    values_to = "value") %>%
  filter(!pathogen %in% c("giar", "sth")) %>%
  group_by(block, pathogen) %>%
  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  ungroup() %>%
  distinct(block, non_na_denom, num, fraction, pathogen)  %>%
  mutate(intervention = "mda", location = "Bangladesh")  %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(pathogen = replace(pathogen, pathogen == "al", "Ascaris")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "tt", "Trichuris")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "hw", "Hookworm")) 
head(sth)

bangl_targeting_cluster = rbind(bangl_targeting_cluster , sth)
head(bangl_targeting_cluster)
# plot histogram of distribution of intervention need again 
ggplot(data = bangl_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")



########################################################
# 2. Kenya multiplex serology 

treatment_assignment = read.csv(file = here("data/kenya/public_ids", 
                                            "cluster_tx_masked.csv")) 

treatment_assignment = read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)
head(treatment_assignment)

antigen_vax = c("Rubella", "Measles", "Tetanus", "Diptheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )

luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                     "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST", "Trachoma")) %>%
  filter(!antigen %in% c("pfmsp1", "glurp", "lsa")) %>% # longer lasting malaria 
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(clusterid, childid, antigen, pathogen, seropos, block) 

prevalence_check_kenya = luminex_kenya %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_kenya)
print(unique(prevalence_check_kenya$antigen))
print(unique(prevalence_check_kenya$pathogen))

prev_antigen_kenya = prevalence_check_kenya  %>%
  distinct(pathogen, antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
kenya_targeting_individual = prevalence_check_kenya %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus","Campylobacter","Salmonella", "E. histolytica", "Shigella") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("P. malariae" , "P. falciparum", "P. ovale") & seropos ==1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("T. solium" , "Schistosomiasis", "Trachoma") & seropos == 1, 1, 0)) %>%
  filter(!pathogen %in% c("Norovirus", "Campylobacter", "Salmonella", "Cryptosporidium", "Giardia", "Shigella", "E. histolytica"))
  
print(unique(kenya_targeting_individual$pathogen))
head(kenya_targeting_individual)
  

# targeting need by cluster 
kenya_targeting_cluster = kenya_targeting_individual %>% 
  group_by(block, pathogen) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, pathogen, non_na_denom, num, fraction) %>%
  mutate(location = "Kenya") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(intervention = ifelse(pathogen %in% c("Measles", "Rubella"), "vpd", 
                               ifelse(pathogen %in% c("Schistosomiasis", "T. solium" ,"Trachoma"), "mda", "malaria" )))

head(kenya_targeting_cluster)


# plot histogram of distribution of intervention need 
ggplot(data = kenya_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Kenya")


########################################################
# 3. Cambodia multiplex serology 

# Load disease data. 
cambodia_serology_public = readr::read_csv(file = here("data/cambodia", "cambodia_serology_public.csv")) %>%
  mutate(womanid = ...1)
head(cambodia_serology_public)
#ttmb     : Tetanous toxoid, bm14     : Lymphatic filariasis bm14, bm33     : Lymphatic filariasis bm33, wb123    : Lymphatic filariasis wb123
#nie      : Strongyloides stercoralis NIE, sag2a    : Toxoplasma gondii SAG2A , t24      : Taenia solium T24, pfmsp19  : Plasmodium falciparum MSP-1(19)
#pvmsp19  : Plasmodium vivax MSP-1(19)

# Load location data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
gps_dat = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon)
head(gps_dat)

# Transform into seropositive vs seronegative using: https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004699#pntd-0004699-t001
# https://journals.asm.org/doi/10.1128/cvi.00052-16#T1 for tetanus
antigen = c("ttmb", "bm14", "wb123", "bm33", "nie", "sag2a", "t24", "pfmsp19", "pvmsp19")
cutoff = c(100, 65, 115, 966,792, 159, 486, 343, 196 )
pathogen = c("Tetanus", "Lymphatic filariasis", "Lymphatic filariasis", "Lymphatic filariasis", 
             "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
             "Plasmodium vivax")
antigen_path_dat = data.frame(antigen, cutoff, pathogen)

antigen_vax = c( "Tetanus")
antigen_path = c("Lymphatic filariasis", "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
                 "Plasmodium vivax")

prevalence_check_cam = cambodia_serology_public %>%
  pivot_longer(cols = ttmb:gst, names_to = "antigen", values_to = "value") %>%
  filter(antigen != "gst") %>%
  filter(antigen != "bm33") %>% # we dont want this in our defintion
  mutate(psuid = psuid + 1) %>% 
  left_join(antigen_path_dat, by = "antigen") %>%
  mutate(seropos = ifelse(value > cutoff & pathogen %in% antigen_path , 1, 
                          ifelse(value < cutoff & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(womanid, psuid, age, parity, antigen, pathogen, seropos) %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90) 
head(prevalence_check_cam) 
print(unique(prevalence_check_cam$antigen))
print(unique(prevalence_check_cam$pathogen))

cam_antigen = prevalence_check_cam %>%
  distinct(pathogen, pop_seropos)

# Targeting groups by individual
# make sure to use two antigen positvity defintion for LF
cam_targeting_individual = prevalence_check_cam %>%
  group_by(womanid) %>%
  mutate(lf_seropos = ifelse(any(antigen == "bm14" & seropos == 1) & any(antigen == "wb123" & seropos == 1), 1, 0 ),
         seropos = ifelse(pathogen == "Lymphatic filariasis", lf_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Tetanus") & seropos == 1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("Strongyloides stercoralis" , "Lymphatic filariasis" ) & seropos == 1, 1, 0)) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Toxoplasma gondii") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("Plasmodium vivax", "P. falciparum") & seropos ==1, 1, 0)) %>%
  ungroup() %>%
  filter(pathogen != "Toxoplasma gondii") %>%
  distinct(womanid, psuid, pathogen, seropos)
head(cam_targeting_individual)

# targeting need by cluster 
cam_targeting_cluster = cam_targeting_individual %>% 
  group_by(psuid, pathogen) %>%
  summarise(non_na_denom = n_distinct(womanid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num / non_na_denom, location = "Cambodia", spatial_cluster = psuid ) %>%
  dplyr::select(-psuid) %>%
  ungroup() %>%
  mutate(intervention = ifelse(pathogen == "Tetanus", "vpd",
                               ifelse(pathogen %in% c("Plasmodium falciparum", "Plasmodium vivax"), "malaria", "mda")))
head(cam_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = cam_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Cambodia")

########################################################
# Join data together 

multipathogen_indices = rbind(cam_targeting_cluster, 
                              kenya_targeting_cluster,
                              bangl_targeting_cluster) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup()
head(multipathogen_indices )


# plot out 
########################################################
# Compare targeting by multipathogen index 

# diversity functions 
shannon_entropy <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  -sum(p * log(p))}

gini_simpson <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  1 - sum(p^2)}

alpha_diversity <- function(p) sum(p > 0)

rao_quadratic <- function(p) {
  if (length(p) < 2) return(0)
  sum(sapply(combn(p, 2, simplify = FALSE), prod))}

# make data into wide, one row per spatial_cluster per location, columns = interventions
cluster_wide <- multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, fraction) %>%
  pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
head(cluster_wide)

min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
                                      rank_col, interventions_in_combo,
                                      threshold = 0.80) {
  
  # Use fraction_rao for rao_quadratic, fraction for everything else
  # frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
  frac_col <- if (rank_col == "rao_quadratic") "fraction" else "fraction"
  
  total_burden <- cluster_prev_sub %>%
    filter(pathogen %in% interventions_in_combo) %>%
    group_by(pathogen) %>%
    #  summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
    summarise(total = sum(.data[["num"]], na.rm = TRUE), .groups = "drop")
  
  
  ranked_clusters <- cluster_scores_sub %>%
    arrange(desc(.data[[rank_col]])) %>%
    pull(spatial_cluster)
  
  cumulative <- tibble(pathogen = interventions_in_combo, cum_burden = 0)
  
  for (i in seq_along(ranked_clusters)) {
    clust <- ranked_clusters[i]
    this_cluster <- cluster_prev_sub %>%
      filter(spatial_cluster == clust, pathogen %in% interventions_in_combo) %>%
      dplyr::select(pathogen, all_of(frac_col), num) %>%
      rename(frac_val = all_of(frac_col))
    
    cumulative <- cumulative %>%
      left_join(this_cluster, by = "pathogen") %>%
      # mutate(cum_burden = cum_burden + replace_na(frac_val, 0)) %>%
      mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
      dplyr::select(pathogen, cum_burden)
    
    pct_reached <- cumulative %>%
      left_join(total_burden, by = "pathogen") %>%
      mutate(pct = cum_burden / total)
    
    if (all(pct_reached$pct >= threshold)) {
      return(tibble(n_clusters = i, rank_method = rank_col))
    }
  }
  return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
}

# run across all locations for combinations of pathogens with at least 3 intervetions 
rank_methods <- c("shannon", "alpha_div", "gini_simpson", "rao_quadratic")

results_all <- map_dfr(unique(multipathogen_indices$location), function(loc) {
  
  loc_data <- multipathogen_indices %>% filter(location == loc)
  all_interventions <- unique(loc_data$pathogen)
  n_interventions <- length(all_interventions)
  
  # All combinations of size 3 up to n_interventions
  combos <- map(3:n_interventions, ~ combn(all_interventions, .x, simplify = FALSE)) %>%
    flatten()
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Total clusters in location — calculated from full loc_data, not the combo subset
    n_total_clusters <- n_distinct(loc_data$spatial_cluster)
    
    cluster_prev_sub <- loc_data %>% filter(pathogen %in% combo)
    
    # Recompute diversity scores for this combo only
    cluster_scores_sub <- cluster_wide %>%
      filter(location == loc) %>%
      dplyr::select(spatial_cluster, any_of(combo)) %>%
      rowwise() %>%
      mutate(shannon      = shannon_entropy(c_across(all_of(combo))),
             alpha_div    = alpha_diversity(c_across(all_of(combo))),
             gini_simpson = gini_simpson(c_across(all_of(combo))),
             rao_quadratic = rao_quadratic(c_across(all_of(combo)))  ) %>%
      ungroup()
    
    n_total_clusters <- n_distinct(cluster_prev_sub$spatial_cluster)
    
    map_dfr(rank_methods, function(rm) {
      res <- min_clusters_to_threshold(cluster_prev_sub      = cluster_prev_sub,
                                       cluster_scores_sub    = cluster_scores_sub,
                                       rank_col              = rm,
                                       interventions_in_combo = combo )
      res %>% mutate(location = loc,
                     combo = combo_label,
                     n_total_clusters = n_total_clusters,
                     prop_clusters   = n_clusters / n_total_clusters )
    })
  })
})

# ---------------------------------------------------------------
# PLOT: distribution of proportion of clusters needed, facet by location
# ---------------------------------------------------------------

method_labels <- c(shannon      = "Shannon diversity index",
                   alpha_div    = "Alpha diversity",
                   gini_simpson = "Gini-Simpson index",
                   rao_quadratic = "Rao's quadratic index")

results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = fct_reorder(rank_method, prop_clusters, median, na.rm = TRUE)) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 21, width = 0.5) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    title    = "Proportion of clusters needed to reach 80% coverage",
    # subtitle = "Across all intervention combinations of ≥3 groups",
    x        = NULL,
    y        = "Proportion of spatial clusters required" ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

head(results_all)
summary_results_all = results_all %>%
  group_by(rank_method, location) %>%
  mutate(mean_clust = mean(prop_clusters)) %>%
  ungroup() %>%
  distinct(rank_method, location, mean_clust)
  

fig1a_build = results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = factor(rank_method, levels = c(
    "Rao's quadratic index",
    "Shannon diversity index", 
    "Gini-Simpson index",
    "Alpha diversity"))) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  # stat_summary(fun = mean, geom = "bar", alpha = 0.7, width = 0.6) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.6, shape = 16) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    #title = "Proportion of clusters needed to reach 80% coverage",
    x     = NULL,
    y     = "Proportion of clusters to reach 80% coverage"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0.55, .86)) +
  scale_fill_viridis_d(option = "D" , end = .7 , name = "Strategy", alpha = .66) +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    legend.position = "bottom")  +
  theme(strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(size = 14))
fig1a_build

fig1a <- fig1a_build +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1a


fig1a <- fig1a_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1a


#################################### Map out all Rao surfaces 
# Bangl 
gps_dat_bangl = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat)  %>%
  mutate(spatial_cluster = block_r)
head(gps_dat_bangl)

bangl_rao_clusters = left_join(bangl_targeting_cluster, gps_dat_bangl, by = "spatial_cluster") %>%
  mutate(lat = med_qgpslat, long = med_qgpslong) %>%
  dplyr::select(-block, -block_r, -med_qgpslat, -med_qgpslong ) 
head(bangl_rao_clusters)

# Cam
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 

gps_dat_cam = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon) %>%
  mutate(spatial_cluster = psuid)
head(gps_dat_cam)

cam_rao_clusters = left_join(cam_targeting_cluster, gps_dat_cam, by = "spatial_cluster") %>%
  mutate(lat = dhslat, long = dhslon) %>%
  dplyr::select(-psuid, -dhslat, -dhslon ) 
head(cam_rao_clusters)

# Kenya 
gps_dat_kenya = readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat))  %>%
  distinct(block, long, lat) %>%
  mutate(spatial_cluster = block) %>%
  ungroup() %>%
  dplyr::select(-block)
head(gps_dat_kenya)

kenya_rao_clusters = left_join(kenya_targeting_cluster, gps_dat_kenya, by = "spatial_cluster") 
head(kenya_rao_clusters)

###################################
# Put together 

dat_locations = rbind(bangl_rao_clusters, cam_rao_clusters, kenya_rao_clusters) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup() %>% 
  drop_na()
head(dat_locations)

####################################
# Map 

# Calculate Rao per spatial cluster using all interventions present
rao_by_cluster = dat_locations %>%
  group_by(location, spatial_cluster, lat, long) %>%
  summarise(
    rao = rao_quadratic(fraction),
    # rao = rao_quadratic(fraction_rao),
    .groups = "drop") %>%
  group_by(location) %>%
  mutate(max_rao = max(rao)) %>%
  mutate(rao = rao/max_rao) %>%
  ungroup() %>%
  drop_na()
head(rao_by_cluster)

# Calculate Moran's I per location
library(spdep)
morans_by_location = rao_by_cluster %>%
  group_by(location) %>%
  group_modify(~ {
    coords <- as.matrix(.x[, c("long", "lat")])
    nb     <- knn2nb(knearneigh(coords, k = 4))
    lw     <- nb2listw(nb, style = "W")
    mi     <- moran.test(.x$rao, lw)
    tibble(moran_i = mi$estimate[["Moran I statistic"]])
  }) %>%
  ungroup() %>%
  mutate(label = paste0("I = ", formatC(moran_i, digits = 2, format = "f")))

# Join labels back for plotting
rao_plot_dat = left_join(rao_by_cluster, morans_by_location, by = "location")

# Plot
fig1b_build = ggplot(rao_by_cluster, aes(x = long, y = lat, color = rao)) +
  geom_point(size = 6, alpha = 0.9) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  facet_wrap(vars(location), scales = "free") +
  geom_text(data = morans_by_location,
            aes(label = label),
            x = Inf, y = Inf,
            hjust = 1.05, vjust = 1.5,
            inherit.aes = FALSE,
            size = 5.5, color = "black" ) +
  labs(
    #  title = "Spatial distribution of multipathogen burden diversity",
    # subtitle = "Rao's quadratic entropy across spatial clusters",
    x = "Longitude",
    y = "Latitude") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x =  element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 14), 
    legend.position = "bottom") +
      theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))
fig1b_build

fig1b <- fig1b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1b

# figure 1
plot_grid(fig1a, fig1b, ncol = 1, rel_heights = c(.5, .6))


########### step 2, spatial autocorrelation against effiency 

#### Part 2 ###################################
################################################
# plot effiency vs autocorrelation within treated units 
dat_locations = dat_locations %>% drop_na()

results_combo = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data          <- dat_locations %>% filter(location == loc)
  all_pathogens     <- unique(loc_data$pathogen)
  n_total           <- n_distinct(loc_data$spatial_cluster)
  combos            <- map(3:length(all_pathogens), ~ combn(all_pathogens, .x, simplify = FALSE)) %>% flatten()
  
  coords <- loc_data %>%
    distinct(spatial_cluster, lat, long) %>%
    arrange(spatial_cluster)
  nb <- knn2nb(knearneigh(as.matrix(coords[, c("long", "lat")]), k = 4))
  lw <- nb2listw(nb, style = "W")
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    rao_surface <- loc_data %>%
      filter(pathogen %in% combo) %>%
      group_by(spatial_cluster) %>%
      summarise(rao_val = rao_quadratic(fraction), .groups = "drop") %>%
      arrange(spatial_cluster)
    
    moran_i <- moran.test(rao_surface$rao_val, lw)$estimate[["Moran I statistic"]]
    
    ranked <- rao_surface %>% arrange(desc(rao_val)) %>% pull(spatial_cluster)
    
    totals <- loc_data %>%
      filter(pathogen %in% combo) %>%
      group_by(pathogen) %>%
      summarise(total = sum(num, na.rm = TRUE), .groups = "drop")
    
    cumulative <- tibble(pathogen = combo, cum = 0)
    
    n_clusters <- NA_integer_
    for (i in seq_along(ranked)) {
      this <- loc_data %>%
        filter(spatial_cluster == ranked[i], pathogen %in% combo) %>%
        dplyr::select(pathogen, num)
      
      cumulative <- cumulative %>%
        left_join(this, by = "pathogen") %>%
        mutate(cum = cum + replace_na(num, 0)) %>%
        dplyr::select(pathogen, cum)
      
      pct <- cumulative %>%
        left_join(totals, by = "pathogen") %>%
        mutate(pct = cum / total)
      
      if (all(pct$pct >= 0.80)) {
        n_clusters <- i
        break
      }
    }
    
    tibble(location = loc, combo = combo_label,
           n_clusters = n_clusters,
           prop_clusters = n_clusters / n_total,
           moran_i = moran_i)
  })
})

# Plot with labels
to_label = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  group_by(location) %>%
  mutate(label = case_when(
    prop_clusters == min(prop_clusters) ~ combo,
    moran_i == max(moran_i)             ~ combo,
    TRUE                                ~ NA_character_
  )) %>%
  ungroup()

ggplot(to_label, aes(x = moran_i, y = prop_clusters, color = location)) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  facet_wrap(vars(location), scales = "free_x") +
  geom_label_repel(data = filter(to_label, !is.na(label)),
                   aes(label = label),
                   size = 4, show.legend = FALSE,
                   max.overlaps = 20, col = "black") +
  labs(x     = "Spatial autocorrelation (Moran's I)",
       y     = "Proportion of clusters to reach 80% coverage",
       color = "Location") +
  scale_color_viridis_d(option = "D", end = 0.7) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom")

ggplot(data = to_label) +
  geom_histogram(aes(x = moran_i, fill = location))+
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.7) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom") +
  xlab("Spatial autocorrelation (Moran's I)")

ggplot(data = to_label) +
  geom_point(aes(x = moran_i, y = prop_clusters, color= location), alpha = .7, cex = 3)+
 # facet_wrap(vars(location), scales = "free_x") +
  scale_color_viridis_d(option = "D", end = 0.9) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom") +
  xlab("Spatial autocorrelation (Moran's I)") +
  ylab("Proportion of clusters to reach 80% coverage")

fig2b_build = ggplot(data = to_label %>%
                       mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia")))) +
  geom_point(aes(x = moran_i, y = prop_clusters), color = "gray23", alpha = .6, cex = 4)+
  # facet_wrap(vars(location), scales = "free_x") +
  scale_color_viridis_d(option = "D", end = 0.9) +
  facet_wrap(vars(location), scales ="free_x") +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 10),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom") +
  xlab("Spatial autocorrelation (Moran's I)") +
  ylab("Proportion of clusters\nto reach 80% coverage") +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))  + scale_x_continuous(n.breaks = 4) +
  theme(panel.spacing = unit(0.5, "cm"))
fig2b_build

fig2b<- fig2b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2b



## simulate morans i 

# --- tune rho until realized Moran's I hits target ---
simulate_moran_tuned <- function(n = 20, target_I, seed = 42, k = 4) {
  set.seed(seed)
  coords <- matrix(runif(n * 2), ncol = 2)
  nb     <- knn2nb(knearneigh(coords, k = k))
  lw     <- nb2listw(nb, style = "W")
  W      <- listw2mat(lw)
  I_mat  <- diag(n)
  
  # bisection to find rho that gives target Moran's I
  lo <- -0.99; hi <- 0.99
  for (iter in 1:60) {
    rho   <- (lo + hi) / 2
    A     <- solve(I_mat - rho * W)
    set.seed(seed)
    x     <- as.numeric(A %*% rnorm(n))
    realized_I <- moran(x, lw, n, Szero(lw))$I
    if (realized_I < target_I) lo <- rho else hi <- rho
    if (abs(realized_I - target_I) < 1e-4) break
  }
  
  # rescale to 0-1 for cleaner color contrast
  x_scaled <- (x - min(x)) / (max(x) - min(x))
  
  data.frame(
    x          = coords[, 1],
    y          = coords[, 2],
    value      = x_scaled,
    realized_I = round(realized_I, 2),
    panel      = paste0("Moran's I = ", target_I)
  )
}

moran_vals <- c( 0, .5, 1)

sim_data <- map_dfr(moran_vals, ~ simulate_moran_tuned(n = 100, target_I = .x)) %>%
  mutate(panel = factor(panel, levels = paste0("Moran's I = ", moran_vals)))

# check realized values
sim_data %>% distinct(panel, realized_I)

fig_moran <- ggplot(sim_data, aes(x = x, y = y, color = value)) +
  geom_point(alpha = 0.9, size = 4) +
  facet_wrap(vars(panel), nrow = 1) +
  scale_color_viridis_c(option = "D", name = "Value",
                        breaks = c(0, 0.5, 1),
                        labels = c("Low", "Mid", "High")) +
  theme_bw(base_size = 11) +
  theme(
    strip.text       = element_text(size = 15),
    strip.background = element_rect(fill = "white", colour = "black"),
    axis.title       = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.text      = element_text(size = 14),
    legend.title     = element_text(size = 14),
    legend.position  = "bottom",
    plot.title       = element_text(size = 14) ) 

fig_moran

fig2a<- fig_moran +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2a


########################################################################
########################################################################
############# part 3 consider specific strategies 
head(results_combo)

# cluster_by_pathogen directly from multipathogen_indices
cluster_by_pathogen = multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, num, fraction)

# Step 1: best combos
best_combos = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  filter(moran_i >= 0.20) %>%
  filter(str_detect(combo, "Measles|Rubella|Tetanus")) %>%
  group_by(location) %>%
  slice_min(prop_clusters, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(location, combo, prop_clusters, moran_i)

# Step 2: single pathogen rankings from multipathogen_indices
single_pathogen_ranking = bind_rows(
  cluster_by_pathogen %>%
    filter(location == "Bangladesh", pathogen == "Measles") %>%
    dplyr::select(location, spatial_cluster, fraction),
  
  cluster_by_pathogen %>%
    filter(location == "Kenya", pathogen == "Measles") %>%
    dplyr::select(location, spatial_cluster, fraction),
  
  cluster_by_pathogen %>%
    filter(location == "Cambodia", pathogen == "Tetanus") %>%
    dplyr::select(location, spatial_cluster, fraction)
) %>%
  group_by(location) %>%
  arrange(desc(fraction)) %>%
  mutate(rank_single = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, spatial_cluster, rank_single)

# Step 3: Rao ranking per location using best combo pathogens
rao_ranking_pathogen = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  pathogens_in_combo <- str_split(best$combo[[1]], " \\+ ")[[1]]
  
  path_wide <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo) %>%
    dplyr::select(spatial_cluster, pathogen, fraction) %>%
    pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
  
  path_cols <- setdiff(names(path_wide), "spatial_cluster")
  
  path_wide %>%
    rowwise() %>%
    mutate(rao_val = rao_quadratic(c_across(all_of(path_cols)))) %>%
    ungroup() %>%
    arrange(desc(rao_val)) %>%
    mutate(rank_rao = row_number(), location = loc) %>%
    dplyr::select(location, spatial_cluster, rao_val, rank_rao)
})

# Step 4: cumulative curves
cumulative_pathogen_curves_v2 = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  combo_label        <- best$combo[[1]]
  pathogens_in_combo <- str_split(combo_label, " \\+ ")[[1]]
  
  n_clusters <- n_distinct(cluster_by_pathogen %>% filter(location == loc) %>% pull(spatial_cluster))
  
  rao_order <- rao_ranking_pathogen %>%
    filter(location == loc) %>% arrange(rank_rao) %>% pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>% arrange(rank_single) %>% pull(spatial_cluster)
  
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated = rao_cum, cum_fraction = rao_cum / total_need,
             strategy = "Rao's quadratic index"),
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated = single_cum, cum_fraction = single_cum / total_need,
             strategy = "Vaccine only")
    )
  })
})

# Step 5: clusters_80
clusters_80 = cumulative_pathogen_curves_v2 %>%
  group_by(location, pathogen, strategy, combo) %>%
  summarise(prop_clusters_80 = frac_clusters[which(cum_fraction >= 0.80)[1]], .groups = "drop")

pathogen_order = clusters_80 %>%
  filter(strategy == "Rao") %>%
  arrange(location, prop_clusters_80) %>%
  group_by(location) %>%
  mutate(rao_rank = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, pathogen, rao_rank)

clusters_80 = left_join(clusters_80, pathogen_order, by = c("location", "pathogen"))

fig2c_build  = ggplot(clusters_80 %>%  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))), aes(x = reorder(pathogen, rao_rank), y = prop_clusters_80, fill = strategy)) +
  geom_point(aes(fill = strategy), cex = 4, shape = 24) +
  #facet_wrap(vars(location, combo), scales = "free_x", labeller = label_wrap_gen(width = 40)) +
 # facet_wrap(vars(location, combo), scales = "free_x", 
   #          labeller = labeller(location = label_value, combo = NULL)) +
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.8, begin = 0, name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  labs(x = "Pathogen", y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 11), axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 13),
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        legend.position = "bottom")+
  ylab("Proportion of clusters\nto reach 80% coverage") +
  theme( #strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14), 
        strip.text       = element_text(size = 15))
       # strip.text = element_blank()) 
fig2c_build



fig2c <- fig2c_build +
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2c

#plot_grid(fig2a, fig2b, fig2c, ncol = 1)

left = plot_grid(fig2a, fig2b, ncol = 1, rel_heights = c(.6, .5))

fig2 = plot_grid(left, fig2c, ncol =2)
fig2


##################################################
# last analysis, fig 3 

##################################################
# may 14
# compare rao to poverty 


# Multidimensional Poverty Index 
######################################################################################


# Cambodia 
######################################################################################
cam_wealth_data <- read_dta(file = here("projects/6-multipathogen-burden/data/cambodia", "KHPR61FL.DTA"))
head(cam_wealth_data)
table(cam_wealth_data$hv213)
table(cam_wealth_data$hv214)
table(cam_wealth_data$hv215)

# make mpi 
mpi_cambodia = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2777, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > 0.2777], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , dhsclust) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity)

head(mpi_cambodia)

# correlation with wealth 
cam_wealth_dat = cam_wealth_data %>%
  dplyr::select(hv001, hv270, hv271) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  summarize(across(hv270:hv271, ~ mean(.x))) %>%
  left_join(mpi_cambodia, by = c("dhsclust")) %>% drop_na()
head(cam_wealth_dat )
cor(cam_wealth_dat$hv271, cam_wealth_dat$mpi, method = "spearman")

ggplot(data = cam_wealth_dat) +
  geom_point(aes(x = hv271, y = mpi)) + ylab("MPI") +
  xlab("DHS Wealth index") + theme_minimal()

# add gps data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
head(gps_cambodia)

load(file = here("data/cambodia/cambodia_serology.Rdata"))
gps_dat = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat)

cam_gps_mpi = left_join(gps_cambodia, mpi_cambodia ,  by = "dhsclust") %>%
  left_join(gps_dat, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  dplyr::select(psuid, mpi) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) 
head(cam_gps_mpi)


# Bangladesh 
######################################################################################
# treatment clusters
treatment_bangl = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr) %>%
  distinct(clusterid, block, tr)
head(treatment_bangl)
print(treatment_bangl$tr)

bangl_metadata <- read.csv(file = here("data/bangl/enrollment_wealth/untouched", "washb-bangladesh-enrol-public.csv"))
head(bangl_metadata)

table(bangl_metadata$hfiacat)
table(bangl_metadata$momedu)
table(bangl_metadata$latfeces)
table(bangl_metadata$elec)
table(bangl_metadata$roof)
table(bangl_metadata$floor)
table(bangl_metadata$walls)

mpi_bangladesh = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = asset_radio + asset_tv + asset_phone + asset_bike + asset_refrig, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    #  mortality*(1/6),
    mat_edu_deprived*(1/6),
    # cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2037, 1, 0)) %>%
  group_by(block) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > 0.2037], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , block) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity) %>%
  mutate(block_r = 91 - block) %>%
  mutate(spatial_cluster = block_r) %>% mutate(Location = "Bangladesh") %>% dplyr::select(-block_r, -block) %>%
  ungroup() %>%
  dplyr::select(mpi, spatial_cluster, Location)
head(mpi_bangladesh)

#(1/6 + 1/6 + 5/18 )*.33333
#head(mpi_bangladesh)

#############################################################
# Kenya 
#washk_child_vars has whether they died 

treatment_assignment_k = read.csv(file = here("data/kenya/public_ids", 
                                              "cluster_tx_masked.csv")) 
head(treatment_assignment_k)

# household level 
kenya_meta = read_dta(file = here("data/kenya/master_files", "washk_household_vars_20190305.dta")) %>%
  left_join(treatment_assignment_k, by = "clusterid") 
head(kenya_meta)

table(kenya_meta$HHS_bi)
table(kenya_meta$mother_edu)
table(kenya_meta$tr_masked)

#child level 
child_death = read_dta(file = here("data/kenya/master_files", "washk_child_vars_20190305.dta")) %>%
  group_by(hhid) %>%
  mutate(mortality = ifelse(isdead_el %in% c(1, 3) | childdeath == 1, 1, 0)) %>%
  distinct(hhid, mortality)
head(child_death)

table(child_death$isdead_el)
table(child_death$childdeath)
table(child_death$mortality)

# blocks 
block_kenya = luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                                   "washb_kenya_luminex_igg_seropos_pathogen_2026-01-29.csv")) %>%
  distinct(clusterid,block, tr)
head(block_kenya)
table(block_kenya$tr)

head(luminex_kenya)

mpi_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2778, 1, 0)) %>%
  group_by(block) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > .2778], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , block) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity)  %>%
  ungroup() %>%
  mutate(spatial_cluster = block) %>%
  mutate(Location = "Kenya") %>%
  dplyr::select(spatial_cluster, mpi, Location ) 
  

#### join mpa across locations 
mpi_all_locations = rbind(mpi_kenya, mpi_bangladesh, cam_gps_mpi) %>%
  mutate(location = Location) %>% dplyr::select(-Location)
head(mpi_all_locations)

# join to Rao
rao_mpi = left_join(mpi_all_locations, rao_by_cluster, by = c("location", "spatial_cluster")) %>%
  group_by(location) %>%
  mutate(max_mpi = max(mpi, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mpi_standard = mpi/max_mpi)
head(rao_mpi)

fig3a_build = ggplot(data = rao_mpi %>%  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia")))) +
  geom_point(aes(x = mpi_standard, y = rao), col = "gray20", cex = 3, alpha = .8) +
  facet_wrap(vars(location)) +
  geom_smooth(aes(x = mpi_standard, y = rao), 
            method = "lm", se = TRUE, color = "darkmagenta" ) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 10),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom") +
  xlab("Multidimensional poverty") +
  ylab("Rao's quadratic index") +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))  + scale_x_continuous(n.breaks = 4) +
  theme(panel.spacing = unit(0.5, "cm"))
fig3a_build

fig3a <- fig3a_build +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3a

# correlations 

rao_mpi %>%
  group_by(location) %>%
  summarise(
    test = list(cor.test(rao, mpi, method = "spearman")),
    n    = sum(!is.na(rao) & !is.na(mpi)) ) %>%
  mutate(
    cor     = map_dbl(test, ~ .x$estimate),
    p_value = map_dbl(test, ~ .x$p.value) ) %>%
  dplyr::select(-test)


##########################################
# last figure which is differences in wsh conditions

# Bangladesh 
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 

wash_characterize_bangl = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation", "WSH") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(hygiene_deprived = ifelse(hwsws == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(floor_deprived = ifelse(cement == 0, 1, 0)) %>% # added
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = rowSums(across(asset_radio:asset_refrig), na.rm = TRUE)) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>% dplyr::select(-asset_count, -nutrition_deprived, -mat_edu_deprived, 
                              -electricity_deprived, -assets_deprived ) %>%
  mutate(block_r = block) %>% dplyr::select(-block) %>%
  left_join(gps_dat, by = "block_r") %>%
  mutate(spatial_cluster = block) %>%
  mutate(location = "Bangladesh") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(rao_quartile = ntile(rao, 3)) %>%
  dplyr::select(block, sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived, block_r, spatial_cluster,
                rao, rao_quartile, location) %>%
  pivot_longer(
    cols = c(sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived),
    names_to = "condition",
    values_to = "deprivation") %>%
  mutate(condition = factor(condition, levels = c( "water_deprived", "hygiene_deprived",  "sanitation_deprived", "floor_deprived"))) %>%
  mutate(condition = gsub("_deprived", "", condition)) %>%
  dplyr::select(-block, -block_r)
head(wash_characterize_bangl)

ggplot(data = wash_characterize_bangl) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_minimal() +
  scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) )  +
  ggtitle("Bangladesh")



### kenya 
head(kenya_meta)
wash_characterize_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(hygiene_deprived = ifelse(hws_bl == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(floor == 0, 1, 0)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2778, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  dplyr::select(block, sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived),
    names_to = "condition",
    values_to = "deprivation") %>%
  mutate(condition = factor(condition, levels = c( "water_deprived", "hygiene_deprived",  "sanitation_deprived", "floor_deprived"))) %>%
  mutate(condition = gsub("_deprived", "", condition))  %>%
  mutate(spatial_cluster = block, location = "Kenya") %>%
  dplyr::select(-block)  %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(rao_quartile = ntile(rao, 3)) %>%
  drop_na() %>%
  dplyr::select(spatial_cluster, rao, rao_quartile, location, condition, deprivation)
head(wash_characterize_kenya)


ggplot(data = wash_characterize_kenya) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_minimal() +
  scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) )  +
  ggtitle("Kenya")


######## cambodia 
gps_dat = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat)

wash_characterize_cam = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(hygiene_deprived = ifelse(sh138 ==1 & sh139a == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(hv123 ==1, 0, 1)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2777, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(gps_dat, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) %>% 
  mutate(floor_deprived = housing_deprived) %>%
  dplyr::select(spatial_cluster, sanitation_deprived, water_deprived, floor_deprived, hygiene_deprived) %>%
  group_by(spatial_cluster) %>%
  summarize(across(sanitation_deprived:hygiene_deprived, ~mean(.x, na.rm = TRUE))) %>%
  dplyr::select(spatial_cluster, sanitation_deprived, water_deprived, floor_deprived, hygiene_deprived) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(sanitation_deprived, water_deprived,floor_deprived, hygiene_deprived),
    names_to = "condition",
    values_to = "deprivation") %>%
  mutate(condition = factor(condition, levels = c( "water_deprived", "hygiene_deprived",  "sanitation_deprived", "floor_deprived"))) %>%
  mutate(condition = gsub("_deprived", "", condition))  %>%
  mutate(location = "Cambodia") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(rao_quartile = ntile(rao, 3)) %>%
  drop_na() %>%
  dplyr::select(spatial_cluster, rao, rao_quartile, location, condition, deprivation)
head(wash_characterize_cam)
  
ggplot(data = wash_characterize_cam) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_minimal() +
  scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) )  +
  ggtitle("Cambodia")


wash_all_loc = rbind(wash_characterize_cam, wash_characterize_kenya, wash_characterize_bangl)

fig3b_build = ggplot(data = wash_all_loc) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_bw() +
  facet_wrap(vars(location)) +
  scale_fill_viridis_d(option = "magma", name = "Rao's quadratic\nindex", begin = .2, end = .8) +
  #scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) ) +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))


fig3b <- fig3b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3b

fig3 = plot_grid(fig3a,fig3b, ncol =1, rel_heights = c(.7, .6))
fig3


################################################### may 18 

head(rao_by_cluster)
# make real maps 
kenya_map_rao <- ggplot(
  data = rao_by_cluster  %>% filter(location == "Kenya")) +
  geom_sf(data = admin_k_study,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.30),
          lwd   = 0.50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .86 ) + #, shape = 24) +  #shape = 24) +
  theme_minimal() +
  xlim(c(xmin_k, xmax_k)) +
  ylim(c(ymin_k, ymax_k)) +
  theme(legend.position = "bottom") +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20),
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank() )
kenya_map_rao

bangl_map_rao = ggplot(rao_by_cluster  %>% filter(location == "Bangladesh")) +
  geom_sf(data = admin_b_study, fill = alpha("seagreen", .06),  color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .86) +theme_minimal() +
  ylab("Latitude") + xlab("Longitude") +
  xlim(c(89.9, 90.8)) +
  ylim(c(23.9, 25.0))  +
  theme(legend.position = "bottom") +
  ggtitle("Bangladesh") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20)) + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
bangl_map_rao

plot_grid(kenya_map_rao, bangl_map_rao, nrow = 1)


# Load country-level boundaries for the world
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Filter for Cambodia
cambodia_map <- countries[countries$name == "Cambodia", ]

# First administrative boundaries (provinces)
cambodia_admin1 <- ne_states(
  country = "Cambodia",
  returnclass = "sf")

# second admin 
library(geodata)
library(sf)

# Download Cambodia admin2 boundaries
khm_admin2 <- geodata::gadm(
  country = "KHM",
  level = 2,
  path = tempdir()
)

# Convert to sf
khm_admin2_sf <- st_as_sf(khm_admin2)

cam_map_rao <- ggplot(data = rao_by_cluster  %>% filter(location == "Cambodia") ) + 
 # geom_sf(data = cambodia_map, fill =  alpha("seagreen", .06), color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .86) +theme_minimal() +
 # geom_sf(data = cambodia_admin1,
   #  fill =  alpha("seagreen", .06),
   # color = alpha("black", 0.35),
  #  linewidth = 0.35) +
  geom_sf(
    data = khm_admin2_sf,
    fill =  alpha("seagreen", .06),
    color = alpha("black", 0.25),
    linewidth = 0.2) +
  ylab("Latitude") + xlab("Longitude") +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 23), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20), 
    legend.position = "none") + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
cam_map_rao 
  
plot_grid(kenya_map_rao, bangl_map_rao, cam_map_rao, nrow = 1)
  
  
 


##################################### plot study sites 

# make real maps 
kenya_sites <- ggplot(
  data = rao_by_cluster  %>% filter(location == "Kenya")) +
  geom_sf(data = admin_k_study,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.30),
          lwd   = 0.50) +
  geom_point(aes(x = long, y = lat),fill = "black", cex = 4, alpha = .80, shape = 24 ) + #, shape = 24) +  #shape = 24) +
  theme_minimal() +
  xlim(c(xmin_k, xmax_k)) +
  ylim(c(ymin_k, ymax_k)) +
  theme(legend.position = "bottom") +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20),
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank() )
kenya_sites

bangl_sites = ggplot(rao_by_cluster  %>% filter(location == "Bangladesh")) +
  geom_sf(data = admin_b_study, fill = alpha("seagreen", .06),  color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat), fill = "black", cex = 4, alpha = .86, shape = 24) +theme_minimal() +
  ylab("Latitude") + xlab("Longitude") +
  xlim(c(89.9, 90.8)) +
  ylim(c(23.9, 25.0))  +
  theme(legend.position = "bottom") +
  ggtitle("Bangladesh") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20)) + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
bangl_sites

plot_grid(bangl_sites, kenya_sites, nrow = 1)


# Load country-level boundaries for the world
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Filter for Cambodia
cambodia_map <- countries[countries$name == "Cambodia", ]

# First administrative boundaries (provinces)
cambodia_admin1 <- ne_states(
  country = "Cambodia",
  returnclass = "sf")

# second admin 
library(geodata)
library(sf)

# Download Cambodia admin2 boundaries
khm_admin2 <- geodata::gadm(
  country = "KHM",
  level = 2,
  path = tempdir()
)

# Convert to sf
khm_admin2_sf <- st_as_sf(khm_admin2)

cam_sites <- ggplot(data = rao_by_cluster  %>% filter(location == "Cambodia") ) + 
  # geom_sf(data = cambodia_map, fill =  alpha("seagreen", .06), color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat), fill = "black", cex = 4, alpha = .86, shape = 24) +theme_minimal() +
  # geom_sf(data = cambodia_admin1,
  #  fill =  alpha("seagreen", .06),
  # color = alpha("black", 0.35),
  #  linewidth = 0.35) +
  geom_sf(
    data = khm_admin2_sf,
    fill =  alpha("seagreen", .06),
    color = alpha("black", 0.25),
    linewidth = 0.2) +
  ylab("Latitude") + xlab("Longitude") +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 23), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20), 
    legend.position = "none") + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
cam_sites

plot_grid(kenya_sites, bangl_sites, cam_sites, nrow = 1)




######################################################################
# JUNE 7 #############################################################
  
########################################################
# Pull in data 
########################################################
# 1. Bangladesh multiplex serology 

# Read in public ids 
public_ids = read.csv(file = here("data/bangl/public_ids", "public-ids.csv")) %>%
  distinct(dataid, clusterid, block, clusterid_r, block_r)
head(public_ids)

# Read in GPS data to join to ids to orient blocks, public blocks, and their locations
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 
head(gps_dat)

# Joing this to cluster information
public_id_cluster = public_ids %>%
  distinct(clusterid, block, block_r, clusterid_r) %>%
  left_join(gps_dat, by = c("block", "block_r"))
head(public_id_cluster)

# Define what is a vaccine vs pathogen antigen 
antigen_vax = c("Rubella", "Measles", "Tetanus", "Diptheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )

# Load in multiplex serology 
# remove pathogens with no signal- schisto control and covid  
# define seropositivity based on cutoff 
# only visit 3 data 
luminex_bangl <- read.csv(file = here("data/bangl/luminex/final", 
                                      "washb_bangl_luminex_igg_seropos_2025-09-21.csv")) %>%
  left_join(public_id_cluster, by = c("clusterid")) %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST")) %>%
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(mfi, agemonth, clusterid, childid, antigen, pathogen, seropos, block, block_r, med_qgpslong, med_qgpslat) 

# Check prevalences of invdividual antigens 
prevalence_check_bangl = luminex_bangl %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_bangl)
print(unique(prevalence_check_bangl$antigen))
print(unique(prevalence_check_bangl$pathogen))

prev_antigen_bangl = prevalence_check_bangl  %>%
  distinct(antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
bangl_targeting_individual = prevalence_check_bangl %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate( crypto_seropos = ifelse(
    any(antigen == "c17"   & seropos == 1) |
      any(antigen == "c23"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Cryptosporidium", crypto_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Tetanus" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus", "Campylobacter","Salmonella") & seropos ==1, 1 , 0 )) %>%
  filter(!pathogen %in% c("Norovirus", "Campylobacter", "Salmonella", "Cryptosporidium", "Giardia", "Shigella"))
print(unique(bangl_targeting_individual$pathogen))
head(bangl_targeting_individual)


# summarise(vpd = max(vpd), wash_water = max(wash_water), wash_fh = max(wash_fh), across(c(block, block_r, med_qgpslong, med_qgpslat), first)) %>%
# ungroup() %>%
# distinct(childid, block, block_r, med_qgpslong, med_qgpslat, vpd, wash_water, wash_fh) %>%
# pivot_longer(cols = c(vpd, wash_water, wash_fh), names_to = "intervention", values_to = "intervention_needed")
#head(bangl_targeting_individual)
#length(unique(bangl_targeting_individual$childid))


# targeting need by cluster 
bangl_targeting_cluster = bangl_targeting_individual %>% 
  group_by(block, pathogen) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, pathogen, non_na_denom, num, fraction) %>%
  mutate(location = "Bangladesh") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(intervention = "vpd")
head(bangl_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = bangl_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")


# add Bangl STH not using 
sth = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
  pivot_longer(cols = c(al, tt, giar, hw, sth),
               names_to = "pathogen",
               values_to = "value") %>%
  filter(!pathogen %in% c("giar", "sth")) %>%
  group_by(block, pathogen) %>%
  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  ungroup() %>%
  distinct(block, non_na_denom, num, fraction, pathogen)  %>%
  mutate(intervention = "mda", location = "Bangladesh")  %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(pathogen = replace(pathogen, pathogen == "al", "Ascaris")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "tt", "Trichuris")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "hw", "Hookworm")) 
head(sth)

# add Bangl STH 
sth = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
  pivot_longer(cols = c(al, tt, giar, hw, sth),
               names_to = "pathogen",
               values_to = "value") %>%
  filter(pathogen %in% c( "sth")) %>%
  group_by(block, pathogen) %>%
  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  ungroup() %>%
  distinct(block, non_na_denom, num, fraction, pathogen)  %>%
  mutate(intervention = "mda", location = "Bangladesh")  %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) 
head(sth)
print(unique(sth$pathogen))

#sth_all = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
#  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
#  pivot_longer(cols = c(al, tt, giar, hw, sth),
#               names_to = "pathogen",
#               values_to = "value") %>%
#  filter(!pathogen %in% c("giar", "sth")) %>%
#  group_by(block, pathogen) %>%
#  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
#  mutate(fraction = num/non_na_denom) %>%
#  ungroup() %>%
#  group_by(block) %>%
#  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
#  mutate(fraction = num/non_na_denom) %>%
#  distinct(block, non_na_denom, num, fraction)  %>%
#  ungroup() 
#head(sth_all) 
  

bangl_targeting_cluster = rbind(bangl_targeting_cluster , sth)
head(bangl_targeting_cluster)
# plot histogram of distribution of intervention need again 
ggplot(data = bangl_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")



########################################################
# 2. Kenya multiplex serology 

treatment_assignment = read.csv(file = here("data/kenya/public_ids", 
                                            "cluster_tx_masked.csv")) 

treatment_assignment = read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)
head(treatment_assignment)

antigen_vax = c("Rubella", "Measles", "Tetanus", "Diptheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )
luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                     "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST", "Trachoma")) %>%
  filter(!antigen %in% c("pfmsp1", "glurp", "lsa")) %>% # longer lasting malaria 
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(clusterid, childid, antigen, pathogen, seropos, block) 

prevalence_check_kenya = luminex_kenya %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_kenya)
print(unique(prevalence_check_kenya$antigen))
print(unique(prevalence_check_kenya$pathogen))

prev_antigen_kenya = prevalence_check_kenya  %>%
  distinct(pathogen, antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
kenya_targeting_individual = prevalence_check_kenya %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus","Campylobacter","Salmonella", "E. histolytica", "Shigella") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("P. malariae" , "P. falciparum", "P. ovale") & seropos ==1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("T. solium" , "Schistosomiasis", "Trachoma") & seropos == 1, 1, 0)) %>%
  filter(!pathogen %in% c("Norovirus", "Campylobacter", "Salmonella", "Cryptosporidium", "Giardia", "Shigella", "E. histolytica"))

print(unique(kenya_targeting_individual$pathogen))
head(kenya_targeting_individual)


# targeting need by cluster 
kenya_targeting_cluster = kenya_targeting_individual %>% 
  group_by(block, pathogen) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, pathogen, non_na_denom, num, fraction) %>%
  mutate(location = "Kenya") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(intervention = ifelse(pathogen %in% c("Measles", "Rubella"), "vpd", 
                               ifelse(pathogen %in% c("Schistosomiasis", "T. solium" ,"Trachoma"), "mda", "malaria" )))

head(kenya_targeting_cluster)


# plot histogram of distribution of intervention need 
ggplot(data = kenya_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Kenya")


########################################################
# 3. Cambodia multiplex serology 

# Load disease data. 
cambodia_serology_public = readr::read_csv(file = here("data/cambodia", "cambodia_serology_public.csv")) %>%
  mutate(womanid = ...1)
head(cambodia_serology_public)
#ttmb     : Tetanous toxoid, bm14     : Lymphatic filariasis bm14, bm33     : Lymphatic filariasis bm33, wb123    : Lymphatic filariasis wb123
#nie      : Strongyloides stercoralis NIE, sag2a    : Toxoplasma gondii SAG2A , t24      : Taenia solium T24, pfmsp19  : Plasmodium falciparum MSP-1(19)
#pvmsp19  : Plasmodium vivax MSP-1(19)

# Load location data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
gps_dat = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon)
head(gps_dat)

# Transform into seropositive vs seronegative using: https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004699#pntd-0004699-t001
# https://journals.asm.org/doi/10.1128/cvi.00052-16#T1 for tetanus
antigen = c("ttmb", "bm14", "wb123", "bm33", "nie", "sag2a", "t24", "pfmsp19", "pvmsp19")
cutoff = c(100, 65, 115, 966,792, 159, 486, 343, 196 )
pathogen = c("Tetanus", "Lymphatic filariasis", "Lymphatic filariasis", "Lymphatic filariasis", 
             "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
             "Plasmodium vivax")
antigen_path_dat = data.frame(antigen, cutoff, pathogen)

antigen_vax = c( "Tetanus")
antigen_path = c("Lymphatic filariasis", "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
                 "Plasmodium vivax")

prevalence_check_cam = cambodia_serology_public %>%
  pivot_longer(cols = ttmb:gst, names_to = "antigen", values_to = "value") %>%
  filter(antigen != "gst") %>%
  filter(antigen != "bm33") %>% # we dont want this in our defintion
  mutate(psuid = psuid + 1) %>% 
  left_join(antigen_path_dat, by = "antigen") %>%
  mutate(seropos = ifelse(value > cutoff & pathogen %in% antigen_path , 1, 
                          ifelse(value < cutoff & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(womanid, psuid, age, parity, antigen, pathogen, seropos) %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90) 
head(prevalence_check_cam) 
print(unique(prevalence_check_cam$antigen))
print(unique(prevalence_check_cam$pathogen))

cam_antigen = prevalence_check_cam %>%
  distinct(pathogen, pop_seropos)

# Targeting groups by individual
# make sure to use two antigen positvity defintion for LF
cam_targeting_individual = prevalence_check_cam %>%
  group_by(womanid) %>%
  mutate(lf_seropos = ifelse(any(antigen == "bm14" & seropos == 1) & any(antigen == "wb123" & seropos == 1), 1, 0 ),
         seropos = ifelse(pathogen == "Lymphatic filariasis", lf_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Tetanus") & seropos == 1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("Strongyloides stercoralis" , "Lymphatic filariasis" ) & seropos == 1, 1, 0)) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Toxoplasma gondii") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("Plasmodium vivax", "P. falciparum") & seropos ==1, 1, 0)) %>%
  ungroup() %>%
  filter(pathogen != "Toxoplasma gondii") %>%
  distinct(womanid, psuid, pathogen, seropos)
head(cam_targeting_individual)

# targeting need by cluster 
cam_targeting_cluster = cam_targeting_individual %>% 
  group_by(psuid, pathogen) %>%
  summarise(non_na_denom = n_distinct(womanid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num / non_na_denom, location = "Cambodia", spatial_cluster = psuid ) %>%
  dplyr::select(-psuid) %>%
  ungroup() %>%
  mutate(intervention = ifelse(pathogen == "Tetanus", "vpd",
                               ifelse(pathogen %in% c("Plasmodium falciparum", "Plasmodium vivax"), "malaria", "mda")))
head(cam_targeting_cluster)

# plot histogram of distribution of intervention need 
ggplot(data = cam_targeting_cluster) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(intervention), scales = "free") + theme_minimal() +
  ggtitle("Cambodia")

########################################################
# Join data together 

multipathogen_indices = rbind(cam_targeting_cluster, 
                              kenya_targeting_cluster,
                              bangl_targeting_cluster) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup()
head(multipathogen_indices )


# plot out 
########################################################
# Compare targeting by multipathogen index 

# diversity functions 
shannon_entropy <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  -sum(p * log(p))}

gini_simpson <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  1 - sum(p^2)}

alpha_diversity <- function(p) sum(p > 0)

rao_quadratic <- function(p) {
  if (length(p) < 2) return(0)
  sum(sapply(combn(p, 2, simplify = FALSE), prod))}

# make data into wide, one row per spatial_cluster per location, columns = interventions
cluster_wide <- multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, fraction) %>%
  pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
head(cluster_wide)

min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
                                      rank_col, interventions_in_combo,
                                      threshold = 0.80) {
  
  # Use fraction_rao for rao_quadratic, fraction for everything else
  # frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
  frac_col <- if (rank_col == "rao_quadratic") "fraction" else "fraction"
  
  total_burden <- cluster_prev_sub %>%
    filter(pathogen %in% interventions_in_combo) %>%
    group_by(pathogen) %>%
    #  summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
    summarise(total = sum(.data[["num"]], na.rm = TRUE), .groups = "drop")
  
  
  ranked_clusters <- cluster_scores_sub %>%
    arrange(desc(.data[[rank_col]])) %>%
    pull(spatial_cluster)
  
  cumulative <- tibble(pathogen = interventions_in_combo, cum_burden = 0)
  
  for (i in seq_along(ranked_clusters)) {
    clust <- ranked_clusters[i]
    this_cluster <- cluster_prev_sub %>%
      filter(spatial_cluster == clust, pathogen %in% interventions_in_combo) %>%
      dplyr::select(pathogen, all_of(frac_col), num) %>%
      rename(frac_val = all_of(frac_col))
    
    cumulative <- cumulative %>%
      left_join(this_cluster, by = "pathogen") %>%
      # mutate(cum_burden = cum_burden + replace_na(frac_val, 0)) %>%
      mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
      dplyr::select(pathogen, cum_burden)
    
    pct_reached <- cumulative %>%
      left_join(total_burden, by = "pathogen") %>%
      mutate(pct = cum_burden / total)
    
    if (all(pct_reached$pct >= threshold)) {
      return(tibble(n_clusters = i, rank_method = rank_col))
    }
  }
  return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
}

# run across all locations for combinations of pathogens with at least 3 intervetions 
rank_methods <- c("shannon", "alpha_div", "gini_simpson", "rao_quadratic")

library(purrr)
results_all <- map_dfr(unique(multipathogen_indices$location), function(loc) {
  
  loc_data <- multipathogen_indices %>% filter(location == loc)
  all_interventions <- unique(loc_data$pathogen)
  n_interventions <- length(all_interventions)
  
  # All combinations of size 3 up to n_interventions
  combos <- map(3:n_interventions, ~ combn(all_interventions, .x, simplify = FALSE)) %>%
    flatten()
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Total clusters in location — calculated from full loc_data, not the combo subset
    n_total_clusters <- n_distinct(loc_data$spatial_cluster)
    
    cluster_prev_sub <- loc_data %>% filter(pathogen %in% combo)
    
    # Recompute diversity scores for this combo only
    cluster_scores_sub <- cluster_wide %>%
      filter(location == loc) %>%
      dplyr::select(spatial_cluster, any_of(combo)) %>%
      rowwise() %>%
      mutate(shannon      = shannon_entropy(c_across(all_of(combo))),
             alpha_div    = alpha_diversity(c_across(all_of(combo))),
             gini_simpson = gini_simpson(c_across(all_of(combo))),
             rao_quadratic = rao_quadratic(c_across(all_of(combo)))  ) %>%
      ungroup()
    
    n_total_clusters <- n_distinct(cluster_prev_sub$spatial_cluster)
    
    map_dfr(rank_methods, function(rm) {
      res <- min_clusters_to_threshold(cluster_prev_sub      = cluster_prev_sub,
                                       cluster_scores_sub    = cluster_scores_sub,
                                       rank_col              = rm,
                                       interventions_in_combo = combo )
      res %>% mutate(location = loc,
                     combo = combo_label,
                     n_total_clusters = n_total_clusters,
                     prop_clusters   = n_clusters / n_total_clusters )
    })
  })
})

# ---------------------------------------------------------------
# PLOT: distribution of proportion of clusters needed, facet by location
# ---------------------------------------------------------------

method_labels <- c(shannon      = "Shannon diversity index",
                   alpha_div    = "Alpha diversity",
                   gini_simpson = "Gini-Simpson index",
                   rao_quadratic = "Rao's quadratic index")
library(forcats)
results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = fct_reorder(rank_method, prop_clusters, median, na.rm = TRUE)) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 21, width = 0.5) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    title    = "Proportion of clusters needed to reach 80% coverage",
    # subtitle = "Across all intervention combinations of ≥3 groups",
    x        = NULL,
    y        = "Proportion of spatial clusters required" ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

head(results_all)
summary_results_all = results_all %>%
  group_by(rank_method, location) %>%
  mutate(mean_clust = mean(prop_clusters)) %>%
  ungroup() %>%
  distinct(rank_method, location, mean_clust)


fig1a_build = results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = factor(rank_method, levels = c(
    "Rao's quadratic index",
    "Alpha diversity", 
    "Shannon diversity index", 
    "Gini-Simpson index"))) %>%
  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_hline(yintercept = .80, col = "gray", lty = "dashed", lwd = 1.25) +
  # stat_summary(fun = mean, geom = "bar", alpha = 0.7, width = 0.6) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.6, shape = 16) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    #title = "Proportion of clusters needed to reach 80% coverage",
    x     = NULL,
    y     = "Proportion of clusters to reach 80% coverage" ) + # to\nreac
  theme_bw(base_size = 11) +
 # theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0.55, .86)) +
  scale_fill_viridis_d(option = "D" , end = .7 , name = "Strategy", alpha = .66) +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    legend.position = "right")  +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))
fig1a_build

fig1a <- fig1a_build +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1a



#################################### Map out all Rao surfaces 
# Bangl 
gps_dat_bangl = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat)  %>%
  mutate(spatial_cluster = block_r)
head(gps_dat_bangl)

bangl_rao_clusters = left_join(bangl_targeting_cluster, gps_dat_bangl, by = "spatial_cluster") %>%
  mutate(lat = med_qgpslat, long = med_qgpslong) %>%
  dplyr::select(-block, -block_r, -med_qgpslat, -med_qgpslong ) 
head(bangl_rao_clusters)

# Cam
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 

gps_dat_cam = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon) %>%
  mutate(spatial_cluster = psuid)
head(gps_dat_cam)

cam_rao_clusters = left_join(cam_targeting_cluster, gps_dat_cam, by = "spatial_cluster") %>%
  mutate(lat = dhslat, long = dhslon) %>%
  dplyr::select(-psuid, -dhslat, -dhslon ) 
head(cam_rao_clusters)

# Kenya 
gps_dat_kenya = readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat))  %>%
  distinct(block, long, lat) %>%
  mutate(spatial_cluster = block) %>%
  ungroup() %>%
  dplyr::select(-block)
head(gps_dat_kenya)

kenya_rao_clusters = left_join(kenya_targeting_cluster, gps_dat_kenya, by = "spatial_cluster") 
head(kenya_rao_clusters)

###################################
# Put together 

dat_locations = rbind(bangl_rao_clusters, cam_rao_clusters, kenya_rao_clusters) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup() %>% 
  drop_na()
head(dat_locations)

place_loc = dat_locations %>%
  distinct(location, lat, long)
head(place_loc)
write.csv(place_loc, "place_loc.csv")

####################################
# Map 

# Calculate Rao per spatial cluster using all interventions present
rao_by_cluster = dat_locations %>%
  group_by(location, spatial_cluster, lat, long) %>%
  summarise(
    rao = rao_quadratic(fraction),
    # rao = rao_quadratic(fraction_rao),
    .groups = "drop") %>%
  group_by(location) %>%
  mutate(max_rao = max(rao)) %>%
  mutate(rao = rao/max_rao) %>%
  ungroup() %>%
  drop_na()
head(rao_by_cluster)

# Calculate Moran's I per location
library(spdep)
morans_by_location = rao_by_cluster %>%
  group_by(location) %>%
  group_modify(~ {
    coords <- as.matrix(.x[, c("long", "lat")])
    nb     <- knn2nb(knearneigh(coords, k = 4))
    lw     <- nb2listw(nb, style = "W")
    mi     <- moran.test(.x$rao, lw)
    tibble(moran_i = mi$estimate[["Moran I statistic"]])
  }) %>%
  ungroup() %>%
  mutate(label = paste0("I = ", formatC(moran_i, digits = 2, format = "f")))

# Join labels back for plotting
rao_plot_dat = left_join(rao_by_cluster, morans_by_location, by = "location")

# Plot
fig1b_build = ggplot(rao_by_cluster, aes(x = long, y = lat, color = rao)) +
  geom_point(size = 6, alpha = 0.9) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  facet_wrap(vars(location), scales = "free") +
  geom_text(data = morans_by_location,
            aes(label = label),
            x = Inf, y = Inf,
            hjust = 1.05, vjust = 1.5,
            inherit.aes = FALSE,
            size = 5.5, color = "black" ) +
  labs(
    #  title = "Spatial distribution of multipathogen burden diversity",
    # subtitle = "Rao's quadratic entropy across spatial clusters",
    x = "Longitude",
    y = "Latitude") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x =  element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 14), 
    legend.position = "bottom") +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))
fig1b_build

fig1b <- fig1b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1b

# figure 1
plot_grid(fig1a, fig1b, ncol = 1, rel_heights = c(.5, .6))


########### step 2, spatial autocorrelation against effiency 

#### Part 2 ###################################
################################################
# plot effiency vs autocorrelation within treated units 
dat_locations = dat_locations %>% drop_na()

results_combo = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data          <- dat_locations %>% filter(location == loc)
  all_pathogens     <- unique(loc_data$pathogen)
  n_total           <- n_distinct(loc_data$spatial_cluster)
  combos            <- map(3:length(all_pathogens), ~ combn(all_pathogens, .x, simplify = FALSE)) %>% flatten()
  
  coords <- loc_data %>%
    distinct(spatial_cluster, lat, long) %>%
    arrange(spatial_cluster)
  nb <- knn2nb(knearneigh(as.matrix(coords[, c("long", "lat")]), k = 4))
  lw <- nb2listw(nb, style = "W")
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    rao_surface <- loc_data %>%
      filter(pathogen %in% combo) %>%
      group_by(spatial_cluster) %>%
      summarise(rao_val = rao_quadratic(fraction), .groups = "drop") %>%
      arrange(spatial_cluster)
    
    moran_i <- moran.test(rao_surface$rao_val, lw)$estimate[["Moran I statistic"]]
    
    ranked <- rao_surface %>% arrange(desc(rao_val)) %>% pull(spatial_cluster)
    
    totals <- loc_data %>%
      filter(pathogen %in% combo) %>%
      group_by(pathogen) %>%
      summarise(total = sum(num, na.rm = TRUE), .groups = "drop")
    
    cumulative <- tibble(pathogen = combo, cum = 0)
    
    n_clusters <- NA_integer_
    for (i in seq_along(ranked)) {
      this <- loc_data %>%
        filter(spatial_cluster == ranked[i], pathogen %in% combo) %>%
        dplyr::select(pathogen, num)
      
      cumulative <- cumulative %>%
        left_join(this, by = "pathogen") %>%
        mutate(cum = cum + replace_na(num, 0)) %>%
        dplyr::select(pathogen, cum)
      
      pct <- cumulative %>%
        left_join(totals, by = "pathogen") %>%
        mutate(pct = cum / total)
      
      if (all(pct$pct >= 0.80)) {
        n_clusters <- i
        break
      }
    }
    
    tibble(location = loc, combo = combo_label,
           n_clusters = n_clusters,
           prop_clusters = n_clusters / n_total,
           moran_i = moran_i)
  })
})


#results_combo_final = results_combo %>% filter(
 # location == "Bangladesh",
 # str_detect(combo, "Ascaris"),
#  str_detect(combo, "Trichuris"),
#  str_detect(combo, "Hookworm"))

# Plot with labels
to_label = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  group_by(location) %>%
  mutate(label = case_when(
    prop_clusters == min(prop_clusters) ~ combo,
    moran_i == max(moran_i)             ~ combo,
    TRUE                                ~ NA_character_
  )) %>%
  ungroup()

library(ggrepel)

head(to_label)
ggplot(to_label, aes(x = moran_i, y = prop_clusters, color = location)) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  facet_wrap(vars(location), scales = "free_x") +
  geom_label_repel(data = filter(to_label, !is.na(label)),
                   aes(label = label),
                   size = 4, show.legend = FALSE,
                   max.overlaps = 20, col = "black") +
  labs(x     = "Spatial autocorrelation (Moran's I)",
       y     = "Proportion of clusters to reach 80% coverage",
       color = "Location") +
  scale_color_viridis_d(option = "D", end = 0.7) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom")

ggplot(data = to_label) +
  geom_histogram(aes(x = moran_i, fill = location))+
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.7) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom") +
  xlab("Spatial autocorrelation (Moran's I)")

ggplot(data = to_label) +
  geom_point(aes(x = moran_i, y = prop_clusters, color= location), alpha = .7, cex = 3)+
  # facet_wrap(vars(location), scales = "free_x") +
  scale_color_viridis_d(option = "D", end = 0.9) +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 15),
        axis.text       = element_text(size = 14),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.position = "bottom") +
  xlab("Spatial autocorrelation (Moran's I)") +
  ylab("Proportion of clusters to reach 80% coverage")

fig2b_build = ggplot(data = to_label %>%
                       mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia")))) +
  geom_point(aes(x = moran_i, y = prop_clusters), color = "gray60", alpha = .6, cex = 3)+
  geom_point(data = to_label %>% filter(combo %in% best_combos$combo), aes(x = moran_i, y = prop_clusters), col = "darkred", shape = 17, cex = 5) + 
  # facet_wrap(vars(location), scales = "free_x") +
  scale_color_viridis_d(option = "D", end = 0.9) +
  facet_wrap(vars(location), scales ="free_x") +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 20),
        panel.spacing.x = unit(1.15, "cm"),
        axis.text       = element_text(size = 16),
        axis.text.x = element_text( size = 14),
        axis.title      = element_text(size = 18),
        legend.text     = element_text(size = 16),
        legend.title    = element_text(size = 16),
        legend.position = "bottom") +
  xlab("Spatial autocorrelation (Moran's I)") +
  ylab("Proportion of clusters\nto reach 80% coverage") +
  theme(strip.text = element_text(size = 20, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))  + scale_x_continuous(n.breaks = 4) +
  scale_x_continuous(
    breaks = scales::breaks_extended(n = 4) ) + 
  theme(panel.spacing = unit(0.5, "cm"))
fig2b_build

fig2a<- fig2b_build +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2a




########################################################################
########################################################################
############# part 3 consider specific strategies 
head(results_combo)

# cluster_by_pathogen directly from multipathogen_indices
cluster_by_pathogen = multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, num, fraction)
print(unique(cluster_by_pathogen $pathogen))

# Step 1: best combos
best_combos = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  filter(location != "Bangladesh") %>%
  filter(moran_i >= 0.20) %>%
  filter(str_detect(combo, "Measles|Rubella|Tetanus")) %>%
  group_by(location) %>%
  slice_min(prop_clusters, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(location, combo, prop_clusters, moran_i)
head(best_combos)

bangl_select = results_combo %>%
  filter(combo == "Hookworm + Measles + Rubella + Trichuris")%>%
  dplyr::select(location, combo, prop_clusters, moran_i)
head(bangl_select)

best_combos = rbind(best_combos, bangl_select)
head(best_combos)

# Step 2: single pathogen rankings from multipathogen_indices
single_pathogen_ranking = bind_rows(
  cluster_by_pathogen %>%
    filter(location == "Bangladesh", pathogen == "Measles") %>%
    dplyr::select(location, spatial_cluster, fraction),
  
  cluster_by_pathogen %>%
    filter(location == "Kenya", pathogen == "Measles") %>%
    dplyr::select(location, spatial_cluster, fraction),
  
  cluster_by_pathogen %>%
    filter(location == "Cambodia", pathogen == "Tetanus") %>%
    dplyr::select(location, spatial_cluster, fraction)) %>%
  group_by(location) %>%
  arrange(desc(fraction)) %>%
  mutate(rank_single = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, spatial_cluster, rank_single)


# Step 3: Rao ranking per location using best combo pathogens
rao_ranking_pathogen = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  pathogens_in_combo <- str_split(best$combo[[1]], " \\+ ")[[1]]
  
  print(pathogens_in_combo)
  
  path_wide <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo) %>%
    dplyr::select(spatial_cluster, pathogen, fraction) %>%
    pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
  
  path_cols <- setdiff(names(path_wide), "spatial_cluster")
  
  path_wide %>%
    rowwise() %>%
    mutate(rao_val = rao_quadratic(c_across(all_of(path_cols)))) %>%
    ungroup() %>%
    arrange(desc(rao_val)) %>%
    mutate(rank_rao = row_number(), location = loc) %>%
    dplyr::select(location, spatial_cluster, rao_val, rank_rao)
})



# Step 4: cumulative curves
cumulative_pathogen_curves_v2 = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  combo_label        <- best$combo[[1]]
  pathogens_in_combo <- str_split(combo_label, " \\+ ")[[1]]
  
  print(unique( pathogens_in_combo))
  
  n_clusters <- n_distinct(cluster_by_pathogen %>% filter(location == loc) %>% pull(spatial_cluster))
  
  rao_order <- rao_ranking_pathogen %>%
    filter(location == loc) %>% arrange(rank_rao) %>% pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>% arrange(rank_single) %>% pull(spatial_cluster)
  
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated = rao_cum, cum_fraction = rao_cum / total_need,
             strategy = "Rao's quadratic index"),
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated = single_cum, cum_fraction = single_cum / total_need,
             strategy = "Vaccine only")
    )
  })
})




# Step 5: clusters_80
clusters_80 = cumulative_pathogen_curves_v2 %>%
  group_by(location, pathogen, strategy, combo) %>%
  summarise(prop_clusters_80 = frac_clusters[which(cum_fraction >= 0.80)[1]], .groups = "drop")

pathogen_order = clusters_80 %>%
  filter(strategy == "Rao") %>%
  arrange(location, prop_clusters_80) %>%
  group_by(location) %>%
  mutate(rao_rank = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, pathogen, rao_rank)

clusters_80 = left_join(clusters_80, pathogen_order, by = c("location", "pathogen"))

fig2c_build  = ggplot(clusters_80 %>%  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))), aes(x = reorder(pathogen, rao_rank), y = prop_clusters_80, fill = strategy)) +
  geom_point(aes(fill = strategy), cex = 4, shape = 24) +
  #facet_wrap(vars(location, combo), scales = "free_x", labeller = label_wrap_gen(width = 40)) +
  # facet_wrap(vars(location, combo), scales = "free_x", 
  #          labeller = labeller(location = label_value, combo = NULL)) +
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.8, begin = 0, name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  labs(x = "Pathogen", y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 11), axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 13),
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        legend.position = "bottom")+
 # ylab("Proportion of clusters\nto reach 80% coverage") +
  ylab("Proportion of clusters to reach 80% coverage") +
  theme( #strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(size = 14), 
    strip.text       = element_text(size = 15))
# strip.text = element_blank()) 
fig2c_build



###### try to order the pathogens 

# Step 1: get the vaccine-only value per pathogen per location
vaccine_order <- clusters_80 %>%
  filter(strategy == "Vaccine only") %>%  # replace with your exact strategy label
  group_by(location, pathogen) %>%
  summarise(vaccine_val = mean(prop_clusters_80), .groups = "drop")

# Step 2: create a per-location rank and a combined dummy x variable
clusters_80_ordered <- clusters_80 %>%
  left_join(vaccine_order, by = c("location", "pathogen")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
  group_by(location) %>%
  mutate(pathogen_ordered = reorder(pathogen, vaccine_val)) %>%
  ungroup()

# Step 3: plot using the new ordered factor
fig2c_build <- ggplot(clusters_80_ordered %>% 
                        mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))),
                      aes(x = pathogen_ordered, y = prop_clusters_80, fill = strategy)) +
  geom_point(aes(fill = strategy), cex = 5, shape = 24) +
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.8, begin = 0, name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray") +
  labs(x = "Pathogen", y = "Proportion of clusters\nto reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16), axis.title = element_text(size = 18),
        legend.text = element_text(size = 16), legend.title = element_text(size = 18),
        legend.position = "bottom",
        panel.spacing.x = unit(1.15, "cm"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 20))
fig2c_build




fig2b <- fig2c_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2b


plot_grid(fig2a, fig2b, ncol = 1, rel_heights = c(.4, .65))
#plot_grid(fig2a, fig2b, fig2c, ncol = 1)

#left = plot_grid(fig2a, fig2b, ncol = 1, rel_heights = c(.6, .5))

#fig2 = plot_grid(left, fig2c, ncol =2)
#fig2


##################################################
# last analysis, fig 3 

##################################################
# may 14
# compare rao to poverty 


# Multidimensional Poverty Index 
######################################################################################


# Cambodia 
######################################################################################
cam_wealth_data <- read_dta(file = here("projects/6-multipathogen-burden/data/cambodia", "KHPR61FL.DTA"))
head(cam_wealth_data)
table(cam_wealth_data$hv213)
table(cam_wealth_data$hv214)
table(cam_wealth_data$hv215)

# make mpi 
mpi_cambodia = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2777, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > 0.2777], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , dhsclust) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity)

head(mpi_cambodia)

# correlation with wealth 
cam_wealth_dat = cam_wealth_data %>%
  dplyr::select(hv001, hv270, hv271) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  summarize(across(hv270:hv271, ~ mean(.x))) %>%
  left_join(mpi_cambodia, by = c("dhsclust")) %>% drop_na()
head(cam_wealth_dat )
cor(cam_wealth_dat$hv271, cam_wealth_dat$mpi, method = "spearman")

ggplot(data = cam_wealth_dat) +
  geom_point(aes(x = hv271, y = mpi)) + ylab("MPI") +
  xlab("DHS Wealth index") + theme_minimal()

# add gps data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
head(gps_cambodia)

load(file = here("data/cambodia/cambodia_serology.Rdata"))
gps_dat = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat)

cam_gps_mpi = left_join(gps_cambodia, mpi_cambodia ,  by = "dhsclust") %>%
  left_join(gps_dat, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  dplyr::select(psuid, mpi) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) 
head(cam_gps_mpi)


# Bangladesh 
######################################################################################
# treatment clusters
treatment_bangl = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr) %>%
  distinct(clusterid, block, tr)
head(treatment_bangl)
print(treatment_bangl$tr)

bangl_metadata <- read.csv(file = here("data/bangl/enrollment_wealth/untouched", "washb-bangladesh-enrol-public.csv"))
head(bangl_metadata)

table(bangl_metadata$hfiacat)
table(bangl_metadata$momedu)
table(bangl_metadata$latfeces)
table(bangl_metadata$elec)
table(bangl_metadata$roof)
table(bangl_metadata$floor)
table(bangl_metadata$walls)

mpi_bangladesh = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = asset_radio + asset_tv + asset_phone + asset_bike + asset_refrig, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    #  mortality*(1/6),
    mat_edu_deprived*(1/6),
    # cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2037, 1, 0)) %>%
  group_by(block) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > 0.2037], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , block) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity) %>%
  mutate(block_r = 91 - block) %>%
  mutate(spatial_cluster = block_r) %>% mutate(Location = "Bangladesh") %>% dplyr::select(-block_r, -block) %>%
  ungroup() %>%
  dplyr::select(mpi, spatial_cluster, Location)
head(mpi_bangladesh)

#(1/6 + 1/6 + 5/18 )*.33333
#head(mpi_bangladesh)

#############################################################
# Kenya 
#washk_child_vars has whether they died 

treatment_assignment_k = read.csv(file = here("data/kenya/public_ids", 
                                              "cluster_tx_masked.csv")) 
head(treatment_assignment_k)

# household level 
kenya_meta = read_dta(file = here("data/kenya/master_files", "washk_household_vars_20190305.dta")) %>%
  left_join(treatment_assignment_k, by = "clusterid") 
head(kenya_meta)

table(kenya_meta$HHS_bi)
table(kenya_meta$mother_edu)
table(kenya_meta$tr_masked)

#child level 
child_death = read_dta(file = here("data/kenya/master_files", "washk_child_vars_20190305.dta")) %>%
  group_by(hhid) %>%
  mutate(mortality = ifelse(isdead_el %in% c(1, 3) | childdeath == 1, 1, 0)) %>%
  distinct(hhid, mortality)
head(child_death)

table(child_death$isdead_el)
table(child_death$childdeath)
table(child_death$mortality)

# blocks 
block_kenya = luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                                   "washb_kenya_luminex_igg_seropos_pathogen_2026-01-29.csv")) %>%
  distinct(clusterid,block, tr)
head(block_kenya)
table(block_kenya$tr)

head(luminex_kenya)

mpi_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2778, 1, 0)) %>%
  group_by(block) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > .2778], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , block) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity)  %>%
  ungroup() %>%
  mutate(spatial_cluster = block) %>%
  mutate(Location = "Kenya") %>%
  dplyr::select(spatial_cluster, mpi, Location ) 


#### join mpa across locations 
mpi_all_locations = rbind(mpi_kenya, mpi_bangladesh, cam_gps_mpi) %>%
  mutate(location = Location) %>% dplyr::select(-Location)
head(mpi_all_locations)

# join to Rao
rao_mpi = left_join(mpi_all_locations, rao_by_cluster, by = c("location", "spatial_cluster")) %>%
  group_by(location) %>%
  mutate(max_mpi = max(mpi, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mpi_standard = mpi/max_mpi)
head(rao_mpi)

fig3a_build = ggplot(data = rao_mpi %>%  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia")))) +
  geom_point(aes(x = mpi_standard, y = rao), col = "gray20", cex = 3, alpha = .8) +
  facet_wrap(vars(location)) +
 # geom_smooth(aes(x = mpi_standard, y = rao), 
  #            method = "lm", se = TRUE, color = "darkmagenta" ) +
  geom_smooth(aes(x = mpi_standard, y = rao + 0.001),
              method = "glm",
              formula = y ~ x,
              method.args = list(family = gaussian(link = "log")),
              se = TRUE, color = "orangered") + 
              #color = "darkmagenta") +
  theme_bw(base_size = 11) +
  theme(strip.text      = element_text(size = 20),
        axis.text       = element_text(size = 18),
        axis.title      = element_text(size = 18),
        legend.text     = element_text(size = 16),
        legend.title    = element_text(size = 16),
        legend.position = "bottom") +
  xlab("Multidimensional poverty") +
  ylab("Rao's quadratic index") +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))  + scale_x_continuous(n.breaks = 4) +
  theme(panel.spacing = unit(0.5, "cm"))
fig3a_build

fig3a <- fig3a_build +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3a



# correlations 

rao_mpi %>%
  group_by(location) %>%
  summarise(
    test = list(cor.test(rao, mpi, method = "spearman")),
    n    = sum(!is.na(rao) & !is.na(mpi)) ) %>%
  mutate(
    cor     = map_dbl(test, ~ .x$estimate),
    p_value = map_dbl(test, ~ .x$p.value) ) %>%
  dplyr::select(-test)


##########################################
# last figure which is differences in wsh conditions

# Bangladesh 
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 

wash_characterize_bangl = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation", "WSH") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(hygiene_deprived = ifelse(hwsws == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(floor_deprived = ifelse(cement == 0, 1, 0)) %>% # added
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = rowSums(across(asset_radio:asset_refrig), na.rm = TRUE)) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>% dplyr::select(-asset_count, -nutrition_deprived, -mat_edu_deprived, 
                              -electricity_deprived, -assets_deprived ) %>%
  mutate(block_r = block) %>% dplyr::select(-block) %>%
  left_join(gps_dat, by = "block_r") %>%
  mutate(spatial_cluster = block) %>%
  mutate(location = "Bangladesh") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(rao_quartile = ntile(rao, 3)) %>%
  dplyr::select(block, sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived, block_r, spatial_cluster,
                rao, rao_quartile, location) %>%
  pivot_longer(
    cols = c(sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived),
    names_to = "condition",
    values_to = "deprivation") %>%
  mutate(condition = factor(condition, levels = c( "water_deprived", "hygiene_deprived",  "sanitation_deprived", "floor_deprived"))) %>%
  mutate(condition = gsub("_deprived", "", condition)) %>%
  dplyr::select(-block, -block_r)
head(wash_characterize_bangl)

ggplot(data = wash_characterize_bangl) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_minimal() +
  scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) )  +
  ggtitle("Bangladesh")



### kenya 
head(kenya_meta)
wash_characterize_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(hygiene_deprived = ifelse(hws_bl == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(floor == 0, 1, 0)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2778, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  dplyr::select(block, sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(sanitation_deprived, water_deprived, hygiene_deprived, floor_deprived),
    names_to = "condition",
    values_to = "deprivation") %>%
  mutate(condition = factor(condition, levels = c( "water_deprived", "hygiene_deprived",  "sanitation_deprived", "floor_deprived"))) %>%
  mutate(condition = gsub("_deprived", "", condition))  %>%
  mutate(spatial_cluster = block, location = "Kenya") %>%
  dplyr::select(-block)  %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(rao_quartile = ntile(rao, 3)) %>%
  drop_na() %>%
  dplyr::select(spatial_cluster, rao, rao_quartile, location, condition, deprivation)
head(wash_characterize_kenya)


ggplot(data = wash_characterize_kenya) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_minimal() +
  scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) )  +
  ggtitle("Kenya")


######## cambodia 
gps_dat = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat)

wash_characterize_cam = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(hygiene_deprived = ifelse(sh138 ==1 & sh139a == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(hv123 ==1, 0, 1)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2777, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(gps_dat, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) %>% 
  mutate(floor_deprived = housing_deprived) %>%
  dplyr::select(spatial_cluster, sanitation_deprived, water_deprived, floor_deprived, hygiene_deprived) %>%
  group_by(spatial_cluster) %>%
  summarize(across(sanitation_deprived:hygiene_deprived, ~mean(.x, na.rm = TRUE))) %>%
  dplyr::select(spatial_cluster, sanitation_deprived, water_deprived, floor_deprived, hygiene_deprived) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(sanitation_deprived, water_deprived,floor_deprived, hygiene_deprived),
    names_to = "condition",
    values_to = "deprivation") %>%
  mutate(condition = factor(condition, levels = c( "water_deprived", "hygiene_deprived",  "sanitation_deprived", "floor_deprived"))) %>%
  mutate(condition = gsub("_deprived", "", condition))  %>%
  mutate(location = "Cambodia") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(rao_quartile = ntile(rao, 3)) %>%
  drop_na() %>%
  dplyr::select(spatial_cluster, rao, rao_quartile, location, condition, deprivation)
head(wash_characterize_cam)

ggplot(data = wash_characterize_cam) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_minimal() +
  scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) )  +
  ggtitle("Cambodia")


wash_all_loc = rbind(wash_characterize_cam, wash_characterize_kenya, wash_characterize_bangl)

fig3b_build = ggplot(data = wash_all_loc) +
  geom_boxplot(aes(x = condition, y = deprivation*100, fill = as.character(rao_quartile))) +
  theme_bw() +
  facet_wrap(vars(location)) +
  scale_fill_viridis_d(option = "magma", name = "Rao's quadratic\nindex", begin = .2, end = .8) +
  #scale_fill_viridis_d(option = "G", name = "Rao", begin = 0.3, end = .9) +
  ylab("Spatial cluster deprivation (%)") +
  xlab("Environmental/household risk") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16) ) +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))


fig3b <- fig3b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3b

fig3 = plot_grid(fig3a,fig3b, ncol =1, rel_heights = c(.7, .6))
fig3


################################################### may 18 

# Read admin 2 in 
admin_k_study <- st_read(here("data/kenya/gps/ken_admin_boundaries.shp", "ken_admin2.shp"))
# Load treatment assignment and GPS data
treatment_assignment <- read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)

gps_dat_kenya <- readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat)) %>%
  distinct(block, long, lat)

luminex_bound <- read.csv(file = here("data/kenya/luminex/final",
                                      "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  left_join(gps_dat_kenya, by = "block") %>%
  distinct(long, lat, block, eed) %>%
  group_by(long, lat) %>%
  arrange(desc(eed == "EED substudy")) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Study = eed)

# Bounding box from data
xmin_k <- min(luminex_bound$long, na.rm = TRUE) - 0.1
xmax_k <- max(luminex_bound$long, na.rm = TRUE) + 0.1
ymin_k <- min(luminex_bound$lat,  na.rm = TRUE) - 0.1
ymax_k <- max(luminex_bound$lat,  na.rm = TRUE) + 0.1


head(rao_by_cluster)
# make real maps 
kenya_map_rao <- ggplot(
  data = rao_by_cluster  %>% filter(location == "Kenya")) +
  geom_sf(data = admin_k_study,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.30),
          lwd   = 0.50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .93 ) + #, shape = 24) +  #shape = 24) +
  theme_minimal() +
  xlim(c(xmin_k, xmax_k)) +
  ylim(c(ymin_k, ymax_k)) +
  theme(legend.position = "none") +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20),
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank() )
kenya_map_rao


admin_b_study <- get_map("district")

bangl_map_rao = ggplot(rao_by_cluster  %>% filter(location == "Bangladesh")) +
  geom_sf(data = admin_b_study, fill = alpha("seagreen", .06),  color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .93) +theme_minimal() +
  ylab("Latitude") + xlab("Longitude") +
  xlim(c(89.9, 90.8)) +
  ylim(c(23.9, 25.0))  +
  theme(legend.position = "none") +
  ggtitle("Bangladesh") +
  ylab("") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20)) + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
bangl_map_rao

plot_grid(bangl_map_rao, kenya_map_rao, nrow = 1)


# Load country-level boundaries for the world
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Filter for Cambodia
cambodia_map <- countries[countries$name == "Cambodia", ]

# First administrative boundaries (provinces)
cambodia_admin1 <- ne_states(
  country = "Cambodia",
  returnclass = "sf")

# second admin 
library(geodata)
library(sf)

# Download Cambodia admin2 boundaries
khm_admin2 <- geodata::gadm(
  country = "KHM",
  level = 2,
  path = tempdir()
)

# Convert to sf
khm_admin2_sf <- st_as_sf(khm_admin2)

cam_map_rao <- ggplot(data = rao_by_cluster  %>% filter(location == "Cambodia") ) + 
  # geom_sf(data = cambodia_map, fill =  alpha("seagreen", .06), color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .93) +theme_minimal() +
  # geom_sf(data = cambodia_admin1,
  #  fill =  alpha("seagreen", .06),
  # color = alpha("black", 0.35),
  #  linewidth = 0.35) +
  geom_sf(
    data = khm_admin2_sf,
    fill =  alpha("seagreen", .06),
    color = alpha("black", 0.25),
    linewidth = 0.2) +
  ylab("Latitude") + xlab("Longitude") +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 14), legend.title = element_text(size = 14), 
    legend.position = "right") + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
cam_map_rao 

plot_grid(bangl_map_rao, kenya_map_rao, cam_map_rao, nrow = 1)




two = plot_grid(bangl_map_rao, kenya_map_rao, nrow = 1)
two

maps_rao = plot_grid(two , cam_map_rao )

fig1b_shifted <- ggdraw() +
  draw_plot(maps_rao, x = 0.05, y = 0, width = 0.95, height = 1)


fig1b<- fig1b_shifted +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1b

plot_grid(fig1a,  fig1b, ncol = 1)

plot_grid(
  fig1a,
  NULL,
  fig1b,
  ncol = 1,
  rel_heights = c(1, 0.1, 1)
)




##################################### plot study sites 

# Read admin 2 in 
admin_k_study <- st_read(here("data/kenya/gps/ken_admin_boundaries.shp", "ken_admin2.shp"))
# Load treatment assignment and GPS data
treatment_assignment <- read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)

gps_dat_kenya <- readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat)) %>%
  distinct(block, long, lat)

luminex_bound <- read.csv(file = here("data/kenya/luminex/final",
                                      "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  left_join(gps_dat_kenya, by = "block") %>%
  distinct(long, lat, block, eed) %>%
  group_by(long, lat) %>%
  arrange(desc(eed == "EED substudy")) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Study = eed)

# Bounding box from data
xmin_k <- min(luminex_bound$long, na.rm = TRUE) - 0.1
xmax_k <- max(luminex_bound$long, na.rm = TRUE) + 0.1
ymin_k <- min(luminex_bound$lat,  na.rm = TRUE) - 0.1
ymax_k <- max(luminex_bound$lat,  na.rm = TRUE) + 0.1

head(rao_by_cluster)
# make real maps 
kenya_sites <- ggplot(
  data = rao_by_cluster  %>% filter(location == "Kenya")) +
  geom_sf(data = admin_k_study,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.30),
          lwd   = 0.50) +
  geom_point(aes(x = long, y = lat, col = rao/max_rao), cex = 4, alpha = .80, shape = 24 ) + #, shape = 24) +  #shape = 24) +
  theme_minimal() +
  xlim(c(xmin_k, xmax_k)) +
  ylim(c(ymin_k, ymax_k)) +
  theme(legend.position = "bottom") +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20) ) #,
  #  axis.title    = element_blank(),
   # axis.text     = element_blank(),
  #  axis.ticks    = element_blank() )
kenya_sites

bangl_sites = ggplot(rao_by_cluster  %>% filter(location == "Bangladesh")) +
  geom_sf(data = admin_b_study, fill = alpha("seagreen", .06),  color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat), fill = "black", cex = 4, alpha = .86, shape = 24) +theme_minimal() +
  ylab("Latitude") + xlab("Longitude") +
  xlim(c(89.9, 90.8)) +
  ylim(c(23.9, 25.0))  +
  theme(legend.position = "bottom") +
  ggtitle("Bangladesh") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20)) + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
bangl_sites

plot_grid(bangl_sites, kenya_sites, nrow = 1)


# Load country-level boundaries for the world
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Filter for Cambodia
cambodia_map <- countries[countries$name == "Cambodia", ]

# First administrative boundaries (provinces)
cambodia_admin1 <- ne_states(
  country = "Cambodia",
  returnclass = "sf")

# second admin 
library(geodata)
library(sf)

# Download Cambodia admin2 boundaries
khm_admin2 <- geodata::gadm(
  country = "KHM",
  level = 2,
  path = tempdir()
)

# Convert to sf
khm_admin2_sf <- st_as_sf(khm_admin2)

cam_sites <- ggplot(data = rao_by_cluster  %>% filter(location == "Cambodia") ) + 
  # geom_sf(data = cambodia_map, fill =  alpha("seagreen", .06), color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat), fill = "black", cex = 4, alpha = .86, shape = 24) +theme_minimal() +
  # geom_sf(data = cambodia_admin1,
  #  fill =  alpha("seagreen", .06),
  # color = alpha("black", 0.35),
  #  linewidth = 0.35) +
  geom_sf(
    data = khm_admin2_sf,
    fill =  alpha("seagreen", .06),
    color = alpha("black", 0.25),
    linewidth = 0.2) +
  ylab("Latitude") + xlab("Longitude") +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 23), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20), 
    legend.position = "none") + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
cam_sites

plot_grid(kenya_sites, bangl_sites, cam_sites, nrow = 1)


#########################################################################
# try to predict rao 
# kenya 
## load climate 
place_clim =   readr::read_csv(file = here("data", "place_loc_climate.csv")) 
head(place_clim)

pred_rao_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(hygiene_deprived = ifelse(hws_bl == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(floor == 0, 1, 0)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(spatial_cluster = block, location = "Kenya") %>%
  dplyr::select(-block)  %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  left_join(place_clim, by = c("location", "lat", "long")) %>%
  drop_na() 
head(pred_rao_kenya)

mod = glm(rao ~ mpi_standard + water_deprived + mat_edu_deprived +
            annual_precip_mm, data = pred_rao_kenya) 
summary(mod)
library(performance)
r2(mod) #.217

library(BART)
library(tidyverse)

# ── 1. Prep predictor matrix and outcome ──────────────────────────────────────
predictors <- pred_rao_kenya %>%
  dplyr::select(nutrition_deprived:assets_deprived, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()

outcome <- pred_rao_kenya$rao

# Pairwise Spearman correlations between rao and each predictor
spearman_df <- tibble(
  variable = colnames(predictors),
  rho = apply(predictors, 2, function(x) cor(x, outcome, method = "spearman", use = "complete.obs"))
) %>%
  arrange(rho) %>%
  mutate(variable = factor(variable, levels = variable),
         direction = ifelse(rho >= 0, "positive", "negative"))

# Plot
ggplot(spearman_df, aes(x = rho, y = variable, fill = direction)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("positive" = "darkmagenta", "negative" = "steelblue4"), guide = "none") +
  theme_bw(base_size = 13) +
  labs(x = "Spearman's rho", y = NULL, title = "Kenya") +
  theme(axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 13),
        plot.title  = element_text(size = 14),
        panel.grid.major.y = element_blank())

# ── 2. K-fold cross validation (k = 5) ───────────────────────────────────────
set.seed(124)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))

cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  #train_y <- outcome_logit[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 1000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}



### run with logit 
# 1. Transform the outcome to stretch the tails — beta regression scale
# logit-transform pulls extreme values apart
#outcome_logit <- log(outcome / (1 - outcome))  # requires outcome strictly in (0,1)
outcome_logit <- log((outcome + 0.01) / (1 - outcome + 0.01))  # if you have 0s or 1s

cv_preds_logit <- numeric(nrow(predictors))  # store logit-scale predictions

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome_logit[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 1000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds_logit[folds == i] <- fit$yhat.test.mean  # store on logit scale
}

# back-transform AFTER loop completes
cv_preds <- exp(cv_preds_logit) / (1 + exp(cv_preds_logit))



# ── 3. Error metrics ──────────────────────────────────────────────────────────
mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- 1 - (ss_res / ss_tot)
r2 = round(r2, digits = 3)
r2



# ── 4. Figure 1: Observed vs Predicted ───────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit <- ggplot(cv_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  annotate("text", x = min(outcome, na.rm = TRUE), 
           y = max(cv_preds, na.rm = TRUE),
           label =  " ", 
           hjust = 0, vjust = 1, size = 5) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Kenya", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14))

fig_fit

# ── 5. Full model for variable importance ─────────────────────────────────────
fit_full <- wbart(x.train = predictors,
                  y.train = outcome,
                  ndpost  = 1000,
                  nskip   = 250,
                  printevery = 0)

# Variable inclusion proportions (times each var used in tree splits)
var_importance <- tibble(
  variable   = colnames(predictors),
  importance = colMeans(fit_full$varcount / rowSums(fit_full$varcount))
) %>%
  arrange(importance) %>%
  mutate(variable = factor(variable, levels = variable))

# ── 6. Figure 2: Variable Importance ─────────────────────────────────────────
fig_varimp <- ggplot(var_importance, aes(x = importance, y = variable)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  theme_bw(base_size = 13) +
  labs(x = "Relative variable inclusion proportion",
       y = NULL,
       title = "Variable importance- Kenya") +
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 12))

fig_varimp

plot_grid(fig_fit, fig_varimp, nrow = 1, rel_widths = c(.4, .7))



###########################################
# bangl 
# Bangladesh 
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 


pred_rao_bangl = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation", "WSH") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(hygiene_deprived = ifelse(hwsws == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(floor_deprived = ifelse(cement == 0, 1, 0)) %>% # added
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = rowSums(across(asset_radio:asset_refrig), na.rm = TRUE)) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(block_r = block) %>% dplyr::select(-block) %>%
  left_join(gps_dat, by = "block_r") %>%
  mutate(spatial_cluster = block) %>%
  mutate(location = "Bangladesh") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster"))  %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>%  mutate(lat = round(lat, 5), long = round(long, 5)), by = c("location", "lat", "long")) %>%
  drop_na() 
head(pred_rao_bangl)


# ── 1. Prep predictor matrix and outcome ──────────────────────────────────────
predictors <- pred_rao_bangl %>%
  dplyr::select(nutrition_deprived:assets_deprived, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()

outcome <- pred_rao_bangl$rao

# Pairwise Spearman correlations between rao and each predictor
spearman_df <- tibble(
  variable = colnames(predictors),
  rho = apply(predictors, 2, function(x) cor(x, outcome, method = "spearman", use = "complete.obs"))
) %>%
  arrange(rho) %>%
  mutate(variable = factor(variable, levels = variable),
         direction = ifelse(rho >= 0, "positive", "negative"))

# Plot
ggplot(spearman_df, aes(x = rho, y = variable, fill = direction)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("positive" = "darkmagenta", "negative" = "steelblue4"), guide = "none") +
  theme_bw(base_size = 13) +
  labs(x = "Spearman's rho", y = NULL, title = "Bangladesh") +
  theme(axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 13),
        plot.title  = element_text(size = 14),
        panel.grid.major.y = element_blank())


# ── 2. K-fold cross validation (k = 5) ───────────────────────────────────────
set.seed(129)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))

cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 2000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

# ── 3. Error metrics ──────────────────────────────────────────────────────────
mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- 1 - (ss_res / ss_tot)
r2
r2 = round(r2, digits = 3)


# ── 4. Figure 1: Observed vs Predicted ───────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit <- ggplot(cv_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  annotate("text", x = min(outcome, na.rm = TRUE), 
           y = max(cv_preds, na.rm = TRUE),
           label = "",
           hjust = 0, vjust = 1, size = 5) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Bangladesh", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14))

fig_fit

# Plot residuals vs observed - will show the pattern clearly
cv_df %>%
  mutate(residual = predicted - observed) %>%
  ggplot(aes(x = observed, y = residual)) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkmagenta") +
  theme_bw() +
  labs(title = "Bangladesh", x = "Observed Rao's index", y = "Residual (predicted - observed)")

# ── 5. Full model for variable importance ─────────────────────────────────────
fit_full <- wbart(x.train = predictors,
                  y.train = outcome,
                  ndpost  = 1000,
                  nskip   = 250,
                  printevery = 0)

# Variable inclusion proportions (times each var used in tree splits)
var_importance <- tibble(
  variable   = colnames(predictors),
  importance = colMeans(fit_full$varcount / rowSums(fit_full$varcount))) %>%
  arrange(importance) %>%
  mutate(variable = factor(variable, levels = variable))

# ── 6. Figure 2: Variable Importance ─────────────────────────────────────────
fig_varimp <- ggplot(var_importance, aes(x = importance, y = variable)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  theme_bw(base_size = 13) +
  labs(x = "Relative variable inclusion proportion",
       y = NULL,
       title = "Variable Importance- Bangladesh") +
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 12))

fig_varimp

plot_grid(fig_fit, fig_varimp, nrow = 1, rel_widths = c(.4, .7))

################# campbodia 
# Load location data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
gps_dat_cam = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat_cam)


predict_rao_cam = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(hygiene_deprived = ifelse(sh138 ==1 & sh139a == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(hv123 ==1, 0, 1)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  left_join(gps_dat_cam, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) %>% 
  mutate(floor_deprived = housing_deprived) %>%
  group_by(spatial_cluster) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(location = "Cambodia") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>%  mutate(lat = round(lat, 5), long = round(long, 5)), by = c("location", "lat", "long")) %>%
  drop_na() 
head(predict_rao_cam )

cam_distances = readr::read_csv(file = here("data/cambodia", "healthcare_dist_cambodia_with_distances.csv")) %>%
  mutate(spatial_cluster = psuid) %>%
  left_join(predict_rao_cam, by = "spatial_cluster") %>% drop_na()
head(cam_distances)


# ── 1. Prep predictor matrix and outcome ──────────────────────────────────────
predictors <- cam_distances  %>%
  dplyr::select(nutrition_deprived, hygiene_deprived, mat_edu_deprived, 
                sanitation_deprived, water_deprived, electricity_deprived,
                cooking_deprived, floor_deprived, housing_deprived, 
                assets_deprived, mpi_standard, distance_km, mean_temp_C, annual_precip_mm) %>%
  as.matrix()

outcome <- cam_distances$rao

## check correlations 

# Pairwise Spearman correlations between rao and each predictor
spearman_df <- tibble(
  variable = colnames(predictors),
  rho = apply(predictors, 2, function(x) cor(x, outcome, method = "spearman", use = "complete.obs"))
) %>%
  arrange(rho) %>%
  mutate(variable = factor(variable, levels = variable),
         direction = ifelse(rho >= 0, "positive", "negative"))

# Plot
ggplot(spearman_df, aes(x = rho, y = variable, fill = direction)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("positive" = "darkmagenta", "negative" = "steelblue4"), guide = "none") +
  theme_bw(base_size = 13) +
  labs(x = "Spearman's rho", y = NULL, title = "Cambodia") +
  theme(axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 13),
        plot.title  = element_text(size = 14),
        panel.grid.major.y = element_blank())


# ── 2. K-fold cross validation (k = 5) ───────────────────────────────────────
set.seed(129)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))

cv_preds <- numeric(nrow(predictors))


for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 2000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}


#outcome_logit <- log(outcome / (1 - outcome))  # requires outcome strictly in (0,1)
outcome_logit <- log((outcome + 0.01) / (1 - outcome + 0.01))  # if you have 0s or 1s

cv_preds_logit <- numeric(nrow(predictors))  # store logit-scale predictions

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome_logit[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 2000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds_logit[folds == i] <- fit$yhat.test.mean  # store on logit scale
}

# back-transform AFTER loop completes
cv_preds <- exp(cv_preds_logit) / (1 + exp(cv_preds_logit))



# ── 3. Error metrics ──────────────────────────────────────────────────────────
mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- 1 - (ss_res / ss_tot)
r2
r2 = round(r2, digits =3)

# ── 4. Figure 1: Observed vs Predicted ───────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit <- ggplot(cv_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  annotate("text", x = min(outcome, na.rm = TRUE), 
           y = max(cv_preds, na.rm = TRUE),
           label = "",
           hjust = 0, vjust = 1, size = 5) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Cambodia", 
       subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14))

fig_fit

# ── 5. Full model for variable importance ─────────────────────────────────────
fit_full <- wbart(x.train = predictors,
                  y.train = outcome,
                  ndpost  = 1000,
                  nskip   = 250,
                  printevery = 0)

# Variable inclusion proportions (times each var used in tree splits)
var_importance <- tibble(
  variable   = colnames(predictors),
  importance = colMeans(fit_full$varcount / rowSums(fit_full$varcount))
) %>%
  arrange(importance) %>%
  mutate(variable = factor(variable, levels = variable))

# ── 6. Figure 2: Variable Importance ─────────────────────────────────────────
fig_varimp <- ggplot(var_importance, aes(x = importance, y = variable)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  theme_bw(base_size = 13) +
  labs(x = "Relative variable inclusion proportion",
       y = NULL,
       title = "Variable Importance- Cambodia") +
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 12))

fig_varimp

plot_grid(fig_fit, fig_varimp, nrow = 1, rel_widths = c(.4, .7))


ncol(predictors)
colnames(predictors)
ncol(fit_full$varcount)






###################################################################################
############# JUNE 16 #############################################################
###################################################################################

##########
# Figure 1 
#########

public_ids = read.csv(file = here("data/bangl/public_ids", "public-ids.csv")) %>%
  distinct(dataid, clusterid, block, clusterid_r, block_r)
head(public_ids)

# Read in GPS data to join to ids to orient blocks, public blocks, and their locations
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 
head(gps_dat)

# Joing this to cluster information
public_id_cluster = public_ids %>%
  distinct(clusterid, block, block_r, clusterid_r) %>%
  left_join(gps_dat, by = c("block", "block_r"))
head(public_id_cluster)

# Define what is a vaccine vs pathogen antigen 
antigen_vax = c("Rubella", "Measles", "Tetanus", "Diphtheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )

# Load in multiplex serology 
# remove pathogens with no signal- schisto control and covid  
# define seropositivity based on cutoff 
# only visit 3 data 
luminex_bangl <- read.csv(file = here("data/bangl/luminex/final", 
                                      "washb_bangl_luminex_igg_seropos_2025-09-21.csv")) %>%
  left_join(public_id_cluster, by = c("clusterid")) %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST")) %>%
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(mfi, agemonth, clusterid, childid, antigen, pathogen, seropos, block, block_r, med_qgpslong, med_qgpslat) 

# Check prevalences of invdividual antigens 
prevalence_check_bangl = luminex_bangl %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_bangl)
print(unique(prevalence_check_bangl$antigen))
print(unique(prevalence_check_bangl$pathogen))

prev_antigen_bangl = prevalence_check_bangl  %>%
  distinct(antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
bangl_targeting_individual = prevalence_check_bangl %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate( crypto_seropos = ifelse(
    any(antigen == "c17"   & seropos == 1) |
      any(antigen == "c23"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Cryptosporidium", crypto_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Tetanus" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus", "Campylobacter","Salmonella") & seropos ==1, 1 , 0 )) %>%
  filter(!pathogen %in% c("Norovirus", "Campylobacter", "Salmonella", "Cryptosporidium", "Giardia", "Shigella"))
print(unique(bangl_targeting_individual$pathogen))
head(bangl_targeting_individual)


# summarise(vpd = max(vpd), wash_water = max(wash_water), wash_fh = max(wash_fh), across(c(block, block_r, med_qgpslong, med_qgpslat), first)) %>%
# ungroup() %>%
# distinct(childid, block, block_r, med_qgpslong, med_qgpslat, vpd, wash_water, wash_fh) %>%
# pivot_longer(cols = c(vpd, wash_water, wash_fh), names_to = "intervention", values_to = "intervention_needed")
#head(bangl_targeting_individual)
#length(unique(bangl_targeting_individual$childid))


# targeting need by cluster 
bangl_targeting_cluster_lum = bangl_targeting_individual %>% 
  group_by(block, pathogen) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, pathogen, non_na_denom, num, fraction) %>%
  mutate(location = "Bangladesh") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(intervention = "vpd")
head(bangl_targeting_cluster_lum)

# plot histogram of distribution of intervention need 
ggplot(data = bangl_targeting_cluster_lum) +
  geom_histogram(aes(x = fraction)) +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh")


# add Bangl STH not using 
sth = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
  pivot_longer(cols = c(al, tt, giar, hw, sth),
               names_to = "pathogen",
               values_to = "value") %>%
  filter(!pathogen %in% c("giar", "sth")) %>%
  group_by(block, pathogen) %>%
  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  ungroup() %>%
  distinct(block, non_na_denom, num, fraction, pathogen)  %>%
  mutate(intervention = "mda", location = "Bangladesh")  %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(pathogen = replace(pathogen, pathogen == "al", "Ascaris")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "tt", "Trichuris")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "hw", "Hookworm")) 
head(sth)

# add Bangl STH 
#sth = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
 # dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
 # pivot_longer(cols = c(al, tt, giar, hw, sth),
  #             names_to = "pathogen",
     #          values_to = "value") %>%
  #filter(pathogen %in% c( "sth")) %>%
  #group_by(block, pathogen) %>%
 # mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
  #mutate(fraction = num/non_na_denom) %>%
 # ungroup() %>%
#  distinct(block, non_na_denom, num, fraction, pathogen)  %>%
 # mutate(intervention = "mda", location = "Bangladesh")  %>%
 # mutate(spatial_cluster = block) %>% 
 # dplyr::select(-block) 
head(sth)
print(unique(sth$pathogen))

#sth_all = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
#  dplyr::select(dataid, clusterid, block, personid, tr, al, tt, giar, hw, sth) %>%
#  pivot_longer(cols = c(al, tt, giar, hw, sth),
#               names_to = "pathogen",
#               values_to = "value") %>%
#  filter(!pathogen %in% c("giar", "sth")) %>%
#  group_by(block, pathogen) %>%
#  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
#  mutate(fraction = num/non_na_denom) %>%
#  ungroup() %>%
#  group_by(block) %>%
#  mutate(non_na_denom = n_distinct(dataid, personid), num   = sum(value, na.rm = TRUE), .groups = "drop") %>%
#  mutate(fraction = num/non_na_denom) %>%
#  distinct(block, non_na_denom, num, fraction)  %>%
#  ungroup() 
#head(sth_all) 


bangl_targeting_cluster = rbind(bangl_targeting_cluster_lum , sth)
head(bangl_targeting_cluster)
# plot histogram of distribution of intervention need again 
bangl_supp_prev = ggplot(data = bangl_targeting_cluster %>%  mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura"),
                                                                    pathogen = replace(pathogen, pathogen == "Ascaris", "A. lumbricoides") )) +
  geom_histogram(aes(x = fraction), fill = "lightskyblue3") +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Bangladesh") + ylab("Count") + xlab("Cluster-level prevalence")
bangl_supp_prev 


########################################################
# 2. Kenya multiplex serology 

treatment_assignment = read.csv(file = here("data/kenya/public_ids", 
                                            "cluster_tx_masked.csv")) 

treatment_assignment = read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)
head(treatment_assignment)

antigen_vax = c("Rubella", "Measles", "Tetanus", "Diphtheria")
antigen_path = c("T. solium", "Cholera", "E. histolytica" , "Cryptosporidium", "P. falciparum", 
                 "Schistosomiasis", "P. ovale",  "Norovirus",  "P. malariae" , "Onchocerciasis" , "Dengue",         
                 "P. vivax" , "Campylobacter", "Zika", "Salmonella" , "Trachoma" ,"Giardia"  ,      
                 "Chikungunya" ,    "LT-ETEC" ,   "Shigella", "Strongyloides" )
luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                     "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  filter(visit == 3) %>%
  filter(!pathogen %in% c("COVID19", "Schistosoma GST", "Trachoma")) %>%
  filter(!antigen %in% c("pfmsp1", "glurp", "lsa")) %>% # longer lasting malaria 
  mutate(seropos = ifelse(mfi > mficut & pathogen %in% antigen_path , 1, 
                          ifelse(mfi < mficut & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(clusterid, childid, antigen, pathogen, seropos, block) 

prevalence_check_kenya = luminex_kenya %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90)
head(prevalence_check_kenya)
print(unique(prevalence_check_kenya$antigen))
print(unique(prevalence_check_kenya$pathogen))

prev_antigen_kenya = prevalence_check_kenya  %>%
  distinct(pathogen, antigen, pop_seropos)

# Targeting groups by individual 
# Save the max so if a child was positive to any pathogen within a group, they are in need of an intervention 
kenya_targeting_individual = prevalence_check_kenya %>%
  group_by(childid) %>%
  mutate( noro_seropos = ifelse(
    any(antigen == "norogi5"   & seropos == 1) &
      any(antigen == "norogii4"  & seropos == 1) &
      any(antigen == "norogii3"  & seropos == 1) &
      any(antigen == "norogii17" & seropos == 1) &
      any(antigen == "norogii6"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Norovirus", noro_seropos, seropos)) %>%
  mutate( camp_seropos = ifelse(
    any(antigen == "p39"   & seropos == 1) &
      any(antigen == "p18"  & seropos == 1), 1, 0 ),
    seropos = ifelse(pathogen == "Campylobacter", camp_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Measles" , "Rubella") & seropos == 1, 1, 0)) %>%
  mutate(wash_water = ifelse(pathogen %in% c("Cryptosporidium", "Giardia") & seropos == 1, 1,0 )) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Norovirus","Campylobacter","Salmonella", "E. histolytica", "Shigella") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("P. malariae" , "P. falciparum", "P. ovale") & seropos ==1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("T. solium" , "Schistosomiasis", "Trachoma") & seropos == 1, 1, 0)) %>%
  filter(!pathogen %in% c("Norovirus", "Campylobacter", "Salmonella", "Cryptosporidium", "Giardia", "Shigella", "E. histolytica"))

print(unique(kenya_targeting_individual$pathogen))
head(kenya_targeting_individual)


# targeting need by cluster 
kenya_targeting_cluster = kenya_targeting_individual %>% 
  group_by(block, pathogen) %>%
  summarise(non_na_denom = n_distinct(childid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num/non_na_denom) %>%
  distinct(block, pathogen, non_na_denom, num, fraction) %>%
  mutate(location = "Kenya") %>%
  mutate(spatial_cluster = block) %>% 
  dplyr::select(-block) %>%
  mutate(intervention = ifelse(pathogen %in% c("Measles", "Rubella"), "vpd", 
                               ifelse(pathogen %in% c("Schistosomiasis", "T. solium" ,"Trachoma"), "mda", "malaria" )))

head(kenya_targeting_cluster)


# plot histogram of distribution of intervention need 
ken_supp_prev = ggplot(data = kenya_targeting_cluster) +
  geom_histogram(aes(x = fraction), fill = "lightskyblue3") +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Kenya")+ ylab("Count") + xlab("Cluster-level prevalence")
ken_supp_prev


########################################################
# 3. Cambodia multiplex serology 

# Load disease data. 
cambodia_serology_public = readr::read_csv(file = here("data/cambodia", "cambodia_serology_public.csv")) %>%
  mutate(womanid = ...1)
head(cambodia_serology_public)
#ttmb     : Tetanous toxoid, bm14     : Lymphatic filariasis bm14, bm33     : Lymphatic filariasis bm33, wb123    : Lymphatic filariasis wb123
#nie      : Strongyloides stercoralis NIE, sag2a    : Toxoplasma gondii SAG2A , t24      : Taenia solium T24, pfmsp19  : Plasmodium falciparum MSP-1(19)
#pvmsp19  : Plasmodium vivax MSP-1(19)

# Load location data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
gps_dat = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon)
head(gps_dat)

# Transform into seropositive vs seronegative using: https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004699#pntd-0004699-t001
# https://journals.asm.org/doi/10.1128/cvi.00052-16#T1 for tetanus
antigen = c("ttmb", "bm14", "wb123", "bm33", "nie", "sag2a", "t24", "pfmsp19", "pvmsp19")
cutoff = c(100, 65, 115, 966,792, 159, 486, 343, 196 )
pathogen = c("Tetanus", "Lymphatic filariasis", "Lymphatic filariasis", "Lymphatic filariasis", 
             "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
             "Plasmodium vivax")
antigen_path_dat = data.frame(antigen, cutoff, pathogen)

antigen_vax = c( "Tetanus")
antigen_path = c("Lymphatic filariasis", "Strongyloides stercoralis", "Toxoplasma gondii", "Taenia solium", "Plasmodium falciparum",
                 "Plasmodium vivax")

prevalence_check_cam = cambodia_serology_public %>%
  pivot_longer(cols = ttmb:gst, names_to = "antigen", values_to = "value") %>%
  filter(antigen != "gst") %>%
  filter(antigen != "bm33") %>% # we dont want this in our defintion
  mutate(psuid = psuid + 1) %>% 
  left_join(antigen_path_dat, by = "antigen") %>%
  mutate(seropos = ifelse(value > cutoff & pathogen %in% antigen_path , 1, 
                          ifelse(value < cutoff & pathogen %in% antigen_vax, 1, 0))) %>%
  dplyr::select(womanid, psuid, age, parity, antigen, pathogen, seropos) %>%
  group_by(antigen) %>%
  mutate(pop_seropos = mean(seropos, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(pop_seropos > .0499) %>%
  filter(pop_seropos < .90) 
head(prevalence_check_cam) 
print(unique(prevalence_check_cam$antigen))
print(unique(prevalence_check_cam$pathogen))

cam_antigen = prevalence_check_cam %>%
  distinct(pathogen, pop_seropos)

# Targeting groups by individual
# make sure to use two antigen positvity defintion for LF
cam_targeting_individual = prevalence_check_cam %>%
  group_by(womanid) %>%
  mutate(lf_seropos = ifelse(any(antigen == "bm14" & seropos == 1) & any(antigen == "wb123" & seropos == 1), 1, 0 ),
         seropos = ifelse(pathogen == "Lymphatic filariasis", lf_seropos, seropos)) %>%
  mutate(vpd = ifelse(pathogen %in% c("Tetanus") & seropos == 1, 1, 0)) %>%
  mutate(mda = ifelse(pathogen %in% c("Strongyloides stercoralis" , "Lymphatic filariasis" ) & seropos == 1, 1, 0)) %>%
  mutate(wash_fh = ifelse(pathogen %in% c("Toxoplasma gondii") & seropos ==1, 1 , 0 )) %>%
  mutate(malaria = ifelse(pathogen %in% c("Plasmodium vivax", "P. falciparum") & seropos ==1, 1, 0)) %>%
  ungroup() %>%
  filter(pathogen != "Toxoplasma gondii") %>%
  distinct(womanid, psuid, pathogen, seropos)
head(cam_targeting_individual)

# targeting need by cluster 
cam_targeting_cluster = cam_targeting_individual %>% 
  group_by(psuid, pathogen) %>%
  summarise(non_na_denom = n_distinct(womanid), num   = sum(seropos, na.rm = TRUE), .groups = "drop") %>%
  mutate(fraction = num / non_na_denom, location = "Cambodia", spatial_cluster = psuid ) %>%
  dplyr::select(-psuid) %>%
  ungroup() %>%
  mutate(intervention = ifelse(pathogen == "Tetanus", "vpd",
                               ifelse(pathogen %in% c("Plasmodium falciparum", "Plasmodium vivax"), "malaria", "mda")))
head(cam_targeting_cluster)

# plot histogram of distribution of intervention need 
cam_supp_prev = ggplot(data = cam_targeting_cluster %>%
                         mutate(pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum")) %>%
                         mutate(pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax")) %>%
                         mutate(pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis"))) +
  geom_histogram(aes(x = fraction), fill = "lightskyblue3") +
  facet_wrap(vars(pathogen), scales = "free") + theme_minimal() +
  ggtitle("Cambodia") +ylab("Count") + xlab("Cluster-level prevalence")
cam_supp_prev 

#########################################################
# Figure S2 individual level prevalences 

plot_grid(bangl_supp_prev, cam_supp_prev, ken_supp_prev,ncol = 2, rel_heights = c(.7, .9))
plot_grid(bangl_supp_prev, ken_supp_prev, cam_supp_prev, ncol = 2 , rel_heights = c(.9, .7))

########################################################
# Join data together 

multipathogen_indices = rbind(cam_targeting_cluster, 
                              kenya_targeting_cluster,
                              bangl_targeting_cluster) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup()
head(multipathogen_indices )


# plot out 
########################################################
# Compare targeting by multipathogen index 

# diversity functions 
shannon_entropy <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  -sum(p * log(p))}

gini_simpson <- function(p) {
  p <- p[p > 0]
  if (length(p) == 0) return(0)
  p <- p / sum(p)
  1 - sum(p^2)}

alpha_diversity <- function(p) sum(p > 0)

rao_quadratic <- function(p) {
  if (length(p) < 2) return(0)
  sum(sapply(combn(p, 2, simplify = FALSE), prod))}

# make data into wide, one row per spatial_cluster per location, columns = interventions
cluster_wide <- multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, fraction) %>%
  pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
head(cluster_wide)

min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
                                      rank_col, interventions_in_combo,
                                      threshold = 0.80) {
  
  # Use fraction_rao for rao_quadratic, fraction for everything else
  # frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
  frac_col <- if (rank_col == "rao_quadratic") "fraction" else "fraction"
  
  total_burden <- cluster_prev_sub %>%
    filter(pathogen %in% interventions_in_combo) %>%
    group_by(pathogen) %>%
    #  summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
    summarise(total = sum(.data[["num"]], na.rm = TRUE), .groups = "drop")
  
  
  ranked_clusters <- cluster_scores_sub %>%
    arrange(desc(.data[[rank_col]])) %>%
    pull(spatial_cluster)
  
  cumulative <- tibble(pathogen = interventions_in_combo, cum_burden = 0)
  
  for (i in seq_along(ranked_clusters)) {
    clust <- ranked_clusters[i]
    this_cluster <- cluster_prev_sub %>%
      filter(spatial_cluster == clust, pathogen %in% interventions_in_combo) %>%
      dplyr::select(pathogen, all_of(frac_col), num) %>%
      rename(frac_val = all_of(frac_col))
    
    cumulative <- cumulative %>%
      left_join(this_cluster, by = "pathogen") %>%
      # mutate(cum_burden = cum_burden + replace_na(frac_val, 0)) %>%
      mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
      dplyr::select(pathogen, cum_burden)
    
    pct_reached <- cumulative %>%
      left_join(total_burden, by = "pathogen") %>%
      mutate(pct = cum_burden / total)
    
    if (all(pct_reached$pct >= threshold)) {
      return(tibble(n_clusters = i, rank_method = rank_col))
    }
  }
  return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
}

# run across all locations for combinations of pathogens with at least 3 intervetions 
rank_methods <- c("shannon", "alpha_div", "gini_simpson", "rao_quadratic")

library(purrr)
results_all <- map_dfr(unique(multipathogen_indices$location), function(loc) {
  
  loc_data <- multipathogen_indices %>% filter(location == loc)
  all_interventions <- unique(loc_data$pathogen)
  n_interventions <- length(all_interventions)
  
  # All combinations of size 3 up to n_interventions
  combos <- map(3:n_interventions, ~ combn(all_interventions, .x, simplify = FALSE)) %>%
    flatten()
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Total clusters in location — calculated from full loc_data, not the combo subset
    n_total_clusters <- n_distinct(loc_data$spatial_cluster)
    
    cluster_prev_sub <- loc_data %>% filter(pathogen %in% combo)
    
    # Recompute diversity scores for this combo only
    cluster_scores_sub <- cluster_wide %>%
      filter(location == loc) %>%
      dplyr::select(spatial_cluster, any_of(combo)) %>%
      rowwise() %>%
      mutate(shannon      = shannon_entropy(c_across(all_of(combo))),
             alpha_div    = alpha_diversity(c_across(all_of(combo))),
             gini_simpson = gini_simpson(c_across(all_of(combo))),
             rao_quadratic = rao_quadratic(c_across(all_of(combo)))  ) %>%
      ungroup()
    
    n_total_clusters <- n_distinct(cluster_prev_sub$spatial_cluster)
    
    map_dfr(rank_methods, function(rm) {
      res <- min_clusters_to_threshold(cluster_prev_sub      = cluster_prev_sub,
                                       cluster_scores_sub    = cluster_scores_sub,
                                       rank_col              = rm,
                                       interventions_in_combo = combo )
      res %>% mutate(location = loc,
                     combo = combo_label,
                     n_total_clusters = n_total_clusters,
                     prop_clusters   = n_clusters / n_total_clusters )
    })
  })
})

head(results_all)

# ---------------------------------------------------------------
# Plot
# ---------------------------------------------------------------

method_labels <- c(shannon      = "Shannon diversity index",
                   alpha_div    = "Species richness",
                   gini_simpson = "Gini-Simpson index",
                   rao_quadratic = "Rao's quadratic index")
library(forcats)
summary_results_all = results_all %>%
  group_by(rank_method, location) %>%
  mutate(mean_clust = mean(prop_clusters)) %>%
  ungroup() %>%
  distinct(rank_method, location, mean_clust)


fig1a_build = results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = factor(rank_method, levels = c(
    "Rao's quadratic index",
    "Species richness", 
    "Shannon diversity index", 
    "Gini-Simpson index"))) %>%
  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_hline(yintercept = .80, col = "gray", lty = "dashed", lwd = 1.25) +
  # stat_summary(fun = mean, geom = "bar", alpha = 0.7, width = 0.6) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.6, shape = 16) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    #title = "Proportion of clusters needed to reach 80% coverage",
    x     = NULL,
    y     = "Proportion of clusters to reach 80% coverage" ) + # to\nreac
  theme_bw(base_size = 11) +
  # theme_minimal() +
  theme(legend.position = "botttom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0.55, .86)) +
  scale_fill_viridis_d(option = "D" , end = .7 , name = "Strategy", alpha = .66) +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    legend.position = "bottom")  +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))
fig1a_build

fig1a <- fig1a_build +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1a


fig1a <- fig1a_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1a

###################################################
# FIG 1B #########################################

# cluster_by_pathogen directly from multipathogen_indices
cluster_by_pathogen = multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, num, fraction)
print(unique(cluster_by_pathogen $pathogen))

#### add location info 
gps_dat_bangl = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat)  %>%
  mutate(spatial_cluster = block_r)
head(gps_dat_bangl)

bangl_rao_clusters = left_join(bangl_targeting_cluster, gps_dat_bangl, by = "spatial_cluster") %>%
  mutate(lat = med_qgpslat, long = med_qgpslong) %>%
  dplyr::select(-block, -block_r, -med_qgpslat, -med_qgpslong ) 
head(bangl_rao_clusters)

# Cam
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 

gps_dat_cam = gps_cambodia %>%
  mutate(psuid = ...1) %>%
  distinct(psuid, dhslat, dhslon) %>%
  mutate(spatial_cluster = psuid)
head(gps_dat_cam)

cam_rao_clusters = left_join(cam_targeting_cluster, gps_dat_cam, by = "spatial_cluster") %>%
  mutate(lat = dhslat, long = dhslon) %>%
  dplyr::select(-psuid, -dhslat, -dhslon ) 
head(cam_rao_clusters)

# Kenya 
gps_dat_kenya = readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat))  %>%
  distinct(block, long, lat) %>%
  mutate(spatial_cluster = block) %>%
  ungroup() %>%
  dplyr::select(-block)
head(gps_dat_kenya)

kenya_rao_clusters = left_join(kenya_targeting_cluster, gps_dat_kenya, by = "spatial_cluster") 
head(kenya_rao_clusters)

###################################
# Put together 

dat_locations = rbind(bangl_rao_clusters, cam_rao_clusters, kenya_rao_clusters) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup() %>% 
  drop_na()
head(dat_locations)

#place_loc = dat_locations %>%
#  distinct(location, lat, long)
#head(place_loc)
#write.csv(place_loc, "place_loc.csv")

####################################
# Map 

# Calculate Rao per spatial cluster using all interventions present
rao_by_cluster = dat_locations %>%
  group_by(location, spatial_cluster, lat, long) %>%
  summarise(
    rao = rao_quadratic(fraction),
    # rao = rao_quadratic(fraction_rao),
    .groups = "drop") %>%
  group_by(location) %>%
  mutate(max_rao = max(rao)) %>%
  mutate(rao = rao/max_rao) %>%
  ungroup() %>%
  drop_na()
head(rao_by_cluster)

# Calculate Moran's I per location
library(spdep)
morans_by_location = rao_by_cluster %>%
  group_by(location) %>%
  group_modify(~ {
    coords <- as.matrix(.x[, c("long", "lat")])
    nb     <- knn2nb(knearneigh(coords, k = 4))
    lw     <- nb2listw(nb, style = "W")
    mi     <- moran.test(.x$rao, lw)
    tibble(moran_i = mi$estimate[["Moran I statistic"]])
  }) %>%
  ungroup() %>%
  mutate(label = paste0("I = ", formatC(moran_i, digits = 2, format = "f")))

# Join labels back for plotting
rao_plot_dat = left_join(rao_by_cluster, morans_by_location, by = "location")

# combos 
dat_locations = dat_locations %>% drop_na()

results_combo = map_dfr(unique(dat_locations$location), function(loc) {
  loc_data          <- dat_locations %>% filter(location == loc)
  all_pathogens     <- unique(loc_data$pathogen)
  n_total           <- n_distinct(loc_data$spatial_cluster)
  combos            <- map(3:length(all_pathogens), ~ combn(all_pathogens, .x, simplify = FALSE)) %>% flatten()
  
  coords <- loc_data %>%
    distinct(spatial_cluster, lat, long) %>%
    arrange(spatial_cluster)
  nb <- knn2nb(knearneigh(as.matrix(coords[, c("long", "lat")]), k = 4))
  lw <- nb2listw(nb, style = "W")
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    rao_surface <- loc_data %>%
      filter(pathogen %in% combo) %>%
      group_by(spatial_cluster) %>%
      summarise(rao_val = rao_quadratic(fraction), .groups = "drop") %>%
      arrange(spatial_cluster)
    
    moran_i <- moran.test(rao_surface$rao_val, lw)$estimate[["Moran I statistic"]]
    
    ranked <- rao_surface %>% arrange(desc(rao_val)) %>% pull(spatial_cluster)
    
    totals <- loc_data %>%
      filter(pathogen %in% combo) %>%
      group_by(pathogen) %>%
      summarise(total = sum(num, na.rm = TRUE), .groups = "drop")
    
    cumulative <- tibble(pathogen = combo, cum = 0)
    
    n_clusters <- NA_integer_
    for (i in seq_along(ranked)) {
      this <- loc_data %>%
        filter(spatial_cluster == ranked[i], pathogen %in% combo) %>%
        dplyr::select(pathogen, num)
      
      cumulative <- cumulative %>%
        left_join(this, by = "pathogen") %>%
        mutate(cum = cum + replace_na(num, 0)) %>%
        dplyr::select(pathogen, cum)
      
      pct <- cumulative %>%
        left_join(totals, by = "pathogen") %>%
        mutate(pct = cum / total)
      
      if (all(pct$pct >= 0.80)) {
        n_clusters <- i
        break
      }
    }
    
    tibble(location = loc, combo = combo_label,
           n_clusters = n_clusters,
           prop_clusters = n_clusters / n_total,
           moran_i = moran_i)
  })
})




# Step 1: best combos
best_combos = results_combo %>%
  filter(!is.na(prop_clusters)) %>%
  filter(location != "Bangladesh") %>%
  filter(location != "Cambodia") %>%
 # filter(moran_i >= 0.20) %>%
  filter(str_detect(combo, "Measles|Rubella|Tetanus")) %>%
  group_by(location) %>%
  slice_min(prop_clusters, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(location, combo, prop_clusters, moran_i)
head(best_combos)

cam_select = results_combo %>%
  filter(combo == "Lymphatic filariasis + Plasmodium falciparum + Plasmodium vivax + Strongyloides stercoralis + Tetanus")%>% # this tied 
  dplyr::select(location, combo, prop_clusters, moran_i)

bangl_select = results_combo %>%
  filter(combo == "Ascaris + Hookworm + Measles + Rubella + Trichuris")%>%
  dplyr::select(location, combo, prop_clusters, moran_i)
head(bangl_select)

best_combos = rbind(best_combos, bangl_select, cam_select)
head(best_combos)

# Step 2: single pathogen rankings from multipathogen_indices
single_pathogen_ranking = bind_rows(
  cluster_by_pathogen %>%
    filter(location == "Bangladesh", pathogen == "Measles") %>%
    dplyr::select(location, spatial_cluster, fraction),
  
  cluster_by_pathogen %>%
    filter(location == "Kenya", pathogen == "Measles") %>%
    dplyr::select(location, spatial_cluster, fraction),
  
  cluster_by_pathogen %>%
    filter(location == "Cambodia", pathogen == "Tetanus") %>%
    dplyr::select(location, spatial_cluster, fraction)) %>%
  group_by(location) %>%
  arrange(desc(fraction)) %>%
  mutate(rank_single = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, spatial_cluster, rank_single)

#yo = results_combo %>%
 # filter(n_clusters == 72)
#print(unique(yo$combo))

# Step 3: Rao ranking per location using best combo pathogens
rao_ranking_pathogen = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  pathogens_in_combo <- str_split(best$combo[[1]], " \\+ ")[[1]]
  
  print(pathogens_in_combo)
  
  path_wide <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo) %>%
    dplyr::select(spatial_cluster, pathogen, fraction) %>%
    pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
  
  path_cols <- setdiff(names(path_wide), "spatial_cluster")
  
  path_wide %>%
    rowwise() %>%
    mutate(rao_val = rao_quadratic(c_across(all_of(path_cols)))) %>%
    ungroup() %>%
    arrange(desc(rao_val)) %>%
    mutate(rank_rao = row_number(), location = loc) %>%
    dplyr::select(location, spatial_cluster, rao_val, rank_rao)
})



# Step 4: cumulative curves
cumulative_pathogen_curves_v2 = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  combo_label        <- best$combo[[1]]
  pathogens_in_combo <- str_split(combo_label, " \\+ ")[[1]]
  
  print(unique( pathogens_in_combo))
  
  n_clusters <- n_distinct(cluster_by_pathogen %>% filter(location == loc) %>% pull(spatial_cluster))
  
  rao_order <- rao_ranking_pathogen %>%
    filter(location == loc) %>% arrange(rank_rao) %>% pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>% arrange(rank_single) %>% pull(spatial_cluster)
  
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated = rao_cum, cum_fraction = rao_cum / total_need,
             strategy = "Rao's quadratic index"),
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated = single_cum, cum_fraction = single_cum / total_need,
             strategy = "Vaccine only")
    )
  })
})

head(cumulative_pathogen_curves_v2)

# plot cumulative curves 
ggplot(data = cumulative_pathogen_curves_v2) + 
  geom_line(aes(x = frac_clusters,y = cum_fraction, col = pathogen, linetype = strategy), lwd = 1.5) + 
  facet_wrap(vars(location, pathogen), ncol = 3) +
  geom_hline(yintercept = .80, col = "gray", lty = "dashed") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 11), axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 13),
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        legend.position = "bottom") 


library(patchwork)

# Base plot function
plot_location <- function(loc, nc) {
  cumulative_pathogen_curves_v2 %>%
    filter(location == loc) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction,linetype = strategy), lwd = 1) +
    facet_wrap(vars(pathogen), ncol = nc) +
    geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    theme_bw(base_size = 11) +
    ylab("Cumulative disease targeted") + 
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 11),
       #   axis.text.x      = element_text(angle = 35, hjust = 1, size = 12),
          axis.text.y      = element_text(size = 12),
          axis.text.x      = element_blank(),
          axis.ticks.x = element_blank() , 
        #  axis.title       = element_text(size = 13),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          legend.text      = element_text(size = 12),
          legend.title     = element_text(size = 13),
          legend.position  = "none",   # suppress per-panel legends
          plot.title       = element_text(size = 14, face = "bold")) +
    theme(strip.text = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          plot.title = element_text(size = 14))
}

# Build each location panel
# Bangladesh has 5 pathogens — use ncol = 3 so row 1 has 3, row 2 has 2 + blank
p_bangladesh <- plot_location("Bangladesh", nc = 3)
p_kenya      <- plot_location("Kenya",      nc = 3)


plot_location <- function(loc, nc) {
  cumulative_pathogen_curves_v2 %>%
    filter(location == loc) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction, linetype = strategy), lwd = 1) +
    facet_wrap(vars(pathogen), ncol = nc) +
    geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    theme_bw(base_size = 11) +
    ylab("Cumulative disease targeted") + 
    xlab("Spatial clusters") +
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 11),
          axis.text.x      = element_text(angle = 35, hjust = 1, size = 10),
          axis.text.y      = element_text(size = 14),
       #   axis.text.x      = element_blank(),
           axis.title.x       = element_text(size = 14),
          axis.title.y       = element_blank(),
          legend.text      = element_text(size = 14),
          legend.title     = element_text(size = 16),
          legend.position  = "bottom",   # suppress per-panel legends
          plot.title       = element_text(size = 14, face = "bold")) +
    theme(strip.text = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          plot.title = element_text(size = 14))
}
p_cambodia   <- plot_location("Cambodia",   nc = 3)


# Stack with patchwork + legend
fig1b_build <- (p_bangladesh / p_kenya / p_cambodia) / 
 # wrap_elements(shared_legend) +
  plot_layout(heights = c(1, 1, 1, 0.3))

fig1b = fig1b_build + 
  labs(tag = "A", y = "Cumulative disease targeted") +
  theme(plot.tag = element_text(size = 20, face = "bold"),
        plot.tag.position = c(0, 2.8), 
        axis.title.y      = element_text(size = 13, vjust = 9, angle = 90, 
                                         hjust = -.1)) 
fig1b


# Build final patchwork without y label
fig1b_no_label = fig1b_build + 
  labs(tag = "A") +
  theme(plot.tag          = element_text(size = 20, face = "bold"),
        plot.tag.position = c(0, 2.8),
        axis.title.y      = element_blank(),
        plot.margin       = margin(t = 5, r = 5, b = 5, l = 15))  # increase l

# Add a global y axis label using cowplot
fig1b_final <- ggdraw(fig1b_no_label) +
  draw_label("Cumulative disease targeted", 
             x = 0.01,          # horizontal position (0 = far left)
             y = 0.5,           # vertical position (0.5 = middle)
             angle = 90,        # rotate to vertical
             size = 15,
             fontface = "plain")

fig1b_final

# sorrt rebuild 
# Build final patchwork without y label
fig1b_no_label = fig1b_build + 
  labs(tag = "A") +
  theme(plot.tag          = element_text(size = 20, face = "bold"),
        plot.tag.position = c(0, 2.8),
        axis.title.y      = element_blank(),
        plot.margin       = margin(t = 5, r = 5, b = 5, l = 15))  # increase l

fig1b_no_label

# Add a global y axis label using cowplot
fig1b_final <- ggdraw(fig1b_no_label) +
 # labs(tag = "A") +
  draw_label("Cumulative disease targeted", 
             x = 0.01,          # horizontal position (0 = far left)
             y = 0.5,           # vertical position (0.5 = middle)
             angle = 90,        # rotate to vertical
             size = 15,
             fontface = "plain")

fig1b_final


# Step 5: clusters_80
clusters_80 = cumulative_pathogen_curves_v2 %>%
  group_by(location, pathogen, strategy, combo) %>%
  summarise(prop_clusters_80 = frac_clusters[which(cum_fraction >= 0.80)[1]], .groups = "drop")

pathogen_order = clusters_80 %>%
  filter(strategy == "Rao") %>%
  arrange(location, prop_clusters_80) %>%
  group_by(location) %>%
  mutate(rao_rank = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, pathogen, rao_rank)

clusters_80 = left_join(clusters_80, pathogen_order, by = c("location", "pathogen"))

fig2c_build  = ggplot(clusters_80 %>%  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))), aes(x = reorder(pathogen, rao_rank), y = prop_clusters_80, fill = strategy)) +
  geom_point(aes(fill = strategy), cex = 4, shape = 24) +
  #facet_wrap(vars(location, combo), scales = "free_x", labeller = label_wrap_gen(width = 40)) +
  # facet_wrap(vars(location, combo), scales = "free_x", 
  #          labeller = labeller(location = label_value, combo = NULL)) +
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.8, begin = 0, name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
  labs(x = "Pathogen", y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 11), axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 10),
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        legend.position = "bottom")+
  # ylab("Proportion of clusters\nto reach 80% coverage") +
  ylab("Proportion of clusters to reach 80% coverage") +
  theme( #strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black"),
    plot.title = element_text(size = 14), 
    strip.text       = element_text(size = 15))
# strip.text = element_blank()) 
fig2c_build



###### try to order the pathogens 

# Step 1: get the vaccine-only value per pathogen per location
vaccine_order <- clusters_80 %>%
  filter(strategy == "Vaccine only") %>%  # replace with your exact strategy label
  group_by(location, pathogen) %>%
  summarise(vaccine_val = mean(prop_clusters_80), .groups = "drop")

# Step 2: create a per-location rank and a combined dummy x variable
clusters_80_ordered <- clusters_80 %>%
  left_join(vaccine_order, by = c("location", "pathogen")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
  group_by(location) %>%
  mutate(pathogen_ordered = reorder(pathogen, vaccine_val)) %>%
  ungroup()

# Step 3: plot using the new ordered factor
fig1c_build <- ggplot(clusters_80_ordered %>% 
                        filter(strategy != "Rao's quadratic index") %>%
                        mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))),
                      aes(x = pathogen_ordered, y = prop_clusters_80, fill = strategy)) +
  geom_point(aes(fill = strategy), cex = 7, shape = 24, alpha = .8) +
  facet_wrap(vars(location), scales = "free_x") +
 # scale_fill_viridis_d(option = "D", end = 0.8, begin = 0, name = "Strategy") +
 # scale_fill_viridis_d(option = "D", end = 0.8, begin = .4, name = "Strategy") +
  scale_fill_manual(values = c("darkslateblue", "skyblue"), name = "Strategy") +
  scale_fill_manual(values = c("skyblue"), name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray") +
  labs(x = "Pathogen", y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 15),
        legend.text = element_text(size = 16), legend.title = element_text(size = 18),
        legend.position = "bottom",
        panel.spacing.x = unit(1.15, "cm"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16))
fig1c_build




fig1c <- fig1c_build +
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1c

# sorry again 
fig1c <- fig1c_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 16, face = "bold"))
fig1c


plot_grid(fig1b_final, fig1c)


plot_grid(fig1b_final, fig1c, ncol = 2, rel_widths = c(.4, .3), labels = "A")


###################################### 
# add by wealth 

# Multidimensional Poverty Index 
######################################################################################


# Cambodia 
######################################################################################
cam_wealth_data <- read_dta(file = here("projects/6-multipathogen-burden/data/cambodia", "KHPR61FL.DTA"))
head(cam_wealth_data)
table(cam_wealth_data$hv213)
table(cam_wealth_data$hv214)
table(cam_wealth_data$hv215)

# make mpi 
mpi_cambodia = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2777, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > 0.2777], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , dhsclust) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity)

head(mpi_cambodia)

# correlation with wealth 
cam_wealth_dat = cam_wealth_data %>%
  dplyr::select(hv001, hv270, hv271) %>%
  mutate(dhsclust = hv001) %>%
  group_by(dhsclust) %>%
  summarize(across(hv270:hv271, ~ mean(.x))) %>%
  left_join(mpi_cambodia, by = c("dhsclust")) %>% drop_na()
head(cam_wealth_dat )
cor(cam_wealth_dat$hv271, cam_wealth_dat$mpi, method = "spearman")

ggplot(data = cam_wealth_dat) +
  geom_point(aes(x = hv271, y = mpi)) + ylab("MPI") +
  xlab("DHS Wealth index") + theme_minimal()

# add gps data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
head(gps_cambodia)

load(file = here("data/cambodia/cambodia_serology.Rdata"))
gps_dat = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat)

cam_gps_mpi = left_join(gps_cambodia, mpi_cambodia ,  by = "dhsclust") %>%
  left_join(gps_dat, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  dplyr::select(psuid, mpi) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) 
head(cam_gps_mpi)


# Bangladesh 
######################################################################################
# treatment clusters
treatment_bangl = read.csv(file = here("data/bangl/parasites/untouched", "bangl_analysis_parasite.csv")) %>%
  dplyr::select(dataid, clusterid, block, personid, tr) %>%
  distinct(clusterid, block, tr)
head(treatment_bangl)
print(treatment_bangl$tr)

bangl_metadata <- read.csv(file = here("data/bangl/enrollment_wealth/untouched", "washb-bangladesh-enrol-public.csv"))
head(bangl_metadata)

table(bangl_metadata$hfiacat)
table(bangl_metadata$momedu)
table(bangl_metadata$latfeces)
table(bangl_metadata$elec)
table(bangl_metadata$roof)
table(bangl_metadata$floor)
table(bangl_metadata$walls)

mpi_bangladesh = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = asset_radio + asset_tv + asset_phone + asset_bike + asset_refrig, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    #  mortality*(1/6),
    mat_edu_deprived*(1/6),
    # cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2037, 1, 0)) %>%
  group_by(block) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > 0.2037], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , block) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity) %>%
  mutate(block_r = 91 - block) %>%
  mutate(spatial_cluster = block_r) %>% mutate(Location = "Bangladesh") %>% dplyr::select(-block_r, -block) %>%
  ungroup() %>%
  dplyr::select(mpi, spatial_cluster, Location)
head(mpi_bangladesh)

#(1/6 + 1/6 + 5/18 )*.33333
#head(mpi_bangladesh)

#############################################################
# Kenya 
#washk_child_vars has whether they died 

treatment_assignment_k = read.csv(file = here("data/kenya/public_ids", 
                                              "cluster_tx_masked.csv")) 
head(treatment_assignment_k)

# household level 
kenya_meta = read_dta(file = here("data/kenya/master_files", "washk_household_vars_20190305.dta")) %>%
  left_join(treatment_assignment_k, by = "clusterid") 
head(kenya_meta)

table(kenya_meta$HHS_bi)
table(kenya_meta$mother_edu)
table(kenya_meta$tr_masked)

#child level 
child_death = read_dta(file = here("data/kenya/master_files", "washk_child_vars_20190305.dta")) %>%
  group_by(hhid) %>%
  mutate(mortality = ifelse(isdead_el %in% c(1, 3) | childdeath == 1, 1, 0)) %>%
  distinct(hhid, mortality)
head(child_death)

table(child_death$isdead_el)
table(child_death$childdeath)
table(child_death$mortality)

# blocks 
block_kenya = luminex_kenya = read.csv(file = here("data/kenya/luminex/final", 
                                                   "washb_kenya_luminex_igg_seropos_pathogen_2026-01-29.csv")) %>%
  distinct(clusterid,block, tr)
head(block_kenya)
table(block_kenya$tr)

head(luminex_kenya)

mpi_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(poverty = rowSums(cbind(
    nutrition_deprived*(1/6),
    mortality*(1/6),
    mat_edu_deprived*(1/6),
    cooking_deprived*(1/18),
    sanitation_deprived*(1/18),
    water_deprived*(1/18),
    electricity_deprived*(1/18),
    housing_deprived*(1/18),
    assets_deprived*(1/18) ), na.rm = TRUE)) %>%
  mutate(deprived_status = ifelse(poverty > .2778, 1, 0)) %>%
  group_by(block) %>%
  mutate(mean_dhs_deprived = mean(deprived_status, na.rm = TRUE)) %>%
  mutate(mean_dhs_intensity = mean(poverty[poverty > .2778], na.rm = TRUE)) %>% 
  distinct(mean_dhs_deprived, mean_dhs_intensity , block) %>%
  mutate(mpi = mean_dhs_deprived*mean_dhs_intensity)  %>%
  ungroup() %>%
  mutate(spatial_cluster = block) %>%
  mutate(Location = "Kenya") %>%
  dplyr::select(spatial_cluster, mpi, Location ) 


#### join mpa across locations 
mpi_all_locations = rbind(mpi_kenya, mpi_bangladesh, cam_gps_mpi) %>%
  mutate(location = Location) %>% dplyr::select(-Location)
head(mpi_all_locations)


#########################################################
####### Add prioritization by wealth as well 

# ── Step 1: Add MPI ranking per location ─────────────────────────────────────
mpi_ranking = mpi_all_locations %>%
  drop_na(spatial_cluster, mpi) %>%
  group_by(location) %>%
  arrange(desc(mpi)) %>%          # highest MPI (most deprived) targeted first
  mutate(rank_mpi = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, spatial_cluster, rank_mpi)

# ── Step 2: Rebuild cumulative curves with MPI strategy added ─────────────────
cumulative_pathogen_curves_v2 = map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  combo_label        <- best$combo[[1]]
  pathogens_in_combo <- str_split(combo_label, " \\+ ")[[1]]
  
  n_clusters <- n_distinct(cluster_by_pathogen %>% filter(location == loc) %>% pull(spatial_cluster))
  
  rao_order <- rao_ranking_pathogen %>%
    filter(location == loc) %>% arrange(rank_rao) %>% pull(spatial_cluster)
  
  single_order <- single_pathogen_ranking %>%
    filter(location == loc) %>% arrange(rank_single) %>% pull(spatial_cluster)
  
  mpi_order <- mpi_ranking %>%
    filter(location == loc) %>% arrange(rank_mpi) %>% pull(spatial_cluster)
  
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    single_cum <- map_dbl(seq_along(single_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% single_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    mpi_cum <- map_dbl(seq_along(mpi_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% mpi_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_treated = rao_cum, cum_fraction = rao_cum / total_need,
             strategy = "Rao's quadratic index"),
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(single_order) / n_clusters,
             cum_treated = single_cum, cum_fraction = single_cum / total_need,
             strategy = "Vaccine only"),
      tibble(location = loc, pathogen = path, combo = combo_label,
             frac_clusters = seq_along(mpi_order) / n_clusters,
             cum_treated = mpi_cum, cum_fraction = mpi_cum / total_need,
             strategy = "Wealth")
    )
  })
})

# ── Step 3: Rebuild clusters_80 with MPI strategy ────────────────────────────
clusters_80 = cumulative_pathogen_curves_v2 %>%
  group_by(location, pathogen, strategy, combo) %>%
  summarise(prop_clusters_80 = frac_clusters[which(cum_fraction >= 0.80)[1]], .groups = "drop")

pathogen_order = clusters_80 %>%
  filter(strategy == "Vaccine only") %>%
  arrange(location, prop_clusters_80) %>%
  group_by(location) %>%
  mutate(rao_rank = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, pathogen, rao_rank)

clusters_80 = left_join(clusters_80, pathogen_order, by = c("location", "pathogen")) 
  

# ── Step 4: Update fig1b plot_location to include MPI line ───────────────────
# Define color/linetype scales to add to both plots
strategy_colors    <- c("Rao's quadratic index" = "#440154FF", 
                        "Vaccine only"          = "cornflowerblue", 
                        "Wealth"          = "orangered")
strategy_linetypes <- c("Rao's quadratic index" = "solid", 
                        "Vaccine only"          = "solid", 
                        "Wealth"          = "dashed")

# Update plot_location for Bangladesh and Kenya (no x axis)
plot_location_nox <- function(loc, nc) {
  cumulative_pathogen_curves_v2 %>%
    filter(location == loc) %>%
    mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura"),
           pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum"),
           pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax"),
           pathogen = replace(pathogen, pathogen == "Ascaris", "A. lumbricoides"), 
           pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction, 
                  linetype = strategy, color = strategy), lwd = 1.15) +
    facet_wrap(vars(pathogen), ncol = nc) +
    geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    scale_color_manual(values = strategy_colors) +
    scale_linetype_manual(values = strategy_linetypes) +
    theme_bw(base_size = 11) +
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          axis.text.y      = element_text(size = 12),
          axis.text.x      = element_blank(),
          axis.ticks.x     = element_blank(),
          axis.title.x     = element_blank(),
          axis.title.y     = element_blank(),
          legend.position  = "none",
          plot.title       = element_text(size = 14, face = "bold"))
}

# Cambodia gets x axis + legend
plot_location_x <- function(loc, nc) {
  cumulative_pathogen_curves_v2 %>%
    filter(location == loc) %>%
    mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura"),
           pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum"),
           pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax"),
           pathogen = replace(pathogen, pathogen == "Ascaris", "A. lumbricoides"), 
           pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction, 
                  linetype = strategy, color = strategy), lwd = 1.15) +
    facet_wrap(vars(pathogen), ncol = nc) +
    geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    scale_color_manual(values = strategy_colors, name = "Strategy") +
    scale_linetype_manual(values = strategy_linetypes, name = "Strategy") +
    theme_bw(base_size = 11) +
    xlab("Spatial clusters") +
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          axis.text.x      = element_text(angle = 35, hjust = 1, size = 10),
          axis.text.y      = element_text(size = 12),
          axis.title.x     = element_text(size = 14),
          axis.title.y     = element_blank(),
          legend.text      = element_text(size = 13),
          legend.title     = element_text(size = 14),
          legend.position  = "bottom",
          plot.title       = element_text(size = 14, face = "bold"))
}

p_bangladesh <- plot_location_nox("Bangladesh", nc = 5)
p_kenya      <- plot_location_nox("Kenya",      nc = 5)
p_cambodia   <- plot_location_x("Cambodia",     nc = 5)

fig1b_build <- (p_bangladesh / p_kenya / p_cambodia) +
  plot_layout(heights = c(1.5, 1.5, 1.5))

fig1b_no_label <- fig1b_build +
  labs(tag = "B") +
  theme(plot.tag          = element_text(size = 20, face = "bold"),
        plot.tag.position = c(0, 2.3),
        axis.title.y      = element_blank(),
        plot.margin       = margin(t = 5, r = 5, b = 5, l = 40))

fig1b_final <- ggdraw(fig1b_no_label) +
  draw_label("Cumulative disease targeted",
             x = 0.05, y = 0.5, angle = 90, size = 15, fontface = "plain")
fig1b_final


# ── Step 5: Update fig1c (clusters_80) with MPI as red triangle ──────────────

clusters_80_ordered <- clusters_80 %>%
  left_join(vaccine_order, by = c("location", "pathogen")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura"),
         pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum"),
         pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax"),
         pathogen = replace(pathogen, pathogen == "Ascaris", "A. lumbricoides"), 
         pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
  group_by(location) %>%
  mutate(pathogen_ordered = reorder(pathogen, vaccine_val)) %>%
  ungroup() %>%
  mutate(shape_type = case_when(
    strategy == "Wealth"          ~ "triangle",
    strategy == "Rao's quadratic index" ~ "triangle",
    strategy == "Vaccine only"          ~ "triangle" ))


# bangl 
(78-74)/78 # vaccine
(82-74)/82 # wealth
# kenya 
(76-65)/76 # vaccine
(71-65)/71 # wealth
# cam 
(81-72)/81 # vaccine 
(81-72)/81 # wealth

fig1c_build <- ggplot(clusters_80_ordered %>%
                        mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))),
                      aes(x = pathogen_ordered, y = prop_clusters_80, fill = strategy)) +
  geom_point(cex = 7, shape = 24, alpha = 0.8) +
  facet_wrap(vars(location), scales = "free_x") +
  scale_fill_manual(
    values = c("Rao's quadratic index" = "#440154FF",
               "Vaccine only"          = "cornflowerblue",
               "Wealth"          = "orangered"),
    name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray") +
  labs(x = "Pathogen", y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(axis.text.x      = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y      = element_text(size = 12),
        axis.title       = element_text(size = 15),
        legend.text      = element_text(size = 13),
        legend.title     = element_text(size = 14),
        legend.position  = "bottom",
        panel.spacing.x  = unit(1.15, "cm"),
        strip.background = element_rect(fill = "white", colour = "black"),
        strip.text       = element_text(size = 16))

fig1c <- fig1c_build +
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig1c

bottom1 = plot_grid(fig1b_final, fig1c, rel_widths = c(2.3, 1.5))
bottom1 
plot_grid(fig1a, bottom1, ncol = 1, rel_heights = c(.2, .3))

# 1500 x 1300


##############################################
##############################################
##############################################
# Rao at coarser spatial units 

dat_locations = rbind(bangl_rao_clusters, cam_rao_clusters, kenya_rao_clusters) %>%
  group_by(intervention, location) %>%
  mutate(pop_assessed_int = sum(non_na_denom)) %>%
  mutate(fraction_rao = num/pop_assessed_int) %>% ungroup() %>% 
  drop_na()
head(dat_locations)


#############################################
##################################### plot study sites 

# Read admin 2 in 
admin_k_study <- st_read(here("data/kenya/gps/ken_admin_boundaries.shp", "ken_admin.shp"))
# Load treatment assignment and GPS data
treatment_assignment <- read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)

gps_dat_kenya <- readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat)) %>%
  distinct(block, long, lat)

luminex_bound <- read.csv(file = here("data/kenya/luminex/final",
                                      "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  left_join(gps_dat_kenya, by = "block") %>%
  distinct(long, lat, block, eed) %>%
  group_by(long, lat) %>%
  arrange(desc(eed == "EED substudy")) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Study = eed)

# Bounding box from data
xmin_k <- min(luminex_bound$long, na.rm = TRUE) - 0.1
xmax_k <- max(luminex_bound$long, na.rm = TRUE) + 0.1
ymin_k <- min(luminex_bound$lat,  na.rm = TRUE) - 0.1
ymax_k <- max(luminex_bound$lat,  na.rm = TRUE) + 0.1

head(rao_by_cluster)
# make real maps 
kenya_sites <- ggplot(
  data = rao_by_cluster  %>% filter(location == "Kenya")) +
  geom_sf(data = admin_k_study,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.30),
          lwd   = 0.50) +
  geom_point(aes(x = long, y = lat, col = rao/max_rao), cex = 4, alpha = .80, shape = 24 ) + #, shape = 24) +  #shape = 24) +
  theme_minimal() +
  xlim(c(xmin_k, xmax_k)) +
  ylim(c(ymin_k, ymax_k)) +
  theme(legend.position = "bottom") +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20) ) #,
#  axis.title    = element_blank(),
# axis.text     = element_blank(),
#  axis.ticks    = element_blank() )
kenya_sites

bangl_sites = ggplot(rao_by_cluster  %>% filter(location == "Bangladesh")) +
  geom_sf(data = admin_b_study, fill = alpha("seagreen", .06),  color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat), fill = "black", cex = 4, alpha = .86, shape = 24) +theme_minimal() +
  ylab("Latitude") + xlab("Longitude") +
  xlim(c(89.9, 90.8)) +
  ylim(c(23.9, 25.0))  +
  theme(legend.position = "bottom") +
  ggtitle("Bangladesh") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20)) + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
bangl_sites

plot_grid(bangl_sites, kenya_sites, nrow = 1)


# Load country-level boundaries for the world
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Filter for Cambodia
cambodia_map <- countries[countries$name == "Cambodia", ]

# First administrative boundaries (provinces)
cambodia_admin1 <- ne_states(
  country = "Cambodia",
  returnclass = "sf")

# second admin 
library(geodata)
library(sf)

# Download Cambodia admin2 boundaries
khm_admin2 <- geodata::gadm(
  country = "KHM",
  level = 2,
  path = tempdir()
)

# Convert to sf
khm_admin2_sf <- st_as_sf(khm_admin2)

cam_sites <- ggplot(data = rao_by_cluster  %>% filter(location == "Cambodia") ) + 
  # geom_sf(data = cambodia_map, fill =  alpha("seagreen", .06), color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat), fill = "black", cex = 4, alpha = .86, shape = 24) +theme_minimal() +
  # geom_sf(data = cambodia_admin1,
  #  fill =  alpha("seagreen", .06),
  # color = alpha("black", 0.35),
  #  linewidth = 0.35) +
  geom_sf(
    data = khm_admin2_sf,
    fill =  alpha("seagreen", .06),
    color = alpha("black", 0.25),
    linewidth = 0.2) +
  ylab("Latitude") + xlab("Longitude") +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 23), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20), 
    legend.position = "none") + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
cam_sites

plot_grid(kenya_sites, bangl_sites, cam_sites, nrow = 1)



########################################################
#######################################################
#######################################################

library(sf)
library(geodata)
library(tidyverse)
library(patchwork)

# ── Step 1: Get admin boundaries and assign spatial clusters to broader geography ──

bgd_admin3 = st_read(here("data/bangl/gps/untouched/bangl_admin3", "bgd_admbnda_adm3_bbs_20201113.shp")) # from ben pairmatching
st_crs(bgd_admin3) <- 4326  # WGS84
ken_admin3 = st_read(here("data/kenya/gps/ken_adm3", "ken_adm3.shp")) 
st_crs(ken_admin3) <- 4326  
cam_admin1 = st_read(here("data/cambodia/khm_admin_boundaries.shp", "khm_admin1.shp")) 
st_crs(cam_admin1) <- 4326  
# Fix invalid geometries
cam_admin1 <- st_make_valid(cam_admin1)
all(st_is_valid(cam_admin1))


# quick plot
ggplot(data =bgd_admin3 ) +
  geom_sf()
ggplot(data =ken_admin3  ) +
  geom_sf()


# ── Step 2: Assign each spatial cluster to its broader admin unit ─────────────

assign_admin <- function(loc_data, admin_sf, admin_name_col) {
  # Convert cluster points to sf
  pts <- loc_data %>%
    distinct(spatial_cluster, lat, long) %>%
    drop_na(lat, long) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # Spatial join to admin boundary
  joined <- st_join(pts, admin_sf %>% dplyr::select(all_of(admin_name_col)),
                    join = st_within) %>%
    st_drop_geometry() %>%
    rename(admin_unit = all_of(admin_name_col))
  
  loc_data %>% left_join(joined, by = "spatial_cluster")
}

# Apply to each location
dat_kenya <- dat_locations %>% filter(location == "Kenya")
dat_bangl <- dat_locations %>% filter(location == "Bangladesh")
dat_cambo <- dat_locations %>% filter(location == "Cambodia")

dat_kenya_admin <- assign_admin(dat_kenya, ken_admin3, "NAME_3")
head(ken_admin3)
dat_bangl_admin <- assign_admin(dat_bangl, bgd_admin3, "ADM3_EN")
head(bgd_admin3)
dat_cambo_admin <- assign_admin(dat_cambo, cam_admin1, "adm1_name")
head(cam_admin1)

# Combine
dat_admin <- bind_rows(dat_kenya_admin, dat_bangl_admin, dat_cambo_admin)

# Check assignment
dat_admin %>% count(location, admin_unit) %>% print(n = 30)

# ── Step 3: Calculate Rao at admin level ──────────────────────────────────────

rao_by_admin <- dat_admin %>%
  drop_na(admin_unit) %>%
  group_by(location, admin_unit, pathogen) %>%
  summarise(
    num      = sum(num, na.rm = TRUE),
    denom    = sum(non_na_denom, na.rm = TRUE),
    fraction = num / denom,
    lat      = mean(lat, na.rm = TRUE),
    long     = mean(long, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(location, admin_unit) %>%
  summarise(
    rao  = rao_quadratic(fraction),
    lat  = mean(lat),
    long = mean(long),
    .groups = "drop"
  ) %>%
  group_by(location) %>%
  mutate(max_rao = max(rao),
         rao_norm = rao / max_rao) %>%
  ungroup()

head(rao_by_admin)

# ── Step 4: Build admin-level Rao ranking and cumulative curves ───────────────

# Rao ranking at admin level
rao_admin_ranking <- rao_by_admin %>%
  group_by(location) %>%
  arrange(desc(rao)) %>%
  mutate(rank_admin = row_number()) %>%
  ungroup() %>%
  dplyr::select(location, admin_unit, rank_admin)

# Map spatial clusters to their admin unit
cluster_to_admin <- dat_admin %>%
  distinct(location, spatial_cluster, admin_unit) %>%
  drop_na(admin_unit)

# Admin-ordered cluster sequence: all clusters within each admin unit,
# ordered by admin Rao rank
admin_order_by_loc <- cluster_to_admin %>%
  left_join(rao_admin_ranking, by = c("location", "admin_unit")) %>%
  group_by(location) %>%
  arrange(rank_admin, spatial_cluster) %>%
  ungroup()

# ── Step 5: Cumulative curves — cluster-level Rao vs admin-level Rao ──────────

cumulative_admin_curves <- map_dfr(unique(best_combos$location), function(loc) {
  best               <- best_combos %>% filter(location == loc)
  combo_label        <- best$combo[[1]]
  pathogens_in_combo <- str_split(combo_label, " \\+ ")[[1]]
  
  n_clusters <- n_distinct(cluster_by_pathogen %>%
                             filter(location == loc) %>% pull(spatial_cluster))
  
  # Cluster-level Rao order (existing)
  rao_order <- rao_ranking_pathogen %>%
    filter(location == loc) %>% arrange(rank_rao) %>% pull(spatial_cluster)
  
  # Admin-level Rao order (new)
  admin_order <- admin_order_by_loc %>%
    filter(location == loc) %>% pull(spatial_cluster)
  # Remove clusters not in path data
  admin_order <- admin_order[admin_order %in% 
                               (cluster_by_pathogen %>% 
                                  filter(location == loc) %>% 
                                  pull(spatial_cluster) %>% unique())]
  
  path_data <- cluster_by_pathogen %>%
    filter(location == loc, pathogen %in% pathogens_in_combo)
  
  map_dfr(unique(path_data$pathogen), function(path) {
    p_data     <- path_data %>% filter(pathogen == path)
    total_need <- sum(p_data$num, na.rm = TRUE)
    
    rao_cum <- map_dbl(seq_along(rao_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% rao_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    admin_cum <- map_dbl(seq_along(admin_order), function(i) {
      sum(p_data %>% filter(spatial_cluster %in% admin_order[1:i]) %>% pull(num), na.rm = TRUE)
    })
    
    bind_rows(
      tibble(location     = loc,
             pathogen     = path,
             combo        = combo_label,
             frac_clusters = seq_along(rao_order) / n_clusters,
             cum_fraction = rao_cum / total_need,
             strategy     = "Rao (cluster-level)"),
      tibble(location     = loc,
             pathogen     = path,
             combo        = combo_label,
             frac_clusters = seq_along(admin_order) / n_clusters,
             cum_fraction = admin_cum / total_need,
             strategy     = "Rao (admin-level)")
    )
  })
})

head(cumulative_admin_curves)

# ── Step 6: Efficiency curves plot ────────────────────────────────────────────

strategy_colors_admin    <- c("Rao (cluster-level)" = "black",
                              "Rao (admin-level)"   = "seagreen")
strategy_linetypes_admin <- c("Rao (cluster-level)" = "solid",
                              "Rao (admin-level)"   = "dashed")

plot_admin_location_nox <- function(loc, nc) {
  cumulative_admin_curves %>%
    filter(location == loc) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction,
                  color = strategy, linetype = strategy), lwd = 1.25) +
    facet_wrap(vars(pathogen), ncol = nc) +
    geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    scale_color_manual(values = strategy_colors_admin, name = "Strategy") +
    scale_linetype_manual(values = strategy_linetypes_admin, name = "Strategy") +
    theme_bw(base_size = 11) +
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          axis.text.y      = element_text(size = 12),
          axis.text.x      = element_blank(),
          axis.ticks.x     = element_blank(),
          axis.title       = element_blank(),
          legend.position  = "none",
          plot.title       = element_text(size = 14, face = "bold"))
}

plot_admin_location_x <- function(loc, nc) {
  cumulative_admin_curves %>%
    filter(location == loc) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction,
                  color = strategy, linetype = strategy), lwd = 1.25) +
    facet_wrap(vars(pathogen), ncol = nc) +
    geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    scale_color_manual(values = strategy_colors_admin, name = "Strategy") +
    scale_linetype_manual(values = strategy_linetypes_admin, name = "Strategy") +
    theme_bw(base_size = 11) +
    xlab("Spatial clusters") +
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          axis.text.x      = element_text(angle = 35, hjust = 1, size = 10),
          axis.text.y      = element_text(size = 12),
          axis.title.x     = element_text(size = 14),
          axis.title.y     = element_blank(),
          legend.text      = element_text(size = 14),
          legend.title     = element_text(size = 16),
          legend.position  = "bottom",
          plot.title       = element_text(size = 14, face = "bold"))
}

p_bangl_admin <- plot_admin_location_nox("Bangladesh", nc = 3)
p_kenya_admin <- plot_admin_location_nox("Kenya",      nc = 3)
p_cambo_admin <- plot_admin_location_x("Cambodia",    nc = 3)


###################### new plot 


library(patchwork)

# Build all three WITH x-axis text/ticks (not suppressed)
plot_admin_location_full <- function(loc, nc) {
  cumulative_admin_curves %>%
    mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura"),
           pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum"),
           pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax"),
           pathogen = replace(pathogen, pathogen == "Ascaris", "A. lumbricoides"), 
           pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
    filter(location == loc) %>%
    ggplot() +
    geom_line(aes(x = frac_clusters, y = cum_fraction,
                  color = strategy, linetype = strategy), lwd = 1.25, alpha = .85) +
    facet_wrap(vars(pathogen), ncol = nc) +
   # geom_hline(yintercept = .80, col = "gray70", lty = "dashed") +
    scale_color_manual(values = strategy_colors_admin, name = "Strategy") +
    scale_linetype_manual(values = strategy_linetypes_admin, name = "Strategy") +
    theme_bw(base_size = 11) +
    xlab("Spatial clusters") +
    ggtitle(loc) +
    theme(strip.text       = element_text(size = 13, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          axis.text.x      = element_text(angle = 35, hjust = 1, size = 10),
          axis.text.y      = element_text(size = 12),
          axis.title.x     = element_text(size = 14),
          axis.title.y     = element_blank(),
          legend.position  = "none",
          plot.title       = element_text(size = 14))
}

p_bangl_admin <- plot_admin_location_full("Bangladesh", nc = 3)

p_kenya_admin <- plot_admin_location_full("Kenya",      nc = 3)
p_cambo_admin <- plot_admin_location_full("Cambodia",   nc = 3)

p_bangl_admin <- p_bangl_admin + theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 15))
p_kenya_admin <- p_kenya_admin + theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 15))
p_cambo_admin <- p_cambo_admin + theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5))



#######################
library(ggpubr)

legend_source <- cumulative_admin_curves %>%
  filter(location == "Cambodia") %>%
  ggplot() +
  geom_line(aes(x = frac_clusters, y = cum_fraction,
                color = strategy, linetype = strategy), lwd = 1.25) +
  scale_color_manual(values = strategy_colors_admin, name = "Strategy") +
  scale_linetype_manual(values = strategy_linetypes_admin, name = "Strategy") +
  theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        legend.text      = element_text(size = 14),
        legend.title     = element_text(size = 16))

shared_legend <- get_legend(legend_source)
legend_plot <- as_ggplot(shared_legend)

layout <- "
AC
AC
BD
"

fig_admin_build <- p_bangl_admin + p_kenya_admin + p_cambo_admin + legend_plot +
  plot_layout(design = layout, heights = c(2, 2, 2))

fig_admin_final <- ggdraw(fig_admin_build) +
  draw_label("Cumulative disease targeted",
             x = 0.01, y = 0.5, angle = 90, size = 15, fontface = "plain")

fig_admin_final



fig2b <- fig_admin_final+
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2b







# ── Step 7: Updated maps at coarser admin level ───────────────────────────────

# Kenya map — admin3
kenya_map_admin <- ggplot(data = rao_by_admin %>% filter(location == "Kenya")) +
  geom_sf(data = ken_admin3,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.25),
          lwd   = 0.35) +
  geom_point(aes(x = long, y = lat, fill = rao_norm), cex = 4, alpha = 0.85, shape = 24) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex (admin)") +
  xlim(c(xmin_k, xmax_k)) + ylim(c(ymin_k, ymax_k)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  ggtitle("Kenya (admin 3)") +
  theme_minimal() +
  theme(plot.title    = element_text(size = 20),
        legend.text   = element_text(size = 12),
        legend.title  = element_text(size = 14),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())

# Bangladesh map — admin3
xmin_b <- min(dat_bangl$long, na.rm = TRUE) - 0.1
xmax_b <- max(dat_bangl$long, na.rm = TRUE) + 0.1
ymin_b <- min(dat_bangl$lat,  na.rm = TRUE) - 0.1
ymax_b <- max(dat_bangl$lat,  na.rm = TRUE) + 0.1

bangl_map_admin <- ggplot(data = rao_by_admin %>% filter(location == "Bangladesh")) +
  geom_sf(data = bgd_admin3,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.25),
          lwd   = 0.35) +
  geom_point(aes(x = long, y = lat, fill = rao_norm), cex = 4, alpha = 0.85, shape = 24) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex (admin)") +
  xlim(c(xmin_b, xmax_b)) + ylim(c(ymin_b, ymax_b)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  ggtitle("Bangladesh (admin 3)") +
  theme_minimal() +
  theme(plot.title    = element_text(size = 20),
        legend.text   = element_text(size = 12),
        legend.title  = element_text(size = 14),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())

# Cambodia map — admin1
xmin_c <- min(dat_cambo$long, na.rm = TRUE) - 0.5
xmax_c <- max(dat_cambo$long, na.rm = TRUE) + 0.5
ymin_c <- min(dat_cambo$lat,  na.rm = TRUE) - 0.5
ymax_c <- max(dat_cambo$lat,  na.rm = TRUE) + 0.5

cambo_map_admin <- ggplot(data = rao_by_admin %>% filter(location == "Cambodia")) +
  geom_sf(data = cam_admin1,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.25),
          lwd   = 0.35) +
  geom_point(aes(x = long, y = lat, fill = rao_norm), cex = 4, alpha = 0.85, shape = 24) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex (admin)") +
  xlim(c(xmin_c, xmax_c)) + ylim(c(ymin_c, ymax_c)) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  ggtitle("Cambodia (province)") +
  theme_minimal() +
  theme(plot.title    = element_text(size = 20),
        legend.text   = element_text(size = 12),
        legend.title  = element_text(size = 14),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank())

plot_grid(bangl_map_admin, kenya_map_admin, cambo_map_admin, nrow = 1)

# check numbers by location 
dat_admin %>%
  group_by(location) %>%
  summarise(n_admin_units = n_distinct(admin_unit), .groups = "drop")


########## make figure 2 

####################### rao at spatial cluster level 


# Read admin 2 in 
admin_k_study <- st_read(here("data/kenya/gps/ken_admin_boundaries.shp", "ken_admin2.shp"))
# Load treatment assignment and GPS data
treatment_assignment <- read.csv(file = here("data/kenya/primary_outcomes", "endline-anthro.csv")) %>%
  distinct(block, clusterid, tr)

gps_dat_kenya <- readRDS(file = here("data/kenya/gps", "kenya_analysis_gps.rds")) %>%
  group_by(block) %>%
  mutate(long = median(lon), lat = median(lat)) %>%
  distinct(block, long, lat)

luminex_bound <- read.csv(file = here("data/kenya/luminex/final",
                                      "washb_kenya_luminex_igg_seropos_2025-09-21.csv")) %>%
  mutate(dataid = str_extract(childid, "(?<=-)\\d{5}(?=-)")) %>%
  left_join(treatment_assignment, by = "clusterid") %>%
  left_join(gps_dat_kenya, by = "block") %>%
  distinct(long, lat, block, eed) %>%
  group_by(long, lat) %>%
  arrange(desc(eed == "EED substudy")) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Study = eed)

# Bounding box from data
xmin_k <- min(luminex_bound$long, na.rm = TRUE) - 0.1
xmax_k <- max(luminex_bound$long, na.rm = TRUE) + 0.1
ymin_k <- min(luminex_bound$lat,  na.rm = TRUE) - 0.1
ymax_k <- max(luminex_bound$lat,  na.rm = TRUE) + 0.1


head(rao_by_cluster)
# make real maps 
kenya_map_rao <- ggplot(
  data = rao_by_cluster  %>% filter(location == "Kenya")) +
  geom_sf(data = ken_admin3,
    #data = admin_k_study,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.15),
          lwd   = 0.50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .93 ) + #, shape = 24) +  #shape = 24) +
  theme_minimal() +
  xlim(c(xmin_k, xmax_k)) +
  ylim(c(ymin_k, ymax_k)) +
  theme(legend.position = "none") +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20),
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank() )
kenya_map_rao


admin_b_study <- get_map("district")

bangl_map_rao = ggplot(rao_by_cluster  %>% filter(location == "Bangladesh")) +
#  geom_sf(data = admin_b_study, fill = alpha("seagreen", .06),  color = alpha("black", 0.30), lwd = .50) +
  geom_sf(data = bgd_admin3, fill = alpha("seagreen", .06),  color = alpha("black", 0.15), lwd = .50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .93) +theme_minimal() +
  ylab("Latitude") + xlab("Longitude") +
  xlim(c(89.9, 90.8)) +
  ylim(c(23.9, 25.0))  +
  theme(legend.position = "none") +
  ggtitle("Bangladesh") +
  ylab("") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18), legend.title = element_text(size = 20)) + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
bangl_map_rao

plot_grid(bangl_map_rao, kenya_map_rao, nrow = 1)


# Load country-level boundaries for the world
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Filter for Cambodia
cambodia_map <- countries[countries$name == "Cambodia", ]

# First administrative boundaries (provinces)
cambodia_admin1 <- ne_states(
  country = "Cambodia",
  returnclass = "sf")

# second admin 
library(geodata)
library(sf)

# Download Cambodia admin2 boundaries
khm_admin2 <- geodata::gadm(
  country = "KHM",
  level = 2,
  path = tempdir()
)

# Convert to sf
khm_admin2_sf <- st_as_sf(khm_admin2)

cam_map_rao <- ggplot(data = rao_by_cluster  %>% filter(location == "Cambodia") ) + 
  # geom_sf(data = cambodia_map, fill =  alpha("seagreen", .06), color = alpha("black", 0.30), lwd = .50) +
  geom_point(aes(x = long, y = lat, color = rao), cex = 5, alpha = .93) +theme_minimal() +
  # geom_sf(data = cambodia_admin1,
  #  fill =  alpha("seagreen", .06),
  # color = alpha("black", 0.35),
  #  linewidth = 0.35) +
  geom_sf( 
    #data = cam_admin1,
    data = khm_admin2_sf,
    fill =  alpha("seagreen", .06),
    color = alpha("black", 0.15),
    linewidth = 0.2) +
  ylab("Latitude") + xlab("Longitude") +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_color_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  theme(
    plot.title      = element_text(size = 20), 
    plot.subtitle =   element_text(size = 20), 
    plot.tag         = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 14), legend.title = element_text(size = 14), 
    legend.position = "right") + 
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank())
cam_map_rao 

plot_grid(bangl_map_rao, kenya_map_rao, cam_map_rao, nrow = 1)




two = plot_grid(bangl_map_rao, kenya_map_rao, nrow = 1)
two

maps_rao = plot_grid(two , cam_map_rao )

fig2a_shifted <- ggdraw() +
  draw_plot(maps_rao, x = 0.05, y = 0, width = 0.95, height = 1)


fig2a<- fig2a_shifted +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig2a

fig2a <- fig2a_shifted +
  labs(tag = "A") +
  theme(plot.tag          = element_text(size = 20, face = "bold"),
        plot.tag.position = c(0, 0.95))  # default top is (0, 1); lower y shifts down
fig2a

#### combine for fig 2 

plot_grid(fig2a, fig2b, ncol =1 , rel_heights = c(1, 1.25))
# 1300 x 1200



####################################################################
# OKAY BART models 
place_clim =   readr::read_csv(file = here("data", "place_loc_climate.csv")) 
head(place_clim)

pred_rao_kenya = kenya_meta %>% 
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>% # moderate to severe hunger 
  mutate(hygiene_deprived = ifelse(hws_bl == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>% # did not complete primary school
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>% # at endline improved latrine, maybe want to include treatment  
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH" , 0, 1 )) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0 ,1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(floor == 0, 1, 0)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(spatial_cluster = block, location = "Kenya") %>%
  dplyr::select(-block)  %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  left_join(place_clim, by = c("location", "lat", "long")) %>%
  drop_na() 
head(pred_rao_kenya)

# ── 1. Prep predictor matrix and outcome ──────────────────────────────────────
predictors <- pred_rao_kenya %>%
  dplyr::select(nutrition_deprived:assets_deprived, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()

outcome <- log(pred_rao_kenya$rao + .001)

# ── 2. K-fold cross validation (k = 5) ───────────────────────────────────────
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))

cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  #train_y <- outcome_logit[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 1000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}


# ── 3. Error metrics ──────────────────────────────────────────────────────────
mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- 1 - (ss_res / ss_tot)
r2 = round(r2, digits = 3)
r2


rank_cor <- cor(outcome, cv_preds, method = "spearman")
rank_cor

top_k <- round(nrow(predictors) * 0.30)  # top 20% of clusters
true_top      <- order(outcome, decreasing = TRUE)[1:top_k]
predicted_top <- order(cv_preds,    decreasing = TRUE)[1:top_k]
overlap <- length(intersect(true_top, predicted_top))
overlap / top_k

top_k/89

# ── 4. Figure 1: Observed vs Predicted ───────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  annotate("text", x = min(outcome, na.rm = TRUE), 
           y = max(cv_preds, na.rm = TRUE),
           label =  " ", 
           hjust = 0, vjust = 1, size = 5) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Kenya", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1 )) + 
  xlim(c(0,1))

fig_fit

# ── 5. Full model for variable importance ─────────────────────────────────────
fit_full <- wbart(x.train = predictors,
                  y.train = outcome,
                  ndpost  = 1000,
                  nskip   = 250,
                  printevery = 0)

# Variable inclusion proportions (times each var used in tree splits)
var_importance <- tibble(
  variable   = colnames(predictors),
  importance = colMeans(fit_full$varcount / rowSums(fit_full$varcount))
) %>%
  arrange(importance) %>%
  mutate(variable = factor(variable, levels = variable))

# ── 6. Figure 2: Variable Importance ─────────────────────────────────────────
fig_varimp <- ggplot(var_importance, aes(x = importance/((100/14)/100), y = variable)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  theme_bw(base_size = 13) +
  labs(x = "Relative variable inclusion proportion",
       y = NULL,
       title = "Variable importance- Kenya") +
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 12)) +
  geom_vline(xintercept = 1) 
  geom_vline(xintercept = (100/14)/100) 

fig_varimp

plot_grid(fig_fit, fig_varimp, nrow = 1, rel_widths = c(.4, .7))




###########################################
# bangl 
# Bangladesh 
gps_dat = read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>% # had to add later? 
  left_join(public_ids, by = "dataid") %>% 
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat) 


pred_rao_bangl = bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  #  filter(tr %in% c("Control", "Nutrition")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  # mortality not available 
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  # cooking not avaiable 
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation", "WSH") , 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH") , 0, 1 )) %>%
  mutate(hygiene_deprived = ifelse(hwsws == 1 |  tr %in% c("Handwashing", "WSH") , 0, 1)) %>% # added
  mutate(floor_deprived = ifelse(cement == 0, 1, 0)) %>% # added
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = rowSums(across(asset_radio:asset_refrig), na.rm = TRUE)) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(block_r = block) %>% dplyr::select(-block) %>%
  left_join(gps_dat, by = "block_r") %>%
  mutate(spatial_cluster = block) %>%
  mutate(location = "Bangladesh") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster"))  %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>%  mutate(lat = round(lat, 5), long = round(long, 5)), by = c("location", "lat", "long")) %>%
  drop_na() 
head(pred_rao_bangl)


# ── 1. Prep predictor matrix and outcome ──────────────────────────────────────
predictors <- pred_rao_bangl %>%
  dplyr::select(nutrition_deprived:assets_deprived, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()

outcome <- log(pred_rao_bangl$rao)

# ── 2. K-fold cross validation (k = 5) ───────────────────────────────────────
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))

cv_preds <- numeric(nrow(predictors))
#cv_preds_logit <- numeric(nrow(predictors)) 

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  #train_y <- outcome_logit[folds != i]
  test_x  <- predictors[folds == i, ]
  
  # ── Oversample high-Rao observations in the TRAINING set only ──────────────
  # Use outcome_raw (not logit) to define "high" in the original 0-1 scale
  n_repeats <- 4   # increase this to boost weight further (try 3, 5, 8, etc.)
  
 # train_x_boosted <- rbind(train_x, 
  #                         do.call(rbind, replicate(n_repeats, train_x[high_rao_idx, , drop = FALSE], simplify = FALSE)))
 # train_y_boosted <- c(train_y, 
       #                rep(train_y[high_rao_idx], n_repeats))
  
  
  fit <- wbart(
              #x.train = train_x_boosted,
              # y.train = train_y_boosted,
               x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 2000,
               nskip   = 250,
               printevery = 0)
  #cv_preds_logit[folds == i] <- fit$yhat.test.mean
 cv_preds[folds == i] <- fit$yhat.test.mean
}



# ── 3. Error metrics ──────────────────────────────────────────────────────────
mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- 1 - (ss_res / ss_tot)
r2
r2 = round(r2, digits = 3)

rank_cor <- cor(outcome, cv_preds, method = "spearman")
rank_cor

top_k <- round(nrow(predictors) * 0.40)  # top 20% of clusters
true_top      <- order(outcome, decreasing = TRUE)[1:top_k]
predicted_top <- order(cv_preds,    decreasing = TRUE)[1:top_k]
overlap <- length(intersect(true_top, predicted_top))
overlap / top_k

top_k/90


# ── 4. Figure 1: Observed vs Predicted ───────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)


fig_fit <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  annotate("text", x = min(outcome, na.rm = TRUE), 
           y = max(cv_preds, na.rm = TRUE),
           label = "",
           hjust = 0, vjust = 1, size = 5) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Bangladesh", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1 )) + 
  xlim(c(0,1))
fig_fit

# Plot residuals vs observed - will show the pattern clearly
cv_df %>%
  mutate(residual = predicted - observed) %>%
  ggplot(aes(x = observed, y = residual)) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkmagenta") +
  theme_bw() +
  labs(title = "Bangladesh", x = "Observed Rao's index", y = "Residual (predicted - observed)")

# ── 5. Full model for variable importance ─────────────────────────────────────
fit_full <- wbart(x.train = predictors,
                  y.train = outcome,
                  ndpost  = 1000,
                  nskip   = 250,
                  printevery = 0)

# Variable inclusion proportions (times each var used in tree splits)
var_importance <- tibble(
  variable   = colnames(predictors),
  importance = colMeans(fit_full$varcount / rowSums(fit_full$varcount))) %>%
  arrange(importance) %>%
  mutate(variable = factor(variable, levels = variable))

100/13
# ── 6. Figure 2: Variable Importance ─────────────────────────────────────────
fig_varimp <- ggplot(var_importance[1:6,], aes(x = importance/.0769, y = variable)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  theme_bw(base_size = 13) +
  labs(x = "Relative variable inclusion proportion",
       y = NULL,
       title = "Variable Importance- Bangladesh") +
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 12))

fig_varimp

plot_grid(fig_fit, fig_varimp, nrow = 1, rel_widths = c(.4, .7))




################# cambodia 
# Load location data 
gps_cambodia =   readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv")) 
gps_dat_cam = cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()
head(gps_dat_cam)


predict_rao_cam = cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA,
                                     ifelse(ha40 < 1850, 1, 0))) %>% # BMI *100
  mutate(mortality = ifelse(is.na(hv111), NA, 
                            ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA, 
                                   ifelse(ha67 == 99, NA, 
                                          ifelse(ha67  == 0 | ha67 == 1, 1, 0)))) %>% # 0 is no education, # 1 is pre-primary 
  mutate(cooking_deprived = ifelse(hv226 == 6 | hv226 == 7 | hv226 == 8 | hv226 == 9| hv226 == 10| hv226 == 11, 1, 0)) %>% 
  # coal, charcoal, wood, straw/grass/ ag crop residue, animal dung 
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1, # flush idk where, pit w/out slab, no facility, composting, bucket, hanging toilet 
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_ ) ) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(hygiene_deprived = ifelse(sh138 ==1 & sh139a == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(hv123 ==1, 0, 1)) %>%
  mutate( hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0 )) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE ) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  left_join(gps_dat_cam, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) %>% 
  mutate(floor_deprived = housing_deprived) %>%
  group_by(spatial_cluster) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(location = "Cambodia") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>%  mutate(lat = round(lat, 5), long = round(long, 5)), by = c("location", "lat", "long")) %>%
  drop_na() 
head(predict_rao_cam )

cam_distances = readr::read_csv(file = here("data/cambodia", "healthcare_dist_cambodia_with_distances.csv")) %>%
  mutate(spatial_cluster = psuid) %>%
  left_join(predict_rao_cam, by = "spatial_cluster") %>% drop_na()
head(cam_distances)


# ── 1. Prep predictor matrix and outcome ──────────────────────────────────────
predictors <- cam_distances  %>%
  dplyr::select(nutrition_deprived, hygiene_deprived, mat_edu_deprived, 
                sanitation_deprived, water_deprived, electricity_deprived,
                cooking_deprived, floor_deprived, housing_deprived, 
                assets_deprived, mpi_standard, distance_km, mean_temp_C, annual_precip_mm) %>%
  as.matrix()

outcome <- log(cam_distances$rao + .001)
table(outcome)

# ── 2. K-fold cross validation (k = 5) ───────────────────────────────────────
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))

cv_preds <- numeric(nrow(predictors))


for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x,
               y.train = train_y,
               x.test  = test_x,
               ndpost  = 2000,
               nskip   = 250,
               printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}


# ── 3. Error metrics ──────────────────────────────────────────────────────────
mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- 1 - (ss_res / ss_tot)
r2
r2 = round(r2, digits =3)

rank_cor <- cor(outcome, cv_preds, method = "spearman")
rank_cor

# ── 4. Figure 1: Observed vs Predicted ───────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  annotate("text", x = min(outcome, na.rm = TRUE), 
           y = max(cv_preds, na.rm = TRUE),
           label = "",
           hjust = 0, vjust = 1, size = 5) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Cambodia", 
       subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1 )) + 
  xlim(c(0,1))

fig_fit

# ── 5. Full model for variable importance ─────────────────────────────────────
fit_full <- wbart(x.train = predictors,
                  y.train = outcome,
                  ndpost  = 1000,
                  nskip   = 250,
                  printevery = 0)

# Variable inclusion proportions (times each var used in tree splits)
var_importance <- tibble(
  variable   = colnames(predictors),
  importance = colMeans(fit_full$varcount / rowSums(fit_full$varcount))) %>%
  arrange(importance) %>%
  mutate(variable = factor(variable, levels = variable))

# ── 6. Figure 2: Variable Importance ─────────────────────────────────────────
fig_varimp <- ggplot(var_importance, aes(x = importance, y = variable)) +
  geom_col(fill = "darkmagenta", alpha = 0.8) +
  theme_bw(base_size = 13) +
  geom_vline(xintercept = (100/14)/100) + 
  labs(x = "Relative variable inclusion proportion",
       y = NULL,
       title = "Variable Importance- Cambodia") +
  theme(plot.title = element_text(size = 14),
        axis.text.y = element_text(size = 12))

fig_varimp

plot_grid(fig_fit, fig_varimp, nrow = 1, rel_widths = c(.4, .7))







################################################################
####################################################################
# BART models with permutation-based variable importance threshold
####################################################################

# ── Helper function: permutation test for variable importance ────────────────
# Shuffles the outcome, refits BART, and records importance under the null.
# Returns the 95th percentile of null importance for EACH variable, which
# accounts for variables having different numbers of split points
# (continuous variables get more candidate splits than binary indicators).

permutation_importance_threshold <- function(predictors, outcome, n_perms = 50,
                                             ndpost = 200, nskip = 100) {
  null_importance <- matrix(NA, nrow = n_perms, ncol = ncol(predictors))
  colnames(null_importance) <- colnames(predictors)
  
  for (p in 1:n_perms) {
    outcome_shuffled <- sample(outcome)
    
    fit_null <- wbart(x.train = predictors,
                      y.train = outcome_shuffled,
                      ndpost  = ndpost,
                      nskip   = nskip,
                      printevery = 0)
    
    null_importance[p, ] <- colMeans(fit_null$varcount / rowSums(fit_null$varcount))
  }
  
  null_importance
}

# ── Helper function: build the variable importance plot ─────────────────────
plot_var_importance <- function(var_importance, location_name, rank_cor) {
  ggplot(var_importance, aes(x = observed_importance, y = variable)) + 
                         #    fill = exceeds_null)) +
    geom_col(alpha = 0.35, fill = "orangered") +
 #   geom_errorbar(aes(xmin = null_95th_percentile, xmax = null_95th_percentile),
  #                width = 0.4, color = "black", linewidth = 0.6) +
   # scale_fill_manual(#values = c("TRUE" = "darkmagenta", "FALSE" = "gray70"),
        #              aesfill = "orangered") + 
                   #   name = "Above chance\nthreshold") +
    theme_bw(base_size = 13) +
    labs(x = "Variable inclusion proportion", y = NULL,
         title = paste0(location_name),
         subtitle = paste0("ρ = ",
                           round(rank_cor, 2))) +
    theme(plot.title    = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.text.y   = element_text(size = 12),
          legend.position = "bottom")
}

####################################################################
# KENYA
####################################################################
place_clim <- readr::read_csv(file = here("data", "place_loc_climate.csv"))

pred_rao_kenya <- kenya_meta %>%
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>%
  mutate(hygiene_deprived = ifelse(hws_bl == 1 | tr %in% c("Handwashing", "WSH"), 0, 1)) %>%
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>%
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>%
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH", 0, 1)) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(floor == 0, 1, 0)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(animal_ownership = ifelse(poultry > 0 | cow > 0 | goat > 0, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:animal_ownership, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(spatial_cluster = block, location = "Kenya") %>%
  dplyr::select(-block) %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  left_join(place_clim, by = c("location", "lat", "long")) %>%
  drop_na()

predictors <- pred_rao_kenya %>%
  dplyr::select(nutrition_deprived:housing_deprived, assets_deprived, animal_ownership, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()
head(predictors)
colnames(predictors) = c("Food security", "Hygiene", "Maternal education", "Sanitation", "Drinking water",
                         "Electricity", "Cooking",  "Flooring", "Housing Material", "Assets", "Animal Ownership" ,"Poverty",
                         "Temperature", "Precipitation")
head(predictors)

outcome <- log(pred_rao_kenya$rao + .001)

# ── K-fold CV ──────────────────────────────────────────────────────────────
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))
cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x, y.train = train_y, x.test = test_x,
               ndpost = 1000, nskip = 250, printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- round(1 - (ss_res / ss_tot), digits = 3)
rank_cor <- cor(outcome, cv_preds, method = "spearman")

# ── Fig 1: Observed vs predicted ─────────────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit_kenya <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Kenya", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1)) + xlim(c(0,1))
fig_fit_kenya

# ── Full model + permutation-based importance threshold ─────────────────────
fit_full <- wbart(x.train = predictors, y.train = outcome,
                  ndpost = 1000, nskip = 250, printevery = 0)

observed_importance <- colMeans(fit_full$varcount / rowSums(fit_full$varcount))

null_importance_kenya <- permutation_importance_threshold(predictors, outcome, n_perms = 50)
null_95th_kenya <- apply(null_importance_kenya, 2, quantile, 0.95)

var_importance_kenya <- tibble(
  variable             = colnames(predictors),
  observed_importance  = observed_importance,
  null_95th_percentile = null_95th_kenya
) %>%
  mutate(exceeds_null = observed_importance > null_95th_percentile) %>%
  arrange(observed_importance) %>%
  mutate(variable = factor(variable, levels = variable))

fig_varimp_kenya <- plot_var_importance(var_importance_kenya, "Kenya", rank_cor)
fig_varimp_kenya


####################################################################
# BANGLADESH
####################################################################
gps_dat <- read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>%
  left_join(public_ids, by = "dataid") %>%
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat)

pred_rao_bangl <- bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation", "WSH"), 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH"), 0, 1)) %>%
  mutate(hygiene_deprived = ifelse(hwsws == 1 | tr %in% c("Handwashing", "WSH"), 0, 1)) %>%
  mutate(floor_deprived = ifelse(cement == 0, 1, 0)) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = rowSums(across(asset_radio:asset_refrig), na.rm = TRUE)) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(block_r = block) %>% dplyr::select(-block) %>%
  left_join(gps_dat, by = "block_r") %>%
  mutate(spatial_cluster = block) %>%
  mutate(location = "Bangladesh") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>% mutate(lat = round(lat, 5), long = round(long, 5)),
            by = c("location", "lat", "long")) %>%
  drop_na() 

predictors <- pred_rao_bangl %>%
  dplyr::select(nutrition_deprived:housing_deprived, assets_deprived, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()
head(predictors)
colnames(predictors) = c("Food security", "Maternal education", "Sanitation", "Drinking water", "Hygiene",
                         "Flooring", "Electricity", "Housing Material", "Assets", "Poverty",
                         "Temperature", "Precipitation")
head(predictors)

outcome <- log(pred_rao_bangl$rao)

k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))
cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x, y.train = train_y, x.test = test_x,
               ndpost = 2000, nskip = 250, printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- round(1 - (ss_res / ss_tot), digits = 3)
rank_cor <- cor(outcome, cv_preds, method = "spearman")

cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit_bangl <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Bangladesh", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1)) + xlim(c(0,1))
fig_fit_bangl

fit_full <- wbart(x.train = predictors, y.train = outcome,
                  ndpost = 1000, nskip = 250, printevery = 0)

observed_importance <- colMeans(fit_full$varcount / rowSums(fit_full$varcount))

null_importance_bangl <- permutation_importance_threshold(predictors, outcome, n_perms = 50)
null_95th_bangl <- apply(null_importance_bangl, 2, quantile, 0.95)

var_importance_bangl <- tibble(
  variable             = colnames(predictors),
  observed_importance  = observed_importance,
  null_95th_percentile = null_95th_bangl
) %>%
  mutate(exceeds_null = observed_importance > null_95th_percentile) %>%
  arrange(observed_importance) %>%
  mutate(variable = factor(variable, levels = variable))

fig_varimp_bangl <- plot_var_importance(var_importance_bangl, "Bangladesh", rank_cor)
fig_varimp_bangl


####################################################################
# CAMBODIA
####################################################################
gps_cambodia <- readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv"))
gps_dat_cam <- cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()

predict_rao_cam <- cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA, ifelse(ha40 < 1850, 1, 0))) %>%
  mutate(mortality = ifelse(is.na(hv111), NA, ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA,
                                   ifelse(ha67 == 99, NA, ifelse(ha67 == 0 | ha67 == 1, 1, 0)))) %>%
  mutate(cooking_deprived = ifelse(hv226 %in% c(6,7,8,9,10,11), 1, 0)) %>%
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1,
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(hygiene_deprived = ifelse(sh138 == 1 & sh139a == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(hv123 == 1, 0, 1)) %>%
  mutate(hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0)) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  left_join(gps_dat_cam, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) %>%
  mutate(floor_deprived = housing_deprived) %>%
  group_by(spatial_cluster) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(location = "Cambodia") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>% mutate(lat = round(lat, 5), long = round(long, 5)),
            by = c("location", "lat", "long")) %>%
  drop_na()

cam_distances <- readr::read_csv(file = here("data/cambodia", "healthcare_dist_cambodia_with_distances.csv")) %>%
  mutate(spatial_cluster = psuid) %>%
  left_join(predict_rao_cam, by = "spatial_cluster") %>%
  drop_na()

predictors <- cam_distances %>%
  dplyr::select(nutrition_deprived, hygiene_deprived, mat_edu_deprived,
                sanitation_deprived, water_deprived, electricity_deprived,
                cooking_deprived, floor_deprived, housing_deprived,
                assets_deprived, mpi_standard, distance_km, mean_temp_C, annual_precip_mm) %>%
  as.matrix()
head(predictors)
colnames(predictors) = c("Food security", "Hygiene", "Education", "Sanitation", "Drinking water", 
                        "Electricity", "Cooking", "Flooring", "Housing Material", "Assets", "Poverty",
                        "Healthcare proximity", "Temperature", "Precipitation")
head(predictors)






outcome <- log(cam_distances$rao + .001)

k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))
cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x, y.train = train_y, x.test = test_x,
               ndpost = 2000, nskip = 250, printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- round(1 - (ss_res / ss_tot), digits = 3)
rank_cor <- cor(outcome, cv_preds, method = "spearman")

cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit_cambo <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Cambodia", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1)) + xlim(c(0,1))
fig_fit_cambo

fit_full <- wbart(x.train = predictors, y.train = outcome,
                  ndpost = 1000, nskip = 250, printevery = 0)

observed_importance <- colMeans(fit_full$varcount / rowSums(fit_full$varcount))

null_importance_cambo <- permutation_importance_threshold(predictors, outcome, n_perms = 50)
null_95th_cambo <- apply(null_importance_cambo, 2, quantile, 0.95)

var_importance_cambo <- tibble(
  variable             = colnames(predictors),
  observed_importance  = observed_importance,
  null_95th_percentile = null_95th_cambo
) %>%
  mutate(exceeds_null = observed_importance > null_95th_percentile) %>%
  arrange(observed_importance) %>%
  mutate(variable = factor(variable, levels = variable))

fig_varimp_cambo <- plot_var_importance(var_importance_cambo, "Cambodia", rank_cor)
fig_varimp_cambo


# combine 

fig3b_build = plot_grid(fig_varimp_bangl, fig_varimp_kenya, fig_varimp_cambo, ncol = 3) 

fig3b <- fig3b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3b


plot_grid(fig3a, fig3b, ncol = 1)

####################################################
# supp analyses 

# other goals 

cluster_wide <- multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, fraction) %>%
  pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
head(cluster_wide)

min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
                                      rank_col, interventions_in_combo,
                                      threshold = 0.50) {
  
  # Use fraction_rao for rao_quadratic, fraction for everything else
  # frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
  frac_col <- if (rank_col == "rao_quadratic") "fraction" else "fraction"
  
  total_burden <- cluster_prev_sub %>%
    filter(pathogen %in% interventions_in_combo) %>%
    group_by(pathogen) %>%
    #  summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
    summarise(total = sum(.data[["num"]], na.rm = TRUE), .groups = "drop")
  
  
  ranked_clusters <- cluster_scores_sub %>%
    arrange(desc(.data[[rank_col]])) %>%
    pull(spatial_cluster)
  
  cumulative <- tibble(pathogen = interventions_in_combo, cum_burden = 0)
  
  for (i in seq_along(ranked_clusters)) {
    clust <- ranked_clusters[i]
    this_cluster <- cluster_prev_sub %>%
      filter(spatial_cluster == clust, pathogen %in% interventions_in_combo) %>%
      dplyr::select(pathogen, all_of(frac_col), num) %>%
      rename(frac_val = all_of(frac_col))
    
    cumulative <- cumulative %>%
      left_join(this_cluster, by = "pathogen") %>%
      # mutate(cum_burden = cum_burden + replace_na(frac_val, 0)) %>%
      mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
      dplyr::select(pathogen, cum_burden)
    
    pct_reached <- cumulative %>%
      left_join(total_burden, by = "pathogen") %>%
      mutate(pct = cum_burden / total)
    
    if (all(pct_reached$pct >= threshold)) {
      return(tibble(n_clusters = i, rank_method = rank_col))
    }
  }
  return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
}

# run across all locations for combinations of pathogens with at least 3 intervetions 
rank_methods <- c("shannon", "alpha_div", "gini_simpson", "rao_quadratic")

library(purrr)
results_all <- map_dfr(unique(multipathogen_indices$location), function(loc) {
  
  loc_data <- multipathogen_indices %>% filter(location == loc)
  all_interventions <- unique(loc_data$pathogen)
  n_interventions <- length(all_interventions)
  
  # All combinations of size 3 up to n_interventions
  combos <- map(3:n_interventions, ~ combn(all_interventions, .x, simplify = FALSE)) %>%
    flatten()
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Total clusters in location — calculated from full loc_data, not the combo subset
    n_total_clusters <- n_distinct(loc_data$spatial_cluster)
    
    cluster_prev_sub <- loc_data %>% filter(pathogen %in% combo)
    
    # Recompute diversity scores for this combo only
    cluster_scores_sub <- cluster_wide %>%
      filter(location == loc) %>%
      dplyr::select(spatial_cluster, any_of(combo)) %>%
      rowwise() %>%
      mutate(shannon      = shannon_entropy(c_across(all_of(combo))),
             alpha_div    = alpha_diversity(c_across(all_of(combo))),
             gini_simpson = gini_simpson(c_across(all_of(combo))),
             rao_quadratic = rao_quadratic(c_across(all_of(combo)))  ) %>%
      ungroup()
    
    n_total_clusters <- n_distinct(cluster_prev_sub$spatial_cluster)
    
    map_dfr(rank_methods, function(rm) {
      res <- min_clusters_to_threshold(cluster_prev_sub      = cluster_prev_sub,
                                       cluster_scores_sub    = cluster_scores_sub,
                                       rank_col              = rm,
                                       interventions_in_combo = combo )
      res %>% mutate(location = loc,
                     combo = combo_label,
                     n_total_clusters = n_total_clusters,
                     prop_clusters   = n_clusters / n_total_clusters )
    })
  })
})

head(results_all)

# ---------------------------------------------------------------
# Plot
# ---------------------------------------------------------------

method_labels <- c(shannon      = "Shannon diversity index",
                   alpha_div    = "Alpha diversity",
                   gini_simpson = "Gini-Simpson index",
                   rao_quadratic = "Rao's quadratic index")
library(forcats)
summary_results_all = results_all %>%
  group_by(rank_method, location) %>%
  mutate(mean_clust = mean(prop_clusters)) %>%
  ungroup() %>%
  distinct(rank_method, location, mean_clust)


sup50 = results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = factor(rank_method, levels = c(
    "Rao's quadratic index",
    "Alpha diversity", 
    "Shannon diversity index", 
    "Gini-Simpson index"))) %>%
  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_hline(yintercept = .50, col = "gray", lty = "dashed", lwd = 1.25) +
  # stat_summary(fun = mean, geom = "bar", alpha = 0.7, width = 0.6) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.6, shape = 16) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    #title = "Proportion of clusters needed to reach 80% coverage",
    x     = NULL,
    y     = "Proportion of clusters to reach 50% coverage" ) + # to\nreac
  theme_bw(base_size = 11) +
  # theme_minimal() +
  theme(legend.position = "botttom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0.25, .68)) +
  scale_fill_viridis_d(option = "D" , end = .7 , name = "Strategy", alpha = .66) +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    legend.position = "bottom")  +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))
sup50


##### 90% 

cluster_wide <- multipathogen_indices %>%
  dplyr::select(location, spatial_cluster, pathogen, fraction) %>%
  pivot_wider(names_from = pathogen, values_from = fraction, values_fill = 0)
head(cluster_wide)

min_clusters_to_threshold <- function(cluster_prev_sub, cluster_scores_sub,
                                      rank_col, interventions_in_combo,
                                      threshold = 0.90) {
  
  # Use fraction_rao for rao_quadratic, fraction for everything else
  # frac_col <- if (rank_col == "rao_quadratic") "fraction_rao" else "fraction"
  frac_col <- if (rank_col == "rao_quadratic") "fraction" else "fraction"
  
  total_burden <- cluster_prev_sub %>%
    filter(pathogen %in% interventions_in_combo) %>%
    group_by(pathogen) %>%
    #  summarise(total = sum(.data[[frac_col]], na.rm = TRUE), .groups = "drop")
    summarise(total = sum(.data[["num"]], na.rm = TRUE), .groups = "drop")
  
  
  ranked_clusters <- cluster_scores_sub %>%
    arrange(desc(.data[[rank_col]])) %>%
    pull(spatial_cluster)
  
  cumulative <- tibble(pathogen = interventions_in_combo, cum_burden = 0)
  
  for (i in seq_along(ranked_clusters)) {
    clust <- ranked_clusters[i]
    this_cluster <- cluster_prev_sub %>%
      filter(spatial_cluster == clust, pathogen %in% interventions_in_combo) %>%
      dplyr::select(pathogen, all_of(frac_col), num) %>%
      rename(frac_val = all_of(frac_col))
    
    cumulative <- cumulative %>%
      left_join(this_cluster, by = "pathogen") %>%
      # mutate(cum_burden = cum_burden + replace_na(frac_val, 0)) %>%
      mutate(cum_burden = cum_burden + replace_na(num, 0)) %>%
      dplyr::select(pathogen, cum_burden)
    
    pct_reached <- cumulative %>%
      left_join(total_burden, by = "pathogen") %>%
      mutate(pct = cum_burden / total)
    
    if (all(pct_reached$pct >= threshold)) {
      return(tibble(n_clusters = i, rank_method = rank_col))
    }
  }
  return(tibble(n_clusters = NA_integer_, rank_method = rank_col))
}

# run across all locations for combinations of pathogens with at least 3 intervetions 
rank_methods <- c("shannon", "alpha_div", "gini_simpson", "rao_quadratic")

library(purrr)
results_all <- map_dfr(unique(multipathogen_indices$location), function(loc) {
  
  loc_data <- multipathogen_indices %>% filter(location == loc)
  all_interventions <- unique(loc_data$pathogen)
  n_interventions <- length(all_interventions)
  
  # All combinations of size 3 up to n_interventions
  combos <- map(3:n_interventions, ~ combn(all_interventions, .x, simplify = FALSE)) %>%
    flatten()
  
  map_dfr(combos, function(combo) {
    combo_label <- paste(sort(combo), collapse = " + ")
    
    # Total clusters in location — calculated from full loc_data, not the combo subset
    n_total_clusters <- n_distinct(loc_data$spatial_cluster)
    
    cluster_prev_sub <- loc_data %>% filter(pathogen %in% combo)
    
    # Recompute diversity scores for this combo only
    cluster_scores_sub <- cluster_wide %>%
      filter(location == loc) %>%
      dplyr::select(spatial_cluster, any_of(combo)) %>%
      rowwise() %>%
      mutate(shannon      = shannon_entropy(c_across(all_of(combo))),
             alpha_div    = alpha_diversity(c_across(all_of(combo))),
             gini_simpson = gini_simpson(c_across(all_of(combo))),
             rao_quadratic = rao_quadratic(c_across(all_of(combo)))  ) %>%
      ungroup()
    
    n_total_clusters <- n_distinct(cluster_prev_sub$spatial_cluster)
    
    map_dfr(rank_methods, function(rm) {
      res <- min_clusters_to_threshold(cluster_prev_sub      = cluster_prev_sub,
                                       cluster_scores_sub    = cluster_scores_sub,
                                       rank_col              = rm,
                                       interventions_in_combo = combo )
      res %>% mutate(location = loc,
                     combo = combo_label,
                     n_total_clusters = n_total_clusters,
                     prop_clusters   = n_clusters / n_total_clusters )
    })
  })
})

head(results_all)

# ---------------------------------------------------------------
# Plot
# ---------------------------------------------------------------

method_labels <- c(shannon      = "Shannon diversity index",
                   alpha_div    = "Alpha diversity",
                   gini_simpson = "Gini-Simpson index",
                   rao_quadratic = "Rao's quadratic index")
library(forcats)
summary_results_all = results_all %>%
  group_by(rank_method, location) %>%
  mutate(mean_clust = mean(prop_clusters)) %>%
  ungroup() %>%
  distinct(rank_method, location, mean_clust)


sup90 = results_all %>%
  mutate(rank_method = recode(rank_method, !!!method_labels)) %>%
  mutate(rank_method = factor(rank_method, levels = c(
    "Rao's quadratic index",
    "Alpha diversity", 
    "Shannon diversity index", 
    "Gini-Simpson index"))) %>%
  mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))) %>%
  ggplot(aes(x = rank_method, y = prop_clusters, fill = rank_method)) +
  geom_hline(yintercept = .90, col = "gray", lty = "dashed", lwd = 1.25) +
  # stat_summary(fun = mean, geom = "bar", alpha = 0.7, width = 0.6) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.6, shape = 16) +
  facet_wrap(vars(location), scales = "free_y") +
  labs(
    #title = "Proportion of clusters needed to reach 80% coverage",
    x     = NULL,
    y     = "Proportion of clusters to reach 90% coverage" ) + # to\nreac
  theme_bw(base_size = 11) +
  # theme_minimal() +
  theme(legend.position = "botttom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0.70, .95)) +
  scale_fill_viridis_d(option = "D" , end = .7 , name = "Strategy", alpha = .66) +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14), 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    legend.position = "bottom")  +
  theme(strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_rect(fill = "white", colour = "black"),
        plot.title = element_text(size = 14))
sup90

plot_grid(sup50, sup90, ncol = 1)


########################################
# bootstrapped uncertainty 


# ── Step 1: Bootstrap function ────────────────────────────────────────────────
# For each location, resample spatial clusters WITH replacement (same n as
# original), recompute the "clusters to reach 80% coverage" proportion for
# every pathogen x strategy combination, repeat many times.

bootstrap_clusters_80 <- function(n_boot = 100) {
  
  map_dfr(unique(best_combos$location), function(loc) {
    
    best               <- best_combos %>% filter(location == loc)
    combo_label        <- best$combo[[1]]
    pathogens_in_combo <- str_split(combo_label, " \\+ ")[[1]]
    
    all_clusters <- cluster_by_pathogen %>%
      filter(location == loc) %>%
      pull(spatial_cluster) %>%
      unique()
    
    n_clusters <- length(all_clusters)
    
    rao_order_full   <- rao_ranking_pathogen   %>% filter(location == loc) %>% arrange(rank_rao)   %>% pull(spatial_cluster)
    single_order_full<- single_pathogen_ranking%>% filter(location == loc) %>% arrange(rank_single) %>% pull(spatial_cluster)
    mpi_order_full   <- mpi_ranking            %>% filter(location == loc) %>% arrange(rank_mpi)    %>% pull(spatial_cluster)
    
    path_data_full <- cluster_by_pathogen %>%
      filter(location == loc, pathogen %in% pathogens_in_combo)
    
    map_dfr(1:n_boot, function(b) {
      
      # Resample clusters WITH replacement, same n as original
      boot_clusters <- sample(all_clusters, size = n_clusters, replace = TRUE)
      
      # Re-derive each strategy's order, restricted to (duplicated) resampled clusters
      # Order is preserved from the original ranking, just filtered/expanded to the boot sample
      rao_order    <- boot_clusters[order(match(boot_clusters, rao_order_full))]
      single_order <- boot_clusters[order(match(boot_clusters, single_order_full))]
      mpi_order    <- boot_clusters[order(match(boot_clusters, mpi_order_full))]
      
      path_data <- path_data_full %>%
        filter(spatial_cluster %in% boot_clusters)
      
      map_dfr(unique(path_data$pathogen), function(path) {
        
        p_data <- path_data %>% filter(pathogen == path)
        
        # total_need recomputed on the bootstrap sample (with duplicates weighted by count)
        cluster_counts <- table(boot_clusters)
        
        total_need <- sum(p_data$num[match(names(cluster_counts), p_data$spatial_cluster)] *
                            as.numeric(cluster_counts), na.rm = TRUE)
        
        get_cum_fraction <- function(order_vec) {
          cum <- map_dbl(seq_along(order_vec), function(i) {
            included <- order_vec[1:i]
            counts_i <- table(included)
            sum(p_data$num[match(names(counts_i), p_data$spatial_cluster)] *
                  as.numeric(counts_i), na.rm = TRUE)
          })
          frac <- cum / total_need
          first_80 <- which(frac >= 0.80)[1]
          if (is.na(first_80)) NA_real_ else first_80 / n_clusters
        }
        
        tibble(
          location = loc, pathogen = path, boot_id = b,
          prop_80_rao    = get_cum_fraction(rao_order),
          prop_80_single = get_cum_fraction(single_order),
          prop_80_mpi    = get_cum_fraction(mpi_order)
        )
      })
    })
  })
}

# Run bootstrap (500 iterations -- adjust down to 100-200 first to test speed,
# this can be slow given the nested cumulative sum logic)
#set.seed(42)
boot_results <- bootstrap_clusters_80(n_boot = 100)

# ── Step 2: Summarize into median + 95% CI per pathogen x strategy ───────────
boot_summary <- boot_results %>%
  pivot_longer(cols = starts_with("prop_80_"),
               names_to = "strategy_key", values_to = "prop_clusters_80") %>%
  mutate(strategy = case_when(
    strategy_key == "prop_80_rao"    ~ "Rao's quadratic index",
    strategy_key == "prop_80_single" ~ "Vaccine only",
    strategy_key == "prop_80_mpi"    ~ "Wealth"
  )) %>%
  filter(!is.na(prop_clusters_80)) %>%
  group_by(location, pathogen, strategy) %>%
  summarise(
    median_80 = quantile(prop_clusters_80, 0.50),
    lower_80  = quantile(prop_clusters_80, 0.025),
    upper_80  = quantile(prop_clusters_80, 0.975),
    .groups = "drop"
  )

head(boot_summary)

# ── Step 3: Join with pathogen ordering/renaming from your existing pipeline ──
boot_summary_ordered <- boot_summary %>%
  left_join(vaccine_order, by = c("location", "pathogen")) %>%
  mutate(pathogen = replace(pathogen, pathogen == "Trichuris", "T. trichiura"),
         pathogen = replace(pathogen, pathogen == "Plasmodium falciparum", "P. falciparum"),
         pathogen = replace(pathogen, pathogen == "Plasmodium vivax", "P. vivax"),
         pathogen = replace(pathogen, pathogen == "Ascaris", "A. lumbricoides"),
         pathogen = replace(pathogen, pathogen == "Strongyloides stercoralis", "S. stercoralis")) %>%
  group_by(location) %>%
  mutate(pathogen_ordered = reorder(pathogen, vaccine_val)) %>%
  ungroup()

# ── Step 4: Plot with dodged points + error bars ─────────────────────────────
boot_build <- ggplot(boot_summary_ordered %>%
                        mutate(location = factor(location, levels = c("Bangladesh", "Kenya", "Cambodia"))),
                      aes(x = pathogen_ordered, y = median_80, color = strategy)) +
  geom_errorbar(aes(ymin = lower_80, ymax = upper_80),
                width = 0.25, lwd = 0.9,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 4, shape = 16,
             position = position_dodge(width = 0.6)) +
  facet_wrap(vars(location), scales = "free_x") +
  scale_color_manual(
    values = c("Rao's quadratic index" = "#440154FF",
               "Vaccine only"          = "cornflowerblue",
               "Wealth"                = "orangered"),
    name = "Strategy") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray") +
  labs(x = "Pathogen", y = "Proportion of clusters to reach 80% coverage") +
  theme_bw(base_size = 11) +
  theme(axis.text.x      = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y      = element_text(size = 12),
        axis.title       = element_text(size = 15),
        legend.text      = element_text(size = 13),
        legend.title     = element_text(size = 14),
        legend.position  = "bottom",
        panel.spacing.x  = unit(1.15, "cm"),
        strip.background = element_rect(fill = "white", colour = "black"),
        strip.text       = element_text(size = 16))
boot_build 




############################ try to predict ind level rao 
library(BART)
# join to Rao
rao_mpi = left_join(mpi_all_locations, rao_by_cluster, by = c("location", "spatial_cluster")) %>%
  group_by(location) %>%
  mutate(max_mpi = max(mpi, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mpi_standard = mpi/max_mpi)
head(rao_mpi)


################################################################
####################################################################
# BART models with permutation-based variable importance threshold
####################################################################

# ── Helper function: permutation test for variable importance ────────────────
# Shuffles the outcome, refits BART, and records importance under the null.
# Returns the 95th percentile of null importance for EACH variable, which
# accounts for variables having different numbers of split points
# (continuous variables get more candidate splits than binary indicators).

permutation_importance_threshold <- function(predictors, outcome, n_perms = 50,
                                             ndpost = 200, nskip = 100) {
  null_importance <- matrix(NA, nrow = n_perms, ncol = ncol(predictors))
  colnames(null_importance) <- colnames(predictors)
  
  for (p in 1:n_perms) {
    outcome_shuffled <- sample(outcome)
    
    fit_null <- wbart(x.train = predictors,
                      y.train = outcome_shuffled,
                      ndpost  = ndpost,
                      nskip   = nskip,
                      printevery = 0)
    
    null_importance[p, ] <- colMeans(fit_null$varcount / rowSums(fit_null$varcount))
  }
  
  null_importance
}

# ── Helper function: build the variable importance plot ─────────────────────
plot_var_importance <- function(var_importance, location_name, rank_cor) {
  ggplot(var_importance, aes(x = observed_importance, y = variable)) + 
    #    fill = exceeds_null)) +
    geom_col(alpha = 0.35, fill = "orangered") +
    #   geom_errorbar(aes(xmin = null_95th_percentile, xmax = null_95th_percentile),
    #                width = 0.4, color = "black", linewidth = 0.6) +
    # scale_fill_manual(#values = c("TRUE" = "darkmagenta", "FALSE" = "gray70"),
    #              aesfill = "orangered") + 
    #   name = "Above chance\nthreshold") +
    theme_bw(base_size = 13) +
    labs(x = "Variable inclusion proportion", y = NULL,
         title = paste0(location_name),
         subtitle = paste0("ρ = ",
                           round(rank_cor, 2))) +
    theme(plot.title    = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.text.y   = element_text(size = 12),
          legend.position = "bottom")
}

####################################################################
# KENYA
####################################################################
place_clim <- readr::read_csv(file = here("data", "place_loc_climate.csv")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5))

             
pred_rao_kenya <- kenya_meta %>%
  left_join(child_death, by = "hhid") %>%
  left_join(block_kenya, by = c("block", "clusterid")) %>%
  mutate(nutrition_deprived = ifelse(HHS_bi == 1, 1, 0)) %>%
  mutate(hygiene_deprived = ifelse(hws_bl == 1 | tr %in% c("Handwashing", "WSH"), 0, 1)) %>%
  mutate(mat_edu_deprived = ifelse(mother_edu == 0, 1, 0)) %>%
  mutate(sanitation_deprived = ifelse(imp_lat_el == 0 | tr == "WSH", 1, 0)) %>%
  mutate(water_deprived = ifelse(prim_drink_ws_bl == 1 & water_time < 31 | tr == "WSH", 0, 1)) %>%
  mutate(across(roof:car, ~replace(., . > 8, NA))) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(cooking_deprived = ifelse(cooker == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(floor == 0, 1, 0)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = radio + tv + mobilephone + bicycle + car + motorcycle) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(animal_ownership = ifelse(poultry > 0 | cow > 0 | goat > 0, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:animal_ownership, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(spatial_cluster = block, location = "Kenya") %>%
  dplyr::select(-block) %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(as.numeric(lat), 5), long = round(as.numeric(long), 5)) %>%
  left_join(place_clim, by = c("location", "lat", "long")) %>%
  drop_na()
head(pred_rao_kenya)

predictors <- pred_rao_kenya %>%
  dplyr::select(nutrition_deprived:housing_deprived, assets_deprived, animal_ownership, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()
head(predictors)
colnames(predictors) = c("Food security", "Hygiene", "Maternal education", "Sanitation", "Drinking water",
                         "Electricity", "Cooking",  "Flooring", "Housing Material", "Assets", "Animal Ownership" ,"Poverty",
                         "Temperature", "Precipitation")
head(predictors)

outcome <- log(pred_rao_kenya$rao + .001)

# ── K-fold CV ──────────────────────────────────────────────────────────────
k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))
cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x, y.train = train_y, x.test = test_x,
               ndpost = 1000, nskip = 250, printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- round(1 - (ss_res / ss_tot), digits = 3)
rank_cor <- cor(outcome, cv_preds, method = "spearman")

# ── Fig 1: Observed vs predicted ─────────────────────────────────────────────
cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit_kenya <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Kenya", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1)) + xlim(c(0,1))
fig_fit_kenya

# ── Full model + permutation-based importance threshold ─────────────────────
fit_full <- wbart(x.train = predictors, y.train = outcome,
                  ndpost = 1000, nskip = 250, printevery = 0)

observed_importance <- colMeans(fit_full$varcount / rowSums(fit_full$varcount))

null_importance_kenya <- permutation_importance_threshold(predictors, outcome, n_perms = 50)
null_95th_kenya <- apply(null_importance_kenya, 2, quantile, 0.95)

var_importance_kenya <- tibble(
  variable             = colnames(predictors),
  observed_importance  = observed_importance,
  null_95th_percentile = null_95th_kenya
) %>%
  mutate(exceeds_null = observed_importance > null_95th_percentile) %>%
  arrange(observed_importance) %>%
  mutate(variable = factor(variable, levels = variable))

fig_varimp_kenya <- plot_var_importance(var_importance_kenya, "Kenya", rank_cor)
fig_varimp_kenya


####################################################################
# BANGLADESH
####################################################################
gps_dat <- read_dta(file = here("data/bangl/gps/untouched", "6. WASHB_Baseline_gps.dta")) %>%
  mutate(dataid = as.numeric(dataid)) %>%
  left_join(public_ids, by = "dataid") %>%
  dplyr::select(block, block_r, qgpslong, qgpslat) %>%
  group_by(block) %>%
  mutate(med_qgpslong = median(qgpslong), med_qgpslat = median(qgpslat)) %>%
  distinct(block, block_r, med_qgpslong, med_qgpslat)

pred_rao_bangl <- bangl_metadata %>%
  left_join(treatment_bangl, by = c("clusterid", "block")) %>%
  mutate(nutrition_deprived = ifelse(hfiacat == "Food Secure", 0, 1)) %>%
  mutate(mat_edu_deprived  = ifelse(momedu == "Secondary (>5y)", 0, 1)) %>%
  mutate(sanitation_deprived = ifelse(latown == 1 & latseal == 1 & latfeces == 1 | tr %in% c("Sanitation", "WSH"), 0, 1)) %>%
  mutate(water_deprived = ifelse(tubewell == 1 & watmin < 30 | tr %in% c("Water", "WSH"), 0, 1)) %>%
  mutate(hygiene_deprived = ifelse(hwsws == 1 | tr %in% c("Handwashing", "WSH"), 0, 1)) %>%
  mutate(floor_deprived = ifelse(cement == 0, 1, 0)) %>%
  mutate(electricity_deprived = ifelse(elec == 1, 0, 1)) %>%
  mutate(housing_deprived = ifelse(roof == 1 | floor == 1 | walls == 1, 0, 1)) %>%
  mutate(asset_count = rowSums(across(asset_radio:asset_refrig), na.rm = TRUE)) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  group_by(block) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(block_r = block) %>% dplyr::select(-block) %>%
  left_join(gps_dat, by = "block_r") %>%
  mutate(spatial_cluster = block) %>%
  mutate(location = "Bangladesh") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>% mutate(lat = round(lat, 5), long = round(long, 5)),
            by = c("location", "lat", "long")) %>%
  drop_na() 

predictors <- pred_rao_bangl %>%
  dplyr::select(nutrition_deprived:housing_deprived, assets_deprived, mpi_standard, mean_temp_C, annual_precip_mm) %>%
  as.matrix()
head(predictors)
colnames(predictors) = c("Food security", "Maternal education", "Sanitation", "Drinking water", "Hygiene",
                         "Flooring", "Electricity", "Housing Material", "Assets", "Poverty",
                         "Temperature", "Precipitation")
head(predictors)

outcome <- log(pred_rao_bangl$rao)

k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))
cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x, y.train = train_y, x.test = test_x,
               ndpost = 2000, nskip = 250, printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- round(1 - (ss_res / ss_tot), digits = 3)
rank_cor <- cor(outcome, cv_preds, method = "spearman")

cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit_bangl <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Bangladesh", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1)) + xlim(c(0,1))
fig_fit_bangl

fit_full <- wbart(x.train = predictors, y.train = outcome,
                  ndpost = 1000, nskip = 250, printevery = 0)

observed_importance <- colMeans(fit_full$varcount / rowSums(fit_full$varcount))

null_importance_bangl <- permutation_importance_threshold(predictors, outcome, n_perms = 50)
null_95th_bangl <- apply(null_importance_bangl, 2, quantile, 0.95)

var_importance_bangl <- tibble(
  variable             = colnames(predictors),
  observed_importance  = observed_importance,
  null_95th_percentile = null_95th_bangl
) %>%
  mutate(exceeds_null = observed_importance > null_95th_percentile) %>%
  arrange(observed_importance) %>%
  mutate(variable = factor(variable, levels = variable))

fig_varimp_bangl <- plot_var_importance(var_importance_bangl, "Bangladesh", rank_cor)
fig_varimp_bangl


####################################################################
# CAMBODIA
####################################################################
gps_cambodia <- readr::read_csv(file = here("projects/6-multipathogen-burden/data/cambodia", "cambodia_ea_dhs.csv"))
gps_dat_cam <- cambodia_serology %>%
  dplyr::select(dhsclust, psuid) %>% distinct()

predict_rao_cam <- cam_wealth_data %>%
  mutate(nutrition_deprived = ifelse(is.na(ha40), NA, ifelse(ha40 < 1850, 1, 0))) %>%
  mutate(mortality = ifelse(is.na(hv111), NA, ifelse(hv111 == 0, 1, 0))) %>%
  mutate(mat_edu_deprived = ifelse(is.na(ha67), NA,
                                   ifelse(ha67 == 99, NA, ifelse(ha67 == 0 | ha67 == 1, 1, 0)))) %>%
  mutate(cooking_deprived = ifelse(hv226 %in% c(6,7,8,9,10,11), 1, 0)) %>%
  mutate(sanitation_deprived = case_when(
    hv205 %in% c(15,23,31,41,42,43) ~ 1,
    hv205 != 99 & hv225 == 1 ~ 1,
    hv205 != 99 & hv225 == 0 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(sh104 = replace(sh104, sh104 > 900, NA), sh104b = replace(sh104b, sh104b > 900, NA)) %>%
  mutate(water_deprived = case_when(
    hv237 == 0 ~ 1,
    hv237 == 1 & sh104  > 30 ~ 1,
    hv237 == 1 & sh104b > 30 ~ 1,
    hv237 == 1 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(hygiene_deprived = ifelse(sh138 == 1 & sh139a == 1, 0, 1)) %>%
  mutate(floor_deprived = ifelse(hv123 == 1, 0, 1)) %>%
  mutate(hv206 = replace(hv206, hv206 > 2, NA)) %>%
  mutate(electricity_deprived = ifelse(hv206 == 0, 1, 0)) %>%
  mutate(housing_deprived = case_when(
    hv213 %in% c(11,21) |
      hv214 %in% c(11,12,13,21,22,23) |
      hv215 %in% c(11,12,21,22,23,24) ~ 1,
    hv213 == 96 | hv214 %in% c(96,99) | hv215 %in% c(96,99) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(asset_count = hv208 + hv209 + hv210 + hv211 + hv212 + hv221 + hv243c, na.rm = TRUE) %>%
  mutate(assets_deprived = ifelse(asset_count <= 1, 1, 0)) %>%
  mutate(dhsclust = hv001) %>%
  left_join(gps_dat_cam, by = "dhsclust") %>%
  mutate(psuid = psuid + 1) %>%
  mutate(spatial_cluster = psuid) %>% mutate(Location = "Cambodia") %>% dplyr::select(-psuid) %>%
  mutate(floor_deprived = housing_deprived) %>%
  group_by(spatial_cluster) %>%
  summarize(across(nutrition_deprived:assets_deprived, ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(location = "Cambodia") %>%
  left_join(rao_mpi, by = c("location", "spatial_cluster")) %>%
  mutate(lat = round(lat, 5), long = round(long, 5)) %>%
  left_join(place_clim %>% mutate(lat = round(lat, 5), long = round(long, 5)),
            by = c("location", "lat", "long")) %>%
  drop_na()

cam_distances <- readr::read_csv(file = here("data/cambodia", "healthcare_dist_cambodia_with_distances.csv")) %>%
  mutate(spatial_cluster = psuid) %>%
  left_join(predict_rao_cam, by = "spatial_cluster") %>%
  drop_na()

predictors <- cam_distances %>%
  dplyr::select(nutrition_deprived, hygiene_deprived, mat_edu_deprived,
                sanitation_deprived, water_deprived, electricity_deprived,
                cooking_deprived, floor_deprived, housing_deprived,
                assets_deprived, mpi_standard, distance_km, mean_temp_C, annual_precip_mm) %>%
  as.matrix()
head(predictors)
colnames(predictors) = c("Food security", "Hygiene", "Education", "Sanitation", "Drinking water", 
                         "Electricity", "Cooking", "Flooring", "Housing Material", "Assets", "Poverty",
                         "Healthcare proximity", "Temperature", "Precipitation")
head(predictors)






outcome <- log(cam_distances$rao + .001)

k <- 5
folds <- sample(rep(1:k, length.out = nrow(predictors)))
cv_preds <- numeric(nrow(predictors))

for (i in 1:k) {
  train_x <- predictors[folds != i, ]
  train_y <- outcome[folds != i]
  test_x  <- predictors[folds == i, ]
  
  fit <- wbart(x.train = train_x, y.train = train_y, x.test = test_x,
               ndpost = 2000, nskip = 250, printevery = 0)
  
  cv_preds[folds == i] <- fit$yhat.test.mean
}

mae  <- mean(abs(outcome - cv_preds))
rmse <- sqrt(mean((outcome - cv_preds)^2))
ss_res <- sum((outcome - cv_preds)^2)
ss_tot <- sum((outcome - mean(outcome))^2)
r2 <- round(1 - (ss_res / ss_tot), digits = 3)
rank_cor <- cor(outcome, cv_preds, method = "spearman")

cv_df <- tibble(observed = outcome, predicted = cv_preds)

fig_fit_cambo <- ggplot(cv_df, aes(x = exp(observed), y = exp(predicted))) +
  geom_point(alpha = 0.7, color = "gray20", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkmagenta", lwd = 1) +
  theme_bw(base_size = 13) +
  labs(x = "Observed Rao's index", y = "Predicted Rao's index",
       title = "Cambodia", subtitle = paste0("R^2 = ", r2)) +
  theme(plot.title = element_text(size = 14)) +
  ylim(c(0,1)) + xlim(c(0,1))
fig_fit_cambo

fit_full <- wbart(x.train = predictors, y.train = outcome,
                  ndpost = 1000, nskip = 250, printevery = 0)

observed_importance <- colMeans(fit_full$varcount / rowSums(fit_full$varcount))

null_importance_cambo <- permutation_importance_threshold(predictors, outcome, n_perms = 50)
null_95th_cambo <- apply(null_importance_cambo, 2, quantile, 0.95)

var_importance_cambo <- tibble(
  variable             = colnames(predictors),
  observed_importance  = observed_importance,
  null_95th_percentile = null_95th_cambo
) %>%
  mutate(exceeds_null = observed_importance > null_95th_percentile) %>%
  arrange(observed_importance) %>%
  mutate(variable = factor(variable, levels = variable))

fig_varimp_cambo <- plot_var_importance(var_importance_cambo, "Cambodia", rank_cor)
fig_varimp_cambo


# combine 

fig3b_build = plot_grid(fig_varimp_bangl, fig_varimp_kenya, fig_varimp_cambo, ncol = 3) 
fig3b_build 

fig3b <- fig3b_build +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))
fig3b



################################################# 
# try something simpler 
################### no poverty, and no WASH variables for Bangladesh & Kenya
build_tertile_comparison <- function(data, predictor_cols, location_name) {
  
  data_tertile <- data %>%
    mutate(rao_tertile = ntile(rao, 3)) %>%
    mutate(rao_tertile = factor(rao_tertile, levels = 1:3)) %>%
    mutate(across(all_of(predictor_cols), ~ as.numeric(scale(.x))))
  
  variable_effect_sizes <- map_dfr(predictor_cols, function(var) {
    
    formula_str <- paste0(var, " ~ rao_tertile")
    aov_fit <- aov(as.formula(formula_str), data = data_tertile)
    aov_summary <- summary(aov_fit)[[1]]
    
    ss_between <- aov_summary["rao_tertile", "Sum Sq"]
    ss_total   <- sum(aov_summary[["Sum Sq"]])
    eta_sq     <- ss_between / ss_total
    
    # ── NEW: check direction of association ──────────────────────────────────
    # Compare mean of the predictor in tertile 3 (highest Rao) vs tertile 1 (lowest)
    mean_t1 <- mean(data_tertile[[var]][data_tertile$rao_tertile == 1], na.rm = TRUE)
    mean_t3 <- mean(data_tertile[[var]][data_tertile$rao_tertile == 3], na.rm = TRUE)
    direction <- ifelse(mean_t3 > mean_t1, "positive", "negative")
    # ──────────────────────────────────────────────────────────────────────────
    
    tibble(variable = var, eta_squared = eta_sq, direction = direction,
           mean_t1 = mean_t1, mean_t3 = mean_t3)
  })
  
  # Keep only POSITIVELY associated variables (higher Rao -> higher condition),
  # then take the top 4 by effect size among those
  top4_vars <- variable_effect_sizes %>%
    filter(direction == "positive") %>%
    arrange(desc(eta_squared)) %>%
    slice_head(n = 5) %>%
    pull(variable)
  
  list(data = data %>% mutate(rao_tertile = ntile(rao, 3), 
                              rao_tertile = factor(rao_tertile, levels = 1:3)),
       top4 = top4_vars, 
       effect_sizes = variable_effect_sizes)
}

# ── Updated plot function: takes a name_map argument and relabels the x-axis ──

plot_tertile_boxplots <- function(tertile_result, location_name, name_map) {
  
  plot_data <- tertile_result$data %>%
    dplyr::select(rao_tertile, all_of(tertile_result$top4)) %>%
    pivot_longer(cols = -rao_tertile, names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable, !!!name_map)) %>%      # relabel using the map
    mutate(variable = factor(variable, levels = name_map[tertile_result$top4]))  # preserve top4 order
  
  ggplot(plot_data, aes(x = variable, y = value*100, fill = rao_tertile)) +
    geom_boxplot(outlier.size = 1.5, position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("1" = "#2D1160", "2" = "#A63A6E", "3" = "#F4A862"),
                      name = "Rao's quadratic\nindex") +
    theme_bw(base_size = 13) +
    labs(x = "Environmental/household risk factor", y = "Cluster-level condition (%)", title = location_name) +
    theme(plot.title       = element_text(size = 16, hjust = 0.5),
          axis.text.x      = element_text(size = 11),
          axis.title.x     = element_text(size = 15),
          legend.position  = "bottom",
          legend.title     = element_text(size = 11),
          legend.text      = element_text(size = 11),
          panel.grid.minor = element_blank())
}


# 1. Build all three tertile comparison objects

# KENYA — poverty removed, plus sanitation/water/hygiene removed
kenya_predictor_cols <- c("nutrition_deprived", "mat_edu_deprived",
                          "electricity_deprived", "cooking_deprived",
                          "floor_deprived", "housing_deprived",
                          "assets_deprived", "animal_ownership",
                          "mean_temp_C", "annual_precip_mm")
kenya_tertiles <- build_tertile_comparison(
  pred_rao_kenya %>% 
    mutate(annual_precip_mm = (annual_precip_mm - min(annual_precip_mm)) / (max(annual_precip_mm) - min(annual_precip_mm)),
           mean_temp_C = (mean_temp_C - min(mean_temp_C)) / (max(mean_temp_C) - min(mean_temp_C))),
  kenya_predictor_cols, "Kenya")
head(kenya_tertiles)

# BANGLADESH — poverty removed, plus sanitation/water/hygiene removed
bangl_predictor_cols <- c("nutrition_deprived", "mat_edu_deprived",
                          "floor_deprived", "electricity_deprived",
                          "housing_deprived", "assets_deprived",
                          "mean_temp_C", "annual_precip_mm")
bangl_tertiles <- build_tertile_comparison(
  pred_rao_bangl %>% 
    mutate(annual_precip_mm = (annual_precip_mm - min(annual_precip_mm)) / (max(annual_precip_mm) - min(annual_precip_mm)),
           mean_temp_C = (mean_temp_C - min(mean_temp_C)) / (max(mean_temp_C) - min(mean_temp_C))),
  bangl_predictor_cols, "Bangladesh")

# CAMBODIA — keeps sanitation/water/hygiene (only poverty removed)
cam_predictor_cols <- c("nutrition_deprived", "hygiene_deprived", "mat_edu_deprived",
                        "sanitation_deprived", "water_deprived", "electricity_deprived",
                        "cooking_deprived", "floor_deprived", "housing_deprived",
                        "assets_deprived", "distance_km",
                        "mean_temp_C", "annual_precip_mm")
cam_tertiles <- build_tertile_comparison(
  cam_distances %>%
    mutate(distance_km = (distance_km - min(distance_km)) / (max(distance_km) - min(distance_km))),
  cam_predictor_cols, "Cambodia")

# 2. Updated name maps (poverty removed everywhere; WASH removed for Kenya & Bangladesh)

kenya_name_map <- c(
  nutrition_deprived   = "Food security",
  mat_edu_deprived     = "Maternal education",
  electricity_deprived = "Electricity",
  cooking_deprived     = "Cooking",
  floor_deprived       = "Flooring",
  housing_deprived     = "Housing Material",
  assets_deprived      = "Assets",
  animal_ownership     = "Animal Ownership",
  mean_temp_C          = "Temperature",
  annual_precip_mm     = "Precipitation"
)

bangl_name_map <- c(
  nutrition_deprived   = "Food security",
  mat_edu_deprived     = "Maternal education",
  floor_deprived       = "Flooring",
  electricity_deprived = "Electricity",
  housing_deprived     = "Housing Material",
  assets_deprived      = "Assets",
  mean_temp_C          = "Temperature",
  annual_precip_mm     = "Precipitation"
)

cam_name_map <- c(
  nutrition_deprived   = "Food security",
  hygiene_deprived     = "Hygiene",
  mat_edu_deprived     = "Education",
  sanitation_deprived  = "Sanitation",
  water_deprived       = "Drinking water",
  electricity_deprived = "Electricity",
  cooking_deprived     = "Cooking",
  floor_deprived       = "Flooring",
  housing_deprived     = "Housing Material",
  assets_deprived      = "Assets",
  distance_km          = "Healthcare proximity",
  mean_temp_C          = "Temperature",
  annual_precip_mm     = "Precipitation"
)

# 3. Verify the top4 sets all have matching name-map entries
setdiff(kenya_tertiles$top4, names(kenya_name_map))   # should be character(0)
setdiff(bangl_tertiles$top4, names(bangl_name_map))   # should be character(0)
setdiff(cam_tertiles$top4,   names(cam_name_map))     # should be character(0)

# 4. Plot
fig_tertile_kenya <- plot_tertile_boxplots(kenya_tertiles, "Kenya", kenya_name_map)
fig_tertile_bangl <- plot_tertile_boxplots(bangl_tertiles, "Bangladesh", bangl_name_map)
fig_tertile_cambo <- plot_tertile_boxplots(cam_tertiles, "Cambodia", cam_name_map)

fig_tertile_all <- fig_tertile_bangl + fig_tertile_kenya + fig_tertile_cambo +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
fig_tertile_all


################################### try to create smooth multipathogen surfaces: 

library(spaMM)
library(sf)
library(raster)
library(viridis)

# ── Helper function: fit Matern smooth surface and mask to admin units with data ──

fit_smooth_rao_surface <- function(rao_data, admin_sf, admin_id_col, 
                                   grid_n = 100, buffer_km = 10, utm_zone) {
  
  # Prepare data: needs lat, lon, rao
  dat <- rao_data %>%
    rename(lat = lat, lon = long) %>%
    dplyr::select(lat, lon, rao) %>%
    drop_na()
  
  # Identify which admin units actually contain at least one observed point
  pts_sf <- dat %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  admin_with_data <- st_join(admin_sf, pts_sf, join = st_intersects) %>%
    filter(!is.na(rao)) %>%
    pull(!!sym(admin_id_col)) %>%
    unique()
  
  admin_sf_masked <- admin_sf %>%
    filter(!!sym(admin_id_col) %in% admin_with_data)
  
  # Build prediction grid over only the admin units containing data
  box_grid <- st_make_grid(admin_sf_masked, n = c(grid_n, grid_n), what = "centers")
  
  # Transform points to UTM for the buffer/distance step
  pts_utm <- pts_sf %>%
    dplyr::select(geometry) %>%
    st_transform(utm_zone)
  
  cl_buff <- st_buffer(pts_utm, dist = buffer_km) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 4326)
  
  # Fit Matern spatial model
  fit_rao <- spaMM::fitme(rao ~ lat + lon + Matern(1 | lat + lon),
                          data = dat,
                          family = gaussian(link = "identity"))
  
  # Predict over grid
  preds_rao <- get_grid_preds(input_grid = box_grid, spamm_model_fit = fit_rao)
  preds_coords <- st_coordinates(preds_rao)
  
  
  preds_raster <- points_to_raster(
    x = preds_coords[, 1],
   y = preds_coords[, 2],
    z = preds_rao$pred,
    mask1 = admin_sf_masked,
    mask2 = cl_buff,
    crop1 = admin_sf_masked
  )
  
  preds_tibble <- raster_to_tibble(preds_raster)
  
  list(surface = preds_tibble, admin_masked = admin_sf_masked, fit = fit_rao)
}

# ── KENYA ──────────────────────────────────────────────────────────────────────
kenya_smooth <- fit_smooth_rao_surface(
  rao_data    = rao_by_cluster %>% filter(location == "Kenya"),
  admin_sf    = ken_admin3,
  admin_id_col = "NAME_3",   # adjust to your actual admin3 ID column name
  utm_zone    = "+proj=utm +zone=37 +datum=WGS84 +units=km"  # Kenya UTM zone
)

head(ken_admin3)

kenya_map_rao_smooth <- ggplot() +
  geom_sf(data = ken_admin3,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.55),
          lwd   = 0.35) +
  geom_tile(data = kenya_smooth$surface %>% filter(!is.na(value)),
            aes(x = x, y = y, fill = value), na.rm = TRUE, , alpha = .9) +
  geom_sf(data = kenya_smooth$admin_masked,
          fill = NA, color = alpha("black", 0.55), lwd = 0.35, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  coord_sf(crs = 4326) +
  xlim(c(xmin_k, xmax_k)) + ylim(c(ymin_k, ymax_k)) +
  ggtitle("Kenya") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20),
    legend.position = "none",
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank()
  )
kenya_map_rao_smooth


# ── BANGLADESH ─────────────────────────────────────────────────────────────────


library(igraph)

fit_smooth_rao_surface <- function(rao_data, admin_sf, admin_id_col, 
                                   grid_n = 100, buffer_km = 25, utm_zone) {
  
  dat <- rao_data %>%
    rename(lat = lat, lon = long) %>%
    dplyr::select(lat, lon, rao) %>%
    drop_na()
  
  pts_sf <- dat %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  admin_with_data <- st_join(admin_sf, pts_sf, join = st_intersects) %>%
    filter(!is.na(rao)) %>%
    pull(!!sym(admin_id_col)) %>%
    unique()
  
  admin_sf_masked <- admin_sf %>%
    filter(!!sym(admin_id_col) %in% admin_with_data)
  
  # ── NEW: keep only the largest contiguous connected component ───────────────
  admin_adj <- st_intersects(admin_sf_masked, admin_sf_masked)
  adj_matrix <- as.matrix(as(admin_adj, "matrix"))
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
  components <- components(g)
  largest_component_id <- which.max(table(components$membership))
  admin_sf_masked <- admin_sf_masked[components$membership == largest_component_id, ]
  # ──────────────────────────────────────────────────────────────────────────────
  
  box_grid <- st_make_grid(admin_sf_masked, n = c(grid_n, grid_n), what = "centers")
  
  pts_utm <- pts_sf %>%
    dplyr::select(geometry) %>%
    st_transform(utm_zone)
  
  cl_buff <- st_buffer(pts_utm, dist = buffer_km) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = 4326)
  
  fit_rao <- spaMM::fitme(rao ~ lat + lon + Matern(1 | lat + lon),
                          data = dat, family = gaussian(link = "identity"))
  
  preds_rao <- get_grid_preds(input_grid = box_grid, spamm_model_fit = fit_rao)
  preds_coords <- st_coordinates(preds_rao)
  
  preds_raster <- points_to_raster(
    x = preds_coords[, 1], y = preds_coords[, 2], z = preds_rao$pred,
    mask1 = admin_sf_masked, mask2 = admin_sf_masked, crop1 = admin_sf_masked
  )
  
  preds_tibble <- raster_to_tibble(preds_raster)
  
  list(surface = preds_tibble, admin_masked = admin_sf_masked, fit = fit_rao)
}



bangl_smooth <- fit_smooth_rao_surface(
  rao_data    = rao_by_cluster %>% filter(location == "Bangladesh"),
  admin_sf    = bgd_admin3,
  admin_id_col = "ADM3_EN",   # matches your earlier HDX column naming
  utm_zone    = "+proj=utm +zone=46 +datum=WGS84 +units=km")

bangl_map_rao_smooth <- ggplot() +
 geom_sf(data = bgd_admin3,
         fill  = alpha("seagreen", 0.06),
         color = alpha("black", 0.55),
        lwd   = 0.35) +
  geom_tile(data = bangl_smooth$surface %>% filter(!is.na(value)),
            aes(x = x, y = y, fill = value), na.rm = TRUE, alpha = .90) +
  geom_sf(data = bangl_smooth$admin_masked,
          fill = NA, color = alpha("black", 0.55), lwd = 0.35, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  coord_sf(crs = 4326) +
  xlim(c(89.9, 90.8)) + ylim(c(23.9, 25.0)) +
  ggtitle("Bangladesh") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 18),
    legend.title  = element_text(size = 20),
    legend.position = "none",
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank())
bangl_map_rao_smooth


# ── CAMBODIA ───────────────────────────────────────────────────────────────────
cam_smooth <- fit_smooth_rao_surface(
  rao_data    = rao_by_cluster %>% filter(location == "Cambodia"),
  admin_sf    = khm_admin2_sf,
  admin_id_col = "NAME_2",   # adjust to your actual GADM admin2 column
  utm_zone    = "+proj=utm +zone=48 +datum=WGS84 +units=km"
)


head(khm_admin2_sf)

cam_map_rao_smooth <- ggplot() +
  geom_sf(data = khm_admin2_sf,
          fill  = alpha("seagreen", 0.06),
          color = alpha("black", 0.55),
          lwd   = 0.35) +
  geom_tile(data = cam_smooth$surface %>% filter(!is.na(value)),
            aes(x = x, y = y, fill = value), na.rm = TRUE, alpha = .9) +
  geom_sf(data = cam_smooth$admin_masked,
          fill = NA, color = alpha("black", 0.55), lwd = 0.35, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "magma", name = "Rao's quadratic\nindex") +
  coord_sf(crs = 4326) +
  ggtitle("Cambodia") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 20),
    plot.tag      = element_text(face = "bold", size = 20),
    legend.text   = element_text(size = 14),
    legend.title  = element_text(size = 14),
    legend.position = "right",
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank()
  )
cam_map_rao_smooth

# ── Combine, same as before ────────────────────────────────────────────────────
two <- plot_grid(bangl_map_rao_smooth, kenya_map_rao_smooth, nrow = 1)
two


maps_rao_smooth <- plot_grid(two, cam_map_rao_smooth)

fig2a_shifted <- ggdraw() +
  draw_plot(maps_rao_smooth, x = 0.05, y = 0, width = 0.95, height = 1)
fig2a_shifted

fig2a <- fig2a_shifted +
  labs(tag = "A") +
  theme(plot.tag          = element_text(size = 20, face = "bold"),
        plot.tag.position = c(0, 0.95))  # default top is (0, 1); lower y shifts down
fig2a

# 1100 by 1000
plot_grid(fig2a, fig2b, ncol= 1, rel_heights = c(.5, .7))
