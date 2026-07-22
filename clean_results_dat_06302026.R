##################################################################
# Clean HOTSPOT data 
# June 30, 2026 
##################################################################

# Set working directory 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/final_data")

library(dplyr)
library(stringr)
library(officer)

##################################################################

# Pull in cleaned data from hotspot 1 
# Raw data 
touba_raw = read.csv("Touba/clean-data-touba.csv") %>% 
  dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
head(touba_raw)
prikro_raw = read.csv("Prikro/clean-data-prikro.csv") %>% 
  dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
head(prikro_raw)
agbo_raw = read.csv("clean-data-agbo.csv")%>% 
  dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
head(agbo_raw)
dat_111315_raw = read.csv("clean-data-111315.csv") %>% 
  dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
head(dat_111315_raw)

# Join together and remove duplicates 
hotspot1_raw = rbind(touba_raw , prikro_raw, agbo_raw, dat_111315_raw) %>%
  mutate(id = participant_id_format) %>%
  distinct()
head(hotspot1_raw)

print(unique(prikro_raw$a_mansoni_1))
print(unique(prikro_raw$a_lumbri_1))
##################################################################
researcher_name = "Sam Bents"
date = "2026-07-2"
cat("HOTSPOT Results: Comparison of data entry 1 and 2")
cat("Name:", researcher_name)
cat("Date:", date)
##################################################################

# Step 1: Clean the participant level data for data entry 2 and remove duplicates 

hotspot2_raw = read.csv("clean-data-H2.csv") %>%
  dplyr::select(-X) %>%
  dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) %>%
  mutate(id = participant_id_format) %>%
  distinct()
head(hotspot2_raw)

##################################################################

# Step 2: Check hotspot 1 and hotspot 2 data have same data coverage 
# IDs in dataset 2 but NOT in dataset 1
missing_hotspot1 <- setdiff(hotspot2_raw$id, hotspot1_raw$id)
length_missing_h1 = length(missing_hotspot1)
cat("Number of IDs missing from entry 1 but present in entry 2:", length_missing_h1)
cat("IDs missing from entry 1 but present in entry 2:", missing_hotspot1)


# IDs in dataset 1 but NOT in dataset 2
missing_hotspot2 <- setdiff(hotspot1_raw$id, hotspot2_raw$id)
length_missing_h2 = length(missing_hotspot2)
cat("Number of IDs missing from entry 2 but present in entry 1:", length_missing_h2)
cat("IDs missing from entry 2 but present in entry 1:", missing_hotspot2)


##################################################################

# Step 3: Look to see if discordant patterns in the results 
# Columns to verify results: sample_stool1:poc_auto_urine
# Flag if any of these columns are discordant, do not report the issue just 
# say that there is a discrepancy. Not interested in other issues? 
# Both missing, # missing for one, discrepant values 

compare_results = inner_join(hotspot1_raw, hotspot2_raw , by = "id")
cat("Number of IDs consistent between entry 1 and 2:", nrow(compare_results))

# Identify the base column names that have both a .x and .y version
x_cols    <- str_subset(colnames(compare_results), "\\.x$")
base_names <- str_remove(x_cols, "\\.x$")
# Keep only bases that also have a .y counterpart
base_names <- base_names[paste0(base_names, ".y") %in% colnames(compare_results)]

# Loop over records and column pairs, collecting mismatches
for (i in seq_len(nrow(compare_results))) {
  
  rec_id <- compare_results$id[i]
  
  for (base in base_names) {
    x_val <- as.character(compare_results[[paste0(base, ".x")]][i])
    y_val <- as.character(compare_results[[paste0(base, ".y")]][i])
    
    x_na <- is.na(x_val)
    y_na <- is.na(y_val)
    
    if (x_na & y_na) {
      msg <- "Both result records missing"
    } else if (x_na | y_na) {
      msg <- "Single result record missing"
    } else if (x_val != y_val) {
      msg <- "Discordant results recorded"
    } else {
      next   # values match and both present -> no issue, skip
    }
    
    cat("ID:", rec_id, "| Column:", base, "|", msg, "\n")
  }
}









##################################################################
##################################################################
# Clean HOTSPOT data 
# June 30, 2026 
##################################################################

# Set working directory 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/final_data")

library(dplyr)
library(stringr)
library(officer)

# Initialize the Word document
doc <- read_docx()

##################################################################

# Pull in cleaned data from hotspot 1 
touba_raw = read.csv("Touba/clean-data-touba.csv") %>% 
 # dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
  dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine) 
prikro_raw = read.csv("Prikro/clean-data-prikro.csv")  %>% 
  # dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
  dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine) 
agbo_raw = read.csv("clean-data-agbo.csv")  %>% 
  # dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
  dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine) 
dat_111315_raw = read.csv("clean-data-111315.csv")  %>% 
  # dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) 
  dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine) 
# Join together and remove duplicates 
hotspot1_raw = rbind(touba_raw , prikro_raw, agbo_raw, dat_111315_raw) %>%
  mutate(id = participant_id_format) %>%
  distinct() %>%
  dplyr::select(-participant_id_format) %>%
  mutate(n_missing = rowSums(is.na(dplyr::select(., -id)))) %>%  # count NAs per row, excluding id
  group_by(id) %>%
  arrange(n_missing, .by_group = TRUE) %>%   # fewest missing first, within each id
  slice_head(n = 1) %>%                       # keep the most-complete row per id
  ungroup() %>%
  dplyr::select(-n_missing) 
colnames(hotspot1_raw)

##################################################################
researcher_name = "Sam Bents"
date = "2026-07-07"
#doc <- doc %>%
#  body_add_par("HOTSPOT Results: Comparison of data entry 1 and 2") %>%
#  body_add_par(paste("Name:", researcher_name)) %>%
#  body_add_par(paste("Date:", date)) %>%
#  body_add_par("")


doc <- doc %>%
  body_add_fpar(
    fpar(
      ftext(
        "HOTSPOT Results: Comparison of data entry 1 and 2",
        prop = fp_text(bold = TRUE)
      )
    )) %>%
  body_add_par(paste("Name:", researcher_name)) %>%
  body_add_par(paste("Date:", date)) %>%
  body_add_par("")

##################################################################

# Step 1: Clean the participant level data for data entry 2 and remove duplicates 
hotspot2_raw = read.csv("clean-data-H2.csv") %>%
  dplyr::select(-X) %>%
  dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine) %>%
  #dplyr::select(participant_id_format, sample_stool1:poc_auto_urine) %>%
  mutate(id = participant_id_format) %>%
  distinct() %>%
  dplyr::select(-participant_id_format) %>%
  mutate(n_missing = rowSums(is.na(dplyr::select(., -id)))) %>%  # count NAs per row, excluding id
  group_by(id) %>%
  arrange(n_missing, .by_group = TRUE) %>%   # fewest missing first, within each id
  slice_head(n = 1) %>%                       # keep the most-complete row per id
  ungroup() %>%
  dplyr::select(-n_missing) 

##################################################################

# Step 2: Check hotspot 1 and hotspot 2 data have same data coverage 
# IDs in dataset 2 but NOT in dataset 1
missing_hotspot1 <- setdiff(hotspot2_raw$id, hotspot1_raw$id)
length_missing_h1 = length(missing_hotspot1)
doc <- doc %>%
  body_add_par(paste("Number of IDs missing from entry 1 but present in entry 2:", length_missing_h1)) %>%
  body_add_par(paste("IDs missing from entry 1 but present in entry 2:", paste(missing_hotspot1, collapse = ", "))) %>%
  body_add_par("")

# IDs in dataset 1 but NOT in dataset 2
missing_hotspot2 <- setdiff(hotspot1_raw$id, hotspot2_raw$id)
length_missing_h2 = length(missing_hotspot2)
doc <- doc %>%
  body_add_par(paste("Number of IDs missing from entry 2 but present in entry 1:", length_missing_h2)) %>%
  body_add_par(paste("IDs missing from entry 2 but present in entry 1:", paste(missing_hotspot2, collapse = ", "))) %>%
  body_add_par("")

##################################################################

# Step 3: Look to see if discordant patterns in the results 
compare_results = inner_join(hotspot1_raw, hotspot2_raw , by = "id") %>%
  dplyr::select(-poc_auto_urine.x, -poc_auto_urine.y)

doc <- doc %>%
  body_add_par(paste("Number of IDs consistent between entry 1 and 2:", nrow(compare_results))) %>%
  body_add_par("")


x_cols     <- str_subset(colnames(compare_results), "\\.x$")
base_names <- str_remove(x_cols, "\\.x$")
base_names <- base_names[paste0(base_names, ".y") %in% colnames(compare_results)]

# Preallocate a list, fill it in the loop (fast, in-memory only)
mismatch_list <- vector("list", nrow(compare_results) * length(base_names))
counter <- 0

for (i in seq_len(nrow(compare_results))) {
  rec_id <- compare_results$id[i]
  for (base in base_names) {
    x_val <- as.character(compare_results[[paste0(base, ".x")]][i])
    y_val <- as.character(compare_results[[paste0(base, ".y")]][i])
    x_na <- is.na(x_val); y_na <- is.na(y_val)
    
    if (x_na & y_na) {
      next   # both missing -> entries agree, not an error, skip
    } else if (xor(x_na, y_na)) {
      msg <- "Single result record missing, please clarify correct value."   # exactly one missing
    } else if (!identical(x_val, y_val)) {
      msg <- "Discordant results recorded, please clarify correct value."     # both present, different values
    } else {
      next   # both present and equal -> match, skip
    }
    
    if (!is.na(msg)) {
      counter <- counter + 1
      mismatch_list[[counter]] <- paste0("ID: ", rec_id, " | Column: ", base, " | ", msg)
    }
  }
}

mismatch_lines <- unlist(mismatch_list[seq_len(counter)])

# ── Write all mismatch lines to the doc in ONE paragraph with line breaks ─────
if (length(mismatch_lines) > 0) {
  runs <- vector("list", 2 * length(mismatch_lines) - 1)
  runs[[1]] <- ftext(mismatch_lines[1])
  if (length(mismatch_lines) > 1) {
    for (k in 2:length(mismatch_lines)) {
      runs[[2 * k - 2]] <- run_linebreak()
      runs[[2 * k - 1]] <- ftext(mismatch_lines[k])
    }
  }
  doc <- body_add_fpar(doc, fpar(values = runs))
}

# ── Save ──────────────────────────────────────────────────────────────────────
print(doc, target = "HOTSPOT_comparison_results_V3.docx")










### Explicit tracking of issues 
#########################################
################ try counting 

compare_results <- inner_join(hotspot1_raw, hotspot2_raw, by = "id")

cat("Number of IDs consistent between entry 1 and 2:", nrow(compare_results), "\n")

# Identify paired columns
x_cols <- str_subset(colnames(compare_results), "\\.x$")
print(x_cols)
base_names <- str_remove(x_cols, "\\.x$")
base_names <- base_names[paste0(base_names, ".y") %in% colnames(compare_results)]

# Store errors here
error_list <- list()
k <- 1

for (i in seq_len(nrow(compare_results))) {
  
  rec_id <- compare_results$id[i]
  
  for (base in base_names) {
    
    x_val <- as.character(compare_results[[paste0(base, ".x")]][i])
    y_val <- as.character(compare_results[[paste0(base, ".y")]][i])
    x_val <- as.character(x_val)
    y_val <- as.character(y_val)
    
    x_na <- is.na(x_val)
    y_na <- is.na(y_val)
    
    if (x_na & y_na) {
      next   # both missing -> entries agree, not an error, skip
    } else if (xor(x_na, y_na)) {
      msg <- "Single result record missing, please clarify correct value."   # exactly one missing
    } else if (!identical(x_val, y_val)) {
      msg <- "Discordant results recorded, please clarify correct value."     # both present, but different values
    } else {
      next   # both present and equal -> match, skip
    }
    
    error_list[[k]] <- data.frame(
      id = rec_id,
      variable = base,
      value_entry1 = as.character(x_val),
      value_entry2 = as.character(y_val),
      error_type = msg,
      stringsAsFactors = FALSE )
    
    k <- k + 1
  }
}

# Combine into one data frame
error_df <- dplyr::bind_rows(error_list) %>%
  mutate(village = substr(id, 1, 2))
head(error_df)
errors_loc_var = data.frame(table(error_df$village, error_df$variable))

# remove auto_urine 
remove_au = error_df %>%
  filter(variable != "poc_auto_urine")
errors_loc_var = data.frame(table(remove_au$village, remove_au$variable))
# 4000 errors

## 
auto_urine =  error_df %>%
  filter(variable == "poc_auto_urine") 


# Total number of errors
nrow(error_df)

# Number of each error type
table(remove_au$error_type)

# Number by village 
table(error_df$village)

# Number of errors by variable
sort(table(error_df$variable), decreasing = TRUE)

# Number of errors by ID
sort(table(error_df$id), decreasing = TRUE)









#################################################################
#################################################################
#################################################################
#################################################################
# Tues July 7 2026, changes made after discussion with Nathan

##################################################################
# Clean HOTSPOT data — per-village QC reports
# June 30, 2026
##################################################################

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/final_data")

library(dplyr)
library(stringr)
library(officer)

##################################################################
# Pull in cleaned data from hotspot 1
touba_raw = read.csv("Touba/clean-data-touba.csv") %>%
  #dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine)
  dplyr::select(participant_id_format, sample_stool1:poc_gscore_urine)
prikro_raw = read.csv("Prikro/clean-data-prikro.csv") %>%
  dplyr::select(participant_id_format, sample_stool1:poc_gscore_urine)
 # dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine)
agbo_raw = read.csv("clean-data-agbo.csv") %>%
  #dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine)
  dplyr::select(participant_id_format, sample_stool1:poc_gscore_urine)
dat_111315_raw = read.csv("clean-data-111315.csv") %>%
 # dplyr::select(participant_id_format, sample_stool1:b_other_1, sample_urine_micro:poc_auto_urine)
  dplyr::select(participant_id_format, sample_stool1:poc_gscore_urine)

hotspot1_raw = rbind(touba_raw, prikro_raw, agbo_raw, dat_111315_raw) %>%
  mutate(id = participant_id_format) %>%
  distinct() %>%
  dplyr::select(-participant_id_format) %>%
  mutate(n_missing = rowSums(is.na(dplyr::select(., -id)))) %>%
  group_by(id) %>%
  arrange(n_missing, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(-n_missing)

##################################################################
# Hotspot 2
hotspot2_raw = read.csv("clean-data-H2.csv") %>%
  dplyr::select(-X) %>%
  dplyr::select(participant_id_format, sample_stool1:poc_gscore_urine) %>%
  mutate(id = participant_id_format) %>%
  distinct() %>%
  dplyr::select(-participant_id_format) %>%
  mutate(n_missing = rowSums(is.na(dplyr::select(., -id)))) %>%
  group_by(id) %>%
  arrange(n_missing, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(-n_missing)

##################################################################
title = print(names_results)
title = c( "Les selles ont-elles été collectées (Echantillon 1) ?",
  "SELLES_LAME 1A : œufs de Schistosoma mansoni",
  "SELLES_LAME 1A : œufs de Ankylostome",
  "SELLES_LAME 1A : œufs de Ascaris lumbricoides",
  "SELLES_LAME 1A : œufs de Trichuris trichiura",
  "SELLES_LAME 1A (Optional) : Autre Espèce",
  "SELLES_LAME 1B : œufs de Schistosoma mansoni",
  "SELLES_LAME 1B : œufs de Ankylostome",
  "SELLES_LAME 1B :  œufs de Ascaris lumbricoides",
  "SELLES_LAME 1B : œufs de Trichuris trichiura",
  "SELLES_LAME 1B (Optional) : Autre Espèce",
  "Les selles ont-elles été collectées (Echantillon 2) ?",
  "SELLES_LAME 2A : œufs de Schistosoma mansoni",
  "SELLES_LAME 2A : œufs de Ankylostome",
  "SELLES_LAME 2A : œufs de Ascaris lumbricoides",
  "SELLES_LAME 2A : œufs de Trichuris trichiura",
  "SELLES_LAME 2A (Optional) : Autre Espèce",
  "SELLES_LAME 2B : œufs de Schistosoma mansoni",
  "SELLES_LAME 2B : œufs de Ankylostome",
  "SELLES_LAME 2B :  œufs de Ascaris lumbricoides",
  "SELLES_LAME 2B : œufs de Trichuris trichiura",
  "SELLES_LAME 2B (Optional) : Autre Espèce",
  "Les résultats de la microscopie urinaire ont-ils été collectés ?",
  "URINE : Nombre d'œufs de S. haematobium pour 10 mL d'urine",
  "Les résultats de POC-CCA ont-ils été collectés ?",
  "URINE :  Résultats POC-CCA",
  "URINE :  Score-G POC-CCA")
#result_name = colnames(hotspot1_raw[1:27])
#dat_title = data.frame(title, result_name)

# Add a village identifier = first two characters of the id
hotspot1_raw <- hotspot1_raw %>% mutate(village = substr(as.character(id), 1, 2)) 
colnames(hotspot1_raw) = c(title, "id", "village")

hotspot2_raw <- hotspot2_raw %>% mutate(village = substr(as.character(id), 1, 2))
colnames(hotspot2_raw) = c(title, "id", "village")

# All villages present across either dataset
all_villages <- sort(unique(c(hotspot1_raw$village, hotspot2_raw$village)))

researcher_name = "Sam Bents"
date = "2026-07-2"

# ── Before the loop: create the output folder if it doesn't exist ────────────
out_dir <- "HOTSPOT-results-comparison"
if (!dir.exists(out_dir)) dir.create(out_dir)

##################################################################
# Loop over villages, build and save one Word doc per village
##################################################################
for (vil in all_villages) {
  
  # Subset each dataset to this village
  h1_vil <- hotspot1_raw %>% filter(village == vil) %>% dplyr::select(-village)
  h2_vil <- hotspot2_raw %>% filter(village == vil) %>% dplyr::select(-village)
  
  # Initialize this village's document
  doc <- read_docx() %>%
    body_add_fpar(
      fpar(ftext("HOTSPOT Results: Comparison of HOTSPOT Data Entry 1 and 2",
                 prop = fp_text(bold = TRUE)))) %>%
    body_add_par(paste("Name:", researcher_name)) %>%
    body_add_par(paste("Date:", date)) %>%
    body_add_par(paste("Village ID:", vil)) %>%
    body_add_par("")
  
  # ── PART 1: Participant IDs ──────────────────────────────────────────────
  doc <- doc %>%
    body_add_fpar(fpar(ftext("Part 1: Participant IDs", prop = fp_text(bold = TRUE)))) %>%
    body_add_par("")
  
  #doc <- doc %>%
  #  body_add_par(paste("Number of IDs consistent between entry 1 and 2:", nrow(compare_results))) %>%
  #  body_add_par("")
  
  # IDs in entry 2 but NOT in entry 1 (for this village)
  missing_hotspot1 <- setdiff(h2_vil$id, h1_vil$id)
  # IDs in entry 1 but NOT in entry 2 (for this village)
  missing_hotspot2 <- setdiff(h1_vil$id, h2_vil$id)
  
 # id_lines <- c(
  #  paste0("ID: ", missing_hotspot1,
   #        " is missing from data entry 1 but present in entry 2, please clarify."),
  #  paste0("ID: ", missing_hotspot2,
  #         " is missing from data entry 2 but present in entry 1, please clarify.")
  #)
  
  # Build ID lines only for directions that actually have missing IDs
  id_lines <- character(0)
  
  if (length(missing_hotspot1) > 0) {
    id_lines <- c(id_lines,
                  paste0("ID: ", missing_hotspot1,
                         " is missing from data entry 1 but present in entry 2, please clarify."))
  }
  
  if (length(missing_hotspot2) > 0) {
    id_lines <- c(id_lines,
                  paste0("ID: ", missing_hotspot2,
                         " is missing from data entry 2 but present in entry 1, please clarify."))
  }
  
  
  if (length(id_lines) > 0) {
    runs <- vector("list", 2 * length(id_lines) - 1)
    runs[[1]] <- ftext(id_lines[1])
    if (length(id_lines) > 1) {
      for (k in 2:length(id_lines)) {
        runs[[2 * k - 2]] <- run_linebreak()
        runs[[2 * k - 1]] <- ftext(id_lines[k])
      }
    }
    doc <- body_add_fpar(doc, fpar(values = runs))
  }
  doc <- body_add_par(doc, "")
  
  # ── PART 2: Results ──────────────────────────────────────────────────────
  doc <- doc %>%
    body_add_fpar(fpar(ftext("Part 2: Results", prop = fp_text(bold = TRUE)))) %>%
    body_add_par("")
  
  compare_results <- inner_join(h1_vil, h2_vil, by = "id") 
 # doc <- doc %>%
  #  body_add_par(paste("Number of IDs consistent between entry 1 and 2:", nrow(compare_results))) %>%
  #  body_add_par("")
  
  x_cols     <- str_subset(colnames(compare_results), "\\.x$")
  base_names <- str_remove(x_cols, "\\.x$")
  base_names <- base_names[paste0(base_names, ".y") %in% colnames(compare_results)]
  
  # Discordance check
  mismatch_list <- vector("list", max(nrow(compare_results) * length(base_names), 1))
  counter <- 0
  
  if (nrow(compare_results) > 0) {
    for (i in seq_len(nrow(compare_results))) {
      rec_id <- compare_results$id[i]
      for (base in base_names) {
        x_val <- as.character(compare_results[[paste0(base, ".x")]][i])
        y_val <- as.character(compare_results[[paste0(base, ".y")]][i])
        x_na <- is.na(x_val); y_na <- is.na(y_val)
        
        if (x_na & y_na) {
          next
        } else if (xor(x_na, y_na)) {
          msg <- "Single result record missing, please clarify correct value."
        } else if (!identical(x_val, y_val)) {
          msg <- "Discordant results recorded, please clarify correct value."
        } else {
          next
        }
        
        counter <- counter + 1
        mismatch_list[[counter]] <- paste0("ID: ", rec_id, " | Result: ", base, " | ", msg)
      }
    }
  }
  
  mismatch_lines <- if (counter > 0) unlist(mismatch_list[seq_len(counter)]) else character(0)
  
  if (length(mismatch_lines) > 0) {
    runs <- vector("list", 2 * length(mismatch_lines) - 1)
    runs[[1]] <- ftext(mismatch_lines[1])
    if (length(mismatch_lines) > 1) {
      for (k in 2:length(mismatch_lines)) {
        runs[[2 * k - 2]] <- run_linebreak()
        runs[[2 * k - 1]] <- ftext(mismatch_lines[k])
      }
    }
    doc <- body_add_fpar(doc, fpar(values = runs))
  }
  
  # ── Save with village ID in filename ─────────────────────────────────────
  out_name <- file.path(out_dir, paste0("HOTSPOT_Results_Comparison_Village_", vil, ".docx"))
  print(doc, target = out_name)
  cat("Saved:", out_name, "\n")
}








