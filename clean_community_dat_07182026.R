##################################################################
# Clean HOTSPOT data 
# July 18, 2026
##################################################################

# Set working directory 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/unprocessed_data")

library(dplyr)
library(stringr)
library(officer)


h1_comsurvey = read.csv("HOTSPOTCommunitySurv_DATA_2026-07-17_1354.csv") 
head(h1_comsurvey)
h2_comsurvey = read.csv("HOTSPOT2CommunitySur_DATA_2026-07-17_1356.csv")

h1_dat = h1_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h1_dat)

h2_dat = h2_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2)
head(h2_dat)

colnames(h1_dat)
colnames(h2_dat)


### Run the data 

library(dplyr)
library(officer)

# ── Initialize the Word document ──────────────────────────────────────────────
doc <- read_docx()

# ── PART 1: Verify Community Survey Village IDs ──────────────────────────────
doc <- doc %>%
  body_add_par("Verify Community Survey Villages IDs", style = "heading 1") %>%
  body_add_par("")

# Village IDs in one dataset but not the other
missing_from_h2 <- setdiff(h1_dat$vil_code, h2_dat$vil_code)  # in H1, not H2
missing_from_h1 <- setdiff(h2_dat$vil_code, h1_dat$vil_code)  # in H2, not H1

id_error_lines <- character(0)
if (length(missing_from_h2) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h2,
                             " is present in dataset 1 but missing from dataset 2, please verify."))
}
if (length(missing_from_h1) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h1,
                             " is present in dataset 2 but missing from dataset 1, please verify."))
}

if (length(id_error_lines) > 0) {
  for (line in id_error_lines) doc <- body_add_par(doc, line)
}
doc <- body_add_par(doc, "")

# ── Join the two datasets by the shared village variable ─────────────────────
compare_dat <- left_join(h1_dat, h2_dat, by = "vil_code") %>%
  dplyr::select(-district.x, -district.y, -subdist.x, -subdist.y) %>%
  distinct()

# ── PART 2: Verify results village by village (shared villages only) ─────────
# Base column names shared by both datasets, excluding the join key
shared_villages <- intersect(h1_dat$vil_code, h2_dat$vil_code)

# All value columns to check = every column except the join key vil_code
value_cols <- setdiff(colnames(h1_dat), "vil_code")

for (v in shared_villages) {
  
  doc <- doc %>%
    body_add_par(paste("Verify results for Village", v), style = "heading 2")
  
  # The single joined row for this village
  row_i <- compare_dat %>% filter(vil_code == v)
  
  mismatch_lines <- character(0)
  
  for (col in value_cols) {
    x_val <- row_i[[paste0(col, ".x")]]   # value from h1_dat
    y_val <- row_i[[paste0(col, ".y")]]   # value from h2_dat
    
    # Treat NA == NA as a match; otherwise compare directly
    both_na <- is.na(x_val) & is.na(y_val)
    match   <- both_na | (!is.na(x_val) & !is.na(y_val) & x_val == y_val)
    
    if (!isTRUE(match)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
    }
  }
  
  if (length(mismatch_lines) > 0) {
    for (line in mismatch_lines) doc <- body_add_par(doc, line)
  }
  doc <- body_add_par(doc, "")
}

# ── Save ──────────────────────────────────────────────────────────────────────
print(doc, target = "Community_Survey_Verification.docx")


#################################################################################
#################################################################################
#################################################################################

##################################################################
# Clean HOTSPOT data
# July 18, 2026
##################################################################
# Set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/unprocessed_data")
library(dplyr)
library(stringr)
library(officer)

h1_comsurvey = read.csv("HOTSPOTCommunitySurv_DATA_2026-07-17_1354.csv")
head(h1_comsurvey)
h2_comsurvey = read.csv("HOTSPOT2CommunitySur_DATA_2026-07-17_1356.csv")

h1_dat = h1_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h1_dat)

h2_dat = h2_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h2_dat)

colnames(h1_dat)
colnames(h2_dat)

### Run the data
library(dplyr)
library(officer)

# ── CHANGE 1: keep one row per vil_code — the MOST COMPLETE row ───────────────
# For each dataset, count non-missing values per row, then within each vil_code
# keep the row with the fewest missing values (the most complete).
keep_most_complete <- function(df) {
  df %>%
    mutate(.n_missing = rowSums(is.na(dplyr::select(., -vil_code)) |
                                  dplyr::select(., -vil_code) == "")) %>%  # NA or "" both count as missing
    group_by(vil_code) %>%
    arrange(.n_missing, .by_group = TRUE) %>%   # fewest missing first
    slice_head(n = 1) %>%                        # keep the most complete row
    ungroup() %>%
    dplyr::select(-.n_missing)
}

h1_dat <- keep_most_complete(h1_dat)
h2_dat <- keep_most_complete(h2_dat)

# ── CHANGE 2: normalize result columns — strip spaces, underscores, hyphens ──
# Identify the result (value) columns = everything except the ID/metadata columns
value_cols <- setdiff(colnames(h1_dat), c("vil_code", "district", "subdist"))

# Function that removes spaces, underscores, and hyphens, leaving just the
# character string of numbers/characters for comparison.
normalize_results <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols),
                  ~ str_remove_all(as.character(.x), "[ _-]")))  # remove space, underscore, hyphen
}

h1_dat <- normalize_results(h1_dat, value_cols)
h2_dat <- normalize_results(h2_dat, value_cols)

# ── Initialize the Word document ──────────────────────────────────────────────
doc <- read_docx()

# ── PART 1: Verify Community Survey Village IDs ──────────────────────────────
doc <- doc %>%
  body_add_par("Verify Community Survey Villages IDs", style = "heading 1") %>%
  body_add_par("")

# Village IDs in one dataset but not the other
missing_from_h2 <- setdiff(h1_dat$vil_code, h2_dat$vil_code)  # in H1, not H2
missing_from_h1 <- setdiff(h2_dat$vil_code, h1_dat$vil_code)  # in H2, not H1

id_error_lines <- character(0)
if (length(missing_from_h2) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h2,
                             " is present in dataset 1 but missing from dataset 2, please verify."))
}
if (length(missing_from_h1) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h1,
                             " is present in dataset 2 but missing from dataset 1, please verify."))
}
if (length(id_error_lines) > 0) {
  for (line in id_error_lines) doc <- body_add_par(doc, line)
}
doc <- body_add_par(doc, "")

# ── Join the two datasets by the shared village variable ─────────────────────
compare_dat <- left_join(h1_dat, h2_dat, by = "vil_code")  %>%
  dplyr::select(-district.x, -district.y, -subdist.x, -subdist.y) %>%
  distinct()

# ── PART 2: Verify results village by village (shared villages only) ─────────
shared_villages <- intersect(h1_dat$vil_code, h2_dat$vil_code)
shared_villages <- shared_villages[!is.na(shared_villages)]

# Split once up front instead of re-filtering the whole frame each iteration
compare_split <- split(compare_dat, compare_dat$vil_code)

# First loop: build all output text in memory (fast, no officer calls)
village_output <- list()
for (v in shared_villages) {
  row_i <- compare_split[[as.character(v)]]
  
  mismatch_lines <- character(0)
  for (col in value_cols) {
    x_val <- row_i[[paste0(col, ".x")]]
    y_val <- row_i[[paste0(col, ".y")]]
    both_na <- is.na(x_val) & is.na(y_val)
    match   <- both_na | (!is.na(x_val) & !is.na(y_val) & x_val == y_val)
    if (!isTRUE(match)) {
      mismatch_lines <- c(mismatch_lines, paste0(col, ": Results do not match, please verify."))
    }
  }
  village_output[[as.character(v)]] <- mismatch_lines
}

# Second loop: write to the doc — heading per village, mismatches as ONE paragraph
for (v in names(village_output)) {
  doc <- body_add_par(doc, paste("Verify results for Village", v), style = "heading 2")
  
  mismatch_lines <- village_output[[v]]
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
}

# ── Save ──────────────────────────────────────────────────────────────────────
print(doc, target = "Community_Survey_Verification.docx")

#################################################################################
#################################################################################
#################################################################################
# Run 3 

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/unprocessed_data")
library(dplyr)
library(stringr)
library(officer)

h1_comsurvey = read.csv("HOTSPOTCommunitySurv_DATA_2026-07-17_1354.csv")
head(h1_comsurvey)
h2_comsurvey = read.csv("HOTSPOT2CommunitySur_DATA_2026-07-17_1356.csv")

h1_dat = h1_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h1_dat)

h2_dat = h2_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h2_dat)

colnames(h1_dat)
colnames(h2_dat)


# ── CHANGE 1: keep one row per vil_code — the MOST COMPLETE row ───────────────
# For each dataset, count non-missing values per row, then within each vil_code
# keep the row with the fewest missing values (the most complete).
keep_most_complete <- function(df) {
  df %>%
    mutate(.n_missing = rowSums(is.na(dplyr::select(., -vil_code)) |
                                  dplyr::select(., -vil_code) == "")) %>%  # NA or "" both count as missing
    group_by(vil_code) %>%
    arrange(.n_missing, .by_group = TRUE) %>%   # fewest missing first
    slice_head(n = 1) %>%                        # keep the most complete row
    ungroup() %>%
    dplyr::select(-.n_missing)
}

h1_dat <- keep_most_complete(h1_dat)
h2_dat <- keep_most_complete(h2_dat)

# ── CHANGE 2: normalize result columns — strip spaces, underscores, hyphens ──
# Identify the result (value) columns = everything except the ID/metadata columns
value_cols <- setdiff(colnames(h1_dat), c("vil_code", "district", "subdist"))

# Function that removes spaces, underscores, and hyphens, leaving just the
# character string of numbers/characters for comparison.
normalize_results <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols),
                  ~ str_remove_all(as.character(.x), "[ _-]")))  # remove space, underscore, hyphen
}

h1_dat <- normalize_results(h1_dat, value_cols)
h2_dat <- normalize_results(h2_dat, value_cols)

# ── Initialize the Word document ──────────────────────────────────────────────
doc <- read_docx()

# ── PART 1: Verify Community Survey Village IDs ──────────────────────────────
doc <- doc %>%
  body_add_par("Verify Community Survey Villages IDs", style = "heading 1") %>%
  body_add_par("")

# Village IDs in one dataset but not the other
missing_from_h2 <- setdiff(h1_dat$vil_code, h2_dat$vil_code)  # in H1, not H2
missing_from_h1 <- setdiff(h2_dat$vil_code, h1_dat$vil_code)  # in H2, not H1

id_error_lines <- character(0)
if (length(missing_from_h2) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h2,
                             " is present in dataset 1 but missing from dataset 2, please verify."))
}
if (length(missing_from_h1) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h1,
                             " is present in dataset 2 but missing from dataset 1, please verify."))
}
if (length(id_error_lines) > 0) {
  for (line in id_error_lines) doc <- body_add_par(doc, line)
}
doc <- body_add_par(doc, "")

# ── Join the two datasets by the shared village variable ─────────────────────
compare_dat <- left_join(h1_dat, h2_dat, by = "vil_code")  %>%
  dplyr::select(-district.x, -district.y, -subdist.x, -subdist.y) %>%
  distinct()

# ── PART 2: Verify results village by village (shared villages only) ─────────
shared_villages <- intersect(h1_dat$vil_code, h2_dat$vil_code)
shared_villages <- shared_villages[!is.na(shared_villages)]

# Split once up front instead of re-filtering the whole frame each iteration
compare_split <- split(compare_dat, compare_dat$vil_code)

# First loop: build all output text in memory (fast, no officer calls)
village_output <- list()
for (v in shared_villages) {
  row_i <- compare_split[[as.character(v)]]
  
  mismatch_lines <- character(0)
  for (col in value_cols) {
    x_val <- row_i[[paste0(col, ".x")]]
    y_val <- row_i[[paste0(col, ".y")]]
    both_na <- is.na(x_val) & is.na(y_val)
    match   <- both_na | (!is.na(x_val) & !is.na(y_val) & x_val == y_val)
    if (!isTRUE(match)) {
      mismatch_lines <- c(mismatch_lines, paste0(col, ": Results do not match, please verify."))
    }
  }
  village_output[[as.character(v)]] <- mismatch_lines
}

# Second loop: write to the doc — heading per village, mismatches as ONE paragraph
for (v in names(village_output)) {
  doc <- body_add_par(doc, paste("Verify results for Village", v), style = "heading 2")
  
  mismatch_lines <- village_output[[v]]
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
}

# ── PART 2: Verify results village by village (shared villages only) ─────────
shared_villages <- intersect(h1_dat$vil_code, h2_dat$vil_code)
shared_villages <- shared_villages[!is.na(shared_villages)]

compare_split <- split(compare_dat, compare_dat$vil_code)

# Helper: is a value "missing"? (NA or empty string after normalization)
is_missing <- function(x) is.na(x) | x == ""

# Helper: last 4 characters of a string
last4 <- function(x) str_sub(x, -4, -1)

# First loop: build all output text in memory (fast, no officer calls)
village_output <- list()
for (v in shared_villages) {
  row_i <- compare_split[[as.character(v)]]
  
  # Pull all .x and .y result values for this village
  x_vals <- unlist(lapply(value_cols, function(col) row_i[[paste0(col, ".x")]]))
  y_vals <- unlist(lapply(value_cols, function(col) row_i[[paste0(col, ".y")]]))
  
  # ── CHANGE 1: whole-dataset-missing check ────────────────────────────────
  x_all_missing <- all(is_missing(x_vals))
  y_all_missing <- all(is_missing(y_vals))
  
  if (x_all_missing) {
    village_output[[as.character(v)]] <- "All results missing from dataset 1"
    next
  }
  if (y_all_missing) {
    village_output[[as.character(v)]] <- "All results missing from dataset 2"
    next
  }
  
  # ── Otherwise, compare column by column ──────────────────────────────────
  mismatch_lines <- character(0)
  digit_len_warning <- FALSE   # flag for CHANGE 2
  
  for (col in value_cols) {
    x_val <- row_i[[paste0(col, ".x")]]
    y_val <- row_i[[paste0(col, ".y")]]
    
    both_na <- is_missing(x_val) & is_missing(y_val)
    if (isTRUE(both_na)) next   # both missing -> match, skip
    
    # If either is missing but not both -> genuine mismatch
    if (is_missing(x_val) | is_missing(y_val)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
      next
    }
    
    # ── CHANGE 2: handle differing digit lengths ─────────────────────────────
    # If the two values have different numbers of characters, compare only the
    # last 4 characters. Flag a warning, and only error if the last-4 differ.
    if (nchar(x_val) != nchar(y_val)) {
      digit_len_warning <- TRUE
    }
    
    if (last4(x_val) != last4(y_val)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
    }
    
      # if last-4 match, it's not an error -> nothing added
    } else {
      # Same length -> compare directly
      if (x_val != y_val) {
        mismatch_lines <- c(mismatch_lines,
                            paste0(col, ": Results do not match, please verify."))
      }
    }
  }
  
  # Assemble this village's output: warning first (if any), then mismatch lines
  out_lines <- character(0)
  if (digit_len_warning) {
    out_lines <- c(out_lines, "Warning: Inconsistent record ID reporting, please verify.")
  }
  out_lines <- c(out_lines, mismatch_lines)
  
  village_output[[as.character(v)]] <- out_lines }
#}

# Second loop: write to the doc — heading per village, lines as ONE paragraph
for (v in names(village_output)) {
  doc <- body_add_par(doc, paste("Verify results for Village", v), style = "heading 2")
  
  out_lines <- village_output[[v]]
  if (length(out_lines) > 0) {
    runs <- vector("list", 2 * length(out_lines) - 1)
    runs[[1]] <- ftext(out_lines[1])
    if (length(out_lines) > 1) {
      for (k in 2:length(out_lines)) {
        runs[[2 * k - 2]] <- run_linebreak()
        runs[[2 * k - 1]] <- ftext(out_lines[k])
      }
    }
    doc <- body_add_fpar(doc, fpar(values = runs))
  }
}

# ── Save ──────────────────────────────────────────────────────────────────────
print(doc, target = "Community_Survey_Verification.docx")



#######################################################
#######################################################
# Run 4 yo
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/unprocessed_data")
library(dplyr)
library(stringr)
library(officer)

h1_comsurvey = read.csv("HOTSPOTCommunitySurv_DATA_2026-07-17_1354.csv")
head(h1_comsurvey)
h2_comsurvey = read.csv("HOTSPOT2CommunitySur_DATA_2026-07-17_1356.csv")

h1_dat = h1_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h1_dat)

h2_dat = h2_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h2_dat)

colnames(h1_dat)
colnames(h2_dat)


# ── CHANGE 1: keep one row per vil_code — the MOST COMPLETE row ───────────────
# For each dataset, count non-missing values per row, then within each vil_code
# keep the row with the fewest missing values (the most complete).
keep_most_complete <- function(df) {
  df %>%
    mutate(.n_missing = rowSums(is.na(dplyr::select(., -vil_code)) |
                                  dplyr::select(., -vil_code) == "")) %>%  # NA or "" both count as missing
    group_by(vil_code) %>%
    arrange(.n_missing, .by_group = TRUE) %>%   # fewest missing first
    slice_head(n = 1) %>%                        # keep the most complete row
    ungroup() %>%
    dplyr::select(-.n_missing)
}

h1_dat <- keep_most_complete(h1_dat)
h2_dat <- keep_most_complete(h2_dat)

# ── CHANGE 2: normalize result columns — strip spaces, underscores, hyphens ──
# Identify the result (value) columns = everything except the ID/metadata columns
value_cols <- setdiff(colnames(h1_dat), c("vil_code", "district", "subdist"))

# Function that removes spaces, underscores, and hyphens, leaving just the
# character string of numbers/characters for comparison.
normalize_results <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols),
                  ~ str_remove_all(as.character(.x), "[ _-]")))  # remove space, underscore, hyphen
}

h1_dat <- normalize_results(h1_dat, value_cols)
h2_dat <- normalize_results(h2_dat, value_cols)

# ── Initialize the Word document ──────────────────────────────────────────────
doc <- read_docx()

# ── PART 1: Verify Community Survey Village IDs ──────────────────────────────
doc <- doc %>%
  body_add_par("Verify Community Survey Villages IDs", style = "heading 1") %>%
  body_add_par("")

# Village IDs in one dataset but not the other
missing_from_h2 <- setdiff(h1_dat$vil_code, h2_dat$vil_code)  # in H1, not H2
missing_from_h1 <- setdiff(h2_dat$vil_code, h1_dat$vil_code)  # in H2, not H1

id_error_lines <- character(0)
if (length(missing_from_h2) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h2,
                             " is present in dataset 1 but missing from dataset 2, please verify."))
}
if (length(missing_from_h1) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h1,
                             " is present in dataset 2 but missing from dataset 1, please verify."))
}
if (length(id_error_lines) > 0) {
  for (line in id_error_lines) doc <- body_add_par(doc, line)
}
doc <- body_add_par(doc, "")

# ── Join the two datasets by the shared village variable ─────────────────────
compare_dat <- left_join(h1_dat, h2_dat, by = "vil_code")  %>%
  dplyr::select(-district.x, -district.y, -subdist.x, -subdist.y) %>%
  distinct()

# ── PART 2: Verify results village by village (shared villages only) ─────────
shared_villages <- intersect(h1_dat$vil_code, h2_dat$vil_code)
shared_villages <- shared_villages[!is.na(shared_villages)]

# Split once up front instead of re-filtering the whole frame each iteration
compare_split <- split(compare_dat, compare_dat$vil_code)


# Helper: is a value "missing"? (NA or empty string after normalization)
is_missing <- function(x) is.na(x) | x == ""

# Helper: last 4 characters of a string
last4 <- function(x) str_sub(x, -4, -1)

# First loop: build all output text in memory (fast, no officer calls)
village_output <- list()
for (v in shared_villages) {
  row_i <- compare_split[[as.character(v)]]
  
  # Pull all .x and .y result values for this village
  x_vals <- unlist(lapply(value_cols, function(col) row_i[[paste0(col, ".x")]]))
  y_vals <- unlist(lapply(value_cols, function(col) row_i[[paste0(col, ".y")]]))
  
  # ── CHANGE 1: whole-dataset-missing check ────────────────────────────────
  x_all_missing <- all(is_missing(x_vals))
  y_all_missing <- all(is_missing(y_vals))
  
  if (x_all_missing) {
    village_output[[as.character(v)]] <- "All results missing from dataset 1"
    next
  }
  if (y_all_missing) {
    village_output[[as.character(v)]] <- "All results missing from dataset 2"
    next
  }
  
  # ── Otherwise, compare column by column ──────────────────────────────────
  mismatch_lines <- character(0)
  digit_len_warning <- FALSE   # flag for CHANGE 2
  
  for (col in value_cols) {
    x_val <- row_i[[paste0(col, ".x")]]
    y_val <- row_i[[paste0(col, ".y")]]
    
    both_na <- is_missing(x_val) & is_missing(y_val)
    if (isTRUE(both_na)) next   # both missing -> match, skip
    
    # If either is missing but not both -> genuine mismatch
    if (is_missing(x_val) | is_missing(y_val)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
      next
    }
    
    # ── CHANGE 2: handle differing digit lengths ─────────────────────────────
    # If the two values have different numbers of characters, flag a warning.
    # Regardless of length, the actual mismatch check compares only the
    # last 4 characters of each value.
    if (nchar(x_val) != nchar(y_val)) {
      digit_len_warning <- TRUE
    }
    
    if (last4(x_val) != last4(y_val)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
    }
  }
  
  # Assemble this village's output: warning first (if any), then mismatch lines
  out_lines <- character(0)
  if (digit_len_warning) {
    out_lines <- c(out_lines, "Warning: Inconsistent record ID reporting, please verify.")
  }
  out_lines <- c(out_lines, mismatch_lines)
  
  village_output[[as.character(v)]] <- out_lines
}

# Second loop: write to the doc — heading per village, lines as ONE paragraph
for (v in names(village_output)) {
  doc <- body_add_par(doc, paste("Verify results for Village", v), style = "heading 2")
  
  out_lines <- village_output[[v]]
  if (length(out_lines) > 0) {
    runs <- vector("list", 2 * length(out_lines) - 1)
    runs[[1]] <- ftext(out_lines[1])
    if (length(out_lines) > 1) {
      for (k in 2:length(out_lines)) {
        runs[[2 * k - 2]] <- run_linebreak()
        runs[[2 * k - 1]] <- ftext(out_lines[k])
      }
    }
    doc <- body_add_fpar(doc, fpar(values = runs))
  }
}

# ── Save ──────────────────────────────────────────────────────────────────────
print(doc, target = "Community_Survey_Verification.docx")



###################################################
###################################################
############# run 5 i think 


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Desktop - Sam’s MacBook Pro/Lo/hotspot/unprocessed_data")
library(dplyr)
library(stringr)
library(officer)

h1_comsurvey = read.csv("HOTSPOTCommunitySurv_DATA_2026-07-17_1354.csv")
head(h1_comsurvey)
h2_comsurvey = read.csv("HOTSPOT2CommunitySur_DATA_2026-07-17_1356.csv")

h1_dat = h1_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h1_dat)

h2_dat = h2_comsurvey %>%
  mutate(subdist = coalesce(subdist_agbo, subdist_prik, subdist_toub)) %>%
  dplyr::select(district, subdist, vil_code, pu_01_1:pu_45_2) %>%
  distinct()
head(h2_dat)

colnames(h1_dat)
colnames(h2_dat)


# ── CHANGE 1: keep one row per vil_code — the MOST COMPLETE row ───────────────
# For each dataset, count non-missing values per row, then within each vil_code
# keep the row with the fewest missing values (the most complete).
keep_most_complete <- function(df) {
  df %>%
    mutate(.n_missing = rowSums(is.na(dplyr::select(., -vil_code)) |
                                  dplyr::select(., -vil_code) == "")) %>%  # NA or "" both count as missing
    group_by(vil_code) %>%
    arrange(.n_missing, .by_group = TRUE) %>%   # fewest missing first
    slice_head(n = 1) %>%                        # keep the most complete row
    ungroup() %>%
    dplyr::select(-.n_missing)
}

h1_dat <- keep_most_complete(h1_dat)
h2_dat <- keep_most_complete(h2_dat)

# ── CHANGE 2: normalize result columns — strip spaces, underscores, hyphens ──
# Identify the result (value) columns = everything except the ID/metadata columns
value_cols <- setdiff(colnames(h1_dat), c("vil_code", "district", "subdist"))

# Function that removes spaces, underscores, and hyphens, leaving just the
# character string of numbers/characters for comparison.
normalize_results <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols),
                  ~ str_remove_all(as.character(.x), "[ _-]")))  # remove space, underscore, hyphen
}

h1_dat <- normalize_results(h1_dat, value_cols)
h2_dat <- normalize_results(h2_dat, value_cols)

# ── Initialize the Word document ──────────────────────────────────────────────
doc <- read_docx()

# ── PART 1: Verify Community Survey Village IDs ──────────────────────────────
doc <- doc %>%
  body_add_par("Verify Community Survey Villages IDs", style = "heading 1") %>%
  body_add_par("")

# Village IDs in one dataset but not the other
missing_from_h2 <- setdiff(h1_dat$vil_code, h2_dat$vil_code)  # in H1, not H2
missing_from_h1 <- setdiff(h2_dat$vil_code, h1_dat$vil_code)  # in H2, not H1

id_error_lines <- character(0)
if (length(missing_from_h2) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h2,
                             " is present in dataset 1 but missing from dataset 2, please verify."))
}
if (length(missing_from_h1) > 0) {
  id_error_lines <- c(id_error_lines,
                      paste0("Village ID ", missing_from_h1,
                             " is present in dataset 2 but missing from dataset 1, please verify."))
}
if (length(id_error_lines) > 0) {
  for (line in id_error_lines) doc <- body_add_par(doc, line)
}
doc <- body_add_par(doc, "")

# ── Heading 2 (top-level): Verify Community Pooling Results, underlined ──────
doc <- doc %>%
  body_add_fpar(
    fpar(
      ftext("2. Verify Community Pooling Results",
            fp_text(underline = TRUE)),
      style = "heading 1"
    )
  ) %>%
  body_add_par("")
# ── Join the two datasets by the shared village variable ─────────────────────
compare_dat <- left_join(h1_dat, h2_dat, by = "vil_code")  %>%
  dplyr::select(-district.x, -district.y, -subdist.x, -subdist.y) %>%
  distinct()

# ── PART 2: Verify results village by village (shared villages only) ─────────
shared_villages <- intersect(h1_dat$vil_code, h2_dat$vil_code)
shared_villages <- shared_villages[!is.na(shared_villages)]

# Split once up front instead of re-filtering the whole frame each iteration
compare_split <- split(compare_dat, compare_dat$vil_code)

# Helper: is a value "missing"? (NA or empty string after normalization)
is_missing <- function(x) is.na(x) | x == ""

# Helper: last 4 characters of a string
last4 <- function(x) str_sub(x, -4, -1)

# First loop: build all output text in memory (fast, no officer calls)
village_output <- list()
for (v in shared_villages) {
  row_i <- compare_split[[as.character(v)]]
  
  # Pull all .x and .y result values for this village
  x_vals <- unlist(lapply(value_cols, function(col) row_i[[paste0(col, ".x")]]))
  y_vals <- unlist(lapply(value_cols, function(col) row_i[[paste0(col, ".y")]]))
  
  # ── CHANGE 1: whole-dataset-missing check ────────────────────────────────
  x_all_missing <- all(is_missing(x_vals))
  y_all_missing <- all(is_missing(y_vals))
  
  if (x_all_missing) {
    village_output[[as.character(v)]] <- "All results missing from dataset 1"
    next
  }
  if (y_all_missing) {
    village_output[[as.character(v)]] <- "All results missing from dataset 2"
    next
  }
  
  # ── Otherwise, compare column by column ──────────────────────────────────
  mismatch_lines <- character(0)
  digit_len_warning <- FALSE   # flag for CHANGE 2
  
  for (col in value_cols) {
    x_val <- row_i[[paste0(col, ".x")]]
    y_val <- row_i[[paste0(col, ".y")]]
    
    both_na <- is_missing(x_val) & is_missing(y_val)
    if (isTRUE(both_na)) next   # both missing -> match, skip
    
    # If either is missing but not both -> genuine mismatch
    if (is_missing(x_val) | is_missing(y_val)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
      next
    }
    
    # ── CHANGE 2: handle differing digit lengths ─────────────────────────────
    # If the two values have different numbers of characters, flag a warning.
    # Regardless of length, the actual mismatch check compares only the
    # last 4 characters of each value.
    if (nchar(x_val) != nchar(y_val)) {
      digit_len_warning <- TRUE
    }
    
    if (last4(x_val) != last4(y_val)) {
      mismatch_lines <- c(mismatch_lines,
                          paste0(col, ": Results do not match, please verify."))
    }
  }
  
  # Assemble this village's output: warning first (if any), then mismatch lines
  out_lines <- character(0)
  if (digit_len_warning) {
    out_lines <- c(out_lines, "Warning: Inconsistent record ID reporting, please verify.")
  }
  out_lines <- c(out_lines, mismatch_lines)
  
  village_output[[as.character(v)]] <- out_lines
}

# Second loop: write to the doc — heading per village, lines as ONE paragraph
for (v in names(village_output)) {
  doc <- body_add_fpar(
    doc,
    fpar(ftext(paste("Village", v), fp_text(bold = TRUE, font.size = 13)))
  )
  
  out_lines <- village_output[[v]]
  if (length(out_lines) > 0) {
    runs <- vector("list", 2 * length(out_lines) - 1)
    runs[[1]] <- ftext(out_lines[1])
    if (length(out_lines) > 1) {
      for (k in 2:length(out_lines)) {
        runs[[2 * k - 2]] <- run_linebreak()
        runs[[2 * k - 1]] <- ftext(out_lines[k])
      }
    }
    doc <- body_add_fpar(doc, fpar(values = runs))
  }
}

# ── Save ──────────────────────────────────────────────────────────────────────
print(doc, target = "Community_Survey_Verification.docx")





