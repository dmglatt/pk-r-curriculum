# ── Module 04: dplyr and tidyverse ───────────────────────────────────────────
# Author: Dylan Glatt
# Date:   2026-05-06

library(tidyverse)

# ── 1. Build a realistic PK dataset ──────────────────────────────────────────
# 12 subjects across 3 dose cohorts (4 subjects each)
# Single ascending dose study, oral small molecule
# Timepoints: 0, 0.5, 1, 2, 4, 8, 12, 24h

set.seed(42)   # makes random numbers reproducible

n_subjects  <- 12
time_points <- c(0, 0.5, 1, 2, 4, 8, 12, 24)

pk_data <- data.frame(
  subject_id = rep(paste0("SUBJ-", sprintf("%03d", 1:n_subjects)),
                   each = length(time_points)),
  dose_mg    = rep(c(rep(50, 4), rep(100, 4), rep(200, 4)),
                   each = length(time_points)),
  time_h     = rep(time_points, times = n_subjects),
  conc_ng_mL = pmax(0, c(
    sapply(1:n_subjects, function(i) {
      dose_scalar <- c(50, 50, 50, 50, 100, 100, 100, 100, 200, 200, 200, 200)[i]
      peak <- dose_scalar * runif(1, 3.5, 5.5)
      c(0,
        peak * 0.45 * runif(1, 0.8, 1.2),
        peak * 0.85 * runif(1, 0.8, 1.2),
        peak * runif(1, 0.85, 1.0),
        peak * 0.65 * runif(1, 0.8, 1.2),
        peak * 0.30 * runif(1, 0.8, 1.2),
        peak * 0.12 * runif(1, 0.8, 1.2),
        peak * 0.02 * runif(1, 0.8, 1.2))
    })
  ))
)

# Always inspect a new dataset immediately
glimpse(pk_data)
head(pk_data, 16)   # first 16 rows = first 2 subjects

# ── 2. filter() — keep rows matching a condition ─────────────────────────────

# Single dose group

pk_100 <- pk_data |> filter(dose_mg == 100)

# Exclude predose samples (time 0)

pk_no_predose <- pk_data |> filter(time_h > 0)

# One subject

pk_subj001 <- pk_data |> filter(subject_id == "SUBJ-001")

# Multiple conditions — concentrations above 200 ng/mL after time 0

pk_data |> filter(time_h > 0, conc_ng_mL > 200)

# ── 3. select() — keep or drop columns ───────────────────────────────────────

pk_data |> select(subject_id, time_h, conc_ng_mL) # keep/select these three columns
pk_data |> select (-dose_mg) # drop dose_mg (keep all else)

# ── 4. mutate() — create or modify columns ───────────────────────────────────

pk_data <- pk_data |>
  mutate(
    log_conc = log10(conc_ng_mL), # log 10 conversion
    conc_ug_mL = conc_ng_mL/1000, # unit conversion
    lloq = 1.0, # assay LLOQ 
    blq_flag = conc_ng_mL < lloq & time_h > 0, # add BLQ flag (for all but predose)
    dose_group = paste0(dose_mg, "mg") #label for plotting
  )

glimpse (pk_data)

# ── 5. arrange() — sort rows ──────────────────────────────────────────────────

# Sort by subject then time — this is how PK datasets should always be ordered

pk_data_1 <- pk_data |> arrange(subject_id,time_h)

# Sort by concentration descending — find the highest observations

pk_data_2 <- pk_data |> arrange(desc(conc_ng_mL)) |> head(10)

# ── 6. group_by() + summarize() — aggregate by group ─────────────────────────

# Mean and SD concentration at each timepoint by dose group

pk_summary <- pk_data |> 
  filter(!blq_flag) |> # exclude BLQ before summarizing
  group_by(dose_mg, time_h) |> # data summarized by dose level and time point
  summarize(
    n = n(),
    mean_conc = mean(conc_ng_mL),
    sd_conc = sd(conc_ng_mL),
    cv_pct = sd_conc / mean_conc *100, 
    geomean = exp(mean(log(conc_ng_mL[conc_ng_mL > 0]))),
    .groups = "drop" # ungroup after summarizing
  )

print(pk_summary, n=30)

# ── 7. Joining datasets ───────────────────────────────────────────────────────

# A demographics table — one row per subject

demographics <- data.frame(
  subject_id = paste0("SUBJ-", sprintf("%03d", 1:n_subjects)),
  age_yr     = c(34, 45, 28, 52, 41, 37, 60, 29, 48, 33, 55, 44),
  weight_kg  = c(72, 85, 63, 91, 78, 68, 95, 71, 83, 76, 88, 70),
  sex        = rep(c("M", "F"), 6),
  race       = rep(c("White", "Black", "Asian", "White"), 3)
)

# left_join: keep all rows from pk_data, add columns from demographics

pk_merged <- pk_data |> 
  left_join(demographics, by = "subject_id")

glimpse(pk_merged)

# Now you can filter by demographic covariates

pk_filter_demo <- pk_merged |>
  filter(sex == "F", time_h == 4) |>
  select(subject_id, sex, dose_mg, time_h, conc_ng_mL, weight_kg)

glimpse(pk_filter_demo)

# Module 04 Checkpoint

pk_summary_2 <- pk_merged |>
  group_by(dose_group) |>
  summarize(
    mean_BW = mean(weight_kg),
    sd_BW = sd(weight_kg),
    cv_pct_BW = sd_BW / mean_BW *100
  )

pk_merged_new <- pk_merged |> 
  mutate(
    tad_flag = time_h %in% c(0.5, 1, 2)
  )

cmax_summary <- pk_merged_new |>
  group_by(subject_id, dose_mg) |> 
  summarize(
    cmax = max(conc_ng_mL),
    tmax = time_h[which.max(conc_ng_mL)],
    .groups = "drop"
  ) |>
arrange(desc(cmax))


  