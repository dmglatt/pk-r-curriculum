install.packages("haven")   # for reading SAS transport files (.xpt)

# ── Module 06: CDISC data structures ─────────────────────────────────────────
# Author: Dylan Glatt
# Date:   2026-05-06
# Data:   CDISC pilot study — publicly available FDA-standard dataset

library(tidyverse)
library(haven)

# ── 1. Download the CDISC pilot ADPC dataset ──────────────────────────────────
# Source: CDISC pilot study, hosted publicly on GitHub

adpc_url <- "https://github.com/cdisc-org/sdtm-adam-pilot-project/raw/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adpc.xpt"

# Download to your raw data folder
download.file(
  url      = adpc_url,
  destfile = "data/raw/adpc.xpt",
  mode     = "wb"   # binary mode required for .xpt files
)

# Read it in
adpc_raw <- read_xpt("data/raw/adpc.xpt")

# Always inspect immediately
glimpse(adpc_raw)
nrow(adpc_raw)
names(adpc_raw)

# ── 2. Explore the dataset structure ─────────────────────────────────────────

# How many unique subjects?
n_distinct(adpc_raw$USUBJID)

# What treatments are present?
adpc_raw |> count(TREATMENT)

# What analytes are measured?
adpc_raw |> count(PARAM)

# Time variable check — nominal vs actual
adpc_raw |>
  select(USUBJID, NFRLT, AFRLT, AVAL, AVALU, MDV, BLQ) |>
  head(20)

# How many BLQ samples?
adpc_raw |>
  summarize(
    total     = n(),
    blq       = sum(BLQ == 1, na.rm = TRUE),
    pct_blq   = blq / total * 100
  )

# Dose levels present
adpc_raw |>
  filter(!is.na(DOSEA)) |>
  count(DOSEA, TREATMENT)

# ── 3. Create analysis-ready dataset ─────────────────────────────────────────

adpc <- adpc_raw |>
  # Keep only PK concentration observations (exclude dose records)
  filter(EVID == 0) |>
  # Exclude missing dependent variables
  filter(MDV == 0) |>
  # Select and rename key variables for clarity
  select(
    subject_id  = USUBJID,
    treatment   = TREATMENT,
    param       = PARAM,
    nom_time    = NFRLT,      # nominal time from first dose
    act_time    = AFRLT,      # actual time from first dose
    conc        = AVAL,       # analysis value (concentration)
    conc_unit   = AVALU,
    blq         = BLQ,
    dose        = DOSEA
  ) |>
  # Handle BLQ — set to 0 for now, flag for sensitivity analysis later
  # This is the simplest approach; M09 covers BLQ handling in detail
  mutate(
    conc_analysis = if_else(blq == 1, 0, conc),
    blq_flag      = blq == 1
  ) |>
  arrange(subject_id, param, act_time)

glimpse(adpc)

# Verify the wrangling worked
adpc |>
  group_by(treatment) |>
  summarize(
    n_subjects  = n_distinct(subject_id),
    n_obs       = n(),
    n_blq       = sum(blq_flag),
    pct_blq     = n_blq / n_obs * 100,
    .groups     = "drop"
  )

# ── 4. Concentration-time plot — real CDISC data ──────────────────────────────

# Individual profiles — actual time
# Mean profile — nominal time
# Faceted by treatment

# Mean profile by treatment and nominal time
adpc_mean <- adpc |>
  filter(!blq_flag) |>
  group_by(treatment, nom_time) |>
  summarize(
    n         = n(),
    mean_conc = mean(conc_analysis),
    sd_conc   = sd(conc_analysis),
    geomean   = exp(mean(log(conc_analysis[conc_analysis > 0]))),
    .groups   = "drop"
  )

# Plot
p_adpc <- ggplot() +
  geom_line(
    data = adpc |> filter(!blq_flag),
    aes(x = act_time, y = conc_analysis, group = subject_id),
    color = "gray70", linewidth = 0.3, alpha = 0.5
  ) +
  geom_line(
    data = adpc_mean,
    aes(x = nom_time, y = mean_conc),
    color = "#185FA5", linewidth = 1.1
  ) +
  geom_point(
    data = adpc_mean,
    aes(x = nom_time, y = mean_conc),
    color = "#185FA5", size = 2.5
  ) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  facet_wrap(~ treatment) +
  labs(
    title   = "CDISC pilot study — PK concentration-time profiles",
    x       = "Time after first dose (h)",
    y       = paste0("Concentration (", unique(adpc$conc_unit)[1], ")"),
    caption = "Blue: mean at nominal time  |  Gray: individual profiles at actual time"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 9)
  )

p_adpc

ggsave(
  filename = "outputs/figures/m06_adpc_profiles.png",
  plot     = p_adpc,
  width    = 10, height = 5, dpi = 300
)

# Enrollment by treatment
adpc |>
  group_by(treatment) |>
  summarize(
    n_subjects = n_distinct(subject_id),
    .groups    = "drop"
  )

# Last timepoint by subject — are some subjects truncated early?
adpc |>
  filter(!blq_flag) |>
  group_by(treatment, subject_id) |>
  summarize(
    last_time = max(act_time),
    n_obs     = n(),
    .groups   = "drop"
  ) |>
  group_by(treatment) |>
  summarize(
    mean_last_time = mean(last_time),
    min_last_time  = min(last_time),
    max_last_time  = max(last_time),
    n_truncated    = sum(last_time < max(last_time)),
    .groups        = "drop"
  )

