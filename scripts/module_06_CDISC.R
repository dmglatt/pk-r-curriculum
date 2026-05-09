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

# (Above failed). Here's an alternative file source
install.packages("pharmaverseadam")

# ── Alternative data load via pharmaverseadam package ─────────────────────────

library(pharmaverseadam)

# Load ADPC directly from the package
adpc_raw <- pharmaverseadam::adpc

# Verify it loaded
glimpse(adpc_raw)
nrow(adpc_raw)
names(adpc_raw)

# ── 2. Explore the dataset structure ─────────────────────────────────────────

# How many unique subjects?
n_distinct(adpc_raw$USUBJID)

# What treatments are present?
adpc_raw |> count(TRT01A)

# What analytes are measured?
adpc_raw |> count(PARAM)

# Time variable check
adpc_raw |>
  select(USUBJID, NFRLT, AFRLT, AVAL, AVALU, ALLOQ, ANL01FL) |>
  head(20)

# LLOQ values present
adpc_raw |> count(ALLOQ, AVALU)

# What does ANL01FL contain — this is the primary analysis flag
adpc_raw |> count(ANL01FL)

# ── 3. Create analysis-ready dataset ─────────────────────────────────────────

adpc <- adpc_raw |>
  # ANL01FL == "Y" flags records included in the primary PK analysis
  filter(ANL01FL == "Y") |>
  # Exclude imputed or derived records
  filter(is.na(DTYPE) | DTYPE == "") |>
  # Select and rename key variables
  select(
    subject_id  = USUBJID,
    treatment   = TRT01A,
    param       = PARAM,
    nom_time    = NFRLT,
    act_time    = AFRLT,
    conc        = AVAL,
    conc_unit   = AVALU,
    lloq        = ALLOQ,
    dose        = DOSEA,
    visit       = AVISIT,
    timepoint   = ATPT
  ) |>
  # Derive BLQ flag from AVAL vs ALLOQ
  mutate(
    blq_flag      = !is.na(lloq) & conc < lloq,
    conc_analysis = if_else(blq_flag, 0, conc)
  ) |>
  arrange(subject_id, param, act_time)

glimpse(adpc)

# Verify
adpc |>
  group_by(treatment) |>
  summarize(
    n_subjects  = n_distinct(subject_id),
    n_obs       = n(),
    n_blq       = sum(blq_flag, na.rm = TRUE),
    pct_blq     = n_blq / n_obs * 100,
    .groups     = "drop"
  )

# ── 4. Mean profile by treatment and nominal time ─────────────────────────────

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

adpc_mean

# ── 5. Plot ───────────────────────────────────────────────────────────────────

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
  geom_errorbar(
    data = adpc_mean,
    aes(x    = nom_time,
        ymin = pmax(mean_conc - sd_conc, 0.001),
        ymax = mean_conc + sd_conc),
    color = "#185FA5", width = 0.5, linewidth = 0.5
  ) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  facet_wrap(~ treatment) +
  labs(
    title   = "Xanomeline PK — CDISC pilot study",
    x       = "Time after first dose (h)",
    y       = paste0("Concentration (", unique(adpc$conc_unit)[1], ")"),
    caption = "Blue: mean ± SD at nominal time  |  Gray: individual profiles at actual time"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 9),
    plot.caption     = element_text(size = 8, color = "gray50")
  )

p_adpc

ggsave(
  filename = "outputs/figures/m06_xanomeline_profiles.png",
  plot     = p_adpc,
  width    = 10, height = 5, dpi = 300
)

