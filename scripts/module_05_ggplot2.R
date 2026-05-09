# ── Module 05: ggplot2 — PK figures ──────────────────────────────────────────
# Author: Dylan Glatt
# Date:   2026-05-06
# Data:   Theoph — built-in R dataset, real theophylline PK
#         12 subjects, single oral dose, rich sampling

library(tidyverse)

# ── 1. Load and inspect the data ─────────────────────────────────────────────

pk <- as_tibble(Theoph)   # convert to tibble for cleaner printing
glimpse(pk)
head(pk, 20)

# Column meanings:
# Subject  — subject ID (factor, 1–12)
# Wt       — body weight (kg)
# Dose     — dose in mg/kg
# Time     — time after dose (hours)
# conc     — theophylline concentration (mg/L)

# How many subjects, timepoints per subject?
pk |> count(Subject)

# Dose range across subjects, 

pk_dose <- pk |>
  group_by(Subject) |>
  summarize(dose_mgkg = first(Dose), weight_kg = first(Wt)) |>
  mutate(total_dose_mg = dose_mgkg * weight_kg) |>
arrange(desc(total_dose_mg))

# merged Time-concentration data with the new total_dose_mg column
pk_merged <- pk |> 
  left_join(pk_dose, by = "Subject")

# removing duplicate columns
pk_merged <- pk_merged |> select(-Dose) # drop dose_mg (keep all else)
pk_merged <- pk_merged |> select(-Wt) # drop dose_mg (keep all else)

# ── 2. Linear scale — individual profiles ────────────────────────────────────

# Start minimal — just the scaffolding
ggplot(data = pk, aes(x=Time, y=conc)) +
  geom_point()

# Time must be capitalized, and conc must be lowercase - it seems 

# Connect each subject's points with lines
# group = Subject tells ggplot which points belong together
ggplot(data = pk, aes(x = Time, y = conc, group = Subject)) +
  geom_point() +
  geom_line()

# Color by subject — spaghetti plot
ggplot(data = pk, aes(x = Time, y = conc, group = Subject, color = Subject)) +
  geom_point() +
  geom_line()

# ── 3. Clean it up with labels and theme ─────────────────────────────────────

p_linear <- ggplot(data = pk, 
                   aes(x=Time, 
                       y=conc, 
                       group=Subject,
                       color=Subject)) + 
  geom_point(size=2, alpha=0.7) + 
  geom_line(linewidth=0.6, alpha=07) + 
  labs(
    title = "Theo PK Profile - by individual",
    x = "Time after dose (h)",
    y = "Concentration (mg/L)",
    color = "Subject",
    caption = "Data: Theo dataset (Boeckman, et al.)"
  ) + 
  theme_bw() + 
  theme(
    legend.position = "right",
    plot.title = element_text(size=12, face="plain"),
    axis.title = element_text(size=11),
    axis.text = element_text(size=10),
    panel.grid.minor = element_blank()
  )

p_linear

# ── 4. Semi-log scale — the standard for PK reporting ────────────────────────

p_log <- p_linear +
  scale_y_log10(
    breaks = c(0.1,0.5,1,2,5,10,20),
    labels = c("0.1","0.5","1","2","5","10","20")
  ) + 
  labs(title = "Theophylline PK - individual (in semilog)") +
  annotation_logticks(sides = "1") # log tick marks on left axis

p_log

# ── 5. Compute mean profile ───────────────────────────────────────────────────

pk_mean <- pk |>
  group_by(Time) |>
  summarize(
    mean_conc = mean(conc),
    sd_conc = sd(conc),
    n = n(),
    se_conc = sd_conc/sqrt(n),
    .groups = "drop"
  )

# ── 6. Individual + mean overlay ─────────────────────────────────────────────

p_overlay <- ggplot() +
  geom_line(data = pk,
            aes(x = Time, y = conc, group = Subject),
            color = "gray70", linewidth = 0.4, alpha = 0.6) +
  geom_point(data = pk,
             aes(x = Time, y = conc, group = Subject),
             color = "gray70", size = 1.5, alpha = 0.6) +
  geom_line(data = pk_mean,
            aes(x = Time, y = mean_conc),
            color = "#185FA5", linewidth = 1.2) +
  geom_point(data = pk_mean,
             aes(x = Time, y = mean_conc),
             color = "#185FA5", size = 3) +
  geom_errorbar(data = pk_mean,
                aes(x = Time,
                    ymin = mean_conc - sd_conc,
                    ymax = mean_conc + sd_conc),
                color = "#185FA5", width = 0.4, linewidth = 0.6) +
  scale_y_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20),
                labels = c("0.1", "0.5", "1", "2", "5", "10", "20")) +
  annotation_logticks(sides = "l") +
  labs(
    title   = "Theophylline PK — individual and mean ± SD profiles",
    x       = "Time after dose (h)",
    y       = "Concentration (mg/L)",
    caption = "Blue: mean ± SD  |  Gray: individual subjects"
  ) +
  theme_bw() +
  theme(
    plot.title       = element_text(size = 12, face = "plain"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

p_overlay

# Update the code to reproduce the mean profile using nominal time 

# ── Nominal time mapping ──────────────────────────────────────────────────────

# First, look at the actual times in the dataset to understand the spread
pk |> group_by(Subject) |> summarize(times = list(round(Time, 2)))
sort(unique(round(pk$Time, 2)))

# ── Add nominal time column ───────────────────────────────────────────────────
# Theoph nominal collection times: 0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24h

pk <- pk |>
  mutate(
    nom_time = case_when(
      Time == 0          ~ 0,
      Time < 0.4         ~ 0.25,
      Time < 0.75        ~ 0.5,
      Time < 1.5         ~ 1,
      Time < 3           ~ 2,
      Time < 4.5         ~ 4,
      Time < 6           ~ 5,
      Time < 8           ~ 7,
      Time < 10.5        ~ 9,
      Time < 18          ~ 12,
      TRUE               ~ 24
    )
  )

# Verify the mapping looks right — each actual time should map sensibly
pk |> select(Subject, Time, nom_time, conc) |> print(n = 30)

# ── Mean profile using nominal time ──────────────────────────────────────────

pk_mean <- pk |>
  group_by(nom_time) |>
  summarize(
    n         = n(),
    mean_conc = mean(conc),
    sd_conc   = sd(conc),
    geomean   = exp(mean(log(conc[conc > 0]))),
    .groups   = "drop"
  )

pk_mean

# ── Corrected overlay plot ────────────────────────────────────────────────────
# Individual data: actual time (Time)
# Mean profile: nominal time (nom_time)

p_overlay_corrected <- ggplot() +
  geom_line(data = pk,
            aes(x = Time, y = conc, group = Subject),
            color = "gray70", linewidth = 0.4, alpha = 0.6) +
  geom_point(data = pk,
             aes(x = Time, y = conc),
             color = "gray70", size = 1.5, alpha = 0.6) +
  geom_line(data = pk_mean,
            aes(x = nom_time, y = mean_conc),
            color = "#185FA5", linewidth = 1.2) +
  geom_point(data = pk_mean,
             aes(x = nom_time, y = mean_conc),
             color = "#185FA5", size = 3) +
  geom_errorbar(data = pk_mean,
                aes(x    = nom_time,
                    ymin = mean_conc - sd_conc,
                    ymax = mean_conc + sd_conc),
                color = "#185FA5", width = 0.4, linewidth = 0.6) +
  scale_y_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20),
                labels = c("0.1", "0.5", "1", "2", "5", "10", "20")) +
  annotation_logticks(sides = "l") +
  labs(
    title   = "Theophylline PK — individual (actual time) and mean ± SD (nominal time)",
    x       = "Time after dose (h)",
    y       = "Concentration (mg/L)",
    caption = "Blue: mean ± SD at nominal time  |  Gray: individual profiles at actual time"
  ) +
  theme_bw() +
  theme(
    plot.title       = element_text(size = 11, face = "plain"),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

p_overlay_corrected

# Save the corrected version
ggsave(
  filename = "outputs/figures/m05_theoph_mean_overlay_corrected.png",
  plot     = p_overlay_corrected,
  width    = 8, height = 5, dpi = 300, units = "in"
)
