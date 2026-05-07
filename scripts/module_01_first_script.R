# ── Module 01: First script ──────────────────────────────────────────────────
# Purpose: Verify R is working, learn basic syntax, preview PK concepts in R
# Author:  Dylan Glatt
# Date:    2026-05-06

# ── 1. Basic arithmetic ───────────────────────────────────────────────────────

2+2
100/4
2^10 #exponentiation; good for log-linear analysis
sqrt(100)

# ── 2. Variables ──────────────────────────────────────────────────────────────

dose_mg <- 100 #dose in mg
volume_L <- 50
cmax_ng_mL <- (dose_mg*100)/volume_L
cmax_ng_mL

# 3 -- 3. Vectors - the core data structure

time_h <- c(0,0.5,1,2,4,8,12,24)
conc_ng_mL<-c(0,450,820,710,430,180,75,12)

# How many observations?
length(time_h) #reports the number of time points

# Summary Stats
mean(conc_ng_mL)
median(conc_ng_mL)
max(conc_ng_mL)
min(conc_ng_mL)

# 4 Your first PK dataframe

pk_data <-  data.frame(
  time_h = time_h,
  conc_ng_mL = conc_ng_mL
)  

pk_data #print the data frame

str(pk_data) #inspect STRUCTURE of the dataframe

# ── 5. Your first plot ────────────────────────────────────────────────────────
# Base R plot — we'll replace this with ggplot2 in Module 05
# but it's useful to know base R plots exist

plot(pk_data$time_h, pk_data$conc_ng_mL,
     type = "b",
     xlab = "Time (h)",
     ylab = "Concentration (ng/mL)",
     main = "My First PK Plot",
     pch = 16, #filled circle
     col = "steelblue"
     )
# ── 6. Log-linear scale — this is how PK data usually looks ──────────────────

plot(pk_data$time_h, pk_data$conc_ng_mL,
     type = "b",
     xlab = "Time (h)",
     ylab = "Concentration (ng/mL)",
     main = "My First PK Plot (Semilog)",
     pch = 16,
     col = "steelblue",
     log = "y") #log transforms the data


