# ── Module 03: R basics I — vectors, data types, indexing ────────────────────
# Author: Dylan Glatt
# Date:   2026-05-06

# ── 1. The four types you'll use constantly ───────────────────────────────────

# Numeric — any number, with or without decimals

dose <- 100;
auc <- 4823.5;
cl <- 12.7;

# Character — text, always in quotes

subject_id <- "SUBJ-001";
drug_name <- "paltusotine";
matrix <- "plasma";

# Logical — TRUE or FALSE only, no quotes

is_dosed <- TRUE; 
excluded <- FALSE;
below_llq <- FALSE;

# Integer — whole numbers, less common but you'll see them

n_subjects <- as.integer(24);

# Check what type something is

class(dose);
class(subject_id);
class(is_dosed);
class(n_subjects);

# is.numeric(), is.character() return TRUE/FALSE — useful for validation

is.numeric(auc);
is.character(drug_name)
is.logical(is_dosed)
is.integer(n_subjects)

# ── 2. Creating vectors ───────────────────────────────────────────────────────

# Nominal time points for a typical PK profile (hours)

time_h <- c(0,0.5,1,2,4,6,8,12,24,48,72);

# Corresponding concentrations (ng/mL) — single subject, oral SM

conc_ng_mL <- c(0,120,310,580,490,380,240,130,45,8,1.2);

# Sequences — useful for generating time grids in simulation

time_dense <- seq(0,72,by=0.5) #every 30 min from 0 to 72h
length(time_dense) #how many points?

# Repeated values — useful for building datasets

dose_mg <- rep(100,times=11) #same dose at every time point
cohort <- rep(c("low", "mid", "high"), each=4) #4 subjects per cohort, 3 cohorts

# ── 3. Vector arithmetic — this is where R shines ────────────────────────────

# Operations apply to every element simultaneously — no loops needed

conc_ug_mL <- conc_ng_mL/100 #convert ng/mL to ug/mL
log_conc <- log(conc_ng_mL) #natural log - not what happens at 0
log10_conc <- log10(conc_ng_mL) #log base 10 

# This is vectorized math — the same operation R uses internally for NCA
# When you compute AUC by the trapezoidal rule, you're doing vector arithmetic

# ── 4. Summary functions on vectors ──────────────────────────────────────────

mean(conc_ng_mL)
median(conc_ng_mL)
sd(conc_ng_mL)
max(conc_ng_mL)
min(conc_ng_mL)
range(conc_ng_mL) #returns c(min,max)
sum(conc_ng_mL)

# Geometric mean — more relevant than arithmetic mean for PK parameters

exp(mean(log(conc_ng_mL[conc_ng_mL > 0]))) #exlcude the 0 conc at time 0

# ── 5. Indexing by position ───────────────────────────────────────────────────

time_h[1] #first element in time_h - R indexes from 1, not 0 (unlike Python)
time_h[3] # third element
time_h[1:4] # elements 1 through 4
time_h[c(1,3,5)] #elements 1, 3, and 5

# Last element — useful when you don't know the length

time_h[length(time_h)] # returns the final value/element in time_h (ie, not the count of all elements in time_h)

# ── 6. Indexing by logical condition — the most important kind ────────────────

# Which concentrations are above 100 ng/mL?

conc_ng_mL > 100 #returns a logical vector [TRUE, FALSE, ..., etc]
conc_ng_mL[conc_ng_mL>100] # returns ONLY those values > 100 from conc_ng_mL

# Corresponding time points where conc > 100

time_h[conc_ng_mL>100]

# This is how you'll filter BLQ samples in real PK data
# e.g., conc[conc > lloq]  where lloq is your lower limit of quantification

# Combining conditions

conc_ng_mL[conc_ng_mL > 50 & conc_ng_mL < 400] # AND
conc_ng_mL[conc_ng_mL < 10 | conc_ng_mL > 500] # OR

# ── 7. Named vectors — indexing by name ───────────────────────────────────────

# NCA parameters as a named vector

nca_params <- c(
  cmax = 500,
  tmax = 2,
  auc_last = 12450, 
  t_half = 18.3, 
  cl_f = 8.0
)

nca_params["cmax"] #returns the value associated with the name, here cmax
nca_params[c("cmax","t_half")] # extracts multiple values by name

# Names are preserved through operations

nca_params * 2

# ── 8. NA — missing values ────────────────────────────────────────────────────

conc_with_missing <- c(0,120,NA,580,490,NA,240,45,8,1.2)

# NA is contagious — any operation involving NA returns NA

mean(conc_with_missing) # returns NA, since NA is in the dataset
mean(conc_with_missing, na.rm = TRUE) # na.rm removes the NAs first, then calculates the mean

sum(conc_with_missing, na.rm = TRUE)

# Detecting NAs

is.na(conc_with_missing) #logical vector, TRUE where NA
sum(is.na(conc_with_missing)) # count of NAs

# Removing NAs from a vector

conc_clean <- conc_with_missing[!is.na(conc_with_missing)] # new vector of conc including ONLY non-NA values
conc_clean


