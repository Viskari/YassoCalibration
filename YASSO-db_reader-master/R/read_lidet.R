# Read LIDET data and save it to results/lidet


# -------------------------------------------------------------------------
# Libraries

# For reading Excel-files
library(readxl)
source("R/gen_funcs.R")

# Tidyverse
# general manipulation
library(dplyr)
# pivot_longer, nest
library(tidyr)
# mapping with nest
library(purrr)
# save files
library(readr)


# -------------------------------------------------------------------------
# Setup

# Define database path
path_db <- "data/YDBFIN_litter.xlsx"

# Define folder for saving the read data
path_out <- "results/lidet/"

# Dataset name
db <- "LIDET"

# Sites
site <- c(
  "AND", "ARC", "BCI", "BNZ", "BSF", "CDR", "CPR", "CWT", "GSF", "HBR", "HFR",
  "JRN", "JUN", "KBS", "KNZ", "LBS", "LUQ", "LVW", "MTV", "NIN", "NLK", "NWT",
  "OLY", "SEV", "SMR", "UFL", "VCR"
)

# Species
spec <- c(
  "ABCO", "ABLA", "ACSA", "AMBR", "ANGE", "ANSC", "BELU", "BOER","BOGR", "CEGR",
  "CONU", "DRGL", "FAGR", "GYLU", "KOMY", "LATR", "LITU", "MYCE", "PIEL", 
  "PIRE", "PIST", "POTR", "PSME", "QUPR", "RHMA", "ROPS", "SPAL", "THPL", 
  "TRAE", "VOFE", "GOBA"
)

# Types
type <- c("L", "R")


# -------------------------------------------------------------------------
# Import data

# Load the litter database
data_in <- read_litter_data(path_db)

# Save Excel sheets to variables
S <- data_in$sites
LT <- data_in$litter_time
LI <- data_in$litter_init


# -------------------------------------------------------------------------
# Preprocess data

# In the LIDET data, there are multiple measurements for each timeout value. We
# should group these by the timeout values and calculate means for both the
# carbons (also calculate sds) and the timeout to obtain measurements similar to
# CIDET, where this has already been done "behind the scenes".

df_group <- LT %>%
  filter(
    ORIGIN %in% db &
      SITE %in% site &
      SPECIES %in% spec &
      LITTERTYPE1 %in% type &
      MREM_A0 != 0
  ) %>%
  select(OBS, LITTERTYPE1, SITE, SPECIES, MESHTOP1, TIMEOUT, MREM_A0) %>% 
  group_by(SITE, SPECIES, LITTERTYPE1, MESHTOP1) %>% 
  # Find the start of each timeout group based on differences between
  # subsequent timeout values
  mutate(
    prev_to = lag(TIMEOUT),
    diff_prev = abs(prev_to - TIMEOUT),
    # The difference limit that splits groups is 0.05 here based on analysing
    # the data. There are variances of 0.04 within each timeout group e.g. (1.00
    # and 1.04 might still belong to the same group), but higher than that
    # everything is in the same group (aside from a couple of cases). There are
    # no values between differences 0.04 and 0.09 and the difference 0.09 always
    # splits groups. So the limit could also be e.g. >0.08, but I prefer >0.05
    # because it looks nice. (>0.04 would miss a couple of cases due to floating
    # point errors)
    group_note = case_when(
      diff_prev > 0.05 | (is.na(prev_to) & is.na(diff_prev)) ~ "start_group"
    )
  )

# Create a unique index for each group
df_group_numbers <- df_group %>% 
  ungroup() %>% 
  select(OBS, group_note) %>%
  filter(!is.na(group_note)) %>% 
  mutate(group = 1:nrow(.))

# Join and group by the unique indexes, calculate means and sds for groups
df_base <- df_group %>% 
  left_join(df_group_numbers, by = c("group_note", "OBS")) %>% 
  fill(group, .direction = "down") %>% 
  group_by(group, .add = TRUE) %>% 
  summarise(
    mean_time = mean(TIMEOUT),
    mean_c = mean(MREM_A0),
    sd_c = sd(MREM_A0)
  ) %>% 
  ungroup()

# Find the (litter bag) measurement periods for splitting the data into training
# and testing set.
df_base <- df_base %>% 
  # Because we averaged the measurements above, each SITE, SPECIES, LITTERTYPE1,
  # MESHTOP1 combination only has a single measurement period
  distinct(SITE, SPECIES, LITTERTYPE1, MESHTOP1) %>% 
  # Mark the periods
  mutate(period = 1:nrow(.)) %>% 
  # Join to base dataset
  right_join(df_base, by = c("SITE", "SPECIES", "LITTERTYPE1", "MESHTOP1")) %>% 
  select(-group)


# -------------------------------------------------------------------------
# Shape data imported from the database

# Timeouts
df_time <- df_base %>% 
  select(time = mean_time)


# Observations
df_obs <- df_base %>% 
  mutate(c_obs = mean_c * 1000) %>% 
  select(c_obs)


# Uncertainties. These require some additional calculations, since we want to
# connect the uncertainty of each observation to its TIMEOUT value. Here, we
# categorize the TIMEOUTS into time windows and use averaged relative standard
# deviations to calculate the uncertainties.

# Categorize relative standard deviations into time windows
df_unc_windows <- df_base %>% 
  # Calculate the relative sds
  mutate(rel_sd = sd_c / mean_c) %>% 
  # Identify and label the time windows
  mutate(
    time_window = cut(
      mean_time,
      breaks = c(0, seq(0.5, 10.5, by = 1.0)),
      labels = c(
        "0.0-0.5", "0.5-1.5", "1.5-2.5", "2.5-3.5", "3.5-4.5", "4.5-5.5",
        "5.5-6.5", "6.5-7.5", "7.5-8.5", "8.5-9.5", "9.5-10.5"
        )
      )
    )

# Calculate means for the relative sds over the time windows
df_rel_unc <- df_unc_windows %>% 
  drop_na() %>% 
  group_by(time_window) %>% 
  summarise(rel_unc = mean(rel_sd))

# Join the averaged relative sds to the base data, calculate absolute
# uncertainties
df_unc <- df_unc_windows %>% 
  left_join(df_rel_unc, by = "time_window") %>%
  # Calculate absolute uncertainties for calibration
  mutate(c_unc = rel_unc * mean_c * 1000) %>% 
  select(c_unc)

# NOTE: The above uncertainty is not currently used. We reverted back to the
# legacy preset 200 uncertainty for now to keep the uncertainties in line with
# the other datasets with questionable preset uncertainties. The uncertainty
# above could be added to the calibration once the uncertainties are rethought.
df_unc <- df_base %>%
  mutate(c_unc = 200) %>% 
  select(c_unc)


# Initial values: calculate initial value for each species
df_init_specs <- LI %>%
  filter(ORIGIN %in% db &
           SPECIES %in% spec) %>%
  # Same species exist with different litter types 
  select(SPECIES, LITTERTYPE1, A_A0, W_A0, E_A0, R_A0) %>%
  # Multiple values for each species, take average
  group_by(LITTERTYPE1, SPECIES) %>%
  summarise(
    A_init = mean(A_A0),
    W_init = mean(W_A0),
    E_init = mean(E_A0),
    N_init = mean(R_A0)
  ) 

# Initial values: write initial values to each measurement
df_init <- df_base %>% 
  left_join(df_init_specs, by = c("SPECIES", "LITTERTYPE1")) %>% 
  mutate(H_init = 0) %>%
  select(A_init, W_init, E_init, N_init, H_init)


# Precipitation (Ryf 0.3.0)
df_prec <- S %>%
  filter(
    ORIGIN %in% db &
      SITE %in% site
  ) %>%
  mutate(prec = pmap_dbl(select(., P01:P12), sum)) %>% 
  right_join(df_base, by = c("SITE")) %>% 
  select(prec)


# Temperature (Ryf 0.3.0)
df_temp <- S %>%
  filter(
    ORIGIN %in% db &
      SITE %in% site
  ) %>%
  right_join(df_base, by = c("SITE")) %>% 
  select(T01:T12)
# Modify column names
colnames(df_temp) <- gsub("T","T_",colnames(df_temp))


# -------------------------------------------------------------------------
# Prepare additional data not read from the database: litter and woody size

# Litter input
litter <- matrix(rep(0, len = nrow(df_obs) * 5), nrow = nrow(df_obs))
colnames(litter) <- c("A_in", "W_in", "E_in", "N_in", "H_in")
df_litter <- as_tibble(litter)

# Woody size
df_size <- tibble(size = integer(nrow(df_obs)))


# -------------------------------------------------------------------------
# Create a tibble with the full dataset

# Combine the data
df_all <- df_base %>% 
  select(period) %>%
  # select(SITE, SPECIES, period) %>%
  bind_cols(
    df_time, df_temp, df_prec, df_init, df_litter, df_size, df_obs, df_unc
  )


# -------------------------------------------------------------------------
# Create a training (80 %) and testing (20 %) set 

# Seed for reproducability
set.seed(555)

# Fraction of measurement groups (averaged observations) which should be used as
# training data
train_frac <- 0.80

# Training set
df_80 <- df_all %>%
  group_by(period) %>%
  nest() %>% 
  ungroup() %>% 
  # Take the given fraction of measurement periods
  sample_frac(train_frac) %>% 
  arrange(period) %>% 
  unnest(cols = data)

# Testing set
df_20 <- df_all %>%
  # Take periods not used for training
  filter(!(period %in% pull(df_80, period))) %>% 
  select(period, !period)

# Check validity, should return TRUE
df_80 %>%
  bind_rows(df_20) %>%
  all_equal(df_all)


# -------------------------------------------------------------------------
# Save

# Save the full dataset
df_all %>% 
  write_csv(paste0(path_out, "full_data_", tolower(db), ".csv"))

# Save the 80 % training set
df_80 %>%
  write_csv(paste0(path_out, "train_data_", tolower(db), ".csv"))

# Save the 20 % testing set
df_20 %>%
  write_csv(paste0(path_out, "test_data_", tolower(db), ".csv"))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Earlier weather dataformat (annual temperature approximation) below

# # Weather: calculate weather inputs
# df_clim_stats <- S %>%
#   filter(
#     ORIGIN %in% db &
#       SITE %in% site
#   ) %>%
#   mutate(
#     # T_mean = pmap_dbl(select(., T01:T12), lift_vd(mean)), # calculate
#     T_mean = MAT, # table value
#     T_min = pmap_dbl(select(., T01:T12), min),
#     T_max = pmap_dbl(select(., T01:T12), max),
#     prec = pmap_dbl(select(., P01:P12), sum),
#     T_amp = (T_max - T_min) / 2
#   ) %>%
#   select(SITE, T_mean, prec, T_amp, T_max, T_min) 
# 
# 
# # Weather: match weather data to measurements
# df_weather <- df_base %>%
#   left_join(df_clim_stats, by = c("SITE")) %>%
#   select(T_mean, prec, T_amp)
