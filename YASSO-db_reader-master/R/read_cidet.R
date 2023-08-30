# Reads CIDET data and saves it to results/cidet


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
path_out <- "results/cidet/"

# Dataset name
db <- "CIDET"
# Sites
site <- c("BAT", "CBR", "CHA", "GI1", "GI2", "HID", "INU", "KAN", "MAR", "MON",
          "NH1", "NH2", "PAL", "PET", "PMC", "SCH", "SHL", "TOP", "WHI")
# Species
spec <- c("WBIR", "CEDA", "DFIR", "BSPR", "ASPN", "JPIN", "BEEC", "TAMM",
          "FESC", "FERN")
# Uncertainty is a set value
uc <- 100


# -------------------------------------------------------------------------
# Import data

# Load the litter database
data_in <- read_litter_data(path_db)

# Save Excel sheets to variables
S <- data_in$sites
LT <- data_in$litter_time
LI <- data_in$litter_init


# -------------------------------------------------------------------------
# Shape data imported from the database

# Basic filtering and shaping, used for everything that follows
df_base <- LT %>%
  filter(
    ORIGIN %in% db &
      SITE %in% site &
      SPECIES %in% spec
  ) %>%
  # Add the measurement period (in CIDET each period has 6 measurements)
  mutate(
    period = rep(1:(nrow(.) / 6), each = 6)
  ) %>% 
  select(ORIGIN, SPECIES, OBS, SITE, TIMEOUT, MREM_A0, period) %>% 
  arrange(OBS)


# Timeouts
df_time <- df_base %>% 
  select(TIMEOUT) %>% 
  rename(time = TIMEOUT)


# Observations
df_obs <- df_base %>% 
  mutate(c_obs = MREM_A0 * 1000) %>% 
  select(c_obs)


# Uncertainties
df_unc <- df_base %>%
  mutate(c_unc = uc) %>% 
  select(c_unc)


# Initial values: find initial value for each species
df_init_specs <- LI %>%
  filter(ORIGIN %in% db &
           SPECIES %in% spec) %>%
  select(SPECIES, A_init = A_A0, W_init = W_A0, E_init = E_A0, R_init = R_A0)

# Initial values: write initial values to each measurement
df_init <- df_base %>% 
  left_join(df_init_specs, by = "SPECIES") %>% 
  mutate(H_init = 0) %>% 
  select(A_init, W_init, E_init, N_init = R_init, H_init)


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
  select(obs = OBS, period) %>% 
  bind_cols(
    df_time, df_temp, df_prec, df_init, df_litter, df_size, df_obs, df_unc
  )


# -------------------------------------------------------------------------
# Create a training (80 %) and testing (20 %) set 

# Seed for reproducability
set.seed(111)

# The fraction of measurement periods which should be used as training data
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
  arrange(period) %>% 
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