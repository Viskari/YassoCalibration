# Reads ED2 data and saves it to results/ED2


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
path_out <- "results/ed2/"

# Dataset name
db <- "ED2"
# Sites
site <- c("BRA", "HAR", "JAD", "MAN", "MAL", "MAS", "NEN", "NOR", "STR", "VOM")
# Species
spec <- c("SP","LP","WB","NS")
# Litter type
type <- c("L")
# Treatment (inclusive)
TR <- c("CONTR")
# TR <- c("I","IF") # old, exclusive
# Uncertainty vector (AWEN)
uc <- c(40, 10, 20, 40)


# -------------------------------------------------------------------------
# Import data

# Load the litter database
data_in <- read_litter_data(path_db)

# Save Excel sheets to variables
S <- data_in$sites
LT <- data_in$litter_time
LI <- data_in$litter_init


# -------------------------------------------------------------------------
# Preprocess

# ED2 data requires some extra filtering compared to ED1 data. ED2 data has some
# redundant measurement periods that lack an initial value.

# Starts of measurement periods
period_starts <- LT %>%
  filter(
    ORIGIN %in% db &
      SITE %in% site &
      SPECIES %in% spec &
      LITTERTYPE1 == type &
      (TREATMENT %in% TR) &
      TIMEOUT == 0
  ) %>%
  select(ORIGIN, OBS, TIMEOUT, A_A1)

# Find redundant periods
period_red <- period_starts %>%
  # Find the first and last OBS of each period
  mutate(nexti = lead(OBS)) %>% 
  select(ORIGIN, OBS, nexti, A_A1) %>%
  mutate(nexti = nexti - 1) %>% 
  # Keep the periods with missing initial values
  filter(is.na(A_A1))

# Note: lead() would not work as such, if the very last period of ED2 had
# missing data. Now, it has data and lead() works.

# Next, we need to filter out all the rows between the OBS and nexti values,
# inclusively

# Create a vector with all the redundant OBS values
obs_red <- period_red %>% 
  select(from = OBS, to = nexti) %>% 
  mutate(indices = map2(from, to, seq)) %>% 
  select(indices) %>% 
  unnest(cols = indices) %>% 
  pull(indices)

# Now we can continue as "normal" using this vector to filter out the redundant
# values


# -------------------------------------------------------------------------
# Shape data imported from the database

# Basic filtering and shaping, used for everything that follows
df_base <- LT %>%
  filter(
    ORIGIN %in% db &
      SITE %in% site &
      SPECIES %in% spec &
      LITTERTYPE1 == type &
      (TREATMENT %in% TR)
  ) %>%
  select(ORIGIN, OBS, SITE, TIMEOUT, MREM_A1, A_A1, W_A1, E_A1, R_A1) %>%
  drop_na() %>% 
  # Filter out the redundant measurement periods
  filter(!(OBS %in% obs_red))

# Identify and mark measurement periods
df_base <- df_base %>% 
  filter(TIMEOUT == 0) %>% 
  select(OBS) %>% 
  mutate(period = 1:nrow(.)) %>% 
  right_join(df_base, by = "OBS") %>%
  arrange(OBS) %>% 
  fill(period, .direction = "down")


# Timeouts
df_time <- df_base %>% 
  select(TIMEOUT) %>% 
  filter(TIMEOUT != 0) %>% 
  rename(time = TIMEOUT)


# Initial values: find the initial values at rows with zero timeout
df_zero_time <- df_base %>%
  select(OBS, TIMEOUT, A_A1:R_A1) %>% 
  filter(TIMEOUT == 0) %>% 
  rename(A_init = A_A1, W_init = W_A1, E_init = E_A1, R_init = R_A1) 

# Initial values: write the initial value to each measurement
df_init <- df_base %>%
  select(OBS, TIMEOUT, A_A1:R_A1) %>% 
  left_join(df_zero_time, by = c("OBS", "TIMEOUT")) %>% 
  fill(c(A_init, W_init, E_init, R_init), .direction = "down") %>% 
  filter(TIMEOUT != 0) %>% 
  mutate(H_init = 0) %>% 
  select(A_init, W_init, E_init, N_init = R_init, H_init)


# Basic filtering and shaping for obs, unc
df_base_obun <- df_base %>%
  filter(TIMEOUT != 0) %>% 
  # Pivot to easily calculate obs and unc for each pool 
  pivot_longer(
    cols = c(A_A1, W_A1, E_A1, R_A1),
    names_pattern = "(.)_*",
    names_to = "pool",
    values_to = "carbon"
  ) %>%
  mutate(c_obs = MREM_A1 * carbon, c_unc = MREM_A1 * uc) %>% 
  select(-carbon)


# Observations
df_obs <- df_base_obun %>% 
  select(OBS, pool, c_obs) %>% 
  # Pivot to save in the nicer wide format
  pivot_wider(
    id_cols = OBS,
    names_from = pool,
    values_from = c_obs
  ) %>% 
  select(A_obs = A, W_obs = W, E_obs = E, N_obs = R)


# Uncertainties
df_unc <- df_base_obun %>% 
  select(OBS, pool, c_unc) %>% 
  # Pivot to save in the nicer wide format
  pivot_wider(
    id_cols = OBS,
    names_from = pool,
    values_from = c_unc
  ) %>% 
  select(A_unc = A, W_unc = W, E_unc = E, N_unc = R)


# Climate (Ryf 0.3.0)
df_clim <- S %>% 
  filter(ORIGIN %in% db &
           SITE %in% site
  ) %>%
  group_by(SITE) %>% 
  # Data has multiple sites with the same name, calculate average over these
  summarise_at(vars(T01:MAT, PTOT), mean) %>% 
  right_join(df_base %>% filter(TIMEOUT != 0), by = c("SITE")) %>% 
  arrange(OBS)


# Precipitation (Ryf 0.3.0)
df_prec <- df_clim %>% 
  select(prec = PTOT)


# Temperature (Ryf 0.3.0)
df_temp <- df_clim %>% 
  select(T01:T12)
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
  filter(TIMEOUT != 0) %>% 
  select(obs = OBS, period) %>%
  bind_cols(
    df_time, df_temp, df_prec, df_init, df_litter, df_size, df_obs, df_unc
  )


# -------------------------------------------------------------------------
# Create a training (80 %) and testing (20 %) set 

# Seed for reproducability
set.seed(333)

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
#   filter(ORIGIN %in% db &
#            SITE %in% site
#   ) %>%
#   group_by(SITE) %>% 
#   # Data has multiple sites with the same name
#   summarise_at(vars(T01:MAT, PTOT), mean) %>% 
#   # Nest temperatures to easily find min and max for each site
#   nest(TEMPS = T01:T12) %>% 
#   mutate(
#     MIN = map(TEMPS, min),
#     MAX = map(TEMPS, max)
#   ) %>% 
#   unnest(cols = c(MIN, MAX)) %>% 
#   # Calculate weather inputs for each site
#   mutate(
#     T_mean = MAT,
#     prec = PTOT,
#     T_amp = (MAX - MIN) / 2
#   ) %>% 
#   select(SITE, T_mean, prec, T_amp)
# 
# # Weather: match climate data to measurements
# df_weather <- df_base %>%
#   filter(TIMEOUT != 0) %>%
#   left_join(df_clim_stats, by = c("SITE")) %>% 
#   select(T_mean, prec, T_amp)
