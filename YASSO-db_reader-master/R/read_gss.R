# Reads GSS data and saves it to results/gss

# This is slightly different to most other readers, since the data in GSS
# consists of global steady states.

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
path_db <- "data/SOC.xlsx"

# Define folder for saving outgoing data
path_out <- "results/gss/"

# Dataset name (for saving)
db <- "GSS"

# Uncertainty is a set value
uc <- 7.5

# Olson codes to be excluded
olson <- c(0:19, 36, 37, 39, 45, 56)
  

# -------------------------------------------------------------------------
# Import data

# Load the steady state database
data_in <- read_ss_data(path_db)

# Save Excel sheets to variables
ss_data <- data_in$ss_data
ss_classes <- data_in$ss_classes


# -------------------------------------------------------------------------
# Shape data

# Basic filtering and shaping, used for everything that follows
df_base <- ss_data %>%
  filter(
    GPP != 0 &
      CARBON != 0 &
      CARBON <= 250 & # removes 17 "unrealistic" observations
      !(OLSON %in% olson)
  ) %>% 
  select(CARBON, GPP, OLSON, T_1:T_12, P_1:P_12)


# Observations
df_obs <- df_base %>% 
  select(c_obs = CARBON)


# Uncertainties
df_unc <- df_base %>% 
  mutate(c_unc = uc) %>% 
  select(c_unc)


# Litter input: There are 3 AWEN rows of litter inputs for each observation
df_litter <- df_base %>%
  left_join(ss_classes, by = c("OLSON" = "ECOSYSTEM CODE")) %>%
  select(CARBON, GPP, OLSON, `NPP/GPP`:R_LARGEW) %>%
  mutate(
    # The NPP/GPP multiplier here is always 0.5 in the data
    NPP = GPP * `NPP/GPP`
  ) %>%
  mutate_at(c("A_NONW", "W_NONW", "E_NONW", "R_NONW"), ~ . * NONW * NPP) %>%
  mutate_at(c("A_SMALLW", "W_SMALLW", "E_SMALLW", "R_SMALLW"), ~ . * SMALLW * NPP) %>%
  mutate_at(c("A_LARGEW", "W_LARGEW", "E_LARGEW", "R_LARGEW"), ~ . * LARGEW * NPP) %>%
  select(A_NONW:R_LARGEW) %>%
  rename(
    "A_in_nonw" = "A_NONW",
    "W_in_nonw" = "W_NONW",
    "E_in_nonw" = "E_NONW",
    "N_in_nonw" = "R_NONW",
    "A_in_smallw" = "A_SMALLW",
    "W_in_smallw" = "W_SMALLW",
    "E_in_smallw" = "E_SMALLW",
    "N_in_smallw" = "R_SMALLW",
    "A_in_largew" = "A_LARGEW",
    "W_in_largew" = "W_LARGEW",
    "E_in_largew" = "E_LARGEW",
    "N_in_largew" = "R_LARGEW"
  ) %>%
  # Create columns for H values (zero)
  mutate(
    H_in_nonw = 0,
    H_in_smallw = 0,
    H_in_largew = 0
  ) %>%
  # Rearrange columns
  select(
    A_in_nonw:N_in_nonw, H_in_nonw,
    A_in_smallw:N_in_smallw, H_in_smallw,
    A_in_largew:N_in_largew, H_in_largew
  ) %>% 
  # Convert units from gC/m^2 to kgC/m^2 to match observations and uncertainties
  mutate_all(~ . / 1000)
  

# Woody size (each time step has three sizes)
df_size <- df_base %>%
  left_join(ss_classes, by = c("OLSON" = "ECOSYSTEM CODE")) %>%
  select(SIZE_NONW:SIZE_LARGEW) %>% 
  rename_all(tolower)


# Precipitation (Ryf 0.3.0)
df_prec <- df_base %>% 
  mutate(prec = pmap_dbl(select(., P_1:P_12), sum)) %>% 
  select(prec)


# Temperature (Ryf 0.3.0)
df_temp <- df_base %>% 
  select(T_1:T_12)
# Modify column names
colnames(df_temp) <- c(paste0("T_0", 1:9), paste0("T_", 10:12))


# -------------------------------------------------------------------------
# Prepare additional data not read from the database: times and initial values

# Times (anything non-zero is fine here)
df_time <- tibble(time = 1L, .rows = nrow(df_obs))

# Inits (everything is zero)
inits <- matrix(rep(0, len = nrow(df_obs) * 5), nrow = nrow(df_obs))
colnames(inits) <- c("A_init", "W_init", "E_init", "N_init", "H_init")
df_init <- as_tibble(inits)


# -------------------------------------------------------------------------
# Create a tibble with the full dataset

# Combine the data
df_all <- bind_cols(
  df_time, df_temp, df_prec, df_init, df_litter, df_size, df_obs, df_unc
) %>% 
  # Create arbitrary obs to help keep track of data and split it to train/test
  mutate(obs = 1:nrow(.)) %>% 
  select(obs, !obs)


# -------------------------------------------------------------------------
# Create a training (80 %) and testing (20 %) set 

# Seed for reproducability
set.seed(444)

# The fraction of observations which should be used as training data
train_frac <- 0.80

# Training set
df_80 <- df_all %>%
  # Take the given fraction of observations
  sample_frac(train_frac)

# Testing set
df_20 <- df_all %>%
  # Take observations not used for training
  filter(!(obs %in% pull(df_80, obs)))

# Check validity, should return TRUE
df_80 %>%
  bind_rows(df_20) %>%
  arrange(obs) %>%
  all_equal(df_all)


# -------------------------------------------------------------------------
# Save

# Save the full dataset
df_all %>% 
  write_csv(paste0(path_out, "full_data25_", tolower(db), ".csv"))

# Save the 80 % training set
df_80 %>%
  write_csv(paste0(path_out, "train_data25_", tolower(db), ".csv"))

# Save the 20 % testing set
df_20 %>%
  write_csv(paste0(path_out, "test_data25_", tolower(db), ".csv"))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Earlier weather dataformat (annual temperature approximation) below


# # Weather: calculate weather inputs
# df_weather <- df_base %>% 
#   mutate(
#     T_mean = pmap_dbl(select(., T_1:T_12), lift_vd(mean)),
#     T_min = pmap_dbl(select(., T_1:T_12), min),
#     T_max = pmap_dbl(select(., T_1:T_12), max),
#     prec = pmap_dbl(select(., P_1:P_12), sum),
#     T_amp = (T_max - T_min) / 2
#   ) %>% 
#   select(T_mean, prec, T_amp)
