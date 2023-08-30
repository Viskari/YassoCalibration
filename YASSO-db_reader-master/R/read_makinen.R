# Reads Mäkinen data and saves it to results/makinen


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
path_out <- "results/makinen/"

# Dataset name
db <- "MAKINEN"

# Uncertainty is a set value
uc <- 250


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
  filter(ORIGIN == db & !(TIMEOUT == 0) & !(LITTERSIZE == -999)) %>% 
  select(OBS, SITE, SPECIES, LITTERSIZE, TIMEOUT, MREM_A1) %>% 
  arrange(OBS)


# Timeouts
df_time <- df_base %>% 
  select(TIMEOUT) %>% 
  rename(time = TIMEOUT)


# Observations
df_obs <- df_base %>% 
  mutate(c_obs = MREM_A1 * 1000) %>% 
  select(c_obs)


# Uncertainties
df_unc <- df_base %>%
  mutate(c_unc = uc) %>% 
  select(c_unc)


# Initial values
df_init <- LI %>% 
  filter(ORIGIN == db) %>%
  right_join(df_base, by = "SPECIES") %>% 
  arrange(OBS) %>%
  select(A_init = A_A0, W_init = W_A0, E_init = E_A0, N_init = R_A0) %>% 
  mutate(H_init = 0)


# Woody size
df_size <- df_base %>% 
  select(size = LITTERSIZE)


# Precipitation
df_prec <- S %>% 
  filter(ORIGIN == db) %>%
  mutate(prec = sum(select(., P01:P12))) %>%
  right_join(df_base, by = "SITE") %>% 
  select(prec)


# Temperature
df_temp <- S %>% 
  filter(ORIGIN == db) %>%
  right_join(df_base, by = "SITE") %>% 
  select(T01:T12)
# Modify column names
colnames(df_temp) <- gsub("T","T_",colnames(df_temp))


# -------------------------------------------------------------------------
# Prepare additional data not read from the database: litter input

# Litter input
litter <- matrix(rep(0, len = nrow(df_base) * 5), nrow = nrow(df_base))
colnames(litter) <- c("A_in", "W_in", "E_in", "N_in", "H_in")
df_litter <- as_tibble(litter)


# -------------------------------------------------------------------------
# Create a tibble with the full dataset

# Combine the data
df_all <- df_base %>% 
  select(obs = OBS) %>% 
  bind_cols(
    df_time, df_temp, df_prec, df_init, df_litter, df_size, df_obs, df_unc
  )


# -------------------------------------------------------------------------
# Create a training (80 %) and testing (20 %) set 

# Seed for reproducability
set.seed(777)

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
  write_csv(paste0(path_out, "full_data_", tolower(db), ".csv"))

# Save the 80 % training set
df_80 %>% 
  write_csv(paste0(path_out, "train_data_", tolower(db), ".csv"))

# Save the 20 % testing set
df_20 %>% 
  write_csv(paste0(path_out, "test_data_", tolower(db), ".csv"))
