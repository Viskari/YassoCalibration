# Reads Hob3 data and saves it to results/hob3


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
path_out <- "results/hob3/"

# Dataset name
db <- "HOB_3"

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
  filter(ORIGIN == db) %>% 
  # Add the measurement period (in HOB_3 each period has 6 measurements)
  mutate(
    period = rep(1:(nrow(.) / 6), each = 6)
  ) %>% 
  select(OBS, TIME, SITE, SPECIES, TIMEOUT, MREM_A1, period) %>% 
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


# Precipitation
df_prec <- S %>% 
  filter(ORIGIN == db) %>%
  mutate(prec = rowSums(select(., P01:P12))) %>%
  select(SITE, prec) %>% 
  # remove duplicates
  distinct() %>%
  right_join(df_base, by = "SITE") %>%
  select(prec)


# Temperature
df_temp <- S %>% 
  filter(ORIGIN == db) %>%
  select(SITE, T01:T12) %>% 
  # remove duplicates
  distinct() %>%
  right_join(df_base, by = "SITE") %>%
  select(T01:T12)
# Modify column names
colnames(df_temp) <- gsub("T","T_",colnames(df_temp))


# -------------------------------------------------------------------------
# Prepare additional data not read from the database: litter input and woody size

# Litter input
litter <- matrix(rep(0, len = nrow(df_base) * 5), nrow = nrow(df_base))
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
set.seed(888)

# The fraction of periods which should be used as training data
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
  arrange(obs) %>%
  all_equal(df_all)


# -------------------------------------------------------------------------
# Save

# Save the full dataset
df_all %>% 
  write_csv(paste0(path_out, "full_data_", gsub("_", "", tolower(db)), ".csv"))

# Save the 80 % training set
df_80 %>% 
  write_csv(paste0(path_out, "train_data_", gsub("_", "", tolower(db)), ".csv"))

# Save the 20 % testing set
df_20 %>% 
  write_csv(paste0(path_out, "test_data_", gsub("_", "", tolower(db)), ".csv"))
