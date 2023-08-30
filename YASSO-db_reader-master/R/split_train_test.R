# A stand-alone script for splitting the data into training and testing set

# The split is already done in the read_<dataset> scripts for each dataset, but
# this is an alternative way to do it. Here it is easy to define which variable
# to split the dataset by.

# NOTE: This file is not part of the main functionality, so it might not be
# updated to reflect newest changes and might require some tweaks.

# -------------------------------------------------------------------------
# Libraries

# Importing, saving
library(readr)
# Nesting
library(tidyr)
# Sampling, shaping
library(dplyr)
# Symbolize split variable (could also use base as.symbol)
library(rlang)


# -------------------------------------------------------------------------
# User-defined variables

# Path to the results folder
path_res <- "results/"

# Dataset to be imported ("ed1", "ed2", "cidet", "lidet", "gss", "tarasov",
# "makinen")
dataset <- "gss"

# Variable (column name) to split the data by
split_variable <- "obs"

# Fraction of the data which should be used as training data
frac_train <- 0.80

# Seed for sampling reproducability
set.seed(42)


# -------------------------------------------------------------------------
# Import the full dataset

df_full <- read_csv(paste0(path_res, dataset, "/full_data_", dataset, ".csv"))


# -------------------------------------------------------------------------
# Split the data into training and testing set by given variable

# Training set
df_train <- df_full %>%
  # Group and nest the data by the given variable
  group_by(!!sym(split_variable)) %>%
  nest() %>%
  ungroup() %>%
  # Sample the given fraction into training set
  sample_frac(frac_train) %>%
  unnest(cols = data)

# Testing set
df_test <- df_full %>%
  # Use the measurements not sampled into the training set
  filter(
    !(!!sym(split_variable) %in% pull(df_train, !!sym(split_variable)))
  ) %>%
  select(!!sym(split_variable), !(!!sym(split_variable)))

# Check validity, should return TRUE
df_train %>%
  bind_rows(df_test) %>%
  all_equal(df_full)


# -------------------------------------------------------------------------
# Save 

# Save the training set
# df_train %>%
#   write_csv(paste0(path_res, dataset, "/train_data_", dataset, ".csv"))

# Save the testing set
# df_test %>%
#   write_csv(paste0(path_res, dataset, "/test_data_", dataset, ".csv"))
