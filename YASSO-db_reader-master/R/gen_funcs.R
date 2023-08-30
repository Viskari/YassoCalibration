# Generic functions for importing databases


read_litter_data <- function(filename) {
  # Loads the litter database
  # IN: path to the database
  # OUT: list of relevant sheets in the database
  
  # Load sheet with site information
  sites <- read_xlsx(
    filename, 
    sheet = "Sites",
    col_types = c("text", "numeric", "text", "text", rep("numeric", 31))
    )
  
  # Load sheet with initial litter information
  litter_init <- read_xlsx(
    filename,
    sheet = "Litter_initial",
    col_types = c(
      "text", "numeric", "text", "text", 
      "skip","text", "text", "skip", "skip", 
      rep("numeric", 5), "skip", rep("numeric", 16)
      )
    )
  
  # Load sheet with litter measurements
  litter_time <- read_xlsx(
    filename, 
    sheet = "Litter_time",
    col_types = c(
      "text", "numeric", rep("text", 8), rep("numeric", 21), rep("text", 3)
      )
    )
  
  # Return as list
  list(sites = sites, litter_init = litter_init, litter_time = litter_time)
}


read_ss_data <- function(filename) {
  # Loads the steady state database
  # IN: path to the database
  # OUT: list of relevant sheets in the database
  
  # Load sheet with measurements
  ss_data <- read_xlsx(
    filename,
    sheet = "DATA",
    col_types = c(
      "text", rep("numeric", 2), rep("skip", 5), "text", 
      "skip", rep("numeric", 25), rep("skip", 5)
    )
  )
  
  # Load sheet with class-specific data
  ss_classes <- read_xlsx(
    filename,
    sheet = "CLASSES",
    col_types = c("text", rep("skip", 4), rep("numeric", 19)),
    na = "NA"
  )
  
  # Return as list
  list(ss_data = ss_data, ss_classes = ss_classes)
}
