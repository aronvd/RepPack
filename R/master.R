# Clear workspace
rm(list = ls())


# Disable automatic output (similar to 'set more off' in Stata)
options(max.print = 10000)

# Define paths
MY_PATH <- "../ReplicationPackage"
DATA_IN <- file.path(MY_PATH, "Data")
DATA_OUT <- file.path(MY_PATH, "out-data")
MY_TAB <- file.path(MY_PATH, "results")
R <- file.path(MY_PATH, "R")

# Create directories if they don't exist
dir.create(DATA_OUT, showWarnings = FALSE)
dir.create(DATA_IN, showWarnings = FALSE)
dir.create(file.path(MY_PATH, "log"), showWarnings = FALSE)
dir.create(MY_TAB, showWarnings = FALSE)

# Disable other paths for adofiles (not applicable in R, but you can set library paths if needed)
# .libPaths(new = c(MY_PATH, .libPaths()))

# Start the log file
#datetime <- format(Sys.time(), "%Y.%m.%d-%H.%M.%S")
#logfile <- file.path(MY_PATH, "log", paste0("v_", datetime, ".log.txt"))
#sink(logfile, append = TRUE)

# Perform main analyses
source(file.path(R, "01_maketables.R"))
#source(file.path(R, "02_makegraphs.R"))
#source(file.path(R, "03_synthetic_control.R"))

# Close the log file
#sink()

# Exit (not needed in R, script ends here)
