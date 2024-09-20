##Concatenating individual rwl files


# loading packages --------------------------------------------------------

library(dplyr)
library(tidyr)

rwl_files <- list.files("tree-H/data/raw/rwl", pattern = "\\.rwl$", full.names = TRUE)

rwl_df <- data.frame()
  
for (file in rwl_files) {
  lines <- readLines(file)
  if (length(lines) == 0) next
  cn <- strsplit(lines[1], "\\s+")[[1]][1]  # Get the CN (first element of the first line)
  temp_data <- data.frame()
  for (line in lines) {
    parts <- strsplit(line, "\\s+")[[1]]
    if (length(parts) < 2) next
    start_year <- as.integer(parts[2])        
    measurements <- as.integer(parts[-c(1, 2)])
    measurements <- measurements[measurements != 999]  # Remove 999 values
    
    # Convert measurements to millimeters
    measurements_mm <- measurements / 100
    
    # Create a data frame for the current line
    if (length(measurements_mm) > 0) {
      current_years <- seq(start_year, length.out = length(measurements_mm))
      current_data <- data.frame(
        CN = rep(cn, length(measurements_mm)),  # Use the CN from the first line
        Year = current_years,
        RW = measurements_mm  # Store the converted measurements
      )
      temp_data <- rbind(temp_data, current_data)
    }
  }
  # Append to the main data frame
  rwl_df <- rbind(rwl_df, temp_data)
}


# write to csv ------------------------------------------------------------

output_file <- file.path("tree-H/data/raw", "wbp_new_rwl.csv")
write.csv(rwl_df, output_file, row.names = FALSE)
