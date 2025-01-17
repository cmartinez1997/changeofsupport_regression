ggplot(dat_all_seas, aes(x = year, y = RW, color = TRE_CN, group = TRE_CN)) +
  geom_line(size = 1) +                     # Add time-series lines
  geom_point(size = 2) +                    # Add points for clarity
  labs(
    title = "Time Series of Ring Width (RW) Grouped by TRE_CN",
    x = "Year",
    y = "RW",
    color = "TRE_CN"
  ) +
  theme_minimal() +                         # Minimal theme for aesthetics
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  )



dat_23T71 <- subset(dat_all_seas, TRE_CN == "23T71")

# Plot RW as a time series for TRE_CN = "23R1"
ggplot(dat_23T71, aes(x = year, y = RW)) +
  geom_line(color = "#caa", size = 1) +    # Soft pastel blue for the line
  geom_point(color = "cadetblue", size = 2) +    # Soft pastel pink for the points
  labs(
    title = "Time Series of Ring Width (RW) for TRE_CN = '23R1'",
    x = "Year",
    y = "RW"
  ) +
  theme_minimal() +                            # Minimal theme for aesthetics
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5D5C61"),  # Soft gray title
    axis.title = element_text(size = 12, color = "#5D5C61"),               # Soft gray axis labels
    axis.text = element_text(size = 10, color = "#5D5C61")                 # Soft gray axis text
  )

library(ggplot2)

# Data for dbh values over time
dbh_data <- data.frame(
  year = c(2003, 2013, 2023),
  dbh = c(12.1, 12.2, 12.5)
)


ggplot() +
  # Primary axis: RW
  geom_line(data = dat_23T71, aes(x = year, y = RW), color = "#caa", size = 1) +
  geom_point(data = dat_23T71, aes(x = year, y = RW), color = "cadetblue", size = 2) +
  # Secondary axis: DBH
  geom_line(data = dbh_data, aes(x = year, y = dbh), color = "darkred", size = 1) +
  geom_point(data = dbh_data, aes(x = year, y = dbh), color = "darkorange", size = 2) +
  # Define scales
  scale_y_continuous(
    name = "Ring Width (RW)",  # Primary axis label
    sec.axis = sec_axis(~ ., name = "DBH")  # Secondary axis label, independent scale
  ) +
  labs(
    title = "Time Series of RW and DBH for TRE_CN = '23R1'",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5D5C61"),
    axis.title.y = element_text(size = 12, color = "#5D5C61"),  # Primary axis title
    axis.title.y.right = element_text(size = 12, color = "darkred"),  # Secondary axis title
    axis.title.x = element_text(size = 12, color = "#5D5C61"),
    axis.text = element_text(size = 10, color = "#5D5C61"),
    axis.text.y.right = element_text(color = "darkred")  # Color secondary axis ticks
  )




library(ggplot2)

# Create the plot
ggplot() +
  # Primary axis: RW
  geom_line(data = dat_23T71, aes(x = year, y = RW), color = "#caa", size = 1) +
  geom_point(data = dat_23T71, aes(x = year, y = RW), color = "cadetblue", size = 2) +
  # Secondary axis: DBH
  geom_line(data = dbh_data, aes(x = year, y = (dbh - 11) * (max(dat_23T71$RW) / 2)), 
            color = "darkred", size = 1) +  # Rescale DBH to align visually with RW
  geom_point(data = dbh_data, aes(x = year, y = (dbh - 11) * (max(dat_23T71$RW) / 2)), 
             color = "darkorange", size = 2) +
  # Define y-axis scales
  scale_y_continuous(
    name = "Ring Width (RW)",                          # Primary axis label
    limits = c(0, max(dat_23T71$RW) * 1.2),           # RW limits
    sec.axis = sec_axis(
      trans = ~ . / (max(dat_23T71$RW) / 2) + 11,     # Rescale DBH back to its original range
      name = "DBH (inches)"                           # Secondary axis label
    )
  ) +
  labs(
    title = "Time Series of RW and DBH for TRE_CN = '23R1'",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5D5C61"),
    axis.title.y = element_text(size = 12, color = "#5D5C61"),       # Primary axis title
    axis.title.y.right = element_text(size = 12, color = "darkred"), # Secondary axis title
    axis.title.x = element_text(size = 12, color = "#5D5C61"),
    axis.text.y.right = element_text(color = "darkred"),             # Secondary axis ticks color
    axis.text = element_text(size = 10, color = "#5D5C61")
  )

library(ggplot2)
library(dplyr)

# Truncate data to years >= 1990
dat_23T71_truncated <- dat_23T71 %>% filter(year >= 1990)
dbh_data_truncated <- dbh_data %>% filter(year >= 1990)

# Create the plot
ggplot() +
  # Primary axis: RW
  geom_line(data = dat_23T71_truncated, aes(x = year, y = RW), color = "#A9BA9D", size = 1) +
  geom_point(data = dat_23T71_truncated, aes(x = year, y = RW), color = "cadetblue", size = 2) +
  # Secondary axis: DBH
  geom_line(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2)), 
            color = "darkred", size = 1) +  # Rescale DBH to align visually with RW
  geom_point(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2)), 
             color = "darkorange", size = 2) +
  # Define y-axis scales
  scale_y_continuous(
    name = "Ring Width (mm)",                          # Primary axis label
    limits = c(0, max(dat_23T71_truncated$RW) * 1.2),  # RW limits
    sec.axis = sec_axis(
      trans = ~ . / (max(dat_23T71_truncated$RW) / 2) + 11,  # Rescale DBH back to its original range
      name = "DBH (in)"                                # Secondary axis label
    )
  ) +
  labs(
    title = "Tree Ring vs Forest Inventory",
    x = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5D5C61"),
    axis.title.y = element_text(size = 12, color = "#5D5C61"),       # Primary axis title
    axis.title.y.right = element_text(size = 12, color = "darkred"), # Secondary axis title
    axis.title.x = element_text(size = 12, color = "#5D5C61"),
    axis.text.y.right = element_text(color = "darkred"),             # Secondary axis ticks color
    axis.text = element_text(size = 10, color = "#5D5C61")
  )




library(ggplot2)
library(dplyr)

# Create the plot with a box around it
ggplot() +
  # Primary axis: RW
  geom_line(data = dat_23T71_truncated, aes(x = year, y = RW), color = "#A9BA9D", size = 1) +
  geom_point(data = dat_23T71_truncated, aes(x = year, y = RW), color = "cadetblue", size = 2) +
  # Secondary axis: DBH
  geom_line(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2)), 
            color = "darkred", size = 1) +
  geom_point(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2)), 
             color = "darkorange", size = 2) +
  # Define y-axis scales
  scale_y_continuous(
    name = "Ring Width (mm)",                          
    limits = c(0, max(dat_23T71_truncated$RW) * 1.2),  
    sec.axis = sec_axis(
      trans = ~ . / (max(dat_23T71_truncated$RW) / 2) + 11,  
      name = "DBH (in)"
    )
  ) +
  labs(
    title = "Tree Ring vs Forest Inventory",
    x = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5D5C61"),
    axis.title.y = element_text(size = 12, color = "#5D5C61"),
    axis.title.y.right = element_text(size = 12, color = "darkred"),
    axis.title.x = element_text(size = 12, color = "#5D5C61"),
    axis.text.y.right = element_text(color = "darkred"),
    axis.text = element_text(size = 10, color = "#5D5C61"),
    # Add a box around the entire plot
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Black border
  )





ggplot() +
  # Primary axis: RW
  geom_line(data = dat_23T71_truncated, aes(x = year, y = RW), color = "#A9BA9D", size = 1.5) +
  geom_point(data = dat_23T71_truncated, aes(x = year, y = RW), color = "cadetblue", size = 3) +  # Slightly larger points
  # Secondary axis: DBH
  geom_line(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2)), 
            color = "darkred", size = 1.5) +
  geom_point(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2)), 
             color = "darkorange", size = 3) +  # Slightly larger points
  # Define y-axis scales
  scale_y_continuous(
    name = "Ring Width (mm)",                          
    limits = c(0, max(dat_23T71_truncated$RW) * 1.2),  
    sec.axis = sec_axis(
      trans = ~ . / (max(dat_23T71_truncated$RW) / 2) + 11,  
      name = "DBH (in)"
    )
  ) +
  labs(
    title = "Tree Ring vs Forest Inventory",
    x = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, color = "#5D5C61"),  # Larger title
    axis.title.y = element_text(size = 14, color = "#5D5C61"),            # Larger y-axis title
    axis.title.y.right = element_text(size = 14, color = "darkred"),      # Larger secondary y-axis title
    axis.title.x = element_text(size = 14, color = "#5D5C61"),            # Larger x-axis title
    axis.text.y = element_text(size = 12, color = "#5D5C61"),             # Larger y-axis tick labels
    axis.text.y.right = element_text(size = 12, color = "darkred"),       # Larger secondary y-axis tick labels
    axis.text.x = element_text(size = 12, color = "#5D5C61"),             # Larger x-axis tick labels
    panel.border = element_rect(color = "black", fill = NA, size = 1)     # Black border around the plot
  )



ggplot() +
  # Primary axis: RW (Tree Ring Time Series)
  geom_line(data = dat_23T71_truncated, aes(x = year, y = RW, color = "Tree Ring Time Series"), size = 1.5) +
  geom_point(data = dat_23T71_truncated, aes(x = year, y = RW, color = "Tree Ring Time Series"), size = 3) +
  # Secondary axis: DBH (Forest Inventory)
  geom_line(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2), color = "Forest Inventory"), size = 1.5) +
  geom_point(data = dbh_data_truncated, aes(x = year, y = (dbh - 11) * (max(dat_23T71_truncated$RW) / 2), color = "Forest Inventory"), size = 3) +
  # Define y-axis scales
  scale_y_continuous(
    name = "Ring Width (mm)",                          
    limits = c(0, max(dat_23T71_truncated$RW) * 1.2),  
    sec.axis = sec_axis(
      trans = ~ . / (max(dat_23T71_truncated$RW) / 2) + 11,  
      name = "DBH (in)"
    )
  ) +
  # Manually define legend colors and labels
  scale_color_manual(
    name = "Legend",  # Legend title
    values = c("Tree Ring Time Series" = "#A9BA9D", "Forest Inventory" = "darkred"),
    labels = c("Tree Ring Time Series", "Forest Inventory")
  ) +
  labs(
    title = "Tree Ring vs Forest Inventory",
    x = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, color = "#5D5C61"),  # Larger title
    axis.title.y = element_text(size = 14, color = "#5D5C61"),            # Larger y-axis title
    axis.title.y.right = element_text(size = 14, color = "darkred"),      # Larger secondary y-axis title
    axis.title.x = element_text(size = 14, color = "#5D5C61"),            # Larger x-axis title
    axis.text.y = element_text(size = 12, color = "#5D5C61"),             # Larger y-axis tick labels
    axis.text.y.right = element_text(size = 12, color = "darkred"),       # Larger secondary y-axis tick labels
    axis.text.x = element_text(size = 12, color = "#5D5C61"),             # Larger x-axis tick labels
    legend.title = element_text(size = 14),                               # Larger legend title
    legend.text = element_text(size = 12),                                # Larger legend text
    legend.position = "top",                                              # Place legend at the top
    panel.border = element_rect(color = "black", fill = NA, size = 1)     # Black border around the plot
  )



ggplot() +
  # Primary axis: RW
  geom_line(data = dat_23T71_truncated, aes(x = year, y = RW), color = "#A9BA9D", size = 2) +
  geom_point(data = dat_23T71_truncated, aes(x = year, y = RW), color = "cadetblue", size = 3) + 
  labs(
    x = "Year",
    y = "Ring Width (mm)" # Ensure y-axis label is properly added
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "#5D5C61"),  # Title size
    axis.title.y = element_text(size = 16, color = "#5D5C61"),             # Y-axis title size
    axis.title.x = element_text(size = 16, color = "#5D5C61"),             # X-axis title size
    axis.text = element_text(size = 14, color = "#5D5C61")                 # Tick labels size
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1) # Add border around the whole plot
  )


