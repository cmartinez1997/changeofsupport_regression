## This script is to make the climate dataframe for climate-tree ring growth analyses - including seasonal climate variables
## Cecilia Martinez 
## Jan 25 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling

# read in tree ring data and associated metadata from Justin DeRose --------

wbp_rw <- read_csv("tree-H/data/T_iwfia_whitebark_rw.txt")
wbp_meta <- read_csv("tree-H/data/T_iwfia_whitebark.txt")

# need to add tre and plot CN to the rw dataframe

wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_rw <- wbp_rw %>% select(CN, TRE_CN, PLT_CN, Year, RW)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN))
wbp_rw <- wbp_rw %>% select(-CN)

length(unique(wbp_rw$TRE_CN)) #219 unique trees

# load in climate data ----------------------------------------------------

ppt <- read_csv("tree-H/data/ppt_extr.csv")
tmax <- read_csv("tree-H/data/tmax_extr.csv")
tmin <- read_csv("tree-H/data/tmin_extr.csv")

# turn climate data into long form and make new columns for year and month from column headers when the file is in wide format

tmin_long <- tmin %>%
  pivot_longer(cols = starts_with("tmin_"), names_to = "variable", values_to = "tmin") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  select(-variable, -CORE_CN, -TRE_CN)

tmax_long <- tmax %>%
  pivot_longer(cols = starts_with("tmax_"), names_to = "variable", values_to = "tmax") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  select(-variable, -CORE_CN, -TRE_CN)

ppt_long <- ppt %>% 
  pivot_longer(cols = starts_with("ppt_"), names_to = "variable", values_to = "ppt") %>%
  mutate(year = as.integer(substr(variable, 5, 8)),
         month = as.integer(substr(variable, 9, 10))) %>%
  select(-variable, -CORE_CN, -TRE_CN)



# Combine the data frames into one climate dataframe
tmin_long <- distinct(tmin_long, PLT_CN, year, month, .keep_all = TRUE)
tmax_long <- distinct(tmax_long, PLT_CN, year, month, .keep_all = TRUE)
ppt_long <- distinct(ppt_long, PLT_CN, year, month, .keep_all = TRUE)

climate_all <- left_join(tmin_long, tmax_long, by = c("PLT_CN", "year", "month")) %>%
  left_join(., ppt_long, by = c("PLT_CN", "year", "month"))
## get help on this 


#reorder columns
climate_all <- climate_all %>%
  select(PLT_CN, year, month, tmin, tmax, ppt)

## making seasonal climate variables, refer to climate-growth analyses




# write csvs --------------------------------------------------------------

write_csv(wbp_rw, "tree-H/data/climate_growth_rw.csv")
write_csv(climate_all, "tree-H/data/climate_data_all.csv")

