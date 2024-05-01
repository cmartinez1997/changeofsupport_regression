## This script is to make the climate dataframe for climate-tree ring growth analyses - including seasonal climate variables
## Cecilia Martinez 
## Jan 25 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(tidyverse) # for data wrangling
library(here)

# load in wbp tree ring data and associated metadata ----------------------

wbp_rw <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark_rw.txt"))
wbp_meta <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark.txt"))


# load in the functions to add climate normals and also biologically significant growth years 

source(here::here("tree-H", "R", "make_climate_functions.R")) 

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe

wbp_rw <- left_join(wbp_rw, wbp_meta)
wbp_rw <- wbp_rw %>% select(CN, TRE_CN, PLT_CN, Year, RW)
wbp_rw <- wbp_rw %>% filter(!is.na(TRE_CN) & !is.na(PLT_CN))
wbp_rw <- wbp_rw %>% select(-CN)

length(unique(wbp_rw$TRE_CN)) #219 unique trees
 
# truncate the years so that they only span the years of climate data that we have, so 1896 onwards
#wbp_rw <- wbp_rw %>%
#  filter(Year >= 1896), don't do this actually

wbp_rw$TRE_CN <- as.character(wbp_rw$TRE_CN)
wbp_rw$PLT_CN <- as.character(wbp_rw$PLT_CN)


# load in climate data ----------------------------------------------------

ppt <- read_csv(here::here("tree-H", "data", "raw", "ppt_extr.csv"))
tmax <- read_csv(here::here("tree-H", "data","raw", "tmax_extr.csv"))
tmin <- read_csv(here::here("tree-H", "data", "raw", "tmin_extr.csv"))

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

climate_dat <- left_join(tmin_long, tmax_long, by = c("PLT_CN", "year", "month")) %>%
  left_join(., ppt_long, by = c("PLT_CN", "year", "month"))

#reorder columns and get rid of 1895 growth year
climate_dat <- climate_dat %>%
  select(PLT_CN, year, month, tmin, tmax, ppt) %>% 
  filter(!(year == 1895 & month >= 1 & month <= 8))

climate_dat$PLT_CN <- as.character(climate_dat$PLT_CN)
## making seasonal climate variables, refer to climate-growth analyses --> do this at some point, have not done yet


## check to make sure that the PLT_CN matches in both dfs (climate and ring width) and filter out the rows for which there isn't a match

unique_climate <- unique(climate_dat$PLT_CN)
unique_wbp <- unique(wbp_rw$PLT_CN)

plots_match <- unique(climate_dat$PLT_CN) %>% 
  intersect(unique(wbp_rw$PLT_CN)).  #161 plots match

wbp_rw <- wbp_rw %>% 
  filter(PLT_CN %in% plots_match)

# add climate norms and growth year (biologically significant years) to df -------------------------------------------------

## note this will require the make_clim_norm function
climate_all <- climate_growthyear(climate_dat)

## adding climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2000)
as.data.frame(norms)

## join climate normals to new_climate
climate_all <- climate_all %>% 
  left_join(norms, by = "PLT_CN")

# write and export as csvs --------------------------------------------------------------

write_csv(wbp_rw, "tree-H/data/processed/climate_growth_rw.csv")
write_csv(climate_all, "tree-H/data/processed/climate_data_all.csv")

