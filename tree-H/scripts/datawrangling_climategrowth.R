## This script is to make the climate dataframe for climate-tree ring growth analyses - including seasonal climate variables
## Cecilia Martinez 
## Jan 25 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(here)
library(readr)
library(tidyr)
library(dplyr)

#=========================================================
# Data Wrangling for tree-ring data -- all 
# These data include Kichas and FIA data so far 
#=========================================================


# load in wbp tree ring data and associated metadata for FIA CORES new -------------------------------------------------------------------------

wbp_meta_new <- read_csv(here::here("tree-H", "data", "raw", "wbp_cores_2020-2023.csv")) 
wbp_meta_new <- wbp_meta_new %>% 
  rename(TRE_CN = CN) %>% 
  rename(LAT = LAT_FUZZED) %>% 
  rename(LON = LON_FUZZED) %>% 
  rename(ELEV = ELEV_FUZZED) 

wbp_meta_new$PLOT_CN <- as.character(wbp_meta_new$PLOT_CN)

wbp_rw_new <- read_csv(here::here("tree-H", "data", "raw", "wbp_new_rwl.csv"))
wbp_rw_new <- wbp_rw_new %>% 
  rename(TRE_CN = CN)

length(unique(wbp_meta_new$TRE_CN)) #71 trees so far

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe
# no TRE_CN 
wbp_rw_new <- left_join(wbp_rw_new, wbp_meta_new)
wbp_rw_new <- wbp_rw_new %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW)

wbp_meta_new_filt <- semi_join(wbp_meta_new, wbp_rw_new, by = "TRE_CN")

# load in wbp tree ring data and associated metadata for FIA CORES old -------------------------------------------------------------------------

wbp_meta_18 <- read_csv(here::here("tree-H", "data", "raw", "wbp_meta_2018.csv")) %>% 
  dplyr::select(-TRE_CN) %>% 
  rename(TRE_CN = CORE_CN)

wbp_meta_18$PLOT_CN <- as.character(wbp_meta_18$PLOT_CN)
wbp_meta_18$TRE_CN <- as.character(wbp_meta_18$TRE_CN)

wbp_rw_18 <- read_csv(here::here("tree-H", "data", "raw", "T_iwfia_whitebark_rw.txt"))
wbp_rw_18 <- wbp_rw_18 %>% 
  rename(TRE_CN = CN) %>% 
  dplyr::select(-SPCD)
wbp_rw_18$TRE_CN <- as.character(wbp_rw_18$TRE_CN)

# now filter out all the data that's been verified!
# only keep trees where verified = y
wbp_meta_18 <- wbp_meta_18 %>% 
  filter(VERIFY == "y")

# okay now only keep rw data where TRE_CN matches with the filtered metadata
wbp_rw_18 <- wbp_rw_18 %>%
  filter(TRE_CN %in% wbp_meta_18$TRE_CN)
length(unique(wbp_rw_18$TRE_CN))

wbp_rw_18 <- left_join(wbp_rw_18, wbp_meta_18)
wbp_rw_18 <- wbp_rw_18 %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW)

# combine FIA dataframes --------------------------------------------------

# ring width df 
wbp_rw_all <- bind_rows(wbp_rw_new, wbp_rw_18)
wbp_meta_all <- bind_rows(wbp_meta_new_filt, wbp_meta_18) #113 total rows

# make a new column for where the data set comes from so we can pottentially test
# that in our models
wbp_rw_all <- wbp_rw_all %>% 
  mutate(dataset = "FIA")
wbp_meta_all <- wbp_meta_all %>% 
  mutate(dataset = "FIA")

#okay make meta df for size backcalculation
wbp_meta_size <- wbp_meta_all %>% 
  dplyr::select(TRE_CN, PLOT_CN, MEASYEAR, DIA, dataset)

###### load in wbp tree ring data and associated metadata for Nick Kichas data ###### 

kichas_meta <- read_csv(here::here("tree-H", "data", "processed", "kichas_meta.csv"))
# removing rows with specific IDs because of the core/tree misalignment, as they are second cores for one tree
kichas_meta <- kichas_meta %>%
  filter(!ID %in% c("U8W8", "T9S5", "R9S2", "K14Ex1", "K14Ex3", "E16E7", "E13A2"))

# renaming CoreID 
kichas_meta <- kichas_meta %>% 
  rename(TRE_CN = ID) %>% 
  rename(PLOT_CN = Plot) %>% 
  mutate(dataset = "Kichas")

# subsetting kichas metadataset
kichas_meta_sub <- kichas_meta %>% 
  dplyr::select(TRE_CN, PLOT_CN, dataset) 

kichas_meta_size <- kichas_meta %>%
  rename(
    DIA = DBH,
    MEASYEAR = "Outer Ring date"
  )

kichas_meta_size <- kichas_meta_size %>%
  dplyr::select(TRE_CN, PLOT_CN, MEASYEAR, DIA, dataset)


##OKAY NOW BIND SIZE META DF FOR BC

combined_meta_size <- bind_rows(kichas_meta_size, wbp_meta_size)
# okay so this df has 184 trees as of now

write_csv(combined_meta_size, "tree-H/data/processed/wbp_meta_size.csv")
#updates as of 02/03/2025

# loading BLD site
# dropping the cores with the core ID mismatch, this is only a temporary fix
bld_rw <- read_csv(here::here("tree-H", "data", "processed", "BLD_kichas.csv"))
bld_rw <- bld_rw %>%
  dplyr::select(-c("U8W8", "T9S5", "R9S2"))

bld_rw <- as.data.frame(bld_rw)
rownames(bld_rw) <- bld_rw$Year
bld_rw <- bld_rw %>%
  dplyr::select(-Year)
bld_rw <- reshape2::melt(as.matrix(bld_rw)) %>% 
  rename(Year = Var1, TRE_CN = Var2, RW = value)
head(bld_rw)

# remove NA values
bld_rw <- bld_rw %>% 
  drop_na(RW)

# adding PLOT_CN to rw
bld_rw <- bld_rw %>% 
  left_join(kichas_meta_sub, by = "TRE_CN")

# loading 3LK site
# dropping the cores with the core ID mismatch, this is only a temporary fix
threelk_rw <- read_csv(here::here("tree-H", "data", "processed", "3lk_kichas.csv"))
threelk_rw <- threelk_rw %>%
  dplyr::select(-c("K14Ex1", "K14Ex3", "E16E7", "E13A2"))

threelk_rw <- as.data.frame(threelk_rw)
rownames(threelk_rw) <- threelk_rw$Year
threelk_rw <- threelk_rw %>%
  dplyr::select(-Year)
threelk_rw <- reshape2::melt(as.matrix(threelk_rw)) %>% 
  rename(Year = Var1, TRE_CN = Var2, RW = value)
head(threelk_rw)

# remove NA values
threelk_rw <- threelk_rw %>% 
  drop_na(RW)

# adding PLOT_CN to rw
threelk_rw <- threelk_rw %>% 
  left_join(kichas_meta_sub, by = "TRE_CN")


# combine the two ring width data frames
kichas_rw <- bind_rows(threelk_rw, bld_rw)
kichas_rw <- kichas_rw %>% 
  dplyr::select(TRE_CN, PLOT_CN, Year, RW)
#=========================================================
# Merging data frames Kichas and FIA
#=========================================================

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe
wbp_rw_all <- wbp_rw_all %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW, dataset)
wbp_rw_all$PLOT_CN <- as.character(wbp_rw_all$PLOT_CN)

#now add the kichas data to the dataframe
# so now this is a dataframe that looks like TRE_CN, PLOT_CN, Year, RW with all of the data so far
wbp_rw_all <- bind_rows(wbp_rw_all, kichas_rw)
length(unique(wbp_rw_all$TRE_CN)) #219 unique tree numbers

#write final csv
write_csv(wbp_meta_all, "tree-H/data/processed/wbp_meta_all.csv")

# truncate the years so that they only span the years of climate data that we have, so 1896 onwards
#wbp_rw <- wbp_rw %>%
#  filter(Year >= 1896), don't do this actually

# load in climate data ----------------------------------------------------
# this was most recently updated on 01/27/2025 to include Nick Kichas data for wbp
ppt <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_ppt_df.csv"))
tmax <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_tmax_df.csv"))
tmin <- read_csv(here::here("PRISM", "data_formatted", "wbp_all_tmin_df.csv"))


# turn climate data into long form and make new columns for year and month from column headers when the file is in wide format

tmin_long <- tmin %>%
  pivot_longer(cols = starts_with("tmin_"), names_to = "variable", values_to = "tmin") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  dplyr::select(-variable, -TRE_CN)

tmax_long <- tmax %>%
  pivot_longer(cols = starts_with("tmax_"), names_to = "variable", values_to = "tmax") %>%
  mutate(year = as.integer(substr(variable, 6, 9)),
         month = as.integer(substr(variable, 10, 11))) %>%
  dplyr::select(-variable, -TRE_CN)

ppt_long <- ppt %>% 
  pivot_longer(cols = starts_with("ppt_"), names_to = "variable", values_to = "ppt") %>%
  mutate(year = as.integer(substr(variable, 5, 8)),
         month = as.integer(substr(variable, 9, 10))) %>%
  dplyr::select(-variable, -TRE_CN)

# Combine the data frames into one climate dataframe
tmin_long <- distinct(tmin_long, PLOT_CN, year, month, .keep_all = TRUE)
tmax_long <- distinct(tmax_long, PLOT_CN, year, month, .keep_all = TRUE)
ppt_long <- distinct(ppt_long, PLOT_CN, year, month, .keep_all = TRUE)

climate_dat <- left_join(tmin_long, tmax_long, by = c("PLOT_CN", "year", "month")) %>%
  left_join(., ppt_long, by = c("PLOT_CN", "year", "month"))

#reorder columns and get rid of 1895 growth year
climate_dat <- climate_dat %>%
  dplyr::select(PLOT_CN, year, month, tmin, tmax, ppt) %>% 
  filter(!(year == 1895 & month >= 1 & month <= 8))

climate_dat$PLOT_CN <- as.character(climate_dat$PLOT_CN)
write_csv(climate_dat, "tree-H/data/processed/climate_dat.csv") ## use this for climwin and for seasonal climate vars
# this was updated last at 01/31/2025

## making seasonal climate variables, refer to climate-growth analyses --> do this at some point, have not done yet


## check to make sure that the PLT_CN matches in both dfs (climate and ring width) and filter out the rows for which there isn't a match

unique_climate <- unique(climate_dat$PLOT_CN)
unique_wbp <- unique(wbp_rw_all$PLOT_CN)

plots_match <- unique(climate_dat$PLOT_CN) %>% 
  intersect(unique(wbp_rw_all$PLOT_CN))  #32 plots match

wbp_rw_all <- wbp_rw_all %>% 
  filter(PLOT_CN %in% plots_match) # this makes sure that the plt_cn match for the climate and the ring width data frame 

# add climate norms and growth year (biologically significant years) to df -------------------------------------------------
source(here::here("tree-H", "R", "make_climate_functions.R")) 

## note this will require the make_clim_norm function
climate_all <- climate_growthyear(climate_dat)

## adding climate normals for 1900-2010 for each site
norms <- climate_normals(climate_dat, 1900, 2000)
as.data.frame(norms)

## join climate normals to new_climate
climate_all <- climate_all %>% 
  left_join(norms, by = "PLOT_CN")

# write and export as csvs --------------------------------------------------------------
#okay this is updated as of 02/03/2024
write_csv(wbp_rw_all, "tree-H/data/processed/wbp_dat_rw.csv")
write_csv(climate_all, "tree-H/data/processed/wbp_datclimate.csv")




