## This script is inteneded to merge dataframes from different sources of tree ring data
## This script also create the dataframe needed for the backcalcualtion of tree diametert_see datawrangling _sizegrowth.R
## Jan 25 2024
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(here) # for relativizing file paths
library(readr)
library(tidyr)
library(dplyr)

#=========================================================
# Data Wrangling for tree-ring data -- all 
# These data include Kichas and FIA data so far 
#=========================================================

# wrangling FIA dataset ---------------------------------------------------

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

length(unique(wbp_meta_new$TRE_CN)) #125 trees so far

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe
# no TRE_CN 
wbp_rw_new <- left_join(wbp_rw_new, wbp_meta_new)
wbp_rw_new <- wbp_rw_new %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW)

wbp_meta_new_filt <- semi_join(wbp_meta_new, wbp_rw_new, by = "TRE_CN") # 71 trees

# load in wbp tree ring data and associated metadata for FIA CORES old

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

# filter out all the data that's been verified!
# only keep trees where verified = y
wbp_meta_18 <- wbp_meta_18 %>% 
  filter(VERIFY == "y")

# only keep rw data where TRE_CN matches with the filtered metadata
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

wbp_meta_all <- wbp_meta_all %>% 
  dplyr::select(c(TRE_CN, PLOT_CN, LAT, LON, dataset))



# wrangling kichas dataset ------------------------------------------------

kichas_meta <- read_csv(here::here("tree-H", "data", "processed", "kichas_meta.csv"))
# removing rows with specific IDs because of the core/tree misalignment, as they are second cores for one tree
kichas_meta <- kichas_meta %>%
  filter(!ID %in% c("U8W8", "T9S5", "R9S2", "K14Ex1", "K14Ex3", "E16E7", "E13A2"))

# renaming CoreID 
kichas_meta <- kichas_meta %>% 
  rename(TRE_CN = ID) %>% 
  rename(PLOT_CN = Plot) %>% 
  mutate(dataset = "kichas")

# subsetting kichas metadataset
kichas_meta_sub <- kichas_meta %>% 
  dplyr::select(TRE_CN, PLOT_CN, LAT, LON, dataset) 

kichas_meta_size <- kichas_meta %>%
  rename(
    DIA = DBH,
    MEASYEAR = "Outer Ring date"
  )

kichas_meta_size <- kichas_meta_size %>%
  dplyr::select(TRE_CN, PLOT_CN, MEASYEAR, DIA, dataset)


# combine data sets for size backcalculation ------------------------------
combined_meta_size <- bind_rows(kichas_meta_size, wbp_meta_size)
# okay so this df has 218 trees as of now
write_csv(combined_meta_size, "tree-H/data/processed/wbp_meta_size.csv")
#updates as of 02/13/2025

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
kichas_rw <- bind_rows(threelk_rw, bld_rw) %>% 
  mutate(dataset = "kichas")
kichas_rw <- kichas_rw %>% 
  dplyr::select(TRE_CN, PLOT_CN, Year, RW, dataset)

#=========================================================
# Merging data frames Kichas and FIA
#=========================================================
# merge the metadata dfs 

wbp_meta_all <-  bind_rows(wbp_meta_all, kichas_meta_sub)

# need to add tre and plot CN (unique tree and plot level identifiers) to the rw dataframe
wbp_rw_all <- wbp_rw_all %>% dplyr::select(TRE_CN, PLOT_CN, Year, RW, dataset)
wbp_rw_all$PLOT_CN <- as.character(wbp_rw_all$PLOT_CN)

#now add the kichas data to the dataframe
# so now this is a dataframe that looks like TRE_CN, PLOT_CN, Year, RW with all of the data so far
wbp_rw_all <- bind_rows(wbp_rw_all, kichas_rw)
length(unique(wbp_rw_all$TRE_CN)) #218 unique tree numbers


#write final csv for the merged data frame
write_csv(wbp_meta_all, "tree-H/data/processed/wbp_meta_all.csv")
write_csv(wbp_rw_all, "tree-H/data/processed/wbp_rw_all.csv")

