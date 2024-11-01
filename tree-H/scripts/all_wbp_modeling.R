##Cecilia Martinez
##
##

# load packages -----------------------------------------------------------
library(here)
library(Matrix)
library(mgcv)
library(gratia)
library(tidyr)
library(climwin)
library(lme4)
library(readr)
library(ggplot2)
library(dplyr)
library(lme4)
library(glmnet)

######################### Analysis for Whitebark Pine Climate-Growth##########################################

# Load the functions
source(here::here("tree-H", "R", "check_overlap.R")) 
source(here::here("tree-H", "R", "make_X.R")) 
source(here::here("tree-H", "R", "make_H.R")) 
source(here::here("tree-H", "R", "make_annualizeDBH.R"))
source(here::here("tree-H", "R", "make_seasonalwindows.R"))


# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "wbp_all_climate_growth_rw.csv"))
dat_bc <- read_csv(here::here("tree-H", "data", "processed", "wbp_size_all.csv"))
#if you want climate scaled, then load "wbp_all_climate_data_scaled_all.csv"
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "wbp_all_climate_data_all.csv"))# Check the data sources for overlap and lack of overlap

missing_overlap <- check_overlap(dat, dat_climate, dat_bc)

# Drop the missing data # do this for backcaluation
message("About ", round(mean((missing_overlap$tree_CN_missing) | (missing_overlap$tree_year_missing)) * 100, digits = 0), "% of tree ring data will be dropped")
message("About ", round(mean((missing_overlap$climate_CN_missing) | (missing_overlap$climate_year_missing)) * 100, digits = 0), "% of climate ring data will be dropped")
message("About ", round(mean((missing_overlap$bc_tree_CN_missing) | (missing_overlap$bc_year_missing)) * 100, digits = 0), "% of backcalculated data will be dropped")

dat         <- dat[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
dat_climate <- dat_climate[!missing_overlap$climate_CN_missing & !missing_overlap$climate_year_missing, ]
dat_climate <- dat_climate %>% 
  dplyr::select(-tmin)
dat_bc      <- dat_bc[!missing_overlap$bc_tree_CN_missing & !missing_overlap$bc_year_missing, ]

###MAKING SEASONAL CLIMATE VARIABLES#####

JunJulAug_ppt          <- making_climate_windows(data = dat_climate, 
                                              variable = "ppt", 
                                              start_month = 6, 
                                              end_month = 8, 
                                              year_shift = 0)
pJunJulAug_ppt         <- making_climate_windows(data = dat_climate, 
                                              variable = "ppt", 
                                              start_month = 6, 
                                              end_month = 8, 
                                              year_shift = -1)

seasonal_clim_dat   <- left_join(JunJulAug_ppt, pJunJulAug_ppt, by = c("PLOT_CN", "year"))
seasonal_clim_dat <- seasonal_clim_dat %>%
  mutate(prev_current_ppt = 
           c_68_ppt + p_68_ppt)

##scaline




# Create the size backcalculated matrix
Z_list     <- backcalculate_DBH(dat_bc)
# rows are trees x years and it is just a single column with backcalculated values
Z          <- Z_list$Z
year_id_Z  <- Z_list$year_id
tree_id    <- Z_list$tree_id
site_id_Z  <- Z_list$site_id

str(dat)
str(dat_bc)
data.frame(Z = Z, Year = year_id_Z, TRE_CN = tree_id) %>% 
  left_join(dat) 


dat_fit <- dat %>% left_join(dat_bc) %>% left_join(data.frame(Z = Z, Year = year_id_Z, TRE_CN = tree_id))
# so we have to create a dataframe to account for seasonal, or time varying monthly climate variables
dat_all <- dat_fit  %>%  right_join(dat_climate, by = join_by(PLOT_CN == PLOT_CN, Year == growthyear)) %>% 
  dplyr::select(-year) %>% 
  filter(!(TRE_CN %in% c("23R48", "23T255"))) # this gets rid of weird outliers

dat_all_seas <- dat_fit %>% right_join(seasonal_clim_dat, by = join_by(PLOT_CN == PLOT_CN, Year == year))
  

# dat_fit for bc and climate data
# align climate to the dataframe - maybe through another join (choose vars well) join_by !!! 
# marginal effect of size
dat_all %>% 
  ggplot(aes(y = log(RW + 0.001), x = Z, color = TRE_CN) ) + 
  # geom_point() + 
  geom_line() + 
  stat_smooth(aes(y = log(RW + 0.001), x = Z), inherit.aes = FALSE, 
              method = "lm", formula = y ~ x + I(x^2))+ 
  stat_smooth(aes(y = log(RW + 0.001), x = Z), inherit.aes = FALSE, 
              color = "black", method = "gam") +
  theme_bw() + 
  theme(legend.position = "none")  # Remove the legend

#run model with aligned data frame

mod_size <- lm(log(RW + 0.001) ~ Z + I(Z^2), data = dat_all)
summary(mod_size) #matrices align issue - redo the matrix 
plot(mod_size)
check_model(mod_size)

mod_climate_01 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + precip, data = dat_all)
summary(mod_climate_01) 

mod_climate_02 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + precip + meantemp, data = dat_all)
summary(mod_climate_02) 

#wrangling to compare time varying climate

dat_wide <- dat_all %>% pivot_wider(names_from = month, values_from = tmax:ppt) %>% 
  dplyr::select(-c("PLOT_CN", "Year", "MEASYEAR", "DIA")) 

idx <- dat_wide$RW == 0
dat_wide$RW[idx] <- NA # getting rid of outlier residual

#mod with time varying climate predictors

dat_wide_state <- dat_wide %>%
  left_join(wbp_meta_all %>% dplyr::select(TRE_CN, STATECD), by = "TRE_CN")

mod_climate_03 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + . -TRE_CN, data = dat_wide)
summary(mod_climate_03) 
check_model(mod_climate_03)
plot(mod_climate_03)

model_summary <- broom::tidy(mod_climate_03, conf.int = TRUE)
model_summary <- model_summary %>% filter(term != "(Intercept)")

ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "cadetblue") +
  coord_flip() +  # Flip coordinates for horizontal plot
  geom_hline(yintercept = 0, color = "black", linetype = "dashed",size = 1) +  labs(
    x = "Coefficients", 
    y = "Estimate (with 95% CI)") +
  theme_minimal()


# Add random effect structure 

variable_ppt <- c("ppt_6", "ppt_7", "ppt_8")
variable_tmax <-  c("tmax_5", "tmax_6", "tmax_7", "tmax_8")
f4 <- paste("log(RW + 0.001) ~ Z + I(Z^2) + meantemp + precip", str_flatten(variable_ppt, collapse = " + "), sep = " + ")

stgrmod_climate_04 <- lm(f4, data = dat_wide)
mod_climate_05 <- lmer(paste(f4, "(1|TRE_CN)", sep = " + "), data = dat_wide)

## slope effect
mod_climate_05 <- lmer(paste(f4, "(1|TRE_CN) + (1+Z|", sep = " + "), data = dat_wide)

AIC(mod_climate_04,mod_climate_05)
check_model(mod_climate_04)

x <- model.matrix(log(RW + 0.001) ~ Z + I(Z^2) + . - 1 - TRE_CN, data = dat_wide)
y <- dat_wide %>% 
  drop_na() %>% 
  pull(RW)
str(y)
# mod_net <- cv.glmnet(x,log(y + 0.001))
# summary(mod_net)
# plot(mod_net)
# coef(mod_net, s = "lambda.min")

mod_net <- cv.glmnet(x, y, lambda = c(0.01, 0.1, 1, 10))
plot(mod_net)
summary(mod_net)
coef(mod_net, s = "lambda.min")
coefficients <- as.data.frame(as.matrix(coef(mod_net, s = "lambda.min")))
colnames(coefficients) <- "Coefficient"
coefficients <- data.frame(Variable = rownames(coefficients), Coefficient = coefficients$Coefficient, row.names = NULL)
coefficients <- coefficients[coefficients$Coefficient != 0 & !is.na(coefficients$Coefficient), ]
coefficients %>%
  kbl(digits = 6, format = "html", caption = "Coefficients Table") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(2, bold = TRUE, color = ifelse(is.na(coefficients$Coefficient), "gray", "black"))


####################### CLIMWIN model for comparison with John Tipton Model ######################################
######################                                                      ######################################

#create ring width dataframe (rwl) for dplR
library(reshape2)
library(dplyr)
wbp_rw_all <- read_csv("tree-H/data/processed/wbp_rw_all.csv")
wbp_meta_all <- read_csv("tree-H/data/processed/wbp_meta_all.csv")

wbp_meta_col <- wbp_meta_all %>% 
  dplyr::select(TRE_CN, PLOT_CN, STATECD) 

wbp_meta_col$PLOT_CN <- as.character(wbp_meta_col$PLOT_CN)
wbp_rw_all$PLOT_CN <- as.character(wbp_rw_all$PLOT_CN)

wbp_rw_all <- left_join(wbp_rw_all, wbp_meta_col, by = c("TRE_CN", "PLOT_CN"))

#do each state separately
wbp_rw_ID_df <- wbp_rw_all %>% 
  filter(STATECD == 16)
wbp_rw_ID <- wbp_rw_ID_df %>% 
  dplyr::select(TRE_CN, Year, RW)
wbp_rw_MT_df <- wbp_rw_all %>% 
  filter(STATECD == 30) 
wbp_rw_MT <- wbp_rw_MT_df %>% 
  dplyr::select(TRE_CN, Year, RW)
wbp_rw_WY_df <- wbp_rw_all %>% 
  filter(STATECD == 56) 
wbp_rw_WY <- wbp_rw_WY_df %>% 
  dplyr::select(TRE_CN, Year, RW)

# idaho -------------------------------------------------------------------

#####IDAHO#########

wbp_rw_ID <- tidyr::spread(wbp_rw_ID,key = "TRE_CN", value = RW,fill = NA,drop = FALSE) #pivot_wider?
wbp_rw_ID <- as.data.frame(wbp_rw_ID)
years <- wbp_rw_ID$Year
wbp_rw_ID <- wbp_rw_ID %>%
  dplyr::select(-Year)
rownames(wbp_rw_ID) <- years
colnames(wbp_rw_ID) <- paste("SER", colnames(wbp_rw_ID), sep = "_")


#explore dataframe in dplR
rwl.report(wbp_rw_ID) #descriptive statistics
rw_stats_ID <- rwl.stats(wbp_rw_ID)
head(rw_stats_ID) #descriptive statistics

# cinvert ring width data frame to appropriate climwin format, but first detrending and making chronology
rwi_ID <- detrend(rwl = wbp_rw_ID, 
                   method = "Spline", nyrs = 30)
rwi.stats(rwi_ID)
rw_crn_ID <- chron(rwi_ID, prefix = "WBP")
plot(rw_crn_ID)

# convertin to propoer format for climwin
rw_crn_ID <- rw_crn_ID %>%
  mutate(year = as.numeric(row.names(rw_crn_ID))) %>%
  dplyr::select(year, everything()) %>% 
  filter(year > 1897) %>% 
  # dplyr::select(-samp.depth)  %>%
  rename(`RWI` = `std`) 

#add an arbitrary month and day for climwin
rw_crn_ID <- rw_crn_ID %>%
  mutate(
    month = 9,
    day = 30,
    Date = as.Date(paste(year, month, day, sep = "-"))
  )

#make climate data frame
climate_dat_ID <-  semi_join(climate_dat, wbp_rw_ID_df, by = "PLOT_CN")
climate_dat_climwin_ID <- climate_dat_ID %>%
  group_by(year, month) %>%
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    ppt = mean(ppt, na.rm = TRUE)
  ) %>%
  ungroup()

#Convert the climate data to the appropriate format for climwin
climate_dat_climwin_ID <- climate_dat_climwin_ID %>%
  mutate(Date = as.Date(paste(year, month, "01", sep = "-"))) #set arbitrary day for monthly data

# Define the response and climate data for climwin
response <- rw_crn_ID
climate <- climate_dat_climwin_ID
reference_date <- c(30, 9) 

# Define the response and climate variables for climwin
response_var <- "RWI"  
climate_vars <- c("tmax", "tmin", "ppt")

# Run  sliding window analysis - this is the MAIN function of this package
climwin_results <- slidingwin(
  xvar =list(tmax = climate$tmax, 
             tmin = climate$tmin, 
             ppt = climate$ppt
  ),
  cinterval = "month",
  cdate = climate$Date,
  bdate = response$Date,
  baseline = lm(response[[response_var]] ~ 1, data = response),
  type = "absolute",
  refday = reference_date,
  range = c(16, 0),  # Specify the range of months to include in the analysis
  stat = "mean",
  func = c("lin", "quad"),
  cmissing = FALSE)

# Looking at best model for each climate variables
climwin_results[[3]]$BestModel

climwin_results$combos
climwin_results$combos %>%
  kbl(caption = "IDAHO") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

best_mod_first_step <- which.min(climwin_results$combos$DeltaAICc)
output <- climwin_results

climwin_plot <- plotall(datasetrand = NULL,
                        dataset = output[[best_mod_first_step]]$Dataset, 
                        bestmodel = output[[best_mod_first_step]]$BestModel,
                        bestmodeldata = output[[best_mod_first_step]]$BestModelData,
                        title=paste((data.frame(lapply(output$combos[best_mod_first_step,], as.character), stringsAsFactors=FALSE)), collapse = "_"))



# montana -----------------------------------------------------------------
#######MONTANA#########

wbp_rw_MT <- tidyr::spread(wbp_rw_MT,key = "TRE_CN", value = RW,fill = NA,drop = FALSE) #pivot_wider?
wbp_rw_MT <- as.data.frame(wbp_rw_MT)
years <- wbp_rw_MT$Year
wbp_rw_MT <- wbp_rw_MT %>%
  dplyr::select(-Year)
rownames(wbp_rw_MT) <- years
colnames(wbp_rw_MT) <- paste("SER", colnames(wbp_rw_MT), sep = "_")


#explore dataframe in dplR
rwl.report(wbp_rw_MT) #descriptive statistics
rw_stats_MT <- rwl.stats(wbp_rw_MT)
head(rw_stats_MT) #descriptive statistics

# cinvert ring width data frame to appropriate climwin format, but first detrending and making chronology
rwi_MT <- detrend(rwl = wbp_rw_MT, 
                  method = "Spline", nyrs = 30)
rwi.stats(rwi_MT)
rw_crn_MT <- chron(rwi_MT, prefix = "WBP")
plot(rw_crn_MT)

# convertin to propoer format for climwin
rw_crn_MT <- rw_crn_MT %>%
  mutate(year = as.numeric(row.names(rw_crn_MT))) %>%
  dplyr::select(year, everything()) %>% 
  filter(year > 1897) %>% 
  # dplyr::select(-samp.depth)  %>%
  rename(`RWI` = `std`) 

#add an arbitrary month and day for climwin
rw_crn_MT <- rw_crn_MT %>%
  mutate(
    month = 9,
    day = 30,
    Date = as.Date(paste(year, month, day, sep = "-"))
  )

#make climate data frame
climate_dat_MT <-  semi_join(climate_dat, wbp_rw_MT_df, by = "PLOT_CN")
climate_dat_climwin_MT <- climate_dat_MT %>%
  group_by(year, month) %>%
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    ppt = mean(ppt, na.rm = TRUE)
  ) %>%
  ungroup()

#Convert the climate data to the appropriate format for climwin
climate_dat_climwin_MT <- climate_dat_climwin_MT %>%
  mutate(Date = as.Date(paste(year, month, "01", sep = "-"))) #set arbitrary day for monthly data

# Define the response and climate data for climwin
response <- rw_crn_MT
climate <- climate_dat_climwin_MT
reference_date <- c(30, 9) 

# Define the response and climate variables for climwin
response_var <- "RWI"  
climate_vars <- c("tmax", "tmin", "ppt")

# Run  sliding window analysis - this is the MAIN function of this package
climwin_results <- slidingwin(
  xvar =list(tmax = climate$tmax, 
             tmin = climate$tmin, 
             ppt = climate$ppt
  ),
  cinterval = "month",
  cdate = climate$Date,
  bdate = response$Date,
  baseline = lm(response[[response_var]] ~ 1, data = response),
  type = "absolute",
  refday = reference_date,
  range = c(16, 0),  # Specify the range of months to include in the analysis
  stat = "mean",
  func = c("lin", "quad"),
  cmissing = FALSE)

# Looking at best model for each climate variables
climwin_results[[3]]$BestModel

climwin_results$combos
climwin_results$combos %>%
  kbl(caption = "MONTANA") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

best_mod_first_step <- which.min(climwin_results$combos$DeltaAICc)
output <- climwin_results

climwin_plot <- plotall(datasetrand = NULL,
                        dataset = output[[best_mod_first_step]]$Dataset, 
                        bestmodel = output[[best_mod_first_step]]$BestModel,
                        bestmodeldata = output[[best_mod_first_step]]$BestModelData,
                        title=paste((data.frame(lapply(output$combos[best_mod_first_step,], as.character), stringsAsFactors=FALSE)), collapse = "_"))

# Wyoming -----------------------------------------------------------------
#######WYOMING#########

wbp_rw_WY <- tidyr::spread(wbp_rw_WY,key = "TRE_CN", value = RW,fill = NA,drop = FALSE) #pivot_wider?
wbp_rw_WY <- as.data.frame(wbp_rw_WY)
years <- wbp_rw_WY$Year
wbp_rw_WY <- wbp_rw_WY %>%
  dplyr::select(-Year)
rownames(wbp_rw_WY) <- years
colnames(wbp_rw_WY) <- paste("SER", colnames(wbp_rw_MT), sep = "_")


#explore dataframe in dplR
rwl.report(wbp_rw_WY) #descriptive statistics
rw_stats_WY <- rwl.stats(rw_stats_WY)
head(rw_stats_WY) #descriptive statistics

# cinvert ring width data frame to appropriate climwin format, but first detrending and making chronology
rwi_WY <- detrend(rwl = wbp_rw_WY, 
                  method = "Spline", nyrs = 30)
rwi.stats(rwi_WY)
rw_crn_WY <- chron(rwi_WY, prefix = "WBP")
plot(rw_crn_WY)

# convertin to propoer format for climwin
rw_crn_WY <- rw_crn_WY %>%
  mutate(year = as.numeric(row.names(rw_crn_WY))) %>%
  dplyr::select(year, everything()) %>% 
  filter(year > 1897) %>% 
  # dplyr::select(-samp.depth)  %>%
  rename(`RWI` = `std`) 

#add an arbitrary month and day for climwin
rw_crn_WY <- rw_crn_WY %>%
  mutate(
    month = 9,
    day = 30,
    Date = as.Date(paste(year, month, day, sep = "-"))
  )

#make climate data frame
climate_dat_WY <-  semi_join(climate_dat, wbp_rw_WY_df, by = "PLOT_CN")
climate_dat_climwin_WY <- climate_dat_WY %>%
  group_by(year, month) %>%
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    ppt = mean(ppt, na.rm = TRUE)
  ) %>%
  ungroup()

#Convert the climate data to the appropriate format for climwin
climate_dat_climwin_WY <- climate_dat_climwin_WY %>%
  mutate(Date = as.Date(paste(year, month, "01", sep = "-"))) #set arbitrary day for monthly data

# Define the response and climate data for climwin
response <- rw_crn_WY
climate <- climate_dat_climwin_WY
reference_date <- c(30, 9) 

# Define the response and climate variables for climwin
response_var <- "RWI"  
climate_vars <- c("tmax", "tmin", "ppt")

# Run  sliding window analysis - this is the MAIN function of this package
climwin_results <- slidingwin(
  xvar =list(tmax = climate$tmax, 
             tmin = climate$tmin, 
             ppt = climate$ppt
  ),
  cinterval = "month",
  cdate = climate$Date,
  bdate = response$Date,
  baseline = lm(response[[response_var]] ~ 1, data = response),
  type = "absolute",
  refday = reference_date,
  range = c(16, 0),  # Specify the range of months to include in the analysis
  stat = "mean",
  func = c("lin", "quad"),
  cmissing = FALSE)

# Looking at best model for each climate variables
climwin_results[[3]]$BestModel

climwin_results$combos
climwin_results$combos %>%
  kbl(caption = "WYOMING") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

best_mod_first_step <- which.min(climwin_results$combos$DeltaAICc)
output <- climwin_results

climwin_plot <- plotall(datasetrand = NULL,
                        dataset = output[[best_mod_first_step]]$Dataset, 
                        bestmodel = output[[best_mod_first_step]]$BestModel,
                        bestmodeldata = output[[best_mod_first_step]]$BestModelData,
                        title=paste((data.frame(lapply(output$combos[best_mod_first_step,], as.character), stringsAsFactors=FALSE)), collapse = "_"))

##################################################################
# DO THIS FOR ALL DATA NOW--------------------------------------

#data wrnagling rw data 
rw_data <- wbp_rw_all %>%
  dplyr::select(TRE_CN, Year, RW)
rw_data <- tidyr::spread(rw_data,key = "TRE_CN", value = RW,fill = NA,drop = FALSE) #pivot_wider?
rw_data <- as.data.frame(rw_data)
years <- rw_data$Year
rw_data <- rw_data %>%
  dplyr::select(-Year)
rownames(rw_data) <- years
colnames(rw_data) <- paste("SER", colnames(rw_data), sep = "_")

library(dplR)
#explore dataframe in dplR
rwl.report(rw_data) #descriptive statistics
rw_stats <- rwl.stats(rw_data)
head(rw_stats)

library(knitr)
library(kableExtra)

#make climate data frame
climate_dat_climwin <- climate_dat %>%
  group_by(year, month) %>%
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    ppt = mean(ppt, na.rm = TRUE)
  ) %>%
  ungroup()

climate_dat_climwin %>%
  kable("html", caption = "Monthly Climate Data Summary") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE) %>%         # Make the 'year' column bold
  column_spec(2, width = "5em") %>%       # Adjust width for 'month' column
  column_spec(3:5, width = "6em") %>%     # Adjust width for 'tmin', 'tmax', and 'ppt' columns
  add_header_above(c(" " = 2, "Climate Variables" = 3))  # Add a header above climate variable columns

# cinvert ring width data frame to appropriate climwin format, but first detrending and making chronology
rwi_all <- detrend(rwl = rw_data, 
                   method = "Spline", nyrs = 30)
rwi.stats(rwi_all)
rw_all_crn <- chron(rwi_all, prefix = "WBP")
plot(rw_all_crn)

# convertin to propoer format
rw_all_crn <- rw_all_crn %>%
  mutate(year = as.numeric(row.names(rw_all_crn))) %>%
  dplyr::select(year, everything()) %>% 
  filter(year > 1897) %>% 
  # dplyr::select(-samp.depth)  %>%
  rename(`RWI` = `std`) 

rw_all_crn %>%
  kable("html", row.names = FALSE, caption = "Tree Ring Width Index Data Summary") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#add an arbitrary month and day for climwin
rw_all_crn <- rw_all_crn %>%
  mutate(
    month = 9,
    day = 30,
    Date = as.Date(paste(year, month, day, sep = "-"))
  )

#Convert the climate data to the appropriate format for climwin
climate_dat_climwin_df <- climate_dat_climwin %>%
  mutate(Date = as.Date(paste(year, month, "01", sep = "-"))) #set arbitrary day for monthly data


# Define the response and climate data for climwin
response <- rw_all_crn
climate <- climate_dat_climwin_df
reference_date <- c(30, 9) 

# Define the response and climate variables for climwin
response_var <- "RWI"  
climate_vars <- c("tmax", "tmin", "ppt")

# Run  sliding window analysis - this is the MAIN function of this package
climwin_results <- slidingwin(
  baseline = lm(response[[response_var]] ~ 1, data = response),
  xvar =list(tmax = climate$tmax, 
             tmin = climate$tmin, 
             ppt = climate$ppt
  ),
  type = "absolute",
  cinterval = "month",
  range = c(17, 0),  # Specify the range of months to include in the analysis
  stat = "mean",
  func = c("lin", "quad"),
  cdate = climate$Date,
  bdate = response$Date,
  refday = reference_date,
  cmissing = FALSE)

# View the results
summary(climwin_results)

# Looking at best model for each climate variables
climwin_results[[3]]$BestModel
climwin_results$combos
climwin_results$combos %>%
  kbl(caption = "ALL") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

best_mod_first_step <- which.min(climwin_results$combos$DeltaAICc)
tmax_mod <- (climwin_results$combos$DeltaAICc)
output <- climwin_results

climwin_plot <- plotall(datasetrand = NULL,
                        dataset = output[[best_mod_first_step]]$Dataset, 
                        bestmodel = output[[best_mod_first_step]]$BestModel,
                        bestmodeldata = output[[best_mod_first_step]]$BestModelData,
                        title=paste((data.frame(lapply(output$combos[best_mod_first_step,], as.character),
                                                             stringsAsFactors=FALSE)), collapse = "_"))


plotdelta(dataset = output[[best_mod_first_step]]$Dataset)

plotbest(dataset = output[[best_mod_first_step]]$Dataset,
         bestmodel = output[[best_mod_first_step]]$BestModel, 
         bestmodeldata =  output[[best_mod_first_step]]$BestModelData)

plotbetas(dataset = output[[best_mod_first_step]]$Dataset)


ggsave("climwin.png", climwin_plot, width = 10, height = 6 )


