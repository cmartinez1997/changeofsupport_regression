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

mod_ad <- glmnet(x, y)
print(mod_ad)
mod_net <- cv.glmnet(x, y)
print(mod_net)
plot(mod_net)
summary(mod_net)
coef(mod_net, s = "lambda.min")
coefficients <- as.data.frame(as.matrix(coef(mod_net, s = "lambda.1se")))
colnames(coefficients) <- "Coefficient"
coefficients <- data.frame(Variable = rownames(coefficients), Coefficient = coefficients$Coefficient, row.names = NULL)
coefficients <- coefficients[coefficients$Coefficient != 0 & !is.na(coefficients$Coefficient), ]
coefficients %>%
  kbl(digits = 6, format = "html", caption = "Coefficients Table") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(2, bold = TRUE, color = ifelse(is.na(coefficients$Coefficient), "gray", "black"))


