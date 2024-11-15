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
library(stringr)

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
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "clim_local_scale.csv"))# Check the data sources for overlap and lack of overlap

# missing_overlap <- check_overlap
# 
# # Drop the missing data # do this for backcaluation
# message("About ", round(mean((missing_overlap$tree_CN_missing) | (missing_overlap$tree_year_missing)) * 100, digits = 0), "% of tree ring data will be dropped")
# message("About ", round(mean((missing_overlap$climate_CN_missing) | (missing_overlap$climate_year_missing)) * 100, digits = 0), "% of climate ring data will be dropped")
# message("About ", round(mean((missing_overlap$bc_tree_CN_missing) | (missing_overlap$bc_year_missing)) * 100, digits = 0), "% of backcalculated data will be dropped")
# 
# dat         <- dat[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
# dat_climate <- dat_climate[!missing_overlap$climate_CN_missing & !missing_overlap$climate_year_missing, ]
# dat_climate <- dat_climate %>% 
#   dplyr::select(-tmin)
# dat_bc      <- dat_bc[!missing_overlap$bc_tree_CN_missing & !missing_overlap$bc_year_missing, ]


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


dat_fit <- dat %>% left_join(dat_bc) %>% left_join(data.frame(Z = Z, Year = year_id_Z, TRE_CN = tree_id)) %>% 
  rename(year = Year)
dat_fit$PLOT_CN <- as.character(dat_fit$PLOT_CN)
dat_climate$PLOT_CN <- as.character(dat_climate$PLOT_CN)


# so we have to create a dataframe to account for seasonal, or time varying monthly climate variables
# dat_all <- dat_fit  %>%  right_join(dat_climate, by = join_by(PLOT_CN == PLOT_CN, Year == year)) %>% 
#   dplyr::select(-year) %>% 
  # filter(!(TRE_CN %in% c("23R48", "23T255"))) # this gets rid of weird outliers

dat_all_seas <- dat_fit %>% left_join(dat_climate, by = c("PLOT_CN", "year"))
  
#okay now remove all rows where climate data = NA.  ###THIS IS THE FINAL DATAFRAME W WILL USE FOR ANALYSIS
dat_all_seas <- dat_all_seas %>%
  filter(!is.na(pJunAug_tmax)) %>% 
  filter(!(TRE_CN %in% c("23R48", "23T255")))


################################# MODEL BUILDING #######################################
############## MODEL 1 ########################

# this model is only a size model 
mod_size <- lm(log(RW + 0.001) ~ Z + I(Z^2), data = dat_all_seas)
summary(mod_size) #matrices align issue - redo the matrix 
plot(mod_size)
check_model(mod_size)

############## MODEL 2 ########################

# model with MAP
mod_climate_01 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + precip, data = dat_all_seas)
summary(mod_climate_01) 
check_model(mod_climate_01)

############## MODEL 3 ########################

# model with MAP/MAT
mod_climate_02 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + precip + meantemp, data = dat_all_seas)
summary(mod_climate_02) 
check_model(mod_climate_02)

############## MODEL 4 ########################

# model with MAP/MAT and aggregated time varying precip 
mod_climate_03 <- lm(log(RW + 0.001) ~ Z + I(Z^2) +  
                     prevJun_currAug_ppt + precip + meantemp, 
                     data = dat_all_seas)
summary(mod_climate_03) 
check_model(mod_climate_03)

############## MODEL 5 ########################

# model with MAP/MAT aggregated time varying precip and previous summer tmax
mod_climate_04 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + prevJun_currAug_ppt + pJunAug_tmax + precip + meantemp, 
     data = dat_all_seas)
summary(mod_climate_04) 
check_model(mod_climate_04) ###Best model with AIC


############## MODEL 6 ######################## Alll of the vars
mod_climate_05 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + JunAug_ppt + pJunAug_ppt + pJunAug_JunAug_ppt + 
                       JunAug_tmax + prevJun_currAug_ppt + pJunAug_tmax + AprMay_tmax + pAprMay_tmax + precip + meantemp, 
                     data = dat_all_seas)
summary(mod_climate_05) 
check_model(mod_climate_05)

#another model 
mod_climate_06 <- lm(log(RW + 0.001) ~ Z + I(Z^2) + prevJun_currAug_ppt + pJunAug_tmax + pAprMay_tmax + precip + meantemp, 
                     data = dat_all_seas)
summary(mod_climate_06) 
check_model(mod_climate_06) ###Best model with AIC


############## Checking models with AIC ######################## 
AIC(mod_climate_01, mod_climate_02, mod_climate_03, mod_climate_04, mod_climate_06)

#best model with AIC is Model 4
model_summary <- broom::tidy(mod_climate_04, conf.int = TRUE)
model_summary <- model_summary %>% filter(term != "(Intercept)")

ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "cadetblue") +
  coord_flip() +  # Flip coordinates for horizontal plot
  geom_hline(yintercept = 0, color = "black", linetype = "dashed",size = 1) +  labs(
    x = "Coefficients", 
    y = "Estimate (with 95% CI)") +
  theme_minimal()


############## MODEL 6 ######################## Alll of the vars
# Add random effect structure 
variable_ppt <- c("JunAug_ppt", "pJunAug_ppt", "pJunAug_JunAug_ppt", "prevJun_currAug_ppt")
variable_tmax <- c("JunAug_tmax", "pJunAug_tmax", "AprMay_tmax", "pAprMay_tmax")
f4 <- as.formula(paste("log(RW + 0.001) ~ Z + I(Z^2) + meantemp + precip", paste(c(variable_ppt, variable_tmax), collapse = " + "), sep = " + "))


mod_climate_RE <- lmer(log(RW + 0.001) ~  Z + I(Z^2) + prevJun_currAug_ppt + 
                         JunAug_tmax + meantemp + precip + (1 + Z | TRE_CN), data = dat_all_seas)

mod_climate_RE_z <- lmer(log(RW + 0.001) ~  Z + (prevJun_currAug_ppt + 
                         JunAug_tmax + meantemp + precip)^2 + (1 + Z | TRE_CN), data = dat_all_seas)

mod_climate_RE_z <- lmer(log(RW + 0.001) ~  Z + prevJun_currAug_ppt + 
                                                   JunAug_tmax + meantemp*JunAug_tmax + precip*prevJun_currAug_ppt + (1 + Z | TRE_CN), data = dat_all_seas)

anova(mod_climate_RE, mod_climate_RE_z)

preds <- predict(mod_climate_RE_z)
str(preds)

all_cns <- unique(dat_all_seas$TRE_CN)
n_plot  <- 36

plot_cns <- sample(all_cns, n_plot)

dat_all_seas %>% drop_na() %>% 
  mutate(preds = preds) %>% 
  filter(TRE_CN %in% plot_cns) %>% 
  ggplot() + geom_point(aes(x = Z, y = (RW), colour = TRE_CN)) + theme(legend.position = "none")  +
 stat_smooth(aes(x = Z, y = (RW), colour = TRE_CN), method = "lm") + 
  theme_classic() + facet_wrap(~TRE_CN)

dat_all_seas %>% drop_na() %>% 
  mutate(preds = preds) %>% 
  filter(TRE_CN %in% plot_cns) %>% 
  ggplot()  +
  geom_point(aes(x = Z, y = (RW), colour = TRE_CN)) + 
  stat_smooth(aes(x = Z, y = (RW), colour = TRE_CN), method = "lm", se = F) + 
  stat_smooth(aes(x = Z, y = (RW)), color = "blue", method = "lm", se = F, linewidth = 2) + 
  theme_classic() + theme(legend.position = "none") + facet_wrap(~TRE_CN)

dat_all_seas %>% drop_na() %>% 
  mutate(preds = preds) %>%
 filter(TRE_CN %in% plot_cns) %>% 
  ggplot()  +
  geom_point(aes(x = Z, y = (RW), colour = TRE_CN)) + 
  stat_smooth(aes(x = Z, y = (RW), colour = TRE_CN), method = "lm", se = F, formula = y~ x + I(x^2)) + 
  stat_smooth(aes(x = Z, y = (RW)), color = "blue", method = "lm", se = F, linewidth = 2, formula = y~ x + I(x^2)) + 
  theme_classic() + theme(legend.position = "none") + facet_wrap(~TRE_CN)

model_summary <- broom::tidy(mod_climate_RE_z, conf.int = TRUE)
model_summary <- model_summary %>% filter(term != "(Intercept)")

ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "cadetblue") +
  coord_flip() +  # Flip coordinates for horizontal plot
  geom_hline(yintercept = 0, color = "black", linetype = "dashed",size = 1) +  labs(
    x = "Coefficients", 
    y = "Estimate (with 95% CI)") +
  theme_minimal()

### OKAY comparing Mixed effects and looking at some model diagnostics

par(mfrow = c(1, 2))
plot(fitted(mod_climate_RE), resid(mod_climate_RE), main = "Mixed Effects")
plot(fitted(mod_climate_04), resid(mod_climate_04), main = "Fixed Effects")

par(mfrow = c(1, 2))

# QQ plot for mixed-effects model residuals
qqnorm(resid(mod_climate_RE), main = "QQ Plot: Mixed Effects")
qqline(resid(mod_climate_RE), col = "red")

# QQ plot for fixed-effects model residuals
qqnorm(resid(mod_climate_04), main = "QQ Plot: Fixed Effects")
qqline(resid(mod_climate_04), col = "blue")


#R^2 for mixed effects models, this uses the performance package
r2_mixed <- r2(mod_climate_RE)
print(r2_mixed)

#R^2 for fixed effects models
r2_fixed <- summary(mod_climate_04)$r.squared
print(r2_fixed)



###Regularization###
# using lassos


dat_all_seas <- dat_all_seas %>% 
  select(-DIA)
x <- model.matrix(log(RW + 0.001) ~ Z + I(Z^2) + . - 1 - TRE_CN -PLOT_CN -year, data = dat_all_seas)
y <- dat_all_seas %>% 
  drop_na() %>% 
  pull(RW)
str(y)


mod_net <- cv.glmnet(
  x, log(y + 0.001), 
  alpha = 1,         
  nfolds = 10        
)
summary(mod_net)
plot(mod_net)
coef(mod_net, s = "lambda.min")

ggplot(data = dat_all_seas, aes(x = log(Z), y = log(RW))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")

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


