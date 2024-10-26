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


# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "wbp_all_climate_growth_rw.csv"))
dat_bc <- read_csv(here::here("tree-H", "data", "processed", "wbp_size_all.csv"))
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
mod_climate_04 <- lmer(log(RW + 0.001) ~ Z + I(Z^2) + meantemp + precip 
                       (1 | STATECD), data = dat_wide_state)

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




#####OKAY CLIMWIN comparison#########
#create ring width dataframe (rwl) for dplR
library(reshape2)

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

#make climate data frame

climate_dat_climwin <- climate_dat %>%
  group_by(year, month) %>%
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    tmax = mean(tmax, na.rm = TRUE),
    ppt = mean(ppt, na.rm = TRUE)
  ) %>%
  ungroup()

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
  filter(year > 1897)

# add the arbitrary month and day
rw_all_crn <- rw_all_crn %>%
  mutate(
    month = 8,
    day = 30,
    Date = as.Date(paste(year, month, day, sep = "-"))
  )

#Convert the climate data to the appropriate format for climwin
climate_dat_climwin_df <- climate_dat_climwin %>%
  mutate(Date = as.Date(paste(year, month, "01", sep = "-")))


# Define the response and climate data
response <- rw_all_crn
climate <- climate_dat_climwin_df
reference_date <- c(30, 8) 

# Define the response and climate variable
response_var <- "std"  
climate_vars <- c("tmax", "tmin", "ppt")

# Run the sliding window analysis
climwin_results <- slidingwin(
  xvar =list(tmax = climate$tmax, 
             # dtr = Clim$dtr, 
             # frs = Clim$frs,# removed wet because model failed to converge 
             tmin = climate$tmin, 
             ppt = climate$ppt
  ),
  cinterval = "month",
  cdate = climate$Date,
  bdate = response$Date,
  baseline = lm(response[[response_var]] ~ 1, data = response),
  type = "absolute",
  refday = reference_date,
  range = c(12, 0),  # Specify the range of months to include in the analysis
  stat = "mean",
  func = c("lin", "quad"),
  cmissing = FALSE)

# View the results
summary(climwin_results)

best_mod_first_step <- which.min(climwin_results$combos$DeltaAICc)
climwin_results$combos[best_mod_first_step,]
climwin_results[[best_mod_first_step]][[1]]

output <- climwin_results

climwin_plot <- plotall(datasetrand = NULL,
                        dataset = output[[best_mod_first_step]]$Dataset, 
                        bestmodel = output[[best_mod_first_step]]$BestModel,
                        bestmodeldata = output[[best_mod_first_step]]$BestModelData,
                        title=paste((data.frame(lapply(output$combos[best_mod_first_step,], as.character), stringsAsFactors=FALSE)), collapse = "_"))


ggsave("climwin.png", climwin_plot, width = 10, height = 6 )


# Create the H matrix for change of support/alignment, this happens fairly quickly
H <- make_H(dat, fit_list) #maps cores to trees with same linked diameter
# Convert H to a sparse matrix
H <- Matrix(H, sparse = T) 

str(H)
# Generate the "design" matrix for a linear regression
HX <- as.matrix(H %*% X)
# binding together all the matrices for the regression
# the problem here is that HX and Z have different number of rows, how to I make sure they are aligned
HXZ <- cbind(HX, Z)
# Warning message:
#   In cbind(HX, Z) :
#   number of rows of result is not a multiple of vector length (arg 2) # how to deal with this?

# Data exploration of the backcalaculated matrix
Z_dat <- as.data.frame(Z_list)

ggplot(Z_dat) + 
  geom_line(aes(x = year_id_Z, y = Z, group = tree_id))

ggplot(dat) + 
  geom_line(aes(Year, log(RW + 0.001), group = TRE_CN)) + 
  stat_smooth(aes(Year, log(RW + 0.001)))


dat %>% mutate(Z = Z) %>%
  ggplot() + 
  geom_point(aes(x = Z, log(RW + 0.001), group = TRE_CN)) 
# stat_smooth(aes(Z, log(RW + 0.001), group = TRE_CN))

data.frame(Z = Z, Year = year_id_Z, TRE_CN = tree_id) %>% 
  left_join(dat) %>% 
  ggplot() + 
  geom_line(aes(x = Year, y = Z, group = TRE_CN, color = TRE_CN)) 



# HX_splines <- as.matrix(H %*% X_splines)
# str(X_splines)
# str(H)
# 
# H %*% X_splines
# nnz(H)

# Linear model of rw growth as a function of monthly climate
sum(dat$RW <= 0)


# mod diagnostics climate only
mod <- lm(log(dat$RW + 0.01) ~  HX)
mod_summary <- summary(mod)
mod_coefficients <- mod_summary$coefficients %>% 
  as.data.frame() 
colnames(mod_coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

significant_coeffs <- mod_coefficients %>%
  filter(`Pr(>|t|)` < 0.05)
significant_coeffs_table <- significant_coeffs %>%
  mutate(Signif = cut(`Pr(>|t|)`, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                      labels = c("***", "**", "*", ".", " ")))
significant_coeffs_table %>%
  kable(format = "html", digits = 4, caption = "Significant Effects Table") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

#size only

mod_size <- lm(log(dat$RW + 0.01) ~  Z + I(Z^2))
modsize_summary <- summary(mod_size)
modsize_coefficients <- modsize_summary$coefficients %>% 
  as.data.frame() 
colnames(modsize_coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

plot(mod_size)
modsize_coefficients %>%
  kable(format = "html", digits = 4, ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
significant_coeffs_size <- modsize_coefficients %>%
  filter(`Pr(>|t|)` < 0.05)
significant_coeffs_table_size <- significant_coeffs_size %>%
  mutate(Signif = cut(`Pr(>|t|)`, 
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                      labels = c("***", "**", "*", ".", " ")))
significant_coeffs_table_size %>%
  kable(format = "html", digits = 4, ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


str(dat)
str(Z)



HX_splines <- as.matrix(H) %*% X_splines
str(HX)
str(X)
str(z_mat$Z)


# include quadratic effects rw as function of monthly climate with quadratic effects

mod_quad <- lm(log(dat$RW + 0.01) ~ HX + I(HX^2) + 0)
summary(mod_quad)


mod_spline <- lm(log(dat$RW + 0.01) ~ HX_splines)
p <- colnames(HX)
print(p)
f <- as.formula(paste0("log(dat$RW + 0.01)~",paste0("s(",p,")",collapse="+")))

mod_mgcv <- gam(f, data = data.frame(HX))

summary(mod_mgcv)
summary(mod_spline)


# plotting the mgcv with the gratia package for visualization
plot(mod_mgcv)

AIC(mod, mod_spline)
BIC(mod, mod_spline)
AIC(mod_mgcv, mod_spline)
BIC(mod_mgcv, mod_spline)



source(here::here("tree-H", "R", "make_bspline.R"))
# Note: THIS IS TOO LARGE TO FIT AS CURRENTLY IMPLEMENTED
# X_splines <- make_bspline(X, interaction = TRUE)

source(here::here("tree-H", "R", "make_bspline.R"))
X_poly <- make_polynomials(X)
X_poly <- make_polynomials(X)


# Create the H matrix for change of support/alignment
H <- make_H(dat, fit_list)
# Convert H to a sparse matrix
H <- Matrix(H, sparse = TRUE)

# Generate the "design" matrix for a linear regression
HX <- as.matrix(H %*% X)

Z[Z <= 0]= 0.1
XZ <- cbind(HX, log(Z))
hist(Z)

tibble(Z = Z, year = z_list$year_id, tree = z_list$tree_id)  %>% 
  ggplot(aes(x=year, y=Z, group = tree)) + 
  geom_line()
# Linear model of rw growth as a function of monthly climate

mod <- lm(log(dat$RW + 0.01) ~ XZ + 0)
summary(mod)


# Generate the "design" matrix for a splines regression
HX_splines <- as.matrix(H %*% matrix(X_splines, dim(X_splines)))

# Linear model of rw growth as a function of monthly climate

mod <- lm(log(dat$RW + 0.01) ~ HX_splines + 0)
summary(mod)


# Generate the "design" matrix for a polynomial regression
HX_poly <- as.matrix(H %*% matrix(X_poly, dim(X_poly)))

# Linear model of rw growth as a function of monthly climate

mod <- lm(log(dat$RW + 0.01) ~ HX_poly + 0)
summary(mod)
# Older code and sketching the ideas for future development.


# setup_model <- function(dat_rw, dat_climate)
# 
# # dat must have plot_id, tree_id, year; may have core_id
# 
# 
# # dat_climate must have plot_id, year, month; may have T_anom, P_anom, T_JJA, P_JJA    
# H <- make_H(data...)
# 
# 
# # no issues with change of support, one tree per plot, etc.
# y = X %*% B
# # H -> I
# 
# # change of support, multiple trees per plot, etc.
# y = (H %*% X) %*% B
# 
# 
# # change of support, multiple trees per plot with nonlinear climate effects (splines)
# y = H %*% (f(X) %*% B)
# 
# # change of support, multiple trees per plot with nonlinear climate effects (splines)
# B = W %*% alpha # W is a spatial basis
# y = (H %*% f(X)) %*% B
# 
# 
# # DLM
# y_t = z_t
# z_t = phi * z_t-1 + (H_t %*% f(X)) %*% B
