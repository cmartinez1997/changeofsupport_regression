# Make sure you have the RStudio project loaded and active
# Then run renv::activate() and renv::restore() when you first setup the project to install packages

library(tidyverse)
library(here)
library(Matrix)
library(kableExtra)
library(easystats)

# Load the data
dat_u <- read_csv(here::here("tree-H", "data", "processed", "utah_climate_growth_rw.csv"))
dat_climate_u <- read_csv(here::here("tree-H", "data", "processed", "utah_climate_data_all.csv"))
dat_bc_u <- read_csv("tree-H/data/processed/es_rw_bc.csv")

# for now truncate dat_bc to 1896 


# Load the functions
source(here::here("tree-H", "R", "check_overlap.R")) 
source(here::here("tree-H", "R", "make_X.R")) 
source(here::here("tree-H", "R", "make_H.R")) 
source(here::here("tree-H", "R", "make_scaling_functions.R")) 
source(here::here("tree-H", "R", "make_annualizeDBH.R"))


# Check the data sources for overlap and lack of overlap

missing_overlap_u <- check_overlap(dat_u, dat_climate_u)

# What data in the tree rings are not in the climate data?
dat_u[missing_overlap_u$tree_CN_missing, ]


unique(dat_u$PLT_CN[missing_overlap_u$tree_CN_missing])

# What data in the climate are not in the tree ring data?
dat_u[missing_overlap_u$tree_year_missing, ]
sort(unique(dat$Year[missing_overlap_u$tree_year_missing]))

# Drop the missing data
message("About ", round(mean((missing_overlap_u$tree_CN_missing) | (missing_overlap_u$tree_year_missing)) * 100, digits = 0), "% of tree ring data will be dropped")
message("About ", round(mean((missing_overlap_u$climate_CN_missing) | (missing_overlap_u$climate_year_missing)) * 100, digits = 0), "% of climate ring data will be dropped")

dat_u         <- dat_u[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
dat_climate_u <- dat_climate_u[!missing_overlap$climate_CN_missing & !missing_overlap$climate_year_missing, ]
dat_bc_u      <- dat_bc_u[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
# Create the design matrix (NOTE: all RW increments are linear combinations of current year climate variables)
# NOTE: This is currently slow but could be made faster
# NOTE: It might make sense to center and scale these here, not currently implemented 
fit_list_u <- make_X(dat_climate_u)
X_u        <- fit_list_u$X
year_id_u  <- fit_list_u$year_id
site_id_u  <- fit_list_u$site_id


# Create the size backcalculated matrix
Z_list_u     <- backcalculate_DBH(dat_bc_u)
Z_u          <- Z_list_u$Z
year_id_Z_u  <- Z_list_u$year_id
tree_id_u   <- Z_list_u$tree_id

Z_dat_u <- as.data.frame(Z_list_u)

ggplot(Z_dat_u) + 
  geom_line(aes(x = year_id, y = Z_u, group = tree_id))

ggplot(dat_u) + 
  geom_line(aes(Year, log(RW + 0.001), group = TRE_CN)) + 
  stat_smooth(aes(Year, log(RW + 0.001)))

ggplot(dat_u) + 
  geom_line(aes(Z, log(RW + 0.001), group = TRE_CN)) + 
  stat_smooth(aes(Z, log(RW + 0.001)))



source(here::here("tree-H", "R", "make_bspline.R"))
# Note: THIS IS TOO LARGE TO FIT AS CURRENTLY IMPLEMENTED
X_splines_u <- make_bspline(X, interaction = FALSE)

source(here::here("tree-H", "R", "make_polynomials.R"))
X_poly_u <- make_polynomials(X, intearction = T)


# Create the H matrix for change of support/alignment
H_u <- make_H(dat_u, fit_list_u)
# Convert H to a sparse matrix
H_u <- Matrix(H_u, sparse = TRUE) # soemthign weird is happening here when doing the spline
# Generate the "design" matrix for a linear regression
HX_u <- as.matrix(H_u %*% X_u)

str(X_u)
str(Z_u)
str(HX_u)

HXZ_u <- cbind(HX_u, Z_u)

HX_splines_u <- as.matrix(H_u %*% X_splines_u)
str(X_splines_u)
str(H_u)

H_u %*% X_splines_u
nnz(H_u)

# Linear model of rw growth as a function of monthly climate

mod <- lm(log(dat$RW + 0.01) ~  HX)
mod_size <- lm(log(dat$RW + 0.01) ~  Z + I(Z^2))
summary(mod)
summary(mod_size)

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

plot(mod_mgcv)

AIC(mod, mod_spline)
BIC(mod, mod_spline)
AIC(mod_mgcv, mod_spline)
BIC(mod_mgcv, mod_spline)


# function to fit regression models, just trying out ----------------------
# before this make sure necessary functions are loaded for this function 
fit_models <- function(dat, dat_climate_u, model_types = c("linear"), scaling = c("none"), random_effects = FALSE) {
  
  climate_vars <- NULL
  clim_norms <- NULL
  clim_locals <- NULL
  
  # figure out which argumentsin scaling to populate
  if ("global" %in% scaling) {
    climate_vars <- c("tmin", "tmax", "ppt", "meantemp", "precip")  
  } 
  if ("local" %in% scaling) {
    clim_norms <- c("meantemp", "precip")  
    clim_locals <- c("tmin", "tmax", "ppt")  
  }
  
  model_results <- list()

  # loop through different models
  for (model_type in model_types) {
    # loop through scaling types
    for (scale_type in scaling) {
      # apply scaling to the dataset
      if (scale_type == "global") {
        dat_climate_scaled <- global_scale(dat_climate, climate_vars)
      } else if (scale_type == "local") {
        dat_climate_scaled <- local_scale(dat_climate, clim_norms, clim_locals)
      } else {
        dat_climate_scaled <- dat_climate
      }
      
      # fit the the model
      if (model_type == "linear") {
        formula <- formula(log(RW + 0.01) ~ HX + 0)
      } else if (model_type == "quadratic") {
        formula <- formula((log(dat$RW + 0.01) ~ HX + I(HX^2) + 0))
      } else if (model_type == "splines") {
        formula <- formula(log(RW + 0.01) ~ HX_splines + 0)
      } else {
        stop("Invalid model type specified.")
      }
      
      model_fit <- lm(formula, data = dat)
      aic_score <- AIC(model_fit)
      model_results[[paste(model_type, scale_type, sep = "_")]] <- aic_score
    }
  }
  aic_df <- data.frame(
    AIC = unlist(model_results)
  )
  aic_table <- aic_df %>%
    kableExtra::kable(escape = FALSE) %>%
    kable_styling(full_width = FALSE)
  
  return(aic_table)
}

fit_models(dat, dat_climate, model_types = c("linear", "quadratic"), scaling = c("local", "global"))





## explore doing this in separate steps

fit_models_1 <- function(dat, dat_climate, model_types = c("linear"), scaling = c("none"), random_effects = FALSE) {
  
  climate_vars <- NULL
  clim_norms <- NULL
  clim_locals <- NULL
  
  # figure out which arguments in scaling to populate
  if ("global" %in% scaling) {
    climate_vars <- c("tmin", "tmax", "ppt", "meantemp", "precip")  
  } 
  if ("local" %in% scaling) {
    clim_norms <- c("meantemp", "precip")  
    clim_locals <- c("tmin", "tmax", "ppt")  
  }
  
  model_results <- list()
  
  # loop through different models
  for (model_type in model_types) {
    # loop through scaling types
    for (scale_type in scaling) {
      # apply scaling to the dataset
      if (scale_type == "global") {
        dat_climate_scaled <- global_scale(dat_climate, climate_vars)
      } else if (scale_type == "local") {
        dat_climate_scaled <- local_scale(dat_climate, clim_norms, clim_locals)
      } else {
        dat_climate_scaled <- dat_climate
      }
      
      # fit the models
      if (model_type == "linear") {
        formula <- formula(log(RW + 0.01) ~ HX + 0)
      } else if (model_type == "quadratic") {
        formula <- formula((log(dat$RW + 0.01) ~ HX + I(HX^2) + 0))
      } else if (model_type == "splines") {
        formula <- formula(log(RW + 0.01) ~ HX_splines + 0)
      } else {
        stop("Invalid model type specified.")
      }
      
      model_fit <- lm(formula, data = dat)
      model_results[[paste(model_type, scale_type, sep = "_")]] <- model_fit
    }
  }
  
  return(model_results)
}


# evalukate models
evaluate_models <- function(model_results) {
  aic_results <- sapply(model_results, function(model) AIC(model))
  aic_df <- data.frame(AIC = aic_results)
  aic_table <- aic_df %>%
    kableExtra::kable(escape = FALSE) %>%
    kable_styling(full_width = FALSE)
  
  return(aic_table)
}


#testing the models

fit_models_1(dat, dat_climate, model_types = c("linear", "quadratic"), scaling = c("global", "local"))

model_results <- fit_models_1(dat, dat_climate, model_types = c("linear", "quadratic"), scaling = c("global"))
print(evaluate_models(model_results))





