# Make sure you have the RStudio project loaded and active
# Then run renv::activate() and renv::restore() when you first setup the project to install packages

library(tidyverse)
library(here)
library(Matrix)
library(kableExtra)

# Load tstats# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "climate_growth_rw.csv"))
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "climate_data_all.csv"))

# Load the functions
source(here::here("tree-H", "R", "check_overlap.R")) 
source(here::here("tree-H", "R", "make_X.R")) 
source(here::here("tree-H", "R", "make_H.R")) 
source(here::here("tree-H", "R", "make_scaling_functions.R")) 

# Check the data sources for overlap and lack of overlap
missing_overlap <- check_overlap(dat, dat_climate)

# What data in the tree rings are not in the climate data?
dat[missing_overlap$tree_CN_missing, ]
unique(dat$PLT_CN[missing_overlap$tree_CN_missing])

# What data in the climate are not in the tree ring data?
dat[missing_overlap$tree_year_missing, ]
sort(unique(dat$Year[missing_overlap$tree_year_missing]))

# Drop the missing data
message("About ", round(mean((missing_overlap$tree_CN_missing) | (missing_overlap$tree_year_missing)) * 100, digits = 0), "% of tree ring data will be dropped")
message("About ", round(mean((missing_overlap$climate_CN_missing) | (missing_overlap$climate_year_missing)) * 100, digits = 0), "% of climate ring data will be dropped")

dat         <- dat[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
dat_climate <- dat_climate[!missing_overlap$climate_CN_missing & !missing_overlap$climate_year_missing, ]


# Create the design matrix (NOTE: all RW increments are lindear combinations of current year climate variables)
# NOTE: This is currently slow but could be made faster
# NOTE: It might make sense to center and scale these here, not currently implemented 
fit_list <- make_X(dat_climate)
X        <- fit_list$X
year_id  <- fit_list$year_id
site_id  <- fit_list$site_id


source(here::here("tree-H", "R", "make_bspline.R"))
# Note: THIS IS TOO LARGE TO FIT AS CURRENTLY IMPLEMENTED
# X_splines <- make_bspline(X, interaction = TRUE)

source(here::here("tree-H", "R", "make_polynomials.R"))
X_poly <- make_polynomials(X, intearction = T)


# Create the H matrix for change of support/alignment
H <- make_H(dat, fit_list)
# Convert H to a sparse matrix
H <- Matrix(H, sparse = TRUE)
# Generate the "design" matrix for a linear regression
HX <- as.matrix(H %*% X)

# Linear model of rw growth as a function of monthly climate

mod <- lm(log(dat$RW + 0.01) ~ HX + 0)
summary(mod)

# include quadratic effects rw as function of monthly climate with quadratic effects

mod_quad <- lm(log(dat$RW + 0.01) ~ HX + I(HX^2) + 0)
summary(mode_quad)


# function to fit regression models, just trying out ----------------------
# before this make sure necessary functions are loaded for this function 
fit_models <- function(dat, dat_climate, model_types = c("linear"), scaling = c("none"), random_effects = FALSE) {
  
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
      
      # fit the model using the scaled dataset
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
      
      # Store AIC scores
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

fit_models(dat, dat_climate, model_types = c("linear", "quadratic"), scaling = c("local"))

