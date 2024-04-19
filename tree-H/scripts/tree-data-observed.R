# Make sure you have the RStudio project loaded and active
# Then run renv::activate() and renv::restore() when you first setup the project to install packages

library(tidyverse)
library(here)
library(Matrix)

# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "climate_growth_rw.csv"))
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "climate_data_all.csv"))
  
# Load the functions
source(here::here("tree-H", "R", "check_overlap.R")) 
source(here::here("tree-H", "R", "make_X.R")) 
source(here::here("tree-H", "R", "make_H.R")) 

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


# Create the design matrix (NOTE: all RW increments are linear combinations of current year climate variables)
# NOTE: This is currently slow but could be made faster
# NOTE: It might make sense to center and scale these here, not currently implemented 
fit_list <- make_X(dat_climate)
X        <- fit_list$X
year_id  <- fit_list$year_id
site_id  <- fit_list$site_id


source(here::here("tree-H", "R", "make_bspline.R"))
# Note: THIS IS TOO LARGE TO FIT AS CURRENTLY IMPLEMENTED
# X_splines <- make_bspline(X, interaction = TRUE)

source(here::here("tree-H", "R", "make_bspline.R"))
X_poly <- make_polynomials(X)


# Create the H matrix for change of support/alignment
H <- make_H(dat, fit_list)
# Convert H to a sparse matrix
H <- Matrix(H, sparse = TRUE)
# Generate the "design" matrix for a linear regression
HX <- as.matrix(H %*% X)

# Linear model of rw growth as a function of monthly climate

mod <- lm(log(dat$RW + 0.01) ~ HX + 0)
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
