# Make sure you have the RStudio project loaded and active
# Then run renv::activate() and renv::restore() when you first setup the project to install packages

library(tidyverse)
library(here)
library(Matrix)
library(mgcv)
library(gratia)

# Load the data
dat <- read_csv(here::here("tree-H", "data", "processed", "wbp_new_climate_growth_rw.csv"))
dat_bc <- read_csv(here::here("tree-H", "data", "processed", "wbp_size.csv"))
dat_climate <- read_csv(here::here("tree-H", "data", "processed", "wbp_new_climate_data_all.csv"))

# Load the functions
source(here::here("tree-H", "R", "check_overlap.R")) 
source(here::here("tree-H", "R", "make_X.R")) 
source(here::here("tree-H", "R", "make_H.R")) 
source(here::here("tree-H", "R", "make_annualizeDBH.R")) 

# Check the data sources for overlap and lack of overlap
missing_overlap <- check_overlap(dat, dat_climate, dat_bc)

# Drop the missing data # do this for backcaluation
message("About ", round(mean((missing_overlap$tree_CN_missing) | (missing_overlap$tree_year_missing)) * 100, digits = 0), "% of tree ring data will be dropped")
message("About ", round(mean((missing_overlap$climate_CN_missing) | (missing_overlap$climate_year_missing)) * 100, digits = 0), "% of climate ring data will be dropped")
message("About ", round(mean((missing_overlap$bc_tree_CN_missing) | (missing_overlap$bc_year_missing)) * 100, digits = 0), "% of backcalculated data will be dropped")

dat         <- dat[!missing_overlap$tree_CN_missing & !missing_overlap$tree_year_missing, ]
dat_climate <- dat_climate[!missing_overlap$climate_CN_missing & !missing_overlap$climate_year_missing, ]
dat_bc      <- dat_bc[!missing_overlap$bc_tree_CN_missing & !missing_overlap$bc_year_missing, ]


# Create the design matrix (NOTE: all RW increments are linear combinations of current year climate variables)
# NOTE: This is currently slow but could be made faster
# NOTE: It might make sense to center and scale these here, not currently implemented 
fit_list   <- make_X(dat_climate)
X          <- fit_list$X
# rows are plots x time and columns are the environmental covariates
year_id_X  <- fit_list$year_id
site_id    <- fit_list$site_id

# Create the size backcalculated matrix
Z_list     <- backcalculate_DBH(dat_bc)
# rows are trees x years and it is just a single column with backcalculated values
Z          <- Z_list$Z
year_id_Z  <- Z_list$year_id
tree_id    <- Z_list$tree_id

# Create the H matrix for change of support/alignment, this happens fairly quickly
H <- make_H(dat, fit_list) #maps cores to trees with same linked diameter
# Convert H to a sparse matrix
H <- Matrix(H, sparse = T) 


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


# HX_splines <- as.matrix(H %*% X_splines)
# str(X_splines)
# str(H)
# 
# H %*% X_splines
# nnz(H)

# Linear model of rw growth as a function of monthly climate
sum(dat$RW <= 0)

mod <- lm(log(dat$RW + 0.01) ~  HX)
mod_size <- lm(log(dat$RW + 0.01) ~  Z + I(Z^2))
summary(mod)
summary(mod_size)

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
