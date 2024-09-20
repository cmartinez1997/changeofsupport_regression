library(testthat)

source("tree-H/R/check_overlap.R")

# giving a context
test_that("check inputs of check overlap", {
  dat = data.frame() 
  dat_climate = data.frame()
  expect_error(check_overlap(dat, dat_climate), "dat must contain the variable PLOT_CN") 
  # checking if PLOT_CN is in dat_climate
  dat = data.frame(PLOT_CN = 1)
  expect_error(check_overlap(dat, dat_climate), "dat_climate must contain the variable PLOT_CN") 
  
})


# 
test_that("check overlap", {
  dat = data.frame(PLOT_CN = c(1,1,2,2,3,3), 
                   Year = c(1,2,1,2,1,2))
  dat_climate = data.frame(PLOT_CN = c(1,1,2,2,3,3), 
                   growthyear = c(1,2,1,2,1,2))
  expect_equal(check_overlap(dat, dat_climate), 
       list(tree_CN_missing = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
            tree_year_missing = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
       ), climate_CN_missing = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
       climate_year_missing = c(FALSE, FALSE, FALSE, FALSE,FALSE, FALSE)))
  
  # changing the data to make sure output changes

  
  
})



