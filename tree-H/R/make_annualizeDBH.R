## Annualizing DBH
## Cecilia Martinez 
## April 15 2024
## cecimartinez333@gmail.com
## backcalculate tree diameter measurements using FIA DBH and ringwidth data

backcalculate_DBH <- function(dat_bc, verbose = TRUE){ # this function requires a df in a specific format with the argument of that df - see datawurangling_sizegrwoth
  # this only handles one core per tree at the moment
  # TO DO: throw warning if more than one core per tree - add this
  dat <- dat_bc |>
    mutate(RW_in = RW * 0.0393701) |> 
    dplyr::group_by(TRE_CN) |>
    dplyr::arrange(desc(Year)) |>
    dplyr::mutate(cum_dia_change = cumsum(dplyr::lag(RW_in, default = 0))) |> 
    dplyr::mutate(total_rw_change = 2 * cum_dia_change) |> #look into dplyr time series tools 
    dplyr::mutate(dia_est = DIA - total_rw_change) |> # this deals with only one DBH/DRC measurement at time of coring, think about more than one DBH meas
    dplyr::mutate(dia_est = case_when(dia_est < 0 ~ 0, TRUE ~dia_est)) %>% # check for when cum_dia change <0
    ungroup()
  
  # this is setting up for matrix construction (Z-matrix)
  years <- sort(unique(dat$Year))
  trees <- unique(dat$TRE_CN)
  
  # initializing the empty matrix #length years by length trees
  
  Z             <- matrix(0, length(years) * length(trees), 1) # this holds the backcauculated dbh values
  colnames(Z)   <- "dbh_bc" #column name of Z is dbh_bc
  row_names     <- rep(0, length(years) * length(trees))
  year_id       <- rep(0, length(years) * length(trees))
  tree_id       <- rep(0, length(years) * length(trees))
  site_id       <- rep(0, length(years) * length(trees))

  
  idx <- 1 # index variable to keep track of current row in the matrix Z
  
  # Z <- dat %>%
  #   arrange(match(Year, years), match(TRE_CN, trees)) %>%
  #   # complete(Year, TRE_CN, fill = 0) %>%
  #   pull(dia_est, years, trees)
  
  for (i in 1:length(years)) { #this iterates over each year i
    if (verbose) message("On year ", i, " out of ", length(years)) 
    for (j in 1:length(trees)) { # this iterates over each tree in each year
      
      temp <-   dat |>
        filter(Year == years[i], TRE_CN == trees[j]) |>  #assumes every year has a ring width measurement, this filters data to only include rows for the current year and tree
        select(-"Year") |> #
        select(-c("TRE_CN", "MEASYEAR", "DIA", "RW", "RW_in", "cum_dia_change", "total_rw_change", "PLOT_CN")) |>
        unlist() 
      
      temp_site <-   dat |>
        filter(Year == years[i], TRE_CN == trees[j]) |>  #assumes every year has a ring width measurement, this filters data to only include rows for the current year and tree
        select(-"Year") |> #
        select(-c("TRE_CN", "MEASYEAR", "DIA", "RW", "RW_in", "cum_dia_change", "total_rw_change", "dia_est")) |>
        unlist() 
      
      # this stores the values in the matrix
      if (length(temp) > 0 ) { #checks if temp contains any values, if it does, it stores the result
        Z[idx, ] <- temp #okay this instersts the value from temp into matrix Z and current index (idx)
        row_names[idx]     <- paste(years[i], trees[j])  # stores corresponding year and tree identifiers for this row in row names, year id adn tree id
        year_id[idx]       <- years[i]
        tree_id[idx]       <- trees[j]
        site_id[idx]       <- temp_site
        idx                <- idx + 1  #then it increments the inex to move to the net row in Z
        
      }
      
    }
  }
  Z <- Z[1:(idx-1), ] #Trims the matrix Z to remove any unused rows (those with 0s, if the loop didn't use all rows)
  year_id <- year_id[1:(idx-1)] #also trims
  tree_id <- tree_id[1:(idx-1)]
  site_id <- site_id[1:(idx-1)]
  

  # rownames(X) <- row_names
  return(list(Z = Z, year_id = year_id, tree_id = tree_id, site_id = site_id)) #returns a list with three elements. Z matrix, year_id(correspoding years for each row), and tree_id(corresopding tree Ids for each row)
}
  

# # Calculate proportional diameter estimates
# wbp_rw_bc_final <- wbp_rw_bc_final |>
#   group_by(TRE_CN) |> 
#   mutate(
#     proportion = ifelse(
#       Year > MEASYEAR1 & Year <= MEASYEAR2,
#       (Year - MEASYEAR1) / (MEASYEAR2 - MEASYEAR1),
#       NA_real_
#     ),
#     dia_est = case_when(
#       Year <= MEASYEAR1 ~ DIA1 - total_rw_change,
#       Year > MEASYEAR1 & Year <= MEASYEAR2 ~ DIA1 - proportion * (DIA1 - DIA2) - total_rw_change,
#       TRUE ~ DIA2 - total_rw_change
#     )
#   )

#