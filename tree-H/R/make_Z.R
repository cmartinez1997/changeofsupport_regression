

backcalculate_DBH <- function(dat_rw, dat_dbh){
  #so ideally we will have a data file with ring widths and a data file with diameters 
  #TRE_CN,DIA_t,MEASYEAR,Year,RW - need these columns in dataframes  these data frames will be created in the scripts
  #create data frame with empty column for annualized dbh
  #tree_df <- data.frame(TRE_CN,DIA_t,MEASYEAR,Year,RW,DIA_bc = NA)
  #N is the row where measure year and ring width year are the same
  # we also need to make sure that it is coming from the same tree
  
  dia_bc <- rep(0, nrow(dat_rw)) # creating an empty vector with backcalculated diameter, but also multiple cores with one tree 
  
  for (i in 1:nrow(dat_rw)) {
    tree_id <- dat_rw$TRE_CN
  } 
  
  dbh_rows <- which(dat_dbh$TRE_CN == tree_id) # this returns index of values that satisfy the fiven condition
  
  N <- which(dat_rw$Year[i] == dat_dbh$MEASYEAR[dbh_rows]) #the year where ring width matches mesaurement year in dbh data and uears + 1
  if(length(N) == 0){
    N <- which(dat_rw$Year[i] + 1 == dat_dbh$MEASYEAR[dbh_rows]) # if no match is found, try matchin giwth the next year in rind width data with first measruement year in dbh data
  }
  
  if(length(N) > 0){ # if the length >0 then set the current row to be one less than N
    Curr_row <- N-1 #each time through subtract 1 and move down one row (or back one year)
    dia_bc[i] <- dat_dbh$DIA_t[dbh_rows[N]] #assign a dbh when year of ring width and measure year are equal
   
     while (Curr_row > 0 & !is.na(dia_bc[Curr_row + 1])) { #loop will stop when it gets to the end of data for that tree, but how do we know bc it doesn't use a tree id to do this
      DIA_1 <- dia_bc[Curr_row+1] #or DIA_t[N] for the first round, assign previous calcualted diameter to do backcalculation estimates from 
      RW1 <- dat_rw$RW[Curr_row+1] # get rw data for next year
      #convert ring width from mm to inches
      RW1 = RW1 * 0.0393701
      dia_bc[Curr_row] <- DIA_1 - (2*RW1) ## okay this is just the absolute ring width way right now
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  
  dat_rw$DIA_bc <- dia_bc
  return(dat_rw)
  
}

