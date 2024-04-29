## Annualizing DBH
## Cecilia Martinez 
## April 15 2024
## cecimartinez333@gmail.com

## initial code from Courntey Giebink github repo: 
## modeified by Ceci Martinez
## backcalculate DBH usng ringwidth data

backcalculate_DBH <- function(dat_rw, dat_dbh){
  #TRE_CN,DIA_t,MEASYEAR,Year,RW - need these columns in dataframes 
  #create data frame with empty column for annualized dbh
  #tree_df <- data.frame(TRE_CN,DIA_t,MEASYEAR,Year,RW,DIA_bc = NA)
  #N is the row where measure year and ring width year are the same
  N <- nrow(dat_rw)
  dia_bc <- rep(0, N)
  
  
  N <- which(dat_rw$Year == dat_dbh$MEASYEAR[1]) #next step is to allow N to be ring width year -1
  if(length(N) == 0){
    N <- which(dat_rw$Year + 1 == dat_dbh$MEASYEAR[1])
  }
  if(length(N) > 0){
    Curr_row <- N-1 #each time through subtract 1 and move down one row (or back one year)
    dia_bc[N] <- dat_dbh$DIA_t[N] #dbh when year of ring width and measure year are equal
    while (Curr_row > 0 & !is.na(dia_bc[Curr_row + 1])) { #loop will stop when it gets to the end of data for that tree
      DIA_1 <- dia_bc[Curr_row+1] #or DIA_t[N] for the first round
      RW1 <- dat_rw$RW[Curr_row+1] 
      #convert ring width from mm to inches
      RW1 = RW1 * 0.0393701
      dia_bc[Curr_row] <- DIA_1 - (2*RW1) ## okay this is just the absolute ring width way right now
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  return(tree_df$DIA_bc)
}

