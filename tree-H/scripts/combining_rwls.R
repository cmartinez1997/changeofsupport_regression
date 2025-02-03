##Concatenating individual rwl files into the format required for FIA tree-ring data 


#=========================================================
# This script takes individual rwl files that we generate 
# and concatenates them and reformats them from wide to long form
#=========================================================

# loading packages --------------------------------------------------------

library(dplyr)
library(reshape2)
library(dplR)

# load files --------------------------------------------------------------

rwl_files <- list.files("tree-H/data/raw/rwl", pattern = "\\.rwl$", full.names = TRUE)
f <- file.path(rwl_files)

# code chunk from Alex Nolin :) ----------------------------------------------------

pb <- txtProgressBar(min = 0, max = length(f), style = 3) 

d <- lapply(seq_along(f), function(i) {
  require(dplR)
  result <- subset(read.rwl(f[i]))
  setTxtProgressBar(pb, i)
  return(result)
})

close(pb) 

# element in list have names that correspond to file path names
names(d) <- gsub(".*/(.*)\\..*", "\\1", f)
names(d)

rwl_combine <- dplR::combine.rwl(d)

melt(as.matrix(rwl_combine))

# adding the as.matrix function preserves the year variable name 
# turning data into long form instead of wide form
rwl_long <- reshape2::melt(as.matrix(rwl_combine)) %>% 
  rename(Year = Var1, CN = Var2, RW = value)
head(rwl_long)

# remove NA values
rwl_long <- rwl_long %>% 
  drop_na(RW)

# write to csv ------------------------------------------------------------

output_file <- file.path("tree-H/data/raw", "wbp_new_rwl.csv")
write.csv(rwl_long, output_file, row.names = FALSE)

