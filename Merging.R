## The goal of this script is merging 4 files in one. 
# In the directory, which should be set up in variable "source" there are a lot of files and each 4 files 
# has their unique name, ending with numbers plus additional letters. 
## This scripts is merging each 4 files and write resyult in new file with ending "_full.csv"

library(data.table)

setwd("/Users/avyny/Desktop/specdata")
source <- '/Users/avyny/Desktop/specdata'
names <- list.files(path = source, pattern = '.*\\.csv$')

indexes = grep(pattern = ".*\\d{6}\\.csv$", names)
unique_names <- gsub("\\.csv$", "", names[indexes])


for (i in unique_names) {
  files_for_merge <- names[grep(i, names)]
  if (paste0(i, "_full.csv") %in% files_for_merge) {
    next
  }
  # df <- do.call(rbind, lapply(files_for_merge[c(4, 1:3)], fread))
  df <- do.call(rbind, lapply(files_for_merge, fread))
  fwrite(df, file = paste0(i, "_full.csv"))
}


