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


