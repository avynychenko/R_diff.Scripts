## This script is using for matching each pair of files in 2 different directories. Matching by content
# As result function matching() returns files, which can't be compared (one file is in one directory, but
# another directory doesn't have this file)
## As result function identity() will create dataset with 2 columns: name of file and TRUE/FALSE if they
# identical or not

library(data.table)


dir_path_sql <- '/Users/avyny/Desktop/specdataSQL/'
dir_path_r<- '/Users/avyny/Desktop/specdataR/'

names_sql <- list.files(path = dir_path_sql, pattern = '.*\\.csv$')
names_r <- list.files(path = dir_path_r, pattern = '.*\\.csv$')


# Function matching() will print list of unmatched files in both directories and return names of files, which
# totally identical in both directories and we can compare them
matching <- function() {
  files_without_comparing <- list()
  files_without_comparing$names_sql <- setdiff(names_sql, names_r)
  files_without_comparing$names_r <- setdiff(names_r, names_sql)
  if (!identical(names_sql, names_r)) {
    if (length(names_sql) > length(names_r)) {
      resulted_files <- names_sql[-match(files_without_comparing[[1]], names_sql)]
      } else if (length(names_sql) < length(names_r)) {
      resulted_files <- names_r[-match(files_without_comparing[[2]], names_r)]
      } else {
      resulted_files <- names_sql[-match(files_without_comparing[[1]], names_sql)]
    }} else {
      files_without_comparing <- 0
      resulted_files <- names_sql
    }
    cat("files without comparing: \n")
    print(files_without_comparing)
    return(resulted_files)
  } 

# Function identity() will return data frame with result if files in both directories is the same
identity <- function() {
  df_res <- data.frame(matrix(ncol = 2))
  for (i in matching()) {
    file1 <- fread(paste(dir_path_sql, i, sep =""))
    file2 <- fread(paste(dir_path_r, i, sep ="")) 
    df_res <- rbind(df_res, c(i, all.equal(file1, file2, ignore.row.order = TRUE)))
  }
  return(df_res[-1,])
}


