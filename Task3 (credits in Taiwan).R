library(readxl)

data <- read_xls('/Users/avyny/R_diff_scripts/Data/default of credit card clients.xls',skip = 1)

### Calculate new column with total sum of payment X18-X23. ('PAY_AMT1' - 'PAY_AMT6')

data$total_sum_payment <- apply(data, 1, function(x) sum(x[which(names(data) == 'PAY_AMT1'):which(names(data) == 'PAY_AMT6')]))

### NORMALIZE DATA 

## If ranges for variables are very different, then it's a good idea to normalize the 
# variables, which puts them in similar ranges. Use custom function for now. Define function 
# for normalizing data (find 2 different approach)

library(data.table)
data <- as.data.table(data)
data[, `:=` (SEX = as.factor(SEX),
             EDUCATION = as.factor(EDUCATION),
             MARRIAGE = as.factor(MARRIAGE),
             `default payment next month` = as.factor(`default payment next month`))]

## 1. Z-score standartization (or to use in R function scale())
# Calculate normalized data for all numeric data by loops.

z_score <- function(num_vect) {
  return(sapply(num_vect, function(x) (x-mean(num_vect))/sd(num_vect)))
}

result_table_z <- as.data.table(lapply(data, function(x) if (is.numeric(x) & x != data[[1]]) z_score(x) else x))

## 2. Min-max normalization (all values become between 0 and 1)  - (or to use in R function formattable::normalize())
# Calculate normalized data for all numeric data by loops.

min_max <- function(num_vect) {
  return(sapply(num_vect, function(x) (x - min(num_vect))/(max(num_vect) - min(num_vect))))
}

result_table_min_max <- as.data.table(lapply(data, function(x) if (is.numeric(x) & x != data[[1]]) min_max(x) else x))

## Split data into training set (2/3) and testing set (1/3) by random choosing raw
library(dplyr)
train_set <- sample_n(result_table_z, size = 2/3*nrow(data))
test_set <- data[-train_set$ID,]
test_set

# Second option, using sample() function
indx <- sample(1:nrow(result_table_z), size = 2/3*nrow(result_table_z))
train_set2 <- data[indx,]
test_set2 <- data[-indx,]

