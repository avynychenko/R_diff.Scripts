# Filling NA in R

library(data.table)
library(tidyverse)
library(xgboost)
library(knitr)
library(viridis)
library(psych)

data(mtcars)

# Select columns to be turned to factors
facs <- c('cyl', 'gear', 'carb')
outvar <- 'mpg'
mtcars <- mtcars[, c(facs, outvar)]

# Upsample the row count of mtcars
num_rows <- 1000
mtcars <- mtcars[sample(1:nrow(mtcars), num_rows, replace = T),]

# Add NAs
set.seed(8)
na_num <- 20
for (i in facs) {
  mtcars[[i]][sample(1:nrow(mtcars), na_num, replace = F)] <- NA
}
knitr::kable(head(mtcars), align = c(rep("l", 4), rep("c", 4), rep("r", 4)), caption = 'Some rows from our mtcars dataset')

## Visualization of NA quantity
mtcars %>% map_df(~sum(is.na(.))) %>% 
  tidyr::gather(key, value) %>% 
  ggplot(aes(x = key, y = value, fill = key)) + 
  geom_bar(stat = 'identity', alpha = 0.8) + 
  viridis::scale_fill_viridis(discrete = T) + 
  ylab('Number of NAs') + 
  xlab('Column Name') + 
  labs(title = 'Number of NAs in Each Column')

## XGBoost
#### Since XGBoost needs a matrix, we have to manually do the work of creating dummy columns from factor columns. We’ll use R’s model.frame 
#### function to do this — there is a dummies package that claims to do this but it doesn’t work very well.

fac_df <- mtcars[, c(facs)]
out_vec <- mtcars[, outvar]

fac_df <- lapply(fac_df, factor)

out_vec %>% 
  data_frame(mpg = .) %>% 
  ggplot(aes(x = mpg)) + 
  geom_density(fill = viridis::viridis(1), alpha = 0.6) + 
  labs(title = "Distribution of the MPG target column")

ff <-  ~ . - 1
mf <- model.frame(formula = ff, data = fac_df, na.action = na.pass)
fac_mat_v1 <- model.matrix(object = ff, data = mf, contrasts.arg = map(mf, ~ contrasts(., contrasts = F)))
head(fac_mat_v1)

## Check "shape" (dimensional) of matrix, which we transformed factors to dummy-variables
dim(fac_mat_v1) # 1000 x 12

# Checking quantity of NA-s in new matrix
fac_mat_v1 %>% apply(2, function(x) sum(is.na(x)))

#### !!! XGBoost Round 1: Dense Matrix, Missing=NA. Left NA as they are

model_mat_v1 <- fac_mat_v1
# Split the data up into training and testing
set.seed(8)
train_idx <- sample(1:nrow(model_mat_v1), 0.8 * nrow(model_mat_v1), replace = F)
test_idx <- setdiff(1:nrow(model_mat_v1), train_idx)
train_v1 <- model_mat_v1[train_idx,]
test_v1 <- model_mat_v1[-train_idx,]
train_out_vec <- out_vec[train_idx]
test_out_vec <- out_vec[-train_idx]

# xgb_v1 <- xgboost(data = train_v1, label = train_out_vec, nrounds = 20, verbose = F)
xgb_v1 <- xgboost(data = train_v1, label = train_out_vec, nrounds = 20, verbose = F, missing = NA)
preds_v1 <- predict(xgb_v1, newdata = test_v1, missing = NA)

# The results are plotted with the line `y=x` dashed added to the plot to give a sense for the goodness of fit. 
data_frame(Actuals = test_out_vec, Predictions = preds_v1) %>% 
  ggplot(aes(x = Actuals, y = Predictions)) +
  geom_point(color = palette()[4]) + 
  geom_abline(intercept = 0, slope = 1, color = 'black', linetype = 'dashed') + 
  geom_point(color = palette()[4], size = 2, alpha = 0.5) +
  labs(title = 'Predictions for First XGBoost Model - Dense Matrix Not Filled In')


#### !! XGBoost Round 2: Sparse Matrix (we can see differencies between predictions of model where it's ordinary 
#### matrix of sparse matrix)

library(Matrix)
train_v2 <- Matrix::Matrix(model_mat_v1[train_idx,], sparse = T)
test_v2 <- Matrix::Matrix(model_mat_v1[-train_idx,], sparse = T)
xgb_v2 <- xgboost(data = train_v2, label = train_out_vec, nrounds = 20, verbose = F, missing = NA)
preds_v2 <- predict(xgb_v2, newdata = test_v2, missing = NA)

data_frame(Model1Predictions = preds_v1, Model2Predictions = preds_v2) %>% 
  ggplot(aes(x = Model1Predictions, y = Model2Predictions)) +
  geom_point(colour = "red", size = 2) + 
  geom_abline(intercept = 0, slope = 1, color = 'black', linetype = 'dashed') + 
  labs(title = 'Comparing Predictions Between XGBoost Models 1 and 2')


#### !! XGBoost Round 3: Dense Matrix, 0-Filled (Filling 0 instead of NA)

train_v3 <- train_v1
train_v3[is.na(train_v3)] <- 0
test_v3 <- test_v1
test_v3[is.na(test_v3)] <- 0
xgb_v3 <- xgboost(data = train_v3, label = train_out_vec, nrounds = 20, verbose = F, missing = NA)
preds_v3 <- predict(xgb_v3, newdata = test_v3, missing = NA)
data_frame(Model1Predictions = preds_v1, Model3Predictions = preds_v3) %>% 
  ggplot(aes(x = Model1Predictions, y = Model3Predictions)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = 0, slope = 1, color = 'black', linetype = 'dashed') + 
  labs(title = 'Comparing Predictions Between XGBoost Models 1 and 3')
