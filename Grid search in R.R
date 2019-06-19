library(ggplot2)
library(caret)

diamonds <- diamonds

# Here we extract all string variables. 
# Actually variables "cut" and "color" we mostly can consider as factor variables and we can transform it to 
# numeric factor variables, but only for testing xgb_model it was easier simply exclude this variables.
diamonds_n <- diamonds[-c(2, 3, 4)]
cor(x = diamonds_n)

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 10,
  eta = c(0.01, 0.001, 0.0001, 0.2, 0.3, 0.4),
  lambda = c(0, 0.1, 0.01, 0.5, 0.7, 0.8, 1),
  alpha = c(0, 0.1, 0.01, 0.5, 0.7, 0.8, 1)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(diamonds_n[-4]),
  y = as.vector(diamonds_n$price),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbLinear"
)

# The best model, which was found with GridSearch
xgb_train_1$bestTune

# Show dataframe with results of all combinations of parameters
xgb_train_1$results

