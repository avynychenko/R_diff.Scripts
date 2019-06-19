library('ggplot2')
diamonds <- diamonds

summary(diamonds)

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

xgb_train_1$bestTune

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y =, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")
