#################################################################################################
# Entrena un modelo de eXtreme Gradient Boosting para el problema
# de ieee-fraud detection.
#################################################################################################

set.seed(89)

# Elección de hiperparámetros

# Puesto que son muchos parámetros que ajustar, dejaremos que sea caret quien haga unas
# cuantas pruebas. Para ello, usamos un conjunto bastante reducido de los datos (más aún
# con random forest, solo 1000 ejemplos de cada clase)
train_reduced <- train %>%
 group_by(isFraud) %>%
  sample_n(1000) %>%
  ungroup()

# Dejamos que caret busque los hiperparámetros:
xgb_hyp <- train(
  isFraud ~ .,
  train_reduced,
  method = 'xgbTree',
  metric = "Accuracy"
)

# The final values used for the model were nrounds = 150, max_depth = 3, eta = 0.3, 
# gamma = 0, colsample_bytree = 0.6, min_child_weight = 1 and subsample = 0.75.

#################################################################################################
# Ahora entrenamos con todos los datos de train, y los hiperparámetros escogidos:

xgbGrid <- expand.grid(nrounds = 150,
                       max_depth = 3,
                       eta = 0.3,
                       gamma = 0,
                       colsample_bytree = .6,
                       min_child_weight = 1,
                       subsample = 0.75)

xgb <- train(
  isFraud ~ .,
  train,
  method = 'xgbTree',
  metric = "Accuracy",
  tuneGrid = xgbGrid
)

# Guardamos el modelo entrenado:
saveRDS(xgb, file = "models/xgb_v1.rds")