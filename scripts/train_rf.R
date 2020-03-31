#################################################################################################
# Entrena y valida un modelo de RandomForest para el problema
# de ieee-fraud detection.
#################################################################################################

# Establecer la semilla para que los resultados sean reproducibles:
set.seed(89)

# Dividir el conjunto de datos en train y val con una proporcion 75-35, manteniendo en
# cada uno la proporción de casos positivos y negativos:
trainIndex <- createDataPartition(data$isFraud, p = .75, list = F, times = 1)
train <- data[trainIndex, ]
val <- data[-trainIndex, ]

#################################################################################################
# Elección de hiperparámetros

# Utilizaremos validación cruzada (5 folds) para escoger el número óptimo de árboles y 
# el número de candidatos sorteados para alimentar el algoritmo (mtry).

# Puesto que sigue habiendo demasiados datos como para hacer cv, utilizaremos para ello
# un subconjunto del conjunto de train, con 5000 ejemplos de cada clase:
train_reduced <- train %>%
   group_by(isFraud) %>%
   sample_n(5000)

trControl <- trainControl(classProbs = T, method = "cv", number = 5)

# Primero escogemos el valor óptimo de mtry:
grid <- expand.grid(.mtry = c(1: 10))

rf_mtry <- train(isFraud ~ .,
                 data = train_reduced,
                 method = "rf",
                 metric = "Accuracy",
                 trControl = trControl,
                 tuneGrid = grid,
                 ntree = 100)
   
# Con ese mejor valor, escogemos ahora el número de árboles optimo.
# Hay que hacerlo de esta manera por un bug de caret con el mtry y
# otros parámetros del grid:
grid <- expand.grid(.mtry = rf_mtry$bestTune$mtry)
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
   set.seed(89)
   rf_maxtrees <- train(isFraud ~ .,
                        data = train_reduced,
                        method = "rf",
                        metric = "Accuracy",
                        trControl = trControl,
                        tuneGrid = grid,
                        ntree = ntree)
   key <- toString(ntree)
   store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#################################################################################################
# Una vez fijados los hiperparámetros, entrenamos todo el modelo

# El mejor valor de accuracy se alcanza para 600 árboles: 0.771
# El mejor valor de accuracy se alncanza para mtry = 10: 0.754
best_n <- 600
best_mtry <- 10

set.seed(89)
grid <- expand.grid(.mtry = best_mtry)

# Empleamos paralelismo para que se entrene más rápido:
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

rf <- train(isFraud ~ ., 
            data = train, 
            method = "rf", 
            metric = "Accuracy", 
            ntree = best_n, 
            tuneGrid = grid)

stopCluster(cl)

# Guardamos el modelo entrenado:
saveRDS(rf, file = "models/rf_v1.rds")