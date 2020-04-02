#################################################################################################
# Entrena un modelo de Penalized Logistic Regression para el problema
# de ieee-fraud detection.
#################################################################################################

# Definimos una función para el preprocesamiento:

# Dado un dataframe y un vector de nombres de columnas que sean
# factors, sustituye todos aquellos levels con una ocurrencia
# menor a thr por new_label:
redDiversity <- function(df, colnames, thr, new_label) {
  out <- df
  for (col in colnames) {
    to_remove <- names(which(table(out[col]) < thr))
    patt <- do.call(paste, c(as.list(to_remove), sep = "|"))
    out <- mutate(out, !!col := as.factor(case_when(
      grepl(x = !!as.symbol(col), pattern = patt) ~ new_label,
      TRUE ~ as.character(!!as.symbol(col))
    )))
  }
  out
}

# Elección de hiperparámetros

# Utilizaremos validación cruzada (5 folds) para escoger el valor óptimo de lambda
# (el cual controla la influencia de la penalización L2):

trControl <- trainControl(classProbs = T, method = "cv", number = 5)

# Puesto que sigue habiendo demasiados datos como para hacer cv, utilizaremos para ello
# un subconjunto del conjunto de train, con 5000 ejemplos de cada clase:
train_reduced <- train %>%
  group_by(isFraud) %>%
  sample_n(5000) %>%
  ungroup()

# Reducimos la diversidad de las variables categóricas:
train_reduced <- redDiversity(train_reduced, c("card3", "card5"), 20, "other")

# Dividir en X e Y:
X <- train_reduced %>% select(-c("isFraud"))
y <- train_reduced$isFraud

# One Hot encoding de los factors:
dmy <- dummyVars("~ .", data = X)
X <- data.frame(predict(dmy, newdata = X))

# Normalizar las variables numéricas:
to_norm = colnames(select_if(train_reduced, is.numeric))
X <- predict(preProcess(X, method = list(center = to_norm, scale = to_norm)), X)

# Crear la rejilla:
library(pracma)
grid <- expand.grid(.lambda = logspace(-4, 4, 10), .cp = "bic")

# Elegir hiperparámetros:
set.seed(89)
lr_lambda <- train(X, y,
  method = 'plr',
  metric = "Accuracy",
  trControl = trControl,
  tuneGrid = grid
)

