#################################################################################################
# Realiza el análisis de componentes principales (PCA) sobre los datos.
#################################################################################################

# PCA trabaja solo con datos numéricos, y puesto que realizar el one hot encoding de
# las variables categóricas no es viable, solo se escogerán las columnas de tipo "numeric":

data <- data.types %>%
  select_if(is.numeric)

# Eliminamos columnas con más de un 50% de valores perdidos, ya que no pueden tratarse
# con tantos valores a NA:

args <- function() list(50)
source('scripts/clean_na.R')

# Puesto que todas las columnas son numéricas, imputaremos los valores perdidos con
# la mediana:

NA2median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data <- replace(data, TRUE, lapply(data, NA2median))

# Para balancear el dataset, utilizamos el método SMOTE. 
# Con perc.over = 100, le indicamos que doble los ejemplos de la clase 1, y con
# perc.under = 200, indicamos que mantenga la mitad de los valores creados como
# negativos:

# Antes, añadimos de nuevo la variable objetivo:
data <- data %>% mutate(isFraud = data.types$isFraud)

# DESCOMENTAR PARA VOLVER A EJECUTAR:
# data.smoted <- SMOTE(isFraud ~ ., as.data.frame(data), perc.over = 100, perc.under = 200)
# write.csv(data.smoted, 'ieee-fraud-detection/data_smoted.csv', row.names = FALSE)

# Cargamos los datos generados por la ejecución anterior:
data.smoted <- read_csv('ieee-fraud-detection/data_smoted.csv')
data.smoted <- data.smoted %>% mutate_if(is.character, as.factor)

# El siguiente paso es eliminar los outliers detectados en el EDA en el 
# campo TransactionAmnt:

outliers <- boxplot(data.smoted$TransactionAmt, plot=FALSE)$out
data.smoted <- data.smoted[-which(data.smoted$TransactionAmt %in% outliers),]

# Antes de realizar el PCA, hay que eliminar todas aquellas columnas cuya varianza sea 0.
# PCA requiere centrar y escalar los datos, y esto no es posible si alguna columna tiene
# varianza 0:

data.smoted <- data.smoted[, -which(apply(data.smoted, 2, var)==0)]

# Por último, aplicamos PCA:
data <- data.smoted
data$isFraud <- NULL
data.pca <- prcomp(data, center = T, scale. = T)

# Borrar objetos temporales para ahorrar memoria:
remove(outliers, NA2median, data, data.types)
