#################################################################################################
# Selecciona aleatoriamente tantos ejemplos de la clase 0 como ejemplos de
# la clase 1 hay en el dataset.
#################################################################################################

set.seed(89)
# Convertir la variable objetivo a factor para poder hacer el downsample:
data <- data %>% mutate(isFraud = as.factor(ifelse(isFraud == 1, 'Yes', 'No')))
data <- downSample(x = data, y = data$isFraud)
data$Class <- NULL