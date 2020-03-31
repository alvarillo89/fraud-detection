#################################################################################################
# Selecciona aleatoriamente tantos ejemplos de la clase 0 como ejemplos de
# la clase 1 hay en el dataset.
#################################################################################################

set.seed(89)
positive <- data %>% filter(isFraud == 1)
negative <- data %>% filter(isFraud == 0) %>% sample_n(nrow(positive))
data <- bind_rows(positive, negative)

# Borrar objetos temporales para ahorrar memoria:
remove(positive, negative)