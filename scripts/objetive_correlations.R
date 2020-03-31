#################################################################################################
# Elimina aquellas columnas que tengan menos de un cierto valor de correlación
# con la variable objetivo.
#################################################################################################

# Variable que determina la correlación mínima:
target_correlation <- 0.05

# Convertir los factors a números y calcular la matriz de correlaciones:
data.num <- data %>% mutate_if(is.factor, as.numeric)
cor_target <- correlation_table(data.num, target='isFraud')

# Quedarnos con las que tienen una correlación superior al umbral:
important_vars <- cor_target %>% filter(abs(isFraud) >= target_correlation)
data <- data %>% select(one_of(important_vars$Variable))

# Borrar objetos temporales para ahorrar memoria:
remove(data.num, cor_target, important_vars, target_correlation)