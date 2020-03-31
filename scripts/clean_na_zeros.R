#################################################################################################
# Limpia el dataset eliminando columnas con más de un cierto porcentaje de ceros 
# y de valores perdidos.
# También elimina todas las filas de la clase 0 con valores perdidos.
#################################################################################################

# Variables que determinan qué porcentajes eliminar:
per_zero <- 70
per_na <- 5

#################################################################################################
# ELIMINAR COLUMNAS:

# Guardamos el status de funModeling:
status <- df_status(data.raw, F)

# Excluir la variable objetivo:
status <- status %>% filter(variable != 'isFraud')

# Seleccionar las columnas a eliminar
na_cols <- status %>% filter(p_na > per_na) %>% select(variable)
zero_cols <- status %>% filter(p_zeros > per_zero) %>% select(variable)

# Eliminarlas:
to_remove <- bind_rows(list(na_cols, zero_cols))
data <- data.raw %>% select(-one_of(to_remove$variable))

#################################################################################################
# ELIMINAR FILAS:

# Eliminamos todas las filas con valores perdidos de la clase 0:
positive <- data %>% filter(isFraud == 1)
negative <- data %>% filter(isFraud == 0) %>% drop_na()
data <- bind_rows(positive, negative)

# Borrar objetos temporales para ahorrar memoria:
remove(positive, negative, per_na, per_zero)
remove(status, na_cols, zero_cols, to_remove)
