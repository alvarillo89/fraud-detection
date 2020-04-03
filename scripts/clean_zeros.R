#################################################################################################
# Limpia el dataset eliminando columnas con más de un cierto porcentaje de ceros.
# El porcentaje se pasa como argumento.
#################################################################################################

# Extraer el valor del argumento:
per_zero <- args()[1]

status <- df_status(data, F)

# Excluir la variable objetivo:
status <- status %>% filter(variable != 'isFraud')

# Seleccionar las columnas a eliminar
zero_cols <- status %>% filter(p_zeros > per_zero) %>% select(variable)

# Eliminarlas:
to_remove <- bind_rows(list(zero_cols))
data <- data %>% select(-one_of(to_remove$variable))

# Borrar objetos temporales para ahorrar memoria:
remove(status, zero_cols, to_remove, per_zero)