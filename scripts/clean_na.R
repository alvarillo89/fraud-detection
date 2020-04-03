#################################################################################################
# Limpia el dataset eliminando columnas con más de un cierto porcentaje de valores perdidos.
# El porcentaje se pasa como argumento.
#################################################################################################

# Extraer el valor del argumento:
per_na <- args()[1]

status <- df_status(data, F)

# Excluir la variable objetivo:
status <- status %>% filter(variable != 'isFraud')

# Seleccionar las columnas a eliminar
na_cols <- status %>% filter(p_na > per_na) %>% select(variable)

# Eliminarlas:
to_remove <- bind_rows(list(na_cols))
data <- data %>% select(-one_of(to_remove$variable))

# Borrar objetos temporales para ahorrar memoria:
remove(status, na_cols, to_remove, per_na)