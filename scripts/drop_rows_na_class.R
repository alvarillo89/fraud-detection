#################################################################################################
# Elimina todas las filas de la clase dada como argumento con valores perdidos.
#################################################################################################

class <- args()[1]

positive <- data %>% filter(isFraud == 1)
negative <- data %>% filter(isFraud == 0)

if (class == 0){
  negative <- negative %>% drop_na()
} else {
  positive <- positive %>% drop_na()
}

data <- bind_rows(positive, negative)

# Borrar objetos temporales:
remove(positive, negative, class)