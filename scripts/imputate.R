#################################################################################################
# Imputa los valores perdidos que no han sido eliminados utilizando KNN para las 
# variables categóricas y la media para las numéricas.
# Recibe como argumento las variables categóricas.
#################################################################################################

categorical = args()

data <- VIM::kNN(data, variable = categorical, k = 3, imp_var = F)
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
data <- replace(data, TRUE, lapply(data, NA2mean))

# Borrar objetos temporales para ahorrar memoria:
remove(NA2mean, categorical)