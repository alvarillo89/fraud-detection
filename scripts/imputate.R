#################################################################################################
# Imputa los valores perdidos que no han sido eliminados utilizando KNN para las 
# variables categóricas y la media para las numéricas.
#################################################################################################

data <- kNN(data, variable = c("card2", "card3", "card4", "card5", "card6"), k = 3, imp_var = F)
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
data <- replace(data, TRUE, lapply(data, NA2mean))

# Borrar objetos temporales para ahorrar memoria:
remove(NA2mean)