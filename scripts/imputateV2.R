#################################################################################################
# Imputación utilizada en test, debido a que KNN no puede emplearse para imputar variables
# catégoricas. Imputa estas últimas de forma aleatoria, atendiendo a una distribución
# de probablidad obtenida a partir del porcentaje de aparción de cada level del factor.
#################################################################################################

library(Hmisc)

set.seed(89)

imputeValues <- function(colname){
  length <- sum(is.na(data[[colname]]))
  tab <- as.data.table(prop.table(table(data[[colname]])))
  sample(tab$V1, size = length, replace = T, prob = tab$N)
}

for (i in colnames(data)){
  if (class(data[[i]]) == "factor" && sum(is.na(data[[i]])) != 0){
    data[[i]] <- as.character(data[[i]])
    data[[i]] <- as.character(impute(data[[i]], imputeValues(i)))
    data[[i]] <- as.factor(data[[i]])
  } else if (class(data[[i]]) == "numeric" && sum(is.na(data[[i]])) != 0) {
    data[[i]] <- as.numeric(impute(data[[i]], fun=median))
  }
}

remove(i, imputeValues)