source("Funciones.R")

##### ARIMA, 10% MCAR

porcentaje_faltantes <- 0.1
n_iteraciones <- 100
h_forecast <- 12     

set.seed(123) 

resultados_acum <- NULL

for (i in 1:n_iteraciones) {
  
  serie_train_NA <- introducir_NA(serie_train, prop = porcentaje_faltantes)
  indices_borrados <- which(is.na(serie_train_NA))
  
  res <- errores(
    serie_train, 
    serie_train_NA, 
    indices_borrados, 
    h = h_forecast, 
    serie_test,
    pronostico = "ARIMA"
  )
  
  if (is.null(resultados_acum)) {
    resultados_acum <- res
  } else {
    resultados_acum[, -1] <- as.matrix(resultados_acum[, -1]) + as.matrix(res[, -1])
  }
}

# Promediar
resultados_finales <- resultados_acum
resultados_finales[, -1] <- resultados_finales[, -1] / n_iteraciones

# Guardar
saveRDS(resultados_finales, file = "Resultados/resultados_MCAR_10_ARIMA.rds")

##### ETS, 10% de faltantes completamente al azar

porcentaje_faltantes <- 0.1
n_iteraciones <- 100
h_forecast <- 12     

set.seed(123) 

resultados_acum <- NULL

for (i in 1:n_iteraciones) {
  
  serie_train_NA <- introducir_NA(serie_train, prop = porcentaje_faltantes)
  indices_borrados <- which(is.na(serie_train_NA))
  
  res <- errores(
    serie_train, 
    serie_train_NA, 
    indices_borrados, 
    h = h_forecast, 
    serie_test,
    pronostico = "ETS"
  )
  
  if (is.null(resultados_acum)) {
    resultados_acum <- res
  } else {
    resultados_acum[, -1] <- as.matrix(resultados_acum[, -1]) + as.matrix(res[, -1])
  }
}

resultados_finales <- resultados_acum
resultados_finales[, -1] <- resultados_finales[, -1] / n_iteraciones

saveRDS(resultados_finales, file = "Resultados/resultados_MCAR_10_ETS.rds")

##### PROPHET, 10% de faltantes completamente al azar

porcentaje_faltantes <- 0.1
n_iteraciones <- 100
h_forecast <- 12     

set.seed(123) 

resultados_acum <- NULL

for (i in 1:n_iteraciones) {
  
  serie_train_NA <- introducir_NA(serie_train, prop = porcentaje_faltantes)
  res <- errores_prophet(serie_train_NA, serie_test, h_forecast)
  
  if (is.null(resultados_acum)) {
    resultados_acum <- res
  } else {
    resultados_acum[, -1] <- resultados_acum[, -1] + res[, -1]
  }
}

resultados_finales <- resultados_acum
resultados_finales[, -1] <- resultados_finales[, -1] / n_iteraciones

saveRDS(resultados_finales, file = "Resultados/resultados_MCAR_10_PROPHET.rds")