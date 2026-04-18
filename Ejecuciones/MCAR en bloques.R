source("Funciones.R")

#-------------------------------------------------------------------------------
# 10%, MCAR en bloques, h=12
#-------------------------------------------------------------------------------

porcentaje_faltantes <- 0.1
n_iteraciones <- 100
h_forecast <- 12     

#-------------------------------------------------------------------------------
# ARIMA
set.seed(123) 
resultados_acum <- NULL

for (i in 1:n_iteraciones) {
  
  serie_train_NA <- introducir_bloques_pct(serie_train, prop = porcentaje_faltantes)
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

resultados_finales <- resultados_acum
resultados_finales[, -1] <- resultados_finales[, -1] / n_iteraciones
saveRDS(resultados_finales, file = "Resultados/resultados_MCAR_bloques_10_ARIMA_h12.rds")

#-------------------------------------------------------------------------------

# ETS
set.seed(123) 
resultados_acum <- NULL

for (i in 1:n_iteraciones) {
  
  serie_train_NA <- introducir_bloques_pct(serie_train, prop = porcentaje_faltantes)
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
saveRDS(resultados_finales, file = "Resultados/resultados_MCAR_bloques_10_ETS_h12.rds")

#-------------------------------------------------------------------------------

# PROPHET
set.seed(123) 
resultados_acum <- NULL

for (i in 1:n_iteraciones) {
  
  serie_train_NA <- introducir_bloques_pct(serie_train, prop = porcentaje_faltantes)
  res <- errores_prophet(serie_train_NA, serie_test, h_forecast)
  
  if (is.null(resultados_acum)) {
    resultados_acum <- res
  } else {
    resultados_acum[, -1] <- resultados_acum[, -1] + res[, -1]
  }
}

resultados_finales <- resultados_acum
resultados_finales[, -1] <- resultados_finales[, -1] / n_iteraciones
saveRDS(resultados_finales, file = "Resultados/resultados_MCAR_bloques_10_PROPHET_h12.rds")
