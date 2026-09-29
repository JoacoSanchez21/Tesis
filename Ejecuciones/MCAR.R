source("Funciones.R")

#-------------------------------------------------------------------------------
# 10%, h=3
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.10, 
  h = 3, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_10_ARIMA_h3.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.10, 
  h = 3, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_10_ETS_h3.rds"
)

### PROPHET
porcentaje_faltantes <- 0.10
n_iteraciones <- 100
h_forecast <- 3
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_10_PROPHET_h3.rds")
#-------------------------------------------------------------------------------
# 25%, h=3
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.25, 
  h = 3, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_25_ARIMA_h3.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.25, 
  h = 3, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_25_ETS_h3.rds"
)

### PROPHET
porcentaje_faltantes <- 0.25
n_iteraciones <- 100
h_forecast <- 3
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_25_PROPHET_h3.rds")

#-------------------------------------------------------------------------------
# 40%, h=3
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.4, 
  h = 3, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_40_ARIMA_h3.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.4, 
  h = 3, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_40_ETS_h3.rds"
)

### PROPHET
porcentaje_faltantes <- 0.4
n_iteraciones <- 100
h_forecast <- 3
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_40_PROPHET_h3.rds")
#-------------------------------------------------------------------------------
# 10%, h=6
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.10, 
  h = 6, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_10_ARIMA_h6.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.10, 
  h = 6, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_10_ETS_h6.rds"
)

### PROPHET
porcentaje_faltantes <- 0.10
n_iteraciones <- 100
h_forecast <- 6
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_10_PROPHET_h6.rds")
#-------------------------------------------------------------------------------
# 25%, h=6
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.25, 
  h = 6, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_25_ARIMA_h6.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.25, 
  h = 6, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_25_ETS_h6.rds"
)

### PROPHET
porcentaje_faltantes <- 0.25
n_iteraciones <- 100
h_forecast <- 6
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_25_PROPHET_h6.rds")

#-------------------------------------------------------------------------------
# 40%, h=6
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.4, 
  h = 6, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_40_ARIMA_h6.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.4, 
  h = 6, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_40_ETS_h6.rds"
)

### PROPHET
porcentaje_faltantes <- 0.4
n_iteraciones <- 100
h_forecast <- 6
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_40_PROPHET_h6.rds")

#-------------------------------------------------------------------------------
# 10%, h=12
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.11, 
  h = 12, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_10_ARIMA_h12.rds"
)


### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.1, 
  h = 12, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_10_ETS_h12.rds"
)

### PROPHET
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_10_PROPHET_h12.rds")

#-------------------------------------------------------------------------------
# 25%, h=12
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.25, 
  h = 12, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_25_ARIMA_h12.rds"
)

### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.25, 
  h = 12, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_25_ETS_h12.rds"
)

### PROPHET
porcentaje_faltantes <- 0.25
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_25_PROPHET_h12.rds")

#-------------------------------------------------------------------------------
# 40%, h=12
#-------------------------------------------------------------------------------

### ARIMA
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.4, 
  h = 12, 
  modelo = "ARIMA", 
  resultado = "Resultados/MCAR/resultados_MCAR_40_ARIMA_h12.rds"
)

### ETS
set.seed(123)
ejecutar_simulacion(
  serie_train = serie_train, 
  serie_test = serie_test, 
  mecanismo_na = introducir_NA,
  prop = 0.4, 
  h = 12, 
  modelo = "ETS", 
  resultado = "Resultados/MCAR/resultados_MCAR_40_ETS_h12.rds"
)

### PROPHET
porcentaje_faltantes <- 0.4
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
saveRDS(resultados_finales, file = "Resultados/MCAR/resultados_MCAR_40_PROPHET_h12.rds")
