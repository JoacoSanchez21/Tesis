Sys.setenv(TZ = "GMT") 


library(dplyr)
library(fable)
library(fpp3)
library(forecast)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(gt)
library(imputeTS)
library(knitr)
library(lubridate)
library(prophet)
library(readr)
library(tidyr)
library(tidyverse)
library(tseries)
library(tsibble)
library(tsibbledata)
library(zoo)

set.seed(123)

datos <- read_csv("demanda.csv", show_col_types = FALSE) %>%
  mutate(fecha = ymd(fecha),
         anio  = year(fecha),
         mes  = month(fecha),
         demanda = demanda/1000)
vals <- datos$demanda

ts <- ts(vals, 
         start = c(2001, 1), 
         frequency = 12)

serie <- window(ts, end = c(2024, 12))
serie_train <- window(serie,start=c(2010,1), end = c(2024, 12))
serie_test <- window(ts, start = c(2025, 1)) 


#-------------------------------------------------------------------------------
#MCAR
introducir_NA <- function(y, prop = 0.1) {
  y_na <- y
  n <- length(y)
  idx <- sample(2:(n - 1), size = floor(prop * n))
  y_na[idx] <- NA
  return(y_na)
}

#-------------------------------------------------------------------------------
#En bloques

introducir_bloques_pct <- function(y, prop = 0.1, tam_min = 2, tam_max = 10) {
  y_na <- y
  n <- length(y)
  total_na <- ceiling(prop * n)
  na_actual <- 0
  while (na_actual < total_na) {
    tam_bloque <- sample(tam_min:tam_max, 1)
    if (na_actual + tam_bloque > total_na) {
      tam_bloque <- total_na - na_actual
    }
    inicio <- sample(2:(n - tam_bloque), 1)
    indices <- inicio:(inicio + tam_bloque - 1)
    nuevos_na <- sum(!is.na(y_na[indices]))
    y_na[indices] <- NA
    na_actual <- na_actual + nuevos_na
  }
  return(y_na)
}

#-------------------------------------------------------------------------------
# En picos
introducir_picos <- function(y, percentil = 0.8, prop = 0.1) {
  y_na <- y
  n <- length(y)
  umbral <- quantile(y, percentil, na.rm = TRUE)
  candidatos <- which(
    y > umbral &
      seq_along(y) > 2 &
      seq_along(y) <= n - 2
  )
  n_eliminar <- floor(prop * length(candidatos))
  idx <- sample(candidatos, n_eliminar)
  y_na[idx] <- NA
  return(y_na)
}


#-------------------------------------------------------------------------------
# MEDIDAS DEL ERROR
# 1) Métricas de imputación
metricas_imputacion <- function(serie_real, serie_imp, indices_na) {
  error <- serie_imp[indices_na] - serie_real[indices_na]
  data.frame(
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE  = mean(abs(error), na.rm = TRUE),
    MAPE = mean(abs(error) / pmax(abs(serie_real[indices_na]), 1e-8) * 100, na.rm = TRUE)
  )
}

# 2) Ajuste y pronóstico
ajustar_pronostico <- function(serie, modelo = c("ARIMA", "ETS"), h = 1) {
  modelo <- match.arg(modelo)
  if (modelo == "ARIMA") {
    forecast(auto.arima(serie), h = h)
  } else {
    forecast(ets(serie), h = h)
  }
}

# 3) Métricas de pronóstico
metricas_pronostico <- function(pronostico, serie_test) {
  acc <- accuracy(pronostico, serie_test)
  data.frame(
    RMSE = acc["Test set", "RMSE"],
    MAE  = acc["Test set", "MAE"],
    MAPE = acc["Test set", "MAPE"]
  )
}

# 4) Imputadores
imputadores <- list(
  `Valor aleatorio`                              = function(x) na_random(x),
  `Valor definido`                               = function(x) na_replace(x),
  `Valor medio`                                  = function(x) na_mean(x, option = "mean"),
  `Valor mediano`                                = function(x) na_mean(x, option = "median"),
  `Valor modal`                                  = function(x) na_mean(x, option = "mode"),
  `Interpolación lineal`                         = function(x) na_interpolation(x, option = "linear"),
  `Interpolación spline`                         = function(x) na_interpolation(x, option = "spline"),
  `Interpolación de Stineman`                    = function(x) na_interpolation(x, option = "stine"),
  `Media móvil simple`                           = function(x) na_ma(x, weighting = "simple"),
  `Media móvil lineal ponderada`                 = function(x) na_ma(x, weighting = "linear"),
  `Media móvil ponderada exponencial`            = function(x) na_ma(x, weighting = "exponential"),
  `Última observación trasladada hacia adelante` = function(x) na_locf(x, option = "locf"),
  `Siguiente observación trasladada hacia atrás` = function(x) na_locf(x, option = "nocb"),
  `Descomposición estacional`                    = function(x) na_seadec(x, algorithm = "interpolation"),
  `División estacional`                          = function(x) na_seasplit(x),
  `Suavizado de Kalman en un modelo estructural` = function(x) na_kalman(x, model = "StructTS"),
  `Representación del espacio en estados de ARIMA y suavizado de Kalman` = function(x) na_kalman(x, model = "auto.arima")
)

# 5) Función principal
errores <- function(serie_train, serie_trainNA, indices_na, h = 1, serie_test, pronostico = "ARIMA") {
  
  errores_imp <- list()
  errores_pro <- list()
  
  for (nombre in names(imputadores)) {
    
    serie_imp <- imputadores[[nombre]](serie_trainNA)
    
    errores_imp[[nombre]] <- metricas_imputacion(serie_train, serie_imp, indices_na)
    
    fc <- ajustar_pronostico(serie_imp, modelo = pronostico, h = h)
    
    errores_pro[[nombre]] <- metricas_pronostico(fc, serie_test)
  }
  
  errores_imp_df <- do.call(rbind, errores_imp)
  errores_pro_df <- do.call(rbind, errores_pro)
  
  resultados <- cbind(
    Metodo = rownames(errores_imp_df),
    errores_imp_df,
    errores_pro_df
  )
  
  colnames(resultados) <- c("Metodo",
                            "RMSE_Imp", "MAE_Imp", "MAPE_Imp",
                            "RMSE_Pro", "MAE_Pro", "MAPE_Pro")
  
  resultados$Metodo <- factor(resultados$Metodo, levels = names(imputadores))
  resultados <- resultados[order(resultados$Metodo), ]
  
  return(resultados)
}


errores_prophet <- function(serie_train_NA, serie_test, h){
  
  # Generar fechas
  fechas <- seq.Date(
    from = as.Date(paste(start(serie_train_NA)[1], start(serie_train_NA)[2], "01", sep = "-")),
    by = "month",
    length.out = length(serie_train_NA)
  )
  
  df_prophet <- data.frame(
    ds = as.character(fechas), 
    y = as.numeric(serie_train_NA)
  )
  
  m <- prophet(
    seasonality.mode = 'multiplicative', 
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE 
  )
  
  m <- fit.prophet(m, df_prophet)
  fechas_todas <- seq.Date(
    from = min(fechas), 
    by = "month", 
    length.out = length(fechas) + h
  )
  futuro <- data.frame(ds = as.character(fechas_todas))
  
  pred <- predict(m, futuro)
  valores_pronosticados <- tail(pred$yhat, h)
  
  reales <- as.numeric(serie_test)[1:h]
  
  rmse_val <- sqrt(mean((valores_pronosticados - reales)^2))
  mae_val  <- mean(abs(valores_pronosticados - reales))
  mape_val <- mean(abs(valores_pronosticados - reales) / abs(reales)) * 100
  
  return(data.frame(
    Metodo = "Prophet",
    RMSE = rmse_val,
    MAE  = mae_val,
    MAPE = mape_val
  ))
}

#-------------------------------------------------------------------------------

ejecutar_simulacion <- function(serie_train, serie_test, mecanismo_na, prop, h, modelo, n_iteraciones = 100, resultado) {
  
  resultados_acum <- NULL
  iteraciones_exitosas <- 0
  
  cat("\nIniciando simulación:", resultado, "\n")
  
  for (i in 1:n_iteraciones) {
    if (i %% 10 == 0) cat("Iteración", i, "de", n_iteraciones, "...\n")
    
    serie_train_NA <- mecanismo_na(serie_train, prop = prop)
    indices_borrados <- which(is.na(serie_train_NA))
    
    res <- tryCatch({
      errores(
        serie_train, 
        serie_train_NA, 
        indices_borrados, 
        h = h, 
        serie_test,
        pronostico = modelo
      )
    }, error = function(e) {
      message("\n[!] Error en iteración ", i, " saltando. Detalle: ", e$message)
      return(NULL)
    })
    
    if (!is.null(res)) {
      iteraciones_exitosas <- iteraciones_exitosas + 1
      if (is.null(resultados_acum)) {
        resultados_acum <- res
      } else {
        resultados_acum[, -1] <- as.matrix(resultados_acum[, -1]) + as.matrix(res[, -1])
      }
    }
  }
  
  if (iteraciones_exitosas > 0) {
    resultados_finales <- resultados_acum
    resultados_finales[, -1] <- resultados_finales[, -1] / iteraciones_exitosas
    saveRDS(resultados_finales, file = resultado)
    cat("Completado. Guardado en:", resultado, "(", iteraciones_exitosas, "iteraciones exitosas)\n")
    return(resultados_finales)
  } else {
    stop("Todas las iteraciones fallaron.")
  }
}