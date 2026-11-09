source("Funciones.R")

resultados <- ejecutar_pronosticos(
  serie_train = serie_train,
  serie_test = serie_test,
  modelos = c("ARIMA", "ETS", "Prophet"),
  horizontes = c(3, 6, 12),
  archivo = "Resultados/evaluacion_pronosticos.rds"
)
