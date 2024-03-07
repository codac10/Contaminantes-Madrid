load("../datos_merged.RData")
d <- merged[["Casa de Campo"]]
d <- d[1:100,-1]
modelo <- lapply(d, function(x) auto.arima(x))
prediccion <- lapply(modelo, function(x)forecast(x, h=10))
mapply(modelo,prediccion, function(df,x) autoplot(ts(df)) + autolayer(x))
autoplot(ts(d[,4])) + autolayer(prediccion[[4]])

