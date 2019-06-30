# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Модуль_2/Data")

# считываем CSV-файл в датафрейм data
data <- read.csv2("results2.csv", dec = ".")
data

# вычисляем сумму квадратов отклонений фактических значений
# зависимой переменной от ее среднего значения
TSS <- sum((data$fact - (mean(data$fact)))^2)
# вычисляем сумму квадратов отклонений фактических 
# значений зависимой переменной от спрогнозированных
RSS <- sum((data$fact - data$pred)^2)
# вычисляем R-квадрат
R2 <- 1 - (RSS / TSS)
R2

# вычисляем MSE
MSE = (sum((data$fact - data$pred) ^ 2)) / nrow(data)
MSE

# вычисляем RMSE
RMSE = sqrt((sum((data$fact - data$pred) ^ 2)) / nrow(data))
RMSE

# вычисляем MAE
MAE <- sum(abs(data$fact - data$pred)) / nrow(data)
MAE

# вычисляем RMSLE
RMSLE = sqrt((1 / nrow(data)) * 
               sum((log(data$pred + 1) - log(data$fact + 1)) ^ 2))
RMSLE

# вычисляем MAPE
MAPE = mean(abs((data$fact - data$pred) / data$fact)) * 100
MAPE