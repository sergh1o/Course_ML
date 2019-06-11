# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Модуль 2/Data")

# считываем CSV-файл в датафрейм data
data <- read.csv2("results.csv")
# взглянем на первые 5 наблюдений
head(data, 5)

# вычисляем матрицу ошибок
confusion <- table(data$fact, data$predict)
# печатаем матрицу ошибок
confusion

# вычисляем общее количество наблюдений
n <- sum(confusion)
# вычисляем количество правильно классифицированных
# наблюдений отрицательного класса и количество
# правильно классифицированных наблюдений 
# положительного класса
diag <- diag(confusion)
# вычисляем правильность
accuracy <- sum(diag) / n
# печатаем правильность
accuracy

# устанавливаем пакет fmsb
# install.packages("fmsb")

# загружаем пакет fmsb
library(fmsb)

# вычисляем каппу Коэна
Kappa.test(data$fact, data$predict)

# вычисляем чувствительность
Sensitivity <- confusion[2,2] / (confusion[2,2] + confusion[2,1])
# вычисляем специфичность
Specificity <- confusion[1,1] / (confusion[1,1] + confusion[1,2])
# вычисляем 1 - специфичность
One_minus_specificity = 1 - Specificity
# вычисляем точность
Precision <- confusion[2,2] / (confusion[2,2] + confusion[1,2])

# печатаем значения чувствительности, специфичности,
# 1 минус специфичности и точности
output <- c("Чувствительность" = Sensitivity, 
            "Специфичность" = Specificity, 
            "1 - специфичность" = One_minus_specificity,
            "Точность" = Precision)
output

# вычисляем F1-меру
F1_score = 2*((Precision * Sensitivity)/(Precision + Sensitivity))
F1_score
