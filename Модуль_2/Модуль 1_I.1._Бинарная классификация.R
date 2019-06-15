# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Модуль_2/Data")

# считываем CSV-файл в датафрейм data
data <- read.csv2("results.csv", dec = ".")
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
# печатаем значение чувствительности 
output <- c("Чувствительность" = Sensitivity)
output

# вычисляем специфичность
Specificity <- confusion[1,1] / (confusion[1,1] + confusion[1,2])
# печатаем значение специфичности 
output <- c("Специфичность" = Specificity)
output

# вычисляем 1 - специфичность
One_minus_specificity = 1 - Specificity
# печатаем значение 1 - специфичности
output <- c("1 - специфичность" = One_minus_specificity)
output

# вычисляем точность
Precision <- confusion[2,2] / (confusion[2,2] + confusion[1,2])
# печатаем значение точности
output <- c("Точность" = Precision)
output

# вычисляем сбалансированную правильность
Bal_accuracy <- (Sensitivity + Specificity) / 2
# печатаем значение сбалансированной правильности
output <- c("Сбалансированная правильность" = Bal_accuracy)
output

# вычисляем F1-меру
F1_score = 2*((Precision * Sensitivity)/(Precision + Sensitivity))
F1_score

# снижаем пороговое значение спрогнозированной
# вероятности положительного класса до 0.3
table(data$fact == 1, data$probability >= 0.30)

# повышаем пороговое значение спрогнозированной
# вероятности положительного класса до 0.7
table(data$fact == 1, data$probability >= 0.70)

# создаем вектор фактических значений (классов)
# зависимой переменной
cls = c("P", "P", "P", "N", "P", "P", "N", "P", "P", "N", "N",
        "N", "N", "N", "P", "N", "N", "N", "N", "N")

# создаем вектор спрогнозированных вероятностей 
# положительного класса
score = c(0.92, 0.9, 0.88, 0.85, 0.82, 0.79, 0.75, 0.73, 0.72, 0.7, 
          0.6, 0.59, 0.58, 0.53, 0.52, 0.4, 0.33, 0.32, 0.24, 0.18)

# записываем положительные и отрицательные примеры 
pos = score[cls == "P"]
neg = score[cls == "N"]

# задаем стартовое значение генератора
# случайных чисел для воспроизводимости
set.seed(14)

# извлекаем случайным образом положительные и  
# отрицательные примеры и вычисляем долю случаев, 
# когда положительные примеры получили более 
# высокий ранг, чем отрицательные
p = replicate(200000, sample(pos, size = 1) > sample(neg, size = 1))
mean(p)

# загружаем пакет pROC
library(pROC)
# передаем функции roc() в качестве аргументов
# фактические значения зависимой переменной и 
# вероятности
roc  <- roc(data$fact, data$probability)
# строим ROC-кривую
plot.roc(roc)
# печатаем AUC-ROC
roc

# загружаем пакет precrec
library(precrec)

# строим PR-кривую (также строится ROC-кривая)
curves <- evalmod(scores = data$probability, 
                  labels = data$fact)
plot(curves)

# печатаем AUC-PR (также выводится AUC-ROC)
curves

# вычисляем logloss
LogLoss <- function(prob, fact){
  (-1 / length(prob)) * sum (fact * log(prob) + (1 - fact) * log(1 - prob))
}
LogLoss(data$probability, data$fact)