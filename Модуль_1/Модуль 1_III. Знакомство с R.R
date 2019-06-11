# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Модуль_1/Data")

# загружаем данные
data <- read.csv2("StateFarm_missing.csv", 
                  sep = ";", 
                  dec = ".",
                  na.strings = "",
                  stringsAsFactors = TRUE)

# выводим первые 5 наблюдений
head(data, 5)

# смотрим типы переменных
str(data)

# разбиваем данные на обучающие и тестовые
set.seed(100)
ind <- sample(2, nrow(data), 
              replace = TRUE, 
              prob = c(0.7, 0.3))
development <- data[ind == 1, ]
holdout <- data[ind == 2, ]

# смотрим пропуски в обучающей выборке
sapply(development, function(x) sum(is.na(x)))

# смотрим пропуски в тестовой выборке
sapply(holdout, function(x) sum(is.na(x)))


# выполняем импутацию пропусков с помощью среднего, обратите внимание, 
# среднее вычислено для обучающей выборки и используется для импутации 
# пропусков в обучающей и тестовой выборках
development$Monthly.Premium.Auto[is.na(development$Monthly.Premium.Auto)] <- mean(development$Monthly.Premium.Auto, 
                                                                                  na.rm = TRUE)
holdout$Monthly.Premium.Auto[is.na(holdout$Monthly.Premium.Auto)] <- mean(development$Monthly.Premium.Auto, 
                                                                          na.rm = TRUE)

# устанавливаем пакет imputeMissings 
# install.packages("imputeMissings") 

# загружаем пакет imputeMissings 
library(imputeMissings)

# вычисляем медианы и моды, т.е. обучаем модель импутации
values <- imputeMissings::compute(development)
# применяем модель импутации к обучающему набору: пропуски в обучающем 
# наборе заменяем модами и медианами, вычисленными на обучающем наборе 
development <- imputeMissings::impute(development, object = values)
# применяем модель импутации к тестовому набору: пропуски в тестовом 
# наборе заменяем модами и медианами, вычисленными на обучающем наборе 
holdout <- imputeMissings::impute(holdout, object = values)

# смотрим пропуски в обучающей выборке
sapply(development, function(x) sum(is.na(x)))

# смотрим пропуски в тестовой выборке
sapply(holdout, function(x) sum(is.na(x)))

# устанавливаем пакет rpart 
# install.packages("rpart") 

# загружаем пакет rpart
library(rpart)

# строим модель дерева классификации CART
set.seed(42)
model <- rpart(Response ~ ., method = "class", data = development)

# получаем спрогнозированные значения зависимой переменной
# для тестовой выборки
predvalues <- predict(model, holdout, type = "class")

# получаем спрогнозированные вероятности классов зависимой переменной
# для тестовой выборки
probabilities <- predict(model, holdout, type = "prob")

# устанавливаем пакет caret 
# install.packages("caret") 

# загружаем пакет caret
library(caret)

# устанавливаем пакет e1071 
# install.packages("e1071") 

# загружаем пакет e1071
library(e1071)

# загружаем пакет randomForest
library(randomForest)

# конкатенируем обучающую и тестовую выборки
full <- rbind(development, holdout)

# выполняем 10-блочную перекрестную проверку
# для модели случайного леса
set.seed(42)
train_control <- trainControl(method = "cv", number = 10)
model <- train(Response ~ ., data = full, 
               method = "rf",
               trControl = train_control, 
               tuneGrid=data.frame(mtry = floor(sqrt(ncol(full)))))

# смотрим результаты
print(model)

# выполняем комбинированную проверку
# для модели случайного леса
tunegrid <- expand.grid(mtry = c(2:5))
set.seed(42)
model <- train(Response ~ ., data = development, 
               method = "rf",
               trControl = train_control, 
               tuneGrid=tunegrid)

# смотрим результаты
print(model)

# вычисляем правильность оптимальной модели
# на тестовой выборке, не участвовшей
# в обучении и настройке гиперпараметров
actual <- holdout$Response
predvalues <- predict(model, holdout)
cm <- as.matrix(table(actual, predvalues))
n <- sum(cm) # количество наблюдений
diag <- diag(cm) # количество правильно классифицированных
accuracy <- sum(diag) / n 
accuracy

