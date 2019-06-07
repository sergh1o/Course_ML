# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Data")

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

# устанавливаем пакет imputeMissings 
# install.packages("imputeMissings") 

# загружаем пакет imputeMissings 
library(imputeMissings)

# вычисляем медианы и моды, т.е. обучаем модель импутации
values <- compute(development)
# применяем модель импутации к обучающему набору: пропуски в обучающем 
# наборе заменяем модами и медианами, вычисленными на обучающем наборе 
development <- impute(development, object = values)
# применяем модель импутации к тестовому набору: пропуски в тестовом 
# наборе заменяем модами и медианами, вычисленными на обучающем наборе 
holdout <- impute(holdout, object = values)

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
