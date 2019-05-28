# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Data")

# загружаем данные
data <- read.csv2("StateFarm.csv", 
                  sep = ";", 
                  na.strings = "")

# разбиваем данные на обучающие и тестовые
set.seed(100)
ind <- sample(2, nrow(data), 
              replace = TRUE, 
              prob = c(0.7, 0.3))
development <- data[ind == 1, ]
holdout <- data[ind == 2, ]

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
